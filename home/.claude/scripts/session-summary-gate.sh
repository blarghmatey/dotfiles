#!/usr/bin/env sh
# Stop hook: ask for a DONE / BLOCKED / DEFERRED close-out before a working
# session ends, so leftovers get named instead of rediscovered next time.
#
# Off unless ~/.claude/session-summary.json exists with {"enabled": true},
# so merging this changes nothing until I turn it on.
#
# Verified contract (raw docs at code.claude.com/docs/en/hooks.md, grepped
# rather than summarized): Stop receives {session_id, transcript_path,
# last_assistant_message, stop_hook_active, ...} on stdin. stop_hook_active is
# true when Claude Code is ALREADY continuing because of a stop hook, so a hook
# that wants to speak once exits early on true. Returning
# hookSpecificOutput.additionalContext keeps the turn going with the same loop
# protections as a block (stop_hook_active plus an 8-continuation cap) but
# reads as hook feedback rather than a hook error. Local guards on top: fire at
# most once per session, and stay quiet when the closing message already has a
# close-out.
set -u

command -v jq >/dev/null 2>&1 || exit 0

config="${CLAUDE_SESSION_SUMMARY_CONFIG:-$HOME/.claude/session-summary.json}"
[ -f "$config" ] || exit 0
if ! jq -e 'has("enabled")' "$config" >/dev/null 2>&1; then
  exit 0
fi
if ! jq -e '.enabled == true' "$config" >/dev/null 2>&1; then
  # A string "true" or 1 is a typo, not a toggle. Say so rather than looking
  # identical to the feature working and staying quiet.
  jq -e '.enabled == false' "$config" >/dev/null 2>&1 ||
    printf 'session-summary: "enabled" in %s must be the boolean true or false.\n' "$config" >&2
  exit 0
fi

input=$(cat)

# Already continuing because a stop hook asked for more: say nothing.
[ "$(printf '%s' "$input" | jq -r '.stop_hook_active // false')" = "true" ] && exit 0

session=$(printf '%s' "$input" | jq -r '.session_id // empty')
transcript=$(printf '%s' "$input" | jq -r '.transcript_path // empty')
last=$(printf '%s' "$input" | jq -r '.last_assistant_message // ""')

# A close-out already written: one of the labels heads a line. Matching only
# line starts keeps a passing mention ("the template mentions DONE") from
# counting, and matching any one label allows the empty buckets to be dropped.
if printf '%s\n' "$last" | grep -Eq '^[[:space:]]*[*>#-]*[[:space:]]*(DONE|BLOCKED|DEFERRED)\b'; then
  exit 0
fi

tmp="${TMPDIR:-/tmp}"
marker="$tmp/claude-session-summary-${session:-unknown}"
[ -f "$marker" ] && exit 0

# Only sessions that changed something. Subagent tool calls live in their own
# transcripts under <dir>/<session id>/subagents/, so a session that delegates
# its edits still counts. Writes under a scratchpad or /tmp don't.
min_changes=$(jq -r '.min_changes // 1' "$config")
case "$min_changes" in '' | *[!0-9]*) min_changes=1 ;; esac
[ -r "$transcript" ] || exit 0

subagents="$(dirname "$transcript")/${session}/subagents"
set -- "$transcript"
if [ -d "$subagents" ]; then
  for f in "$subagents"/*.jsonl; do
    [ -r "$f" ] && set -- "$@" "$f"
  done
fi

changes=$(jq -rs '
  def scratch: test("^(/tmp/|/var/tmp/|/private/var/folders/)");
  [ .[]
    | select(.message.content? != null)
    | .message.content[]?
    | select(type == "object" and .type == "tool_use")
    | if (.name == "Edit" or .name == "Write" or .name == "MultiEdit" or .name == "NotebookEdit"
           or .name == "mcp__filesystem__write_file" or .name == "mcp__filesystem__edit_file")
      then (if ((.input.file_path // .input.path // "") | scratch) then empty else 1 end)
    elif (.name | test("^mcp__github__(create_pull_request|push_files|create_or_update_file|create_issue|merge_pull_request)$"))
      then 1
    elif (.name == "Bash"
          and ((.input.command // "")
               | test("git( +-C +[^ ]+)* +(commit|push)|gh +pr +(create|merge)|gh +issue +create")))
      then 1
    else empty end
  ] | length' "$@" 2>/dev/null)
case "${changes:-0}" in '' | *[!0-9]*) changes=0 ;; esac
[ "$changes" -ge "$min_changes" ] || exit 0

template=$(jq -r '.template // empty' "$config")
# A leading "~/" in the config is text, not a shell expansion, so expand it here.
tilde='~'
case "$template" in
  "$tilde"/*) template="$HOME/${template#"$tilde"/}" ;;
esac
[ -n "$template" ] || template="$HOME/.claude/session-summary-template.md"

# -f, not -r: a directory is readable and would make cat print an error as the
# whole instruction.
if [ -f "$template" ] && [ -r "$template" ]; then
  text=$(cat "$template")
else
  text="Before finishing, close out with DONE, BLOCKED and DEFERRED, leaving out any
bucket that is empty. DONE names the evidence for each line, BLOCKED names the
external blocker and who clears it, DEFERRED gets a filed witan task id.
(Add $template to customize this wording.)"
fi
[ -n "$text" ] || exit 0

touch "$marker"
jq -n --arg t "$text" \
  '{hookSpecificOutput: {hookEventName: "Stop", additionalContext: $t}}'
exit 0
