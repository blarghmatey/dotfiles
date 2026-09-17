#!/usr/bin/env sh
# PreToolUse gate: pause the first attempt to open a PR so the
# pre-submit checks (create-ol-pull-request skill, Step 5) happen
# before the PR opens. Re-running the identical call a second time is let
# through -- this is a one-time speed bump, not a permanent block.
set -u

command -v jq >/dev/null 2>&1 || exit 0

input=$(cat)
tool_name=$(printf '%s' "$input" | jq -r '.tool_name // empty')

case "$tool_name" in
  mcp__github__create_pull_request) : ;;
  *)
    command_str=$(printf '%s' "$input" | jq -r '.tool_input.command // empty')
    # Check each command segment (split on ; & | newline) on its own, so a
    # flag from a neighboring command can't turn a read into a write.
    opens_pr=false
    segments=$(printf '%s\n' "$command_str" | sed 's/[;&|]/\n/g')
    while IFS= read -r seg; do
      case "$seg" in *gh*) : ;; *) continue ;; esac
      w='(^|[^[:alnum:]_-])gh[[:space:]]+'
      if printf '%s' "$seg" | grep -Eq "${w}pr[[:space:]]+((-R|--repo)([[:space:]]+|=)[^[:space:]]+[[:space:]]+)?(create|new)([[:space:]]|\$)" \
        || printf '%s' "$seg" | grep -Eq "${w}stack[[:space:]]+(submit|link)([[:space:]]|\$)" \
        || { printf '%s' "$seg" | grep -Eq "${w}api[[:space:]].*graphql" \
          && printf '%s' "$seg" | grep -q 'createPullRequest'; }; then
        opens_pr=true
      elif printf '%s' "$seg" | grep -Eq "${w}api[[:space:]].*repos/[^/[:space:]]+/[^/[:space:]]+/pulls([[:space:]\"']|\$)" \
        && ! printf '%s' "$seg" | grep -Eq '[[:space:]](-XGET|(-X|--method)[[:space:]=]*GET)([[:space:]]|$)' \
        && printf '%s' "$seg" | grep -Eq '[[:space:]](-f|-F|--field|--raw-field|--input|-XPOST|(-X|--method)[[:space:]=]*POST)([[:space:]=]|$)'; then
        # gh api defaults to POST once fields or --input are given.
        opens_pr=true
      fi
    done <<EOF
$segments
EOF
    [ "$opens_pr" = true ] || exit 0
    ;;
esac

tmp="${TMPDIR:-/tmp}"
# Key the marker on the whole call, exactly what ai-drift-gate.py hashes.
# Both gates run on the same call and each passes on an identical repeat, so
# keying them on different things (or expiring one) leaves them out of phase
# and every repeat hits one gate or the other.
hash=$(printf '%s' "$input" | jq -cS '[.tool_name, .tool_input]' | cksum | cut -d' ' -f1)
marker="$tmp/claude-pr-audit-seen-$hash"

if [ -f "$marker" ]; then
  rm -f "$marker"
  exit 0
fi

touch "$marker"
echo "Before creating this PR (create-ol-pull-request skill, Step 5): (a) claims in the title, body, commit messages, and added text checked against live evidence; (b) a fresh subagent ran code-review against the stated goals, including goal alignment and security, and every blocking finding was fixed or decided by the user; (c) every commit scanned for secrets; (d) the user approved anything that changed and any open findings. If all are done, repeat this exact call to proceed. If not, do them first." >&2
exit 2
