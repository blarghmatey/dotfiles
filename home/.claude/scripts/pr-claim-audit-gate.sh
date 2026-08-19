#!/usr/bin/env sh
# PreToolUse gate: pause the first attempt at a `gh pr create` command so the
# claim audit (create-ol-pull-request skill, Step 5) happens before the PR
# opens. Re-running the identical command a second time is let through --
# this is a one-time speed bump, not a permanent block.
set -u

command -v jq >/dev/null 2>&1 || exit 0

input=$(cat)
command_str=$(printf '%s' "$input" | jq -r '.tool_input.command // empty')

case "$command_str" in
  *"gh pr create"*) : ;;
  *) exit 0 ;;
esac

tmp="${TMPDIR:-/tmp}"
hash=$(printf '%s' "$command_str" | cksum | cut -d' ' -f1)
marker="$tmp/claude-pr-audit-seen-$hash"

if [ -f "$marker" ]; then
  rm -f "$marker"
  exit 0
fi

touch "$marker"
echo "Before creating this PR: have you audited every factual/behavioral claim in the body against live evidence (create-ol-pull-request skill, Step 5)? If yes, run this exact gh pr create command again to proceed. If not, do the audit first." >&2
exit 2
