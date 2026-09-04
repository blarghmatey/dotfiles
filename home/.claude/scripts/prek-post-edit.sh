#!/usr/bin/env sh
# PostToolUse hook: run prek against a just-edited file, scoped to its repo.
set -u

command -v jq >/dev/null 2>&1 || exit 0
command -v prek >/dev/null 2>&1 || exit 0

input=$(cat)
file=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')
[ -z "$file" ] && exit 0
[ -f "$file" ] || exit 0

dir=$(dirname "$file")
root=$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null) || exit 0
[ -f "$root/prek.toml" ] || [ -f "$root/.pre-commit-config.yaml" ] || exit 0

if out=$(cd "$root" && prek run --files "$file" --color=never 2>&1); then
  exit 0
fi

# Report on stderr, exit 2. For PostToolUse that is the only combination the
# agent ever sees: stdout goes to the debug log, and any other non-zero exit
# renders a "hook error" notice built from stderr -- empty if we wrote stdout.
# Exit 2 cannot block here (the edit already happened); it just surfaces.
printf '%s\n' "$out" | tail -n 40 >&2
exit 2
