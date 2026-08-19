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

out=$(cd "$root" && prek run --files "$file" --color=never 2>&1)
code=$?
printf '%s\n' "$out" | tail -n 40
exit "$code"
