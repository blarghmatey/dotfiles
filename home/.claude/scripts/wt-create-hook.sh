#!/usr/bin/env sh
# WorktreeCreate hook: redirect Claude Code's worktrees from the default
# .claude/worktrees/<name> (nested) to <repo>.worktrees/<name> (sibling),
# matching the git-wt (wt.basedir) and pi (pi-worktrees.config.json)
# convention so all three tools land worktrees in the same place.
#
# Verified contract (tested directly, not just documented): stdin is
# {"cwd": "<repo root>", "name": "<branch/slug>", ...}; this hook must
# create the worktree itself and print its path as the LAST line of stdout
# -- everything else must go to stderr.
set -u

command -v jq >/dev/null 2>&1 || exit 0

input=$(cat)
cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')
name=$(printf '%s' "$input" | jq -r '.name // empty')
[ -z "$cwd" ] || [ -z "$name" ] && exit 0

new_path="${cwd}.worktrees/${name}"
mkdir -p "$(dirname "$new_path")"

if git -C "$cwd" show-ref --verify --quiet "refs/heads/$name"; then
  git -C "$cwd" worktree add "$new_path" "$name" 1>&2
else
  git -C "$cwd" worktree add "$new_path" -b "$name" 1>&2
fi

echo "$new_path"
