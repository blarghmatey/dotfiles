#!/usr/bin/env sh
# SessionStart hook: say so when the session starts in a repo's shared
# checkout rather than a worktree. Concurrent sessions switch branches and
# commit there, which has cost me commits and left stale base branches.
#
# Verified contract (docs, code.claude.com/docs/en/hooks): SessionStart gets
# {"cwd": ..., "source": "startup"|"resume"|"clear"|"compact"|"fork", ...} on
# stdin, plain stdout is added to the session context, and exit code 2 is not
# supported for this event. So this warns and never blocks.
set -u

# An inherited GIT_DIR/GIT_WORK_TREE would point git at some other repo and
# make every answer below describe a directory the session isn't in.
unset GIT_DIR GIT_WORK_TREE GIT_COMMON_DIR

command -v jq >/dev/null 2>&1 || exit 0
command -v git >/dev/null 2>&1 || exit 0

input=$(cat)
cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')
[ -n "$cwd" ] || exit 0
[ -d "$cwd" ] || exit 0

toplevel=$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null) || exit 0
[ -n "$toplevel" ] || exit 0

# In a linked worktree the git dir is <common>/worktrees/<name>, so the two
# differ; in every shared checkout they are the same path. Comparing them
# beats stripping "/.git" off the common dir, which misses submodules,
# separate-git-dir checkouts, and a symlinked .git (verified on git 2.55).
gitdir=$(git -C "$cwd" rev-parse --path-format=absolute --git-dir 2>/dev/null) || exit 0
common=$(git -C "$cwd" rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || exit 0
[ "$gitdir" = "$common" ] || exit 0

# Drop the main worktree (git lists it first) and any prunable entry, whose
# directory no longer exists.
existing=$(git -C "$cwd" worktree list --porcelain 2>/dev/null | awk '
  /^worktree /   { path = substr($0, 10); prunable = 0; next }
  /^prunable/    { prunable = 1; next }
  /^[[:space:]]*$/ { if (path != "" && !prunable) print path; path = ""; prunable = 0; next }
  END            { if (path != "" && !prunable) print path }
' | tail -n +2)

printf 'Shared checkout: this session started in %s, which is the main checkout, not a worktree.\n' "$toplevel"
printf 'Another session can switch branches or commit here while you edit. Call EnterWorktree before the first edit unless the user asked for an in-place change.\n'
if [ -n "$existing" ]; then
  printf 'Existing worktrees for this repo:\n%s\n' "$existing"
else
  printf 'This repo has no worktrees yet.\n'
fi
