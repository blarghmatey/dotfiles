# Global Claude Code Instructions

## Environment
- OS: Arch Linux on WSL2 (Manjaro)
- Shell: zsh with starship prompt, atuin history, zoxide navigation
- Preferred editor: Neovim / Emacs
- Python: managed via `uv` (never system pip for tooling)
- Node: npm globals; bun available

## Communication Style
- Terse, direct responses — no trailing summaries of what was just done
- No emojis unless explicitly requested
- Reference code as `file_path:line_number` format
- One-sentence updates while working; brief is good, silent is not

## Code Preferences
- Python: use `uv` for environments, `cyclopts` for CLIs, `ruff` for linting
- Avoid premature abstractions — solve exactly what's needed, no more
- No defensive error handling for internal invariants
- Default to no comments; add only when the WHY is non-obvious
- No backwards-compatibility shims when you can just change the code

## Shell & Tools
- Prefer `rg` (ripgrep) over `grep` for code search
- `bat` for viewing files when suggesting terminal commands
- `eza` instead of `ls`, `fd` instead of `find` where appropriate
- `delta` for git diffs
- `just` for project task runners (prefer over Makefile)

## Repo Context Files
- Always check the repo root for an `AGENTS.md` before relying on these global
  defaults — it takes precedence for anything it covers.
- When a repo has no context file yet and one is needed, create `AGENTS.md`, not
  `CLAUDE.md`. Exception: this file itself (`~/.claude/CLAUDE.md`, synced from
  `home/.claude/CLAUDE.md` in the dotfiles repo) stays `CLAUDE.md` — Claude Code
  loads that specific name for global config, so it isn't a per-repo context file.

## Git
- Commits focus on "why" not "what"
- Prefer creating new commits over amending published ones
- GPG signing is enabled; never skip hooks with `--no-verify`
- Worktree workflow supported (`git wt`, `pi-worktrees`)
- Before making code changes in any git repo, call `EnterWorktree` first so concurrent
  work doesn't stomp on the same files. Skip only when the user explicitly says to edit
  in place. Prefer basing the worktree off of the latest commit of the default branch.
