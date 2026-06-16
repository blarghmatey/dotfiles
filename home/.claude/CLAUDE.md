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
- `dots` CLI manages this dotfiles repo — see `dots --help`

## Git
- Commits focus on "why" not "what"
- Prefer creating new commits over amending published ones
- GPG signing is enabled; never skip hooks with `--no-verify`
- Worktree workflow supported (`git wt`, `pi-worktrees`)

## Project Conventions
- dotfiles repo: `~/code/personal/dotfiles`, managed via `dots` CLI
- `manifest.toml` is the source of truth for packages/profiles
- `home/` mirrors `~/` — files are symlinked by `dots sync`
- Sensitive configs (API keys, tokens) use `{{ pass:name }}` templates
