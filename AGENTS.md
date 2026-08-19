# dotfiles

## Project Conventions
- Managed via the `dots` CLI — see `dots --help`
- `manifest.toml` is the source of truth for packages/profiles
- `home/` mirrors `~/` — files are symlinked into place by `dots sync`
- Sensitive configs (API keys, tokens) use `{{ pass:name }}` templates

## Note on home/.claude/CLAUDE.md
This repo also contains `home/.claude/CLAUDE.md`, which syncs to
`~/.claude/CLAUDE.md` — Claude Code's *global* instructions file, not a
per-repo context file. Keep it named `CLAUDE.md`; don't rename it to
`AGENTS.md`.
