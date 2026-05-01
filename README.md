# dotfiles

Personal dotfiles for Arch Linux / WSL2, managed by the `dots` CLI.

## Quick start (fresh machine)

```bash
git clone https://github.com/blarghmatey/dotfiles ~/.dotfiles
cd ~/.dotfiles
./bootstrap.sh          # installs yay, uv, uvenv, and the dots CLI
dots sync               # symlink dotfiles into ~/
dots install all        # install system packages, Python tools, and npm globals
```

`bootstrap.sh` has no third-party dependencies — it only needs `bash`, `git`, and internet access.

## Repository layout

```
bootstrap.sh          # one-shot setup for a fresh machine
manifest.toml         # source of truth: profiles, packages, sync config
uvenv.lock            # locked Python CLI tools (managed by `dots freeze`)
home/                 # dotfiles — synced to ~/ by `dots sync`
dots/                 # the dots CLI source (Python, cyclopts)
deploy/               # pyinfra deploy used by `dots install`
  components/
    packages.py       # pacman + AUR via pyinfra
    node_tools.py     # npm globals via pyinfra
    python_tools.py   # uvenv thaw via pyinfra
```

## The `dots` CLI

```
dots sync               Symlink home/* into ~/; render .tmpl files in-place
dots status             Audit tracked dotfiles — show link/drift/template state
dots diff               Show what install would change (read-only)
dots install all        Install everything: packages → python tools → npm globals → skills
dots install packages   System packages only (pacman + AUR via pyinfra)
dots install python     Python CLI tools only (uvenv thaw)
dots install node       npm global packages only
dots install skills     Agent skills only (npx skills add … --global)
dots upgrade            Upgrade all managed tools (pacman -Syu, uvenv, npm)
dots freeze             Regenerate uvenv.lock from currently installed tools
```

All install and remove operations use pyinfra (pacman, npm) or uvenv for
idempotent, distribution-aware execution.

## Profiles

Packages are grouped into profiles defined in `manifest.toml`. The default
profile is `arch-wsl2`. Pass `--profile <name>` to any command to override.

```bash
dots install all --profile arch-wsl2
dots diff --profile arch-wsl2
```

## Package management

### Adding a package

1. Add it to the appropriate list in `manifest.toml` under `[profiles.<name>.packages]`
2. Run `dots install packages` — it will be installed

### Removing a package

1. Remove it from `manifest.toml`
2. Run `dots install packages` — the CLI detects the removal via its state file
   (`~/.local/share/dots/state.toml`), prompts for confirmation, and removes it

### Python CLI tools

Python tools are managed by [uvenv](https://github.com/tizz98/uvenv) (a
pipx-like tool built on uv) and locked in `uvenv.lock`.

```bash
uvenv install <tool>    # install a new tool
dots freeze             # update uvenv.lock to reflect installed tools
dots install python     # restore all tools from uvenv.lock on a new machine
```

## Dotfile syncing

`dots sync` creates symlinks from `~/` into `home/` for every tracked file.
Template files (`.tmpl` suffix) are rendered in-place instead of symlinked;
they support two interpolation forms:

| Syntax | Resolved via |
|--------|-------------|
| `{{ pass:secret-name }}` | `pass show secret-name` |
| `{{ env:VAR_NAME }}` | current environment |

Sensitive files (`.npmrc`, MCP configs) are flagged by `dots status` but are
still synced — never commit real values; use templates instead.

## Agent skills

Agent skills are managed by the [Vercel Skills CLI](https://skills.sh) and locked in `skills-lock.json`.

```bash
# Restore all skills on a new machine
dots install skills

# Update all installed skills to latest
npx skills update -g

# Add a new skill and update the lockfile
npx skills add <owner/repo> -g --all -y
npx skills list -g --json > /tmp/new.json   # then update skills-lock.json
```

58 skills are currently tracked across 10 source packages:

| Source package | Skills |
|----------------|--------|
| `dbt-labs/dbt-agent-skills` | dbt workflows, semantic layer, unit tests, migrations |
| `mitodl/agent-kit` | cyclopts, uv, docker, dagster, pulumi, standup, process |
| `dagster-io/skills` | dagster, dg CLI, dignified-python |
| `flutter/skills` | 18 Flutter development skills |
| `pulumi/agent-skills` | Pulumi IaC patterns |
| `anthropics/skills` | doc co-authoring, frontend design, MCP builder |
| `vercel-labs/skills` | find-skills |
| `muratcankoylan/agent-skills-for-context-engineering` | context engineering collection |
| `astronomer/agents` | warehouse-init |
| `pluginagentmarketplace/custom-plugin-sql` | data warehouse design |

`dots diff` also reports missing skills alongside missing packages.

The following AI coding tools are configured and installed:

| Tool | Config location |
|------|----------------|
| GitHub Copilot CLI | `home/.copilot/mcp-config.json` |
| Claude Code | `home/.claude/settings.json` |
| KiloCode | via npm (`@kilocode/cli`) |
| Gemini CLI | via npm (`@google/gemini-cli`) |
| Sourcegraph Amp | via npm (`@sourcegraph/amp`) |
| Emacs (lsp-mode + copilot.el) | `home/.emacs.d/init.el` |

API keys are loaded at runtime from [`pass`](https://www.passwordstore.org/)
or environment variables — never stored in this repository.

## Emacs

Config lives in `home/.emacs.d/`. Key files:

| File | Purpose |
|------|---------|
| `init.el` | Main config; all packages via `use-package` + `straight.el` |
| `functions.el` | User-defined helper functions |
| `configurations.el` | Global settings, keybindings, display |

Emacs Custom is redirected to `~/.emacs.d/custom.el` (gitignored) so that
packages can write API keys and theme preferences without polluting the
tracked config.

## Security notes

- Passwords/tokens use [`pass`](https://www.passwordstore.org/) (GPG-encrypted store)
- Email credentials use GPG-encrypted files (`~/.mail/*/creds.gpg`)
- `~/.emacs.d/custom.el` is gitignored — never committed
- `home/.npmrc` is a template; the real file is rendered from `.npmrc.tmpl`
