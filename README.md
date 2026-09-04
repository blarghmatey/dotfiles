# dotfiles

Personal dotfiles managed by the `dots` CLI, across two independent
machine contexts: Arch Linux on WSL2 (`arch-wsl2` profile) and the Windows
host itself (`windows` profile). `dots` runs natively in each — the
`windows` profile is never reached through WSL interop.

## Quick start (fresh machine)

Arch/WSL2 side:

```bash
git clone https://github.com/blarghmatey/dotfiles ~/.dotfiles
cd ~/.dotfiles
./bootstrap.sh          # installs yay, uv, uvenv, and the dots CLI
dots bootstrap          # prerequisite tooling dots install assumes exists (yay, rustup toolchain)
dots sync               # symlink dotfiles into ~/
dots install all        # install system packages, Python tools, and npm globals
```

`bootstrap.sh` has no third-party dependencies — it only needs `bash`, `git`, and internet access.

Windows side (run in PowerShell, not WSL):

```powershell
git clone https://github.com/blarghmatey/dotfiles $HOME\.dotfiles
cd $HOME\.dotfiles
.\bootstrap.ps1                             # installs Scoop, uv, and the dots CLI
dots bootstrap --profile windows            # WSL2 feature/kernel, Scoop sanity check
dots install packages --profile windows     # installs manifest.toml's [profiles.windows.packages] scoop list
```

`bootstrap.ps1` has no third-party dependencies either. It's independent of
`bootstrap.sh` — running it doesn't touch or require WSL.

## Repository layout

```
bootstrap.sh          # one-shot setup for a fresh Arch/WSL2 machine
bootstrap.ps1          # one-shot setup for a fresh Windows machine
manifest.toml         # source of truth: profiles, packages, sync config
uvenv.lock            # locked Python CLI tools (managed by `dots freeze`)
home/                 # dotfiles — synced to ~/ by `dots sync`
dots/                 # the dots CLI source (Python, cyclopts)
  bootstrap.py         # yay/rustup toolchain (arch-wsl2), WSL2/Scoop (windows)
deploy/               # pyinfra deploy used by `dots install packages` (arch-wsl2 only)
  components/
    packages.py       # pacman + AUR via pyinfra
    node_tools.py     # npm globals via pyinfra
    python_tools.py   # uvenv thaw via pyinfra
```

## The `dots` CLI

```
dots bootstrap          Install prerequisite tooling (yay/rustup toolchain, or WSL2/Scoop)
dots sync               Symlink home/* into ~/; render .tmpl files in-place
dots status             Audit tracked dotfiles — show link/drift/template state
dots diff               Show what install would change (read-only)
dots install all        Install everything (windows skips the pyinfra-backed python/node/cargo/go steps)
dots install packages   System packages (pacman+AUR via pyinfra, or Scoop natively on windows)
dots install python     Python CLI tools only (uvenv thaw)
dots install node       npm global packages only
dots install skills     Agent skills only (npx skills add … --global)
dots upgrade            Upgrade all managed tools for --profile
dots freeze             Regenerate uvenv.lock from currently installed tools
```

`arch-wsl2` installs go through pyinfra (pacman, npm) or uvenv for
idempotent, distribution-aware execution. `windows` bypasses pyinfra
entirely — its Windows support is experimental — and calls Scoop directly.

## Profiles

Packages are grouped into profiles defined in `manifest.toml`: `arch-wsl2`
(the default) and `windows`. Pass `--profile <name>` to any command to
override — but run the command from the matching OS context, since each
profile's tooling only exists there.

```bash
dots install all --profile arch-wsl2
dots diff --profile arch-wsl2
```

```powershell
dots install packages --profile windows
dots diff --profile windows
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
