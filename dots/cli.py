"""dots — dotfiles and system management CLI."""

from __future__ import annotations

import os
import subprocess
import tomllib
from pathlib import Path
from typing import Annotated

import cyclopts
from rich.console import Console

app = cyclopts.App(
    name="dots",
    help="Manage dotfiles and system setup.",
    version_flags=["--version", "-V"],
    version="0.1.0",
)

install = cyclopts.App(name="install", help="Install packages and tools.")
app.command(install, name="install")

repos = cyclopts.App(name="repos", help="Index a directory tree of git checkouts.")
app.command(repos, name="repos")

console = Console()
DEFAULT_PROFILE = "arch-wsl2"


def _find_repo_root() -> Path:
    """Walk up from this file looking for manifest.toml (robust for editable installs)."""
    if repo := os.environ.get("DOTS_REPO"):
        return Path(repo).expanduser().resolve()
    candidate = Path(__file__).parent
    for _ in range(6):
        if (candidate / "manifest.toml").exists():
            return candidate
        candidate = candidate.parent
    msg = "Cannot locate dotfiles repo root. Set the DOTS_REPO environment variable."
    raise RuntimeError(msg)


REPO_ROOT = _find_repo_root()
HOME = Path.home()


def _validate_profile(profile: str) -> None:
    """Reject a profile the manifest doesn't define.

    Without this an unknown profile resolves to an empty dict at every lookup,
    so the command reports "nothing to do" and exits 0 rather than erroring.
    """
    with (REPO_ROOT / "manifest.toml").open("rb") as f:
        known = sorted(tomllib.load(f).get("profiles", {}))
    if profile not in known:
        console.print(
            f"[red]Unknown profile[/red] {profile!r} — manifest.toml defines: {', '.join(known)}"
        )
        raise SystemExit(2)


# ── top-level commands ────────────────────────────────────────────────────────


@app.command
def bootstrap(*, profile: str = DEFAULT_PROFILE) -> None:
    """Install prerequisite tooling for *profile* that `dots install` assumes exists.

    arch-wsl2: yay (AUR helper), a default rustup toolchain.
    windows: the WSL2 feature/kernel, and Scoop. Run natively on Windows —
    see bootstrap.ps1 to get `dots` installed there in the first place.
    """
    from .bootstrap import bootstrap as run_bootstrap

    _validate_profile(profile)
    run_bootstrap(profile)


@app.command
def sync(
    *,
    dry_run: Annotated[bool, cyclopts.Parameter(name=["--dry-run", "-n"])] = False,
    force: Annotated[bool, cyclopts.Parameter(name=["--force", "-f"])] = False,
) -> None:
    """Symlink all tracked dotfiles into ~/; render templates in-place."""
    from .sync import sync_all

    sync_all(REPO_ROOT, HOME, force=force, dry_run=dry_run)


@app.command
def status() -> None:
    """Audit all tracked dotfiles — show link/template/drift state."""
    from .status import print_status

    print_status(REPO_ROOT, HOME)


@app.command
def freeze() -> None:
    """Regenerate uvenv.lock from currently installed Python tools."""
    lockfile = REPO_ROOT / "uvenv.lock"
    subprocess.run(["uvenv", "freeze", "--filename", str(lockfile)], check=True)
    console.print(f"[green]✓[/green] Regenerated [bold]{lockfile.name}[/bold]")
    console.print("[dim]Review: git diff uvenv.lock[/dim]")


@app.command
def upgrade(*, profile: str = DEFAULT_PROFILE) -> None:
    """Upgrade all managed tools for *profile*: pacman or scoop, uvenv, npm globals, Claude Code, pi."""
    from .claude import upgrade_claude
    from .install import upgrade_all
    from .pi import upgrade_pi

    _validate_profile(profile)
    upgrade_all(profile)
    upgrade_claude()
    upgrade_pi()


@app.command
def diff(*, profile: str = DEFAULT_PROFILE) -> None:
    """Show what install would change — queries each package manager, no execution.

    Compares the manifest and uvenv.lock against actually installed packages and
    prints a per-category table of what is present vs missing.
    """
    from .claude import diff_claude
    from .diff import print_diff
    from .pi import diff_pi
    from .skills import diff_skills

    _validate_profile(profile)
    print_diff(REPO_ROOT, profile)
    diff_pi(REPO_ROOT)
    diff_claude(REPO_ROOT)
    diff_skills(REPO_ROOT)


@install.command(name="claude")
def claude_code() -> None:
    """Install the Claude Code CLI globally via npm."""
    from .claude import install_claude

    install_claude(REPO_ROOT)


@install.command(name="pi")
def pi_extensions() -> None:
    """Install the pi coding-agent CLI globally and all extensions from settings.json."""
    from .pi import install_pi

    install_pi(REPO_ROOT)


@install.command
def skills(
    *,
    yes: Annotated[bool, cyclopts.Parameter(name=["--yes", "-y"])] = False,
) -> None:
    """Install global agent skills from skills-lock.json via the Vercel Skills CLI."""
    from .skills import install_skills

    install_skills(REPO_ROOT, yes=yes)


@install.command
def packages(
    *,
    profile: str = DEFAULT_PROFILE,
    verbose: Annotated[bool, cyclopts.Parameter(name=["--verbose", "-v"])] = False,
) -> None:
    """Install system packages for *profile*.

    arch-wsl2: pacman + AUR via pyinfra. windows: Scoop, natively, no pyinfra.
    """
    from .install import install_packages

    _validate_profile(profile)
    install_packages(REPO_ROOT, profile, verbose=verbose)


@install.command(name="python")
def python_tools() -> None:
    """Install Python CLI tools via uvenv thaw."""
    from .install import install_python

    install_python(REPO_ROOT)


@install.command
def node(*, profile: str = DEFAULT_PROFILE) -> None:
    """Install global npm packages."""
    from .install import install_node

    _validate_profile(profile)
    install_node(REPO_ROOT, profile)


@install.command(name="cargo")
def cargo_tools() -> None:
    """Install Rust/Cargo tools from manifest [cargo].tools."""
    from .install import install_cargo

    install_cargo(REPO_ROOT)


@install.command(name="go")
def go_tools() -> None:
    """Install Go tools from manifest [go].tools."""
    from .install import install_go

    install_go(REPO_ROOT)


@repos.command
def manifest(
    *,
    root: Annotated[Path | None, cyclopts.Parameter(name=["--root", "-r"])] = None,
) -> None:
    """Regenerate MANIFEST.yaml for ROOT: org/url/description/topics/worktrees per repo.

    Scans ROOT for git checkouts, pulls description and topics from GitHub
    (batched, github.com only), and writes ROOT/MANIFEST.yaml. Any `note` you've
    hand-written for a repo is preserved across regenerations.
    """
    from .repos import build_manifest, print_summary

    root = (root or Path.cwd()).expanduser().resolve()
    manifest_path = root / "MANIFEST.yaml"
    found = build_manifest(root, manifest_path)
    print_summary(found, manifest_path)


@install.command(name="all")
def install_all(
    *,
    profile: str = DEFAULT_PROFILE,
    verbose: Annotated[bool, cyclopts.Parameter(name=["--verbose", "-v"])] = False,
    yes: Annotated[bool, cyclopts.Parameter(name=["--yes", "-y"])] = False,
) -> None:
    """Run all install subcommands in order.

    arch-wsl2: packages → python → node → cargo → go → claude → pi → skills.
    windows: packages → claude → pi → skills. The python/node/cargo/go steps all
    go through pyinfra, whose Windows local connector is experimental, and uvenv/
    cargo/go aren't part of the native-Windows install to begin with.
    """
    _validate_profile(profile)
    packages(profile=profile, verbose=verbose)
    if profile != "windows":
        python_tools()
        node(profile=profile)
        cargo_tools()
        go_tools()
    claude_code()
    pi_extensions()
    skills(yes=yes)


if __name__ == "__main__":
    app()
