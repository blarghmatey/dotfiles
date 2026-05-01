"""dots — dotfiles and system management CLI."""

from __future__ import annotations

import os
import subprocess
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
    raise RuntimeError(
        "Cannot locate dotfiles repo root. Set the DOTS_REPO environment variable."
    )


REPO_ROOT = _find_repo_root()
HOME = Path.home()


# ── top-level commands ────────────────────────────────────────────────────────


@app.command
def sync(
    *,
    dry_run: Annotated[bool, cyclopts.Parameter(name=["--dry-run", "-n"])] = False,
    force: Annotated[bool, cyclopts.Parameter(name=["--force", "-f"])] = False,
    profile: str = DEFAULT_PROFILE,
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
    """Upgrade all managed tools: pacman, uvenv, npm globals."""
    from .install import upgrade_all

    upgrade_all(profile)


@app.command
def diff(*, profile: str = DEFAULT_PROFILE) -> None:
    """Show what install would change — queries each package manager, no execution.

    Compares the manifest and uvenv.lock against actually installed packages and
    prints a per-category table of what is present vs missing.
    """
    from .diff import print_diff
    from .skills import diff_skills

    print_diff(REPO_ROOT, profile)
    diff_skills(REPO_ROOT)


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
    """Install system packages via pyinfra (pacman + AUR)."""
    from .install import install_packages

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

    install_node(REPO_ROOT, profile)


@install.command(name="all")
def install_all(
    *,
    profile: str = DEFAULT_PROFILE,
    verbose: Annotated[bool, cyclopts.Parameter(name=["--verbose", "-v"])] = False,
    yes: Annotated[bool, cyclopts.Parameter(name=["--yes", "-y"])] = False,
) -> None:
    """Run all install subcommands in order: packages → python → node → skills."""
    packages(profile=profile, verbose=verbose)
    python_tools()
    node(profile=profile)
    skills(yes=yes)


if __name__ == "__main__":
    app()
