from __future__ import annotations

import subprocess
import sys
import tomllib
from pathlib import Path

from rich.console import Console

console = Console()

_PYINFRA = [sys.executable, "-m", "pyinfra"]


def install_packages(repo_root: Path, profile: str, *, verbose: bool = False) -> None:
    """Run the pyinfra deploy to install system packages."""
    deploy_script = repo_root / "deploy" / "deploy.py"
    console.print(
        f"[bold]Running pyinfra deploy[/bold]  profile=[cyan]{profile}[/cyan]"
    )
    cmd = [*_PYINFRA, "@local", str(deploy_script), "--data", f"profile={profile}", "-y"]
    if verbose:
        cmd.append("-v")
    subprocess.run(cmd, check=True, cwd=str(repo_root))


def install_python(repo_root: Path) -> None:
    """Install Python CLI tools by thawing uvenv.lock."""
    lockfile = repo_root / "uvenv.lock"
    console.print(f"[bold]Installing Python tools[/bold] via uvenv thaw ({lockfile.name})")
    subprocess.run(
        ["uvenv", "thaw", "--filename", str(lockfile), "--skip-current"],
        check=True,
    )


def install_node(repo_root: Path, profile: str) -> None:
    """Install global npm packages listed in the manifest for *profile*."""
    with open(repo_root / "manifest.toml", "rb") as f:
        manifest = tomllib.load(f)

    packages: list[str] = (
        manifest.get("profiles", {})
        .get(profile, {})
        .get("node", {})
        .get("global", [])
    )

    if not packages:
        console.print(f"[dim]No npm packages defined for profile {profile!r}[/dim]")
        return

    console.print(f"[bold]Installing {len(packages)} global npm packages[/bold]")
    subprocess.run(["npm", "install", "-g", *packages], check=True)


def upgrade_all(profile: str) -> None:
    """Upgrade system packages, Python tools, and npm globals."""
    console.print("[bold]Upgrading system packages[/bold]  (yay -Syu)")
    subprocess.run(["yay", "-Syu", "--noconfirm"], check=True)

    console.print("[bold]Upgrading Python tools[/bold]  (uvenv upgrade-all)")
    subprocess.run(["uvenv", "upgrade-all"], check=True)

    console.print("[bold]Upgrading npm globals[/bold]  (npm update -g)")
    subprocess.run(["npm", "update", "-g"], check=True)
