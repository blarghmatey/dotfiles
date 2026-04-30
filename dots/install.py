from __future__ import annotations

import subprocess
import sys
import tomllib
from pathlib import Path

from rich.console import Console
from rich.prompt import Confirm

from . import state as pkg_state
from .diff import _npm_global_installed, _pacman_installed, _uvenv_installed

console = Console()

_PYINFRA = [sys.executable, "-m", "pyinfra"]


def _sudo_authenticate() -> None:
    """Pre-authenticate sudo so non-interactive sub-processes can use cached creds.

    yay runs as the current user but calls ``sudo pacman -U`` internally to
    install built AUR packages.  pyinfra spawns commands without a TTY, so if
    the sudo credential cache is cold yay's install step hangs/fails.  Running
    ``sudo -v`` here prompts the user once interactively before pyinfra starts.
    """
    console.print("[dim]Authenticating sudo (required for pacman/AUR installs)…[/dim]")
    subprocess.run(["sudo", "-v"], check=True)


def _confirm_removals(category: str, pkgs: list[str]) -> list[str]:
    """Show packages queued for removal and return the confirmed subset (or [])."""
    console.print(
        f"\n[yellow]Packages removed from manifest[/yellow] ([bold]{category}[/bold]):"
        f" {', '.join(pkgs)}"
    )
    if Confirm.ask(f"  Remove {len(pkgs)} package(s) from system?", default=False):
        return pkgs
    return []


def install_packages(repo_root: Path, profile: str, *, verbose: bool = False) -> None:
    """Run pyinfra deploy: remove stale packages then install manifest packages."""
    _sudo_authenticate()

    with open(repo_root / "manifest.toml", "rb") as f:
        manifest = tomllib.load(f)
    profile_data = manifest.get("profiles", {}).get(profile, {})
    pkg_data = profile_data.get("packages", {})
    current_pacman: list[str] = pkg_data.get("pacman", [])
    current_aur: list[str] = pkg_data.get("aur", [])
    current_npm: list[str] = profile_data.get("node", {}).get("global", [])

    prev = pkg_state.load()

    # Removals are only computed when the state was built for the same profile to
    # prevent accidental mass-removal when switching profiles.
    remove_pkgs: list[str] = []
    remove_npm: list[str] = []

    if prev.profile == profile:
        # pacman + AUR packages all live in the pacman DB; remove as one set.
        prev_sys = set(prev.pacman) | set(prev.aur)
        curr_sys = set(current_pacman) | set(current_aur)
        candidates_sys = prev_sys - curr_sys
        if candidates_sys:
            installed_sys = _pacman_installed()
            stale_sys = sorted(candidates_sys & installed_sys)
            if stale_sys:
                remove_pkgs = _confirm_removals("pacman/AUR", stale_sys)

        candidates_npm = set(prev.npm_global) - set(current_npm)
        if candidates_npm:
            installed_npm = _npm_global_installed()
            stale_npm = sorted(candidates_npm & installed_npm)
            if stale_npm:
                remove_npm = _confirm_removals("npm global", stale_npm)

    deploy_script = repo_root / "deploy" / "deploy.py"
    console.print(
        f"\n[bold]Running pyinfra deploy[/bold]  profile=[cyan]{profile}[/cyan]"
    )
    cmd = [
        *_PYINFRA, "@local", str(deploy_script),
        "--data", f"profile={profile}",
        "--data", f"remove_packages={','.join(remove_pkgs)}",
        "--data", f"remove_npm={','.join(remove_npm)}",
        "--data", "remove_uvenv=",   # uvenv handled separately by install_python
        "-y",
    ]
    if verbose:
        cmd.append("-v")
    subprocess.run(cmd, check=True, cwd=str(repo_root))

    # Update state only after a successful run.
    new_state = pkg_state.load()
    new_state.profile = profile
    new_state.pacman = current_pacman
    new_state.aur = current_aur
    new_state.npm_global = current_npm
    pkg_state.save(new_state, profile)


def install_python(repo_root: Path) -> None:
    """Remove stale Python tools then thaw uvenv.lock.

    uvenv has no pyinfra operation, so removal uses ``uvenv uninstall``
    directly.  Installation delegates to the pyinfra deploy (python_tools
    component) so that the rest of the deploy stack is aware of the operation.
    """
    lockfile = repo_root / "uvenv.lock"
    with open(lockfile, "rb") as f:
        lock = tomllib.load(f)
    current_tools: list[str] = sorted(lock.get("packages", {}).keys())

    prev = pkg_state.load()

    # uvenv tools are global (not profile-scoped); profile mismatch is ignored.
    remove_uvenv: list[str] = []
    candidates = set(prev.uvenv_tools) - set(current_tools)
    if candidates:
        installed = _uvenv_installed()
        stale = sorted(candidates & installed)
        if stale:
            remove_uvenv = _confirm_removals("uvenv", stale)

    # Run pyinfra for removal (server.shell uvenv uninstall) + thaw.
    deploy_script = repo_root / "deploy" / "deploy.py"
    profile = prev.profile or "arch-wsl2"
    console.print(f"[bold]Installing Python tools[/bold] via pyinfra + uvenv thaw")
    cmd = [
        *_PYINFRA, "@local", str(deploy_script),
        "--data", f"profile={profile}",
        "--data", "remove_packages=",
        "--data", "remove_npm=",
        "--data", f"remove_uvenv={','.join(remove_uvenv)}",
        "-y",
    ]
    subprocess.run(cmd, check=True, cwd=str(repo_root))

    # Update state only after a successful run.
    new_state = pkg_state.load()
    new_state.uvenv_tools = current_tools
    pkg_state.save(new_state, new_state.profile)


def install_node(repo_root: Path, profile: str) -> None:
    """Remove stale npm globals then install manifest npm globals via pyinfra."""
    with open(repo_root / "manifest.toml", "rb") as f:
        manifest = tomllib.load(f)
    current_npm: list[str] = (
        manifest.get("profiles", {})
        .get(profile, {})
        .get("node", {})
        .get("global", [])
    )

    if not current_npm:
        console.print(f"[dim]No npm packages defined for profile {profile!r}[/dim]")
        return

    prev = pkg_state.load()

    remove_npm: list[str] = []
    if prev.profile == profile:
        candidates = set(prev.npm_global) - set(current_npm)
        if candidates:
            installed = _npm_global_installed()
            stale = sorted(candidates & installed)
            if stale:
                remove_npm = _confirm_removals("npm global", stale)

    deploy_script = repo_root / "deploy" / "deploy.py"
    console.print(f"[bold]Installing npm globals[/bold]  profile=[cyan]{profile}[/cyan]")
    cmd = [
        *_PYINFRA, "@local", str(deploy_script),
        "--data", f"profile={profile}",
        "--data", "remove_packages=",
        "--data", f"remove_npm={','.join(remove_npm)}",
        "--data", "remove_uvenv=",
        "-y",
    ]
    subprocess.run(cmd, check=True, cwd=str(repo_root))

    new_state = pkg_state.load()
    new_state.npm_global = current_npm
    pkg_state.save(new_state, new_state.profile)


def upgrade_all(profile: str) -> None:
    """Upgrade system packages, Python tools, and npm globals."""
    console.print("[bold]Upgrading system packages[/bold]  (yay -Syu)")
    subprocess.run(["yay", "-Syu", "--noconfirm"], check=True)

    console.print("[bold]Upgrading Python tools[/bold]  (uvenv upgrade-all)")
    subprocess.run(["uvenv", "upgrade-all"], check=True)

    console.print("[bold]Upgrading npm globals[/bold]  (npm update -g)")
    subprocess.run(["npm", "update", "-g"], check=True)
