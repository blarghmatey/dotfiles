from __future__ import annotations

import shutil
import subprocess
import sys
import tomllib
from pathlib import Path

from rich.console import Console
from rich.prompt import Confirm

from . import state as pkg_state
from .diff import (
    _cargo_installed,
    _go_binary_name,
    _go_installed,
    _npm_global_installed,
    _pacman_installed,
    _scoop_installed,
    _uvenv_installed,
)

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


def _scoop_bin() -> str:
    """Resolve the Scoop shim's full path.

    Scoop's own command is a ``.cmd`` shim; ``subprocess`` with
    ``shell=False`` won't resolve a bare "scoop" without the extension on
    Windows, so ``shutil.which`` (which does the PATHEXT lookup) is required.
    """
    scoop = shutil.which("scoop")
    if not scoop:
        msg = "scoop not found on PATH — run 'dots bootstrap --profile windows' first"
        raise RuntimeError(msg)
    return scoop


def _install_scoop_packages(packages: list[str]) -> None:
    """Install *packages* via Scoop, natively on Windows. No-op if empty."""
    if not packages:
        return
    console.print(
        f"[bold]Installing Scoop packages[/bold] ({len(packages)}): {', '.join(packages)}"
    )
    subprocess.run([_scoop_bin(), "install", *packages], check=True)


def _remove_scoop_packages(packages: list[str]) -> None:
    """Uninstall *packages* via Scoop, natively on Windows. No-op if empty."""
    if not packages:
        return
    console.print(
        f"[yellow]Removing Scoop packages[/yellow] ({len(packages)}): {', '.join(packages)}"
    )
    subprocess.run([_scoop_bin(), "uninstall", *packages], check=True)


def _run_pyinfra(
    repo_root: Path,
    profile: str,
    *,
    verbose: bool = False,
    remove_packages: list[str] | None = None,
    remove_npm: list[str] | None = None,
    remove_uvenv: list[str] | None = None,
    remove_cargo: list[str] | None = None,
    remove_go: list[str] | None = None,
    enable_cargo: bool = False,
    enable_go: bool = False,
) -> None:
    """Invoke pyinfra with the full set of data keys.

    Linux/WSL-only: pyinfra's Windows local connector is experimental, so the
    ``windows`` profile must never reach this. Guarded here rather than only at
    the callers because ``install_python``/``install_cargo``/``install_go`` take
    the profile from saved state, not from ``--profile``.
    """
    if profile == "windows":
        msg = (
            "This step runs through pyinfra, which the windows profile does not use."
            " Run it from WSL2 with --profile arch-wsl2."
        )
        raise RuntimeError(msg)

    deploy_script = repo_root / "deploy" / "deploy.py"
    cmd = [
        *_PYINFRA,
        "@local",
        str(deploy_script),
        "--data",
        f"profile={profile}",
        "--data",
        f"remove_packages={','.join(remove_packages or [])}",
        "--data",
        f"remove_npm={','.join(remove_npm or [])}",
        "--data",
        f"remove_uvenv={','.join(remove_uvenv or [])}",
        "--data",
        f"remove_cargo={','.join(remove_cargo or [])}",
        "--data",
        f"remove_go={','.join(remove_go or [])}",
        "--data",
        f"enable_cargo={'true' if enable_cargo else 'false'}",
        "--data",
        f"enable_go={'true' if enable_go else 'false'}",
        "-y",
    ]
    if verbose:
        cmd.append("-v")
    subprocess.run(cmd, check=True, cwd=str(repo_root))


def _install_packages_windows(profile: str, pkg_data: dict) -> None:
    """Install/remove Scoop packages natively — no pyinfra, no WSL involved."""
    current_scoop: list[str] = pkg_data.get("scoop", [])
    prev = pkg_state.load()

    remove_scoop: list[str] = []
    if prev.profile == profile:
        candidates_scoop = set(prev.scoop) - set(current_scoop)
        if candidates_scoop:
            installed_scoop = _scoop_installed()
            stale_scoop = sorted(candidates_scoop & installed_scoop)
            if stale_scoop:
                remove_scoop = _confirm_removals("scoop", stale_scoop)

    _remove_scoop_packages(remove_scoop)
    _install_scoop_packages(current_scoop)

    new_state = pkg_state.load()
    new_state.profile = profile
    new_state.scoop = current_scoop
    pkg_state.save(new_state, profile)


def install_packages(repo_root: Path, profile: str, *, verbose: bool = False) -> None:
    """Install packages for *profile*.

    ``windows`` runs natively (Scoop, no pyinfra); every other profile runs
    through the pyinfra deploy (pacman/AUR + npm), which assumes it's
    executing on the Linux/WSL side.
    """
    with (repo_root / "manifest.toml").open("rb") as f:
        manifest = tomllib.load(f)
    profile_data = manifest.get("profiles", {}).get(profile, {})
    pkg_data = profile_data.get("packages", {})

    if profile == "windows":
        _install_packages_windows(profile, pkg_data)
        return

    _sudo_authenticate()

    current_pacman: list[str] = pkg_data.get("pacman", [])
    current_aur: list[str] = pkg_data.get("aur", [])
    current_npm: list[str] = profile_data.get("node", {}).get("global", [])

    prev = pkg_state.load()

    remove_pkgs: list[str] = []
    remove_npm: list[str] = []

    if prev.profile == profile:
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

    console.print(f"\n[bold]Running pyinfra deploy[/bold]  profile=[cyan]{profile}[/cyan]")
    _run_pyinfra(
        repo_root,
        profile,
        verbose=verbose,
        remove_packages=remove_pkgs,
        remove_npm=remove_npm,
    )

    new_state = pkg_state.load()
    new_state.profile = profile
    new_state.pacman = current_pacman
    new_state.aur = current_aur
    new_state.npm_global = current_npm
    pkg_state.save(new_state, profile)


def install_python(repo_root: Path) -> None:
    """Remove stale Python tools then thaw uvenv.lock."""
    lockfile = repo_root / "uvenv.lock"
    with lockfile.open("rb") as f:
        lock = tomllib.load(f)
    current_tools: list[str] = sorted(lock.get("packages", {}).keys())

    prev = pkg_state.load()

    remove_uvenv: list[str] = []
    candidates = set(prev.uvenv_tools) - set(current_tools)
    if candidates:
        installed = _uvenv_installed()
        stale = sorted(candidates & installed)
        if stale:
            remove_uvenv = _confirm_removals("uvenv", stale)

    profile = prev.profile or "arch-wsl2"
    console.print("[bold]Installing Python tools[/bold] via pyinfra + uvenv thaw")
    _run_pyinfra(repo_root, profile, remove_uvenv=remove_uvenv)

    new_state = pkg_state.load()
    new_state.uvenv_tools = current_tools
    pkg_state.save(new_state, new_state.profile or profile)


def install_node(repo_root: Path, profile: str) -> None:
    """Remove stale npm globals then install manifest npm globals via pyinfra."""
    with (repo_root / "manifest.toml").open("rb") as f:
        manifest = tomllib.load(f)
    current_npm: list[str] = (
        manifest.get("profiles", {}).get(profile, {}).get("node", {}).get("global", [])
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

    console.print(f"[bold]Installing npm globals[/bold]  profile=[cyan]{profile}[/cyan]")
    _run_pyinfra(repo_root, profile, remove_npm=remove_npm)

    new_state = pkg_state.load()
    new_state.profile = profile
    new_state.npm_global = current_npm
    pkg_state.save(new_state, profile)


def install_cargo(repo_root: Path) -> None:
    """Remove stale Cargo tools then install manifest cargo tools."""
    with (repo_root / "manifest.toml").open("rb") as f:
        manifest = tomllib.load(f)
    current_tools: list[str] = manifest.get("cargo", {}).get("tools", [])

    prev = pkg_state.load()

    remove_cargo: list[str] = []
    candidates = set(prev.cargo_tools) - set(current_tools)
    if candidates:
        installed = _cargo_installed()
        stale = sorted(candidates & installed)
        if stale:
            remove_cargo = _confirm_removals("cargo", stale)

    profile = prev.profile or "arch-wsl2"
    console.print("[bold]Installing Cargo tools[/bold]")
    _run_pyinfra(
        repo_root,
        profile,
        remove_cargo=remove_cargo,
        enable_cargo=True,
    )

    new_state = pkg_state.load()
    new_state.cargo_tools = current_tools
    pkg_state.save(new_state, new_state.profile or profile)


def install_go(repo_root: Path) -> None:
    """Remove stale Go tools then install manifest go tools."""
    with (repo_root / "manifest.toml").open("rb") as f:
        manifest = tomllib.load(f)
    current_specs: list[str] = manifest.get("go", {}).get("tools", [])

    prev = pkg_state.load()

    remove_go: list[str] = []
    candidates = set(prev.go_tools) - set(current_specs)
    if candidates:
        installed = _go_installed(list(candidates))
        stale = sorted(candidates & installed)
        if stale:
            remove_go = _confirm_removals("go", [_go_binary_name(s) for s in stale])
            # Pass the full specs (not binary names) to pyinfra for removal
            remove_go = stale if remove_go else []

    profile = prev.profile or "arch-wsl2"
    console.print("[bold]Installing Go tools[/bold]")
    _run_pyinfra(
        repo_root,
        profile,
        remove_go=remove_go,
        enable_go=True,
    )

    new_state = pkg_state.load()
    new_state.go_tools = current_specs
    pkg_state.save(new_state, new_state.profile or profile)


def upgrade_all(profile: str) -> None:
    """Upgrade system packages, Python tools, npm globals, cargo/go tools, and pi."""
    if profile == "windows":
        console.print("[bold]Upgrading Scoop[/bold]  (scoop update; scoop update --all)")
        scoop = _scoop_bin()
        subprocess.run([scoop, "update"], check=True)
        subprocess.run([scoop, "update", "--all"], check=True)
        return

    console.print("[bold]Upgrading system packages[/bold]  (yay -Syu)")
    subprocess.run(["yay", "-Syu", "--noconfirm"], check=True)

    console.print("[bold]Upgrading Python tools[/bold]  (uvenv upgrade-all)")
    subprocess.run(["uvenv", "upgrade-all"], check=True)

    console.print("[bold]Upgrading npm globals[/bold]  (npm update -g)")
    subprocess.run(["npm", "update", "-g"], check=True)

    console.print("[bold]Upgrading Cargo tools[/bold]  (cargo install --all-features)")
    subprocess.run(["cargo", "install-update", "-a"], check=True)

    console.print("[bold]Upgrading Go tools[/bold]  (go install @latest)")
    console.print("[dim]Run 'dots install go' to re-install all go tools at @latest[/dim]")
