"""Bootstrap: install prerequisite tooling that `dots install` assumes exists.

Some tools gate the very machinery that would otherwise install them, so
`dots install packages` can't be the thing that sets them up:

- yay must already exist before AUR packages are reachable (it's built via
  git clone + makepkg, not installed via itself).
- A rustup-managed toolchain must be selected as default before `cargo`/
  `rustc` resolve at all — the `rustup` pacman package installs proxy
  binaries only, no toolchain.
- The WSL2 Windows feature/kernel must be enabled, and Scoop must exist,
  before anything else in the windows profile works.

The `windows` profile runs natively on Windows (its own `dots` install, via
bootstrap.ps1) — not shelled into from WSL2. This is separate from
bootstrap.sh/bootstrap.ps1, which each get a *totally* fresh machine to the
point where the `dots` CLI itself is installed. This module runs after that,
is profile-scoped, and is safe to re-run.
"""

from __future__ import annotations

import shutil
import subprocess
import tempfile
from pathlib import Path

from rich.console import Console

console = Console()


def _sudo_authenticate() -> None:
    """Pre-authenticate sudo so makepkg's internal `sudo pacman -U` doesn't hang."""
    console.print("[dim]Authenticating sudo (required to build yay)…[/dim]")
    subprocess.run(["sudo", "-v"], check=True)


def _bootstrap_yay() -> None:
    """Build and install yay (AUR helper) from source, if not already present."""
    if shutil.which("yay"):
        console.print("[dim]yay already installed — skipping[/dim]")
        return

    console.print("[bold]Installing yay (AUR helper)[/bold]")
    _sudo_authenticate()
    subprocess.run(
        ["sudo", "pacman", "-S", "--needed", "--noconfirm", "base-devel", "git"],
        check=True,
    )
    with tempfile.TemporaryDirectory() as tmpdir:
        clone_dir = Path(tmpdir) / "yay"
        subprocess.run(
            ["git", "clone", "--depth=1", "https://aur.archlinux.org/yay.git", str(clone_dir)],
            check=True,
        )
        subprocess.run(["makepkg", "-si", "--noconfirm"], cwd=clone_dir, check=True)


def _bootstrap_rustup_toolchain() -> None:
    """Select a default rustup toolchain so `cargo`/`rustc` resolve.

    The Arch `rustup` package ships proxy binaries but no toolchain — cargo
    stays "not found" until one is picked as default.
    """
    if not shutil.which("rustup"):
        console.print(
            "[dim]rustup not installed yet — run 'dots install packages' first,"
            " then re-run 'dots bootstrap'[/dim]"
        )
        return

    console.print("[bold]Setting default rustup toolchain[/bold] (stable)")
    subprocess.run(["rustup", "default", "stable"], check=True)


def _bootstrap_wsl2() -> None:
    """Ensure the WSL2 Windows feature and kernel are installed.

    ``wsl --install`` is not idempotent — if WSL is already configured it
    just prints help text instead of a no-op — and a fresh install requires
    a reboot before ``wsl`` is usable at all. So: check status first, only
    run ``--install`` when that fails, and never reboot automatically.
    ``--no-distribution`` installs just the platform (feature + kernel); it
    does not install or configure a Linux distro (Manjaro/Arch is not in the
    Microsoft Store distro list `wsl --install -d` pulls from anyway).
    """
    status = subprocess.run(["wsl.exe", "--status"], capture_output=True, text=True, check=False)
    if status.returncode == 0:
        console.print("[dim]WSL2 already installed — skipping[/dim]")
        return

    console.print("[bold]Installing WSL2[/bold] (feature + kernel, no distro)")
    subprocess.run(["wsl.exe", "--install", "--no-distribution"], check=True)
    console.print(
        "[yellow]WSL2 was just installed — a reboot is required before it's usable.[/yellow]\n"
        "[dim]Reboot, then re-run 'dots bootstrap --profile windows' to continue.[/dim]"
    )


def _bootstrap_scoop() -> None:
    """Install Scoop, if not already present.

    Idempotent: the official installer also detects an existing Scoop
    install and exits 0 without changes rather than erroring, but checking
    first avoids the PowerShell round-trip when there's nothing to do.
    """
    if shutil.which("scoop"):
        console.print("[dim]Scoop already installed — skipping[/dim]")
        return

    console.print("[bold]Installing Scoop[/bold]")
    subprocess.run(
        [
            "powershell.exe",
            "-NoProfile",
            "-NonInteractive",
            "-Command",
            "Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser; irm get.scoop.sh | iex",
        ],
        check=True,
    )


def bootstrap(profile: str) -> None:
    """Install prerequisite tooling for *profile* that `dots install` assumes exists."""
    if profile == "windows":
        _bootstrap_wsl2()
        _bootstrap_scoop()
        return

    _bootstrap_yay()
    _bootstrap_rustup_toolchain()
