"""Pyinfra component: system packages (pacman + AUR).

Reads the package list for the active profile from manifest.toml and installs
via pacman for official packages and yay for AUR packages.
"""

from __future__ import annotations

from pyinfra import host
from pyinfra.api import deploy
from pyinfra.operations import pacman, server

from deploy._manifest import profile as get_profile


@deploy("System packages")
def install_packages() -> None:
    """Install pacman and AUR packages for the active profile."""
    profile_name: str = host.data.get("profile", "arch-wsl2")
    pkg_cfg = get_profile(profile_name).get("packages", {})

    pacman_pkgs: list[str] = pkg_cfg.get("pacman", [])
    aur_pkgs: list[str] = pkg_cfg.get("aur", [])

    if pacman_pkgs:
        pacman.packages(
            name=f"pacman packages [{profile_name}]",
            packages=pacman_pkgs,
            update=True,
            _sudo=True,
        )

    if aur_pkgs:
        # yay --needed is idempotent: skips already-installed packages.
        # yay must NOT run as root — it handles sudo internally for makepkg steps.
        # Bootstrap requirement: yay must be installed before this runs on a
        # fresh system (bootstrap.sh handles that).
        server.shell(
            name=f"AUR packages [{profile_name}]",
            commands=[f"yay -S --needed --noconfirm {' '.join(aur_pkgs)}"],
        )
