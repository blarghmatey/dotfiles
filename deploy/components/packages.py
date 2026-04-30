"""Pyinfra component: system packages (pacman + AUR).

Reads the package list for the active profile from manifest.toml. Installs
via pacman for official packages and yay for AUR packages.

Removals are driven by the dots CLI state diff: the CLI passes a
comma-separated ``remove_packages`` data key containing packages confirmed
by the user for removal.  Both pacman and AUR packages are removed via
``pacman.packages(present=False)`` since AUR packages live in the pacman DB.
"""

from __future__ import annotations

from pyinfra import host
from pyinfra.api import deploy
from pyinfra.operations import pacman, server

from deploy._manifest import profile as get_profile


def _pkg_summary(packages: list[str], limit: int = 8) -> str:
    """Return a compact package list for use in operation names."""
    if not packages:
        return "(none)"
    shown = packages[:limit]
    suffix = f" … +{len(packages) - limit} more" if len(packages) > limit else ""
    return ", ".join(shown) + suffix


def _parse_csv(value: str) -> list[str]:
    """Parse a comma-separated string into a stripped, non-empty list."""
    return [p.strip() for p in value.split(",") if p.strip()]


@deploy("System packages")
def install_packages() -> None:
    """Remove stale and install current pacman/AUR packages for the active profile."""
    profile_name: str = host.data.get("profile", "arch-wsl2")
    pkg_cfg = get_profile(profile_name).get("packages", {})

    pacman_pkgs: list[str] = pkg_cfg.get("pacman", [])
    aur_pkgs: list[str] = pkg_cfg.get("aur", [])

    # Removals are pre-confirmed by the dots CLI after state diff + user prompt.
    # AUR packages are in the pacman DB so pacman.packages handles both.
    remove_pkgs = _parse_csv(host.data.get("remove_packages", ""))
    if remove_pkgs:
        pacman.packages(
            name=f"Remove packages ({len(remove_pkgs)}): {_pkg_summary(remove_pkgs)}",
            packages=remove_pkgs,
            present=False,
            _sudo=True,
        )

    if pacman_pkgs:
        pacman.packages(
            name=f"pacman [{profile_name}] ({len(pacman_pkgs)} pkgs): {_pkg_summary(pacman_pkgs)}",
            packages=pacman_pkgs,
            update=True,
            _sudo=True,
        )

    if aur_pkgs:
        # yay must NOT run as root — it handles sudo internally for makepkg steps.
        # Bootstrap requirement: yay must be installed before this runs on a
        # fresh system (bootstrap.sh handles that).
        server.shell(
            name=f"AUR [{profile_name}] ({len(aur_pkgs)} pkgs): {_pkg_summary(aur_pkgs)}",
            commands=[f"yay -S --needed --noconfirm {' '.join(aur_pkgs)}"],
        )
