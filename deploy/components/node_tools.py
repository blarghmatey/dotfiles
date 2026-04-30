"""Pyinfra component: global npm packages.

Installs the npm packages listed in the active profile's [node.global] section.
Uses pyinfra's npm.packages() operation which checks installed state via
`npm list -g` before installing — idempotent and distribution-aware.
"""

from __future__ import annotations

from pyinfra import host
from pyinfra.api import deploy
from pyinfra.operations import npm

from deploy._manifest import profile as get_profile


def _pkg_summary(packages: list[str], limit: int = 6) -> str:
    """Return a compact package list for use in operation names."""
    if not packages:
        return "(none)"
    shown = packages[:limit]
    suffix = f" … +{len(packages) - limit} more" if len(packages) > limit else ""
    return ", ".join(shown) + suffix


@deploy("Global npm packages")
def install_node_tools() -> None:
    """Install global npm packages for the active profile."""
    profile_name: str = host.data.get("profile", "arch-wsl2")
    packages: list[str] = get_profile(profile_name).get("node", {}).get("global", [])

    if not packages:
        return

    npm.packages(
        name=f"npm global [{profile_name}] ({len(packages)} pkgs): {_pkg_summary(packages)}",
        packages=packages,
        # directory=None means global install (npm install -g)
    )
