"""Pyinfra component: global npm packages.

Installs the npm packages listed in the active profile's [node.global] section.
Uses pyinfra's npm.packages() operation which checks installed state via
`npm list -g` before installing — idempotent and distribution-aware.

Removals are driven by the dots CLI state diff: the CLI passes a
comma-separated ``remove_npm`` data key containing packages confirmed by the
user for removal.  npm.packages(present=False) is used for idempotent removal.
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


def _parse_csv(value: str) -> list[str]:
    """Parse a comma-separated string into a stripped, non-empty list."""
    return [p.strip() for p in value.split(",") if p.strip()]


@deploy("Global npm packages")
def install_node_tools() -> None:
    """Remove stale and install current global npm packages for the active profile."""
    profile_name: str = host.data.get("profile", "arch-wsl2")
    packages: list[str] = get_profile(profile_name).get("node", {}).get("global", [])

    # Removals are pre-confirmed by the dots CLI after state diff + user prompt.
    remove_pkgs = _parse_csv(host.data.get("remove_npm", ""))
    if remove_pkgs:
        npm.packages(
            name=f"Remove npm globals ({len(remove_pkgs)}): {_pkg_summary(remove_pkgs)}",
            packages=remove_pkgs,
            present=False,
            # directory=None means global (npm uninstall -g)
        )

    if not packages:
        return

    npm.packages(
        name=f"npm global [{profile_name}] ({len(packages)} pkgs): {_pkg_summary(packages)}",
        packages=packages,
        # directory=None means global install (npm install -g)
    )
