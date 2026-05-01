"""Pyinfra component: Rust/Cargo CLI tools.

Installs crates listed in the top-level ``[cargo]`` section of manifest.toml
using ``cargo install``.  Cargo skips crates that are already installed at the
current version, so the operation is naturally idempotent.

This component is disabled by default (``enable_cargo=false``) so that running
a full pyinfra deploy for other managers (pacman, npm) does not trigger a
potentially slow cargo rebuild.  Pass ``enable_cargo=true`` when you actually
want cargo installs to run (i.e., from ``dots install cargo``).

Removals are driven by the dots CLI state diff: the CLI passes a
comma-separated ``remove_cargo`` data key containing crate names confirmed by
the user.  ``cargo uninstall`` is used for idempotent removal.
"""

from __future__ import annotations

from pyinfra import host
from pyinfra.api import deploy
from pyinfra.operations import server

from deploy._manifest import load as get_manifest


def _pkg_summary(packages: list[str], limit: int = 6) -> str:
    if not packages:
        return "(none)"
    shown = packages[:limit]
    suffix = f" … +{len(packages) - limit} more" if len(packages) > limit else ""
    return ", ".join(shown) + suffix


def _parse_csv(value: str) -> list[str]:
    return [p.strip() for p in value.split(",") if p.strip()]


@deploy("Cargo tools")
def install_cargo_tools() -> None:
    """Remove stale and install current Cargo tools from manifest [cargo].tools."""
    if not host.data.get("enable_cargo", False):
        return

    manifest = get_manifest()
    tools: list[str] = manifest.get("cargo", {}).get("tools", [])

    remove_crates = _parse_csv(host.data.get("remove_cargo", ""))
    for crate in remove_crates:
        server.shell(
            name=f"cargo uninstall {crate}",
            commands=[f"cargo uninstall {crate}"],
        )

    for crate in tools:
        server.shell(
            name=f"cargo install {crate}",
            commands=[f"cargo install {crate}"],
        )
