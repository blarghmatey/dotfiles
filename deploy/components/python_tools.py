"""Pyinfra component: Python CLI tools via uvenv.

Restores all Python tools declared in uvenv.lock using `uvenv thaw`.
--skip-current prevents unnecessary reinstalls of already-correct versions.

Removals are driven by the dots CLI state diff: the CLI passes a
comma-separated ``remove_uvenv`` data key containing tool names confirmed by
the user for removal.  uvenv has no pyinfra operation, so removal uses
server.shell with ``uvenv uninstall``.
"""

from __future__ import annotations

from pathlib import Path

import tomllib

from pyinfra import host
from pyinfra.api import deploy
from pyinfra.operations import server

_LOCKFILE = Path(__file__).parent.parent.parent / "uvenv.lock"


def _lock_summary() -> str:
    """Return a compact summary of tools in uvenv.lock for operation names."""
    try:
        data = tomllib.loads(_LOCKFILE.read_text())
        pkgs = list(data.get("packages", {}).keys())
        shown = pkgs[:6]
        suffix = f" … +{len(pkgs) - 6} more" if len(pkgs) > 6 else ""
        return f"({len(pkgs)} tools): {', '.join(shown)}{suffix}"
    except Exception:
        return f"from {_LOCKFILE.name}"


def _parse_csv(value: str) -> list[str]:
    """Parse a comma-separated string into a stripped, non-empty list."""
    return [p.strip() for p in value.split(",") if p.strip()]


@deploy("Python CLI tools (uvenv)")
def install_python_tools() -> None:
    """Remove stale and restore current Python CLI tools from uvenv.lock."""
    # Removals are pre-confirmed by the dots CLI after state diff + user prompt.
    # uvenv has no pyinfra operation; server.shell is the correct fallback.
    remove_pkgs = _parse_csv(host.data.get("remove_uvenv", ""))
    for pkg in remove_pkgs:
        server.shell(
            name=f"Remove Python tool: {pkg}",
            commands=[f"uvenv uninstall {pkg}"],
        )

    server.shell(
        name=f"uvenv thaw {_lock_summary()}",
        commands=[f"uvenv thaw --filename {_LOCKFILE} --skip-current"],
    )
