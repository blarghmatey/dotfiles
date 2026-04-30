"""Pyinfra component: Python CLI tools via uvenv.

Restores all Python tools declared in uvenv.lock using `uvenv thaw`.
--skip-current prevents unnecessary reinstalls of already-correct versions.
"""

from __future__ import annotations

from pathlib import Path

import tomllib

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


@deploy("Python CLI tools (uvenv)")
def install_python_tools() -> None:
    """Restore Python CLI tools from uvenv.lock."""
    server.shell(
        name=f"uvenv thaw {_lock_summary()}",
        commands=[f"uvenv thaw --filename {_LOCKFILE} --skip-current"],
    )
