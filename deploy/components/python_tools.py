"""Pyinfra component: Python CLI tools via uvenv.

Restores all Python tools declared in uvenv.lock using `uvenv thaw`.
--skip-current prevents unnecessary reinstalls of already-correct versions.
"""

from __future__ import annotations

from pathlib import Path

from pyinfra.api import deploy
from pyinfra.operations import server

_LOCKFILE = Path(__file__).parent.parent.parent / "uvenv.lock"


@deploy("Python CLI tools (uvenv)")
def install_python_tools() -> None:
    """Restore Python CLI tools from uvenv.lock."""
    server.shell(
        name="Python tools via uvenv thaw",
        commands=[f"uvenv thaw --filename {_LOCKFILE} --skip-current"],
    )
