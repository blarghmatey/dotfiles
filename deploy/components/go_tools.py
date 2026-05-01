"""Pyinfra component: Go CLI tools via ``go install``.

Installs module specs listed in the top-level ``[go]`` section of manifest.toml
using ``go install <spec>``.  Each spec must include a version suffix such as
``@latest`` or ``@v1.2.3``.

This component is disabled by default (``enable_go=false``) so that running a
full pyinfra deploy for other managers does not unnecessarily re-download Go
modules.  Pass ``enable_go=true`` when you actually want go installs to run
(i.e., from ``dots install go``).

Removals: Go has no ``go uninstall`` command.  Stale binaries are removed from
``$GOBIN`` (falling back to ``$GOPATH/bin``) using ``rm -f``.  The binary name
is extracted from the module path as the final path component before ``@``.

Removals are driven by the dots CLI state diff and passed as a
comma-separated ``remove_go`` data key containing full module specs.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

from pyinfra import host
from pyinfra.api import deploy
from pyinfra.operations import server

from deploy._manifest import load as get_manifest


def _go_bin_dir() -> str:
    """Return the directory where 'go install' places binaries.

    Prefers $GOBIN, then first entry of $GOPATH/bin, then ~/go/bin.
    """
    gobin = subprocess.run(
        ["go", "env", "GOBIN"], capture_output=True, text=True
    ).stdout.strip()
    if gobin:
        return gobin
    gopath = subprocess.run(
        ["go", "env", "GOPATH"], capture_output=True, text=True
    ).stdout.strip()
    if gopath:
        return str(Path(gopath.split(":")[0]) / "bin")
    return str(Path.home() / "go" / "bin")


def _binary_name(spec: str) -> str:
    """Extract the installed binary name from a go install spec.

    Examples::

        golang.org/x/tools/gopls@latest        -> gopls
        github.com/barnybug/cli53/cmd/cli53@latest -> cli53
        github.com/foo/bar/v2@latest            -> bar  (strips /vN suffix)
    """
    path = spec.split("@")[0]
    parts = path.rstrip("/").split("/")
    last = parts[-1]
    # Strip Go major version suffix (e.g., /v2, /v3)
    if last.startswith("v") and last[1:].isdigit():
        last = parts[-2] if len(parts) >= 2 else last
    return last


def _parse_csv(value: str) -> list[str]:
    return [p.strip() for p in value.split(",") if p.strip()]


@deploy("Go tools")
def install_go_tools() -> None:
    """Remove stale and install current Go tools from manifest [go].tools."""
    if not host.data.get("enable_go", False):
        return

    manifest = get_manifest()
    specs: list[str] = manifest.get("go", {}).get("tools", [])

    remove_specs = _parse_csv(host.data.get("remove_go", ""))
    if remove_specs:
        go_bin = _go_bin_dir()
        for spec in remove_specs:
            binary = _binary_name(spec)
            server.shell(
                name=f"Remove go binary: {binary}",
                commands=[f"rm -f {go_bin}/{binary}"],
            )

    for spec in specs:
        server.shell(
            name=f"go install {spec}",
            commands=[f"go install {spec}"],
        )
