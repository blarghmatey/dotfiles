"""Pi coding agent management: install, diff, and upgrade the pi CLI and its extensions.

The canonical package list lives in ``home/.pi/agent/settings.json`` (tracked as
a dotfile, symlinked to ``~/.pi/agent/settings.json``).  After ``dots sync``
that file IS the live settings file, so ``pi install`` writes back into the
repo automatically — no separate lock file is needed.

``dots install pi``  — install the pi CLI globally and any missing extensions
``dots diff``        — includes a pi section comparing settings vs installed
``dots upgrade``     — includes ``pi update`` to upgrade pi + all extensions
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

from rich import box
from rich.console import Console
from rich.table import Table

console = Console()

_PI_CLI_PKG = "@earendil-works/pi-coding-agent"
_PI_NPM_MODULES = Path.home() / ".pi" / "agent" / "npm" / "node_modules"


# ── helpers ───────────────────────────────────────────────────────────────────


def _settings_path(repo_root: Path) -> Path:
    """Return the dotfiles copy of pi's settings.json (source of truth)."""
    return repo_root / "home" / ".pi" / "agent" / "settings.json"


def _read_packages(repo_root: Path) -> list[str]:
    """Return the pi package specs declared in settings.json, e.g. ``['npm:pi-lens']``."""
    path = _settings_path(repo_root)
    if not path.exists():
        # Graceful fallback to the live file before first sync
        live = Path.home() / ".pi" / "agent" / "settings.json"
        if live.exists():
            path = live
        else:
            return []
    with open(path) as fh:
        return json.load(fh).get("packages", [])


def _pkg_name(spec: str) -> str:
    """Strip the ``npm:`` prefix and any ``@version`` suffix from a package spec.

    Examples::
        'npm:pi-lens'         -> 'pi-lens'
        'npm:@scope/pkg'      -> '@scope/pkg'
        'npm:pi-lens@3.8.45'  -> 'pi-lens'
        'npm:@scope/pkg@1.0'  -> '@scope/pkg'
    """
    name = spec.removeprefix("npm:")
    if name.startswith("@"):
        # Scoped package: @scope/name[@version]
        slash = name.index("/")
        rest = name[slash + 1:]
        if "@" in rest:
            rest = rest[: rest.index("@")]
        name = name[: slash + 1] + rest
    else:
        if "@" in name:
            name = name[: name.index("@")]
    return name


def _pi_cli_installed() -> bool:
    """Return True if the pi CLI npm package is globally installed."""
    result = subprocess.run(
        ["npm", "list", "-g", "--depth=0", "--json"],
        capture_output=True,
        text=True,
    )
    try:
        data = json.loads(result.stdout)
        return _PI_CLI_PKG in data.get("dependencies", {})
    except (json.JSONDecodeError, KeyError):
        return False


def _extension_installed(pkg_name: str) -> bool:
    """Return True if a pi extension is present in ``~/.pi/agent/npm/node_modules/``."""
    return (_PI_NPM_MODULES / pkg_name).exists()


# ── public API ────────────────────────────────────────────────────────────────


def install_pi(repo_root: Path) -> None:
    """Install the pi CLI globally, then install any missing extensions.

    Extensions are read from ``home/.pi/agent/settings.json`` in the dotfiles
    repo (the same file that ``~/.pi/agent/settings.json`` symlinks to after
    ``dots sync``).
    """
    # ── 1. pi CLI ──────────────────────────────────────────────────────────────
    if _pi_cli_installed():
        console.print(f"[dim]pi CLI already installed ({_PI_CLI_PKG})[/dim]")
    else:
        console.print(f"[bold]Installing pi CLI[/bold]  ({_PI_CLI_PKG})")
        subprocess.run(["npm", "install", "-g", _PI_CLI_PKG], check=True)

    # ── 2. extensions ──────────────────────────────────────────────────────────
    packages = _read_packages(repo_root)
    if not packages:
        console.print("[dim]No pi extensions found in settings.json.[/dim]")
        return

    console.print(
        f"\n[bold]Installing pi extensions[/bold]"
        f"  [dim]({len(packages)} packages)[/dim]\n"
    )

    installed_count = 0
    for spec in packages:
        name = _pkg_name(spec)
        if _extension_installed(name):
            console.print(f"  [dim]already installed[/dim]  {spec}")
            installed_count += 1
        else:
            console.print(f"  [cyan]installing[/cyan]        {spec}")
            subprocess.run(["pi", "install", spec], check=True)
            installed_count += 1

    console.print(f"\n[green]✓[/green] {installed_count} pi extension(s) ready.")


def diff_pi(repo_root: Path) -> None:
    """Print a diff table comparing settings.json packages vs what is installed."""
    packages = _read_packages(repo_root)

    console.print("\n[bold]Pi extensions diff[/bold]\n")

    table = Table(
        box=box.SIMPLE_HEAD,
        show_header=True,
        header_style="bold",
        padding=(0, 1),
    )
    table.add_column("Category", min_width=22)
    table.add_column("Status", min_width=16)
    table.add_column("Details")

    # Row: pi CLI
    cli_ok = _pi_cli_installed()
    table.add_row(
        "pi CLI",
        "[green]✓[/green]" if cli_ok else "[red]✗[/red]",
        "[dim]installed[/dim]"
        if cli_ok
        else f"[red]missing — run: npm install -g {_PI_CLI_PKG}[/red]",
    )

    # Row: extensions
    if not packages:
        table.add_row(
            "pi extensions (0)",
            "[dim]—[/dim]",
            "[dim]no packages in settings.json[/dim]",
        )
    else:
        missing = [s for s in packages if not _extension_installed(_pkg_name(s))]
        ok_count = len(packages) - len(missing)

        status_parts: list[str] = []
        if ok_count:
            status_parts.append(f"[green]{ok_count} ✓[/green]")
        if missing:
            status_parts.append(f"[red]{len(missing)} ✗[/red]")

        detail = (
            "[red]missing: " + ", ".join(_pkg_name(s) for s in missing) + "[/red]"
            if missing
            else "[dim]all present[/dim]"
        )
        table.add_row(
            f"pi extensions ({len(packages)})",
            "  ".join(status_parts) or "[dim]—[/dim]",
            detail,
        )

    console.print(table)


def upgrade_pi() -> None:
    """Upgrade the pi CLI and all installed extensions (``pi update``)."""
    console.print("[bold]Upgrading pi CLI and extensions[/bold]  (pi update)")
    subprocess.run(["pi", "update"], check=True)
