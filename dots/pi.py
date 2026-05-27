"""Pi coding agent management: install, diff, and upgrade the pi CLI and its extensions.

The canonical package list lives in ``home/.pi/agent/settings.json`` (tracked as
a dotfile, symlinked to ``~/.pi/agent/settings.json``).  After ``dots sync``
that file IS the live settings file, so ``pi install`` writes back into the
repo automatically — no separate lock file is needed.

Additional tracked config files (all under ``home/.pi/agent/``):
    mcp.json              — MCP server connections (sensitive: contains org URLs)
    models.json           — Custom model provider definitions
    pi-worktrees.config.json — pi-worktrees extension config
    npm/package.json      — Extension package manifest with semver constraints

Local extensions live under ``home/.pi/agent/extensions/`` and are auto-discovered
by pi after ``dots sync`` symlinks them into ``~/.pi/agent/extensions/``.  No entry
in ``settings.json`` is needed for these.

``dots install pi``  — install the pi CLI globally and any missing extensions
``dots sync``        — symlink local extensions + config into ~/
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
_PI_AGENT = Path.home() / ".pi" / "agent"
_PI_NPM_MODULES = _PI_AGENT / "npm" / "node_modules"
_PI_EXTENSIONS = _PI_AGENT / "extensions"

# Config files tracked in the dotfiles repo (relative to home/.pi/agent/)
_TRACKED_CONFIGS = [
    "mcp.json",
    "models.json",
    "pi-worktrees.config.json",
    "npm/package.json",
]


# ── helpers ───────────────────────────────────────────────────────────────────


def _agent_path(repo_root: Path, rel: str) -> Path:
    """Return the dotfiles copy of a file under home/.pi/agent/."""
    return repo_root / "home" / ".pi" / "agent" / rel


def _settings_path(repo_root: Path) -> Path:
    """Return the dotfiles copy of pi's settings.json (source of truth)."""
    return _agent_path(repo_root, "settings.json")


def _read_packages(repo_root: Path) -> list[str]:
    """Return the pi package specs declared in settings.json, e.g. ``['npm:pi-lens']``."""
    path = _settings_path(repo_root)
    if not path.exists():
        # Graceful fallback to the live file before first sync
        live = _PI_AGENT / "settings.json"
        if live.exists():
            path = live
        else:
            return []
    with path.open() as fh:
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
        rest = name[slash + 1 :]
        if "@" in rest:
            rest = rest[: rest.index("@")]
        name = name[: slash + 1] + rest
    elif "@" in name:
        name = name[: name.index("@")]
    return name


def _pi_cli_installed() -> bool:
    """Return True if the pi CLI npm package is globally installed."""
    result = subprocess.run(
        ["npm", "list", "-g", "--depth=0", "--json"],
        capture_output=True,
        text=True,
        check=False,
    )
    try:
        data = json.loads(result.stdout)
        return _PI_CLI_PKG in data.get("dependencies", {})
    except (json.JSONDecodeError, KeyError):
        return False


def _extension_installed(pkg_name: str) -> bool:
    """Return True if a pi extension is present in ``~/.pi/agent/npm/node_modules/``."""
    return (_PI_NPM_MODULES / pkg_name).exists()


def _local_extension_dirs(repo_root: Path) -> list[str]:
    """Return names of local extension directories tracked under home/.pi/agent/extensions/."""
    ext_src = _agent_path(repo_root, "extensions")
    if not ext_src.exists():
        return []
    return sorted(p.name for p in ext_src.iterdir() if p.is_dir())


def _local_extension_linked(name: str) -> bool:
    """Return True if the extension directory exists under the live ~/.pi/agent/extensions/."""
    return (_PI_EXTENSIONS / name).exists()


def _config_in_sync(repo_root: Path, rel: str) -> bool | None:
    """Compare a tracked config file against the live copy.

    Returns True if identical, False if different, None if either copy is missing.
    """
    repo_copy = _agent_path(repo_root, rel)
    live_copy = _PI_AGENT / rel
    if not repo_copy.exists() or not live_copy.exists():
        return None
    return repo_copy.read_text() == live_copy.read_text()


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
        f"\n[bold]Installing pi extensions[/bold]  [dim]({len(packages)} packages)[/dim]\n"
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
    """Print a diff table comparing settings.json packages vs what is installed.

    Also flags any tracked config files that are out of sync with the live copies.
    """
    packages = _read_packages(repo_root)

    console.print("\n[bold]Pi extensions diff[/bold]\n")

    table = Table(
        box=box.SIMPLE_HEAD,
        show_header=True,
        header_style="bold",
        padding=(0, 1),
    )
    table.add_column("Category", min_width=28)
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

    # Rows: local extensions (auto-discovered from ~/.pi/agent/extensions/)
    local_exts = _local_extension_dirs(repo_root)
    if local_exts:
        missing_exts = [n for n in local_exts if not _local_extension_linked(n)]
        ok_exts = len(local_exts) - len(missing_exts)
        ext_status_parts: list[str] = []
        if ok_exts:
            ext_status_parts.append(f"[green]{ok_exts} ✓[/green]")
        if missing_exts:
            ext_status_parts.append(f"[yellow]{len(missing_exts)} ?[/yellow]")
        ext_detail = (
            "[yellow]not linked: " + ", ".join(missing_exts) + " — run: dots sync[/yellow]"
            if missing_exts
            else "[dim]symlinked[/dim]"
        )
        table.add_row(
            f"local extensions ({len(local_exts)})",
            "  ".join(ext_status_parts) or "[dim]—[/dim]",
            ext_detail,
        )

    # Rows: tracked config files
    for rel in _TRACKED_CONFIGS:
        in_sync = _config_in_sync(repo_root, rel)
        label = f".pi/agent/{rel}"
        if in_sync is None:
            live_missing = not (_PI_AGENT / rel).exists()
            repo_missing = not _agent_path(repo_root, rel).exists()
            if live_missing and repo_missing:
                status, detail = (
                    "[dim]—[/dim]",
                    "[dim]not present in either location[/dim]",
                )
            elif live_missing:
                status, detail = (
                    "[yellow]?[/yellow]",
                    "[yellow]live copy missing — run dots sync[/yellow]",
                )
            else:
                status, detail = (
                    "[yellow]?[/yellow]",
                    "[yellow]not yet tracked in dotfiles[/yellow]",
                )
        elif in_sync:
            status, detail = "[green]✓[/green]", "[dim]in sync[/dim]"
        else:
            status = "[yellow]≠[/yellow]"
            detail = f"[yellow]live copy differs — run: cp ~/.pi/agent/{rel} home/.pi/agent/{rel}[/yellow]"
        table.add_row(label, status, detail)

    console.print(table)


def upgrade_pi() -> None:
    """Upgrade the pi CLI and all installed extensions (``pi update``)."""
    console.print("[bold]Upgrading pi CLI and extensions[/bold]  (pi update)")
    subprocess.run(["pi", "update"], check=True)
