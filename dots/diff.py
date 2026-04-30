"""Native diff: compare manifest against actual installed state."""

from __future__ import annotations

import json
import subprocess
import tomllib
from pathlib import Path

from rich.console import Console
from rich.table import Table
from rich import box

console = Console()


# ── helpers to query current state ───────────────────────────────────────────


def _pacman_installed() -> set[str]:
    result = subprocess.run(["pacman", "-Q"], capture_output=True, text=True)
    return {line.split()[0] for line in result.stdout.splitlines() if line.strip()}


def _npm_global_installed() -> set[str]:
    result = subprocess.run(
        ["npm", "list", "-g", "--depth=0", "--json"],
        capture_output=True,
        text=True,
    )
    try:
        data = json.loads(result.stdout)
        return set(data.get("dependencies", {}).keys())
    except (json.JSONDecodeError, KeyError):
        return set()


def _uvenv_installed() -> set[str]:
    result = subprocess.run(["uvenv", "list"], capture_output=True, text=True)
    tools: set[str] = set()
    for line in result.stdout.splitlines():
        stripped = line.strip()
        if stripped.startswith("- "):
            tools.add(stripped[2:].strip())
    return tools


# ── rendering ─────────────────────────────────────────────────────────────────


def _add_row(
    table: Table,
    category: str,
    expected: list[str],
    installed: set[str],
    *,
    name_label: str | None = None,
) -> None:
    """Add one summary row per category to *table*."""
    missing = [p for p in expected if p not in installed]
    ok_count = len(expected) - len(missing)
    total = len(expected)

    status_parts: list[str] = []
    if ok_count:
        status_parts.append(f"[green]{ok_count} ✓[/green]")
    if missing:
        status_parts.append(f"[red]{len(missing)} ✗[/red]")

    status = "  ".join(status_parts) if status_parts else "[dim]—[/dim]"
    label = name_label or f"{category} ({total})"

    missing_str = (
        "[red]" + ", ".join(missing) + "[/red]" if missing else "[dim]all present[/dim]"
    )

    table.add_row(label, status, missing_str)


# ── public entry point ────────────────────────────────────────────────────────


def print_diff(repo_root: Path, profile: str) -> None:
    """Query each package manager and display what's missing vs installed."""
    with open(repo_root / "manifest.toml", "rb") as f:
        manifest = tomllib.load(f)
    with open(repo_root / "uvenv.lock", "rb") as f:
        lock = tomllib.load(f)

    profile_data = manifest.get("profiles", {}).get(profile, {})
    pkg_data = profile_data.get("packages", {})

    pacman_expected: list[str] = pkg_data.get("pacman", [])
    aur_expected: list[str] = pkg_data.get("aur", [])
    npm_expected: list[str] = profile_data.get("node", {}).get("global", [])
    python_expected: list[str] = sorted(lock.get("packages", {}).keys())

    console.print(f"\n[bold]Dotfiles diff[/bold]  profile=[cyan]{profile}[/cyan]\n")

    with console.status("[dim]Querying installed packages…[/dim]"):
        pacman_inst = _pacman_installed()
        npm_inst = _npm_global_installed()
        uvenv_inst = _uvenv_installed()

    table = Table(box=box.SIMPLE_HEAD, show_header=True, header_style="bold", padding=(0, 1))
    table.add_column("Category", min_width=22)
    table.add_column("Status", min_width=12)
    table.add_column("Missing / note")

    _add_row(table, "pacman", pacman_expected, pacman_inst,
             name_label=f"pacman ({len(pacman_expected)})")
    _add_row(table, "AUR", aur_expected, pacman_inst,
             name_label=f"AUR ({len(aur_expected)})")
    _add_row(table, "npm global", npm_expected, npm_inst,
             name_label=f"npm global ({len(npm_expected)})")
    _add_row(table, "python tools", python_expected, uvenv_inst,
             name_label=f"python tools ({len(python_expected)})")

    console.print(table)
