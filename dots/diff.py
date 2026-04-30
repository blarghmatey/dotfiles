"""Native diff: compare manifest against actual installed state."""

from __future__ import annotations

import json
import subprocess
import tomllib
from pathlib import Path

from rich import box
from rich.console import Console
from rich.table import Table

from . import state as pkg_state

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
    pending_remove: list[str] | None = None,
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
    if pending_remove:
        status_parts.append(f"[yellow]{len(pending_remove)} –[/yellow]")

    status = "  ".join(status_parts) if status_parts else "[dim]—[/dim]"
    label = name_label or f"{category} ({total})"

    notes: list[str] = []
    if missing:
        notes.append("[red]missing: " + ", ".join(missing) + "[/red]")
    if pending_remove:
        notes.append("[yellow]to remove: " + ", ".join(pending_remove) + "[/yellow]")
    notes_str = "  ".join(notes) if notes else "[dim]all present[/dim]"

    table.add_row(label, status, notes_str)


# ── public entry point ────────────────────────────────────────────────────────


def print_diff(repo_root: Path, profile: str) -> None:
    """Query each package manager and display what's missing or pending removal."""
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

    # Load saved state to compute pending removals.
    prev = pkg_state.load()
    state_matches_profile = prev.profile == profile

    pending_pacman: list[str] = []
    pending_aur: list[str] = []
    pending_npm: list[str] = []
    pending_uvenv: list[str] = []

    console.print(f"\n[bold]Dotfiles diff[/bold]  profile=[cyan]{profile}[/cyan]\n")

    with console.status("[dim]Querying installed packages…[/dim]"):
        pacman_inst = _pacman_installed()
        npm_inst = _npm_global_installed()
        uvenv_inst = _uvenv_installed()

        if state_matches_profile:
            prev_sys = set(prev.pacman) | set(prev.aur)
            curr_sys = set(pacman_expected) | set(aur_expected)
            pending_sys = sorted((prev_sys - curr_sys) & pacman_inst)
            # Split back into pacman vs AUR buckets for display.
            pending_pacman = [p for p in pending_sys if p in set(prev.pacman)]
            pending_aur = [p for p in pending_sys if p in set(prev.aur)]

            pending_npm = sorted(
                (set(prev.npm_global) - set(npm_expected)) & npm_inst
            )
            pending_uvenv = sorted(
                (set(prev.uvenv_tools) - set(python_expected)) & uvenv_inst
            )

    table = Table(box=box.SIMPLE_HEAD, show_header=True, header_style="bold", padding=(0, 1))
    table.add_column("Category", min_width=22)
    table.add_column("Status", min_width=16)
    table.add_column("Details")

    _add_row(table, "pacman", pacman_expected, pacman_inst,
             pending_pacman or None, name_label=f"pacman ({len(pacman_expected)})")
    _add_row(table, "AUR", aur_expected, pacman_inst,
             pending_aur or None, name_label=f"AUR ({len(aur_expected)})")
    _add_row(table, "npm global", npm_expected, npm_inst,
             pending_npm or None, name_label=f"npm global ({len(npm_expected)})")
    _add_row(table, "python tools", python_expected, uvenv_inst,
             pending_uvenv or None, name_label=f"python tools ({len(python_expected)})")

    console.print(table)

    if not state_matches_profile and prev.profile:
        console.print(
            f"[dim]Note: state was recorded for profile [bold]{prev.profile}[/bold];"
            f" pending removals not shown for profile [bold]{profile}[/bold].[/dim]\n"
        )
