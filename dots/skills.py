"""Skills management: install and diff agent skills from skills-lock.json.

Uses the Vercel Skills CLI (``npx skills``) to install global agent skills.
The lockfile ``skills-lock.json`` in the repo root records each skill and its
source package so that the full set can be restored on a new machine.

Restoration works by grouping skills by source package and calling::

    npx skills add <owner/repo> --global --all --yes

once per unique source.  This is separate from ``npx skills experimental_install``
which targets project-level (per-repo) skills rather than user-global ones.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

from rich.console import Console
from rich.table import Table
from rich import box

console = Console()


def _load_lock(repo_root: Path) -> dict[str, dict]:
    """Return the skills dict from skills-lock.json, or {} if missing."""
    path = repo_root / "skills-lock.json"
    if not path.exists():
        return {}
    with open(path) as f:
        data = json.load(f)
    return data.get("skills", {})


def _installed_skills() -> set[str]:
    """Return the set of currently installed global skill names."""
    result = subprocess.run(
        ["npx", "skills", "list", "-g", "--json"],
        capture_output=True,
        text=True,
    )
    try:
        return {s["name"] for s in json.loads(result.stdout)}
    except (json.JSONDecodeError, KeyError):
        return set()


def _unique_sources(skills: dict[str, dict]) -> list[str]:
    """Return deduplicated source packages, preserving insertion order."""
    seen: set[str] = set()
    sources: list[str] = []
    for entry in skills.values():
        src = entry.get("source", "")
        if src and src not in seen:
            seen.add(src)
            sources.append(src)
    return sources


def install_skills(repo_root: Path, *, yes: bool = False) -> None:
    """Install all global agent skills declared in skills-lock.json."""
    skills = _load_lock(repo_root)
    if not skills:
        console.print("[yellow]skills-lock.json not found or empty.[/yellow]")
        return

    sources = _unique_sources(skills)
    console.print(
        f"\n[bold]Installing agent skills[/bold]  "
        f"[dim]({len(skills)} skills from {len(sources)} packages)[/dim]\n"
    )

    for source in sources:
        skill_names = [name for name, e in skills.items() if e.get("source") == source]
        console.print(
            f"  [cyan]{source}[/cyan] "
            f"[dim]({len(skill_names)} skills)[/dim]"
        )
        cmd = ["npx", "skills", "add", source, "--global", "--all"]
        if yes:
            cmd.append("--yes")
        subprocess.run(cmd, check=True)

    console.print("\n[green]✓[/green] Skills installed.")


def diff_skills(repo_root: Path) -> None:
    """Show which skills from skills-lock.json are missing vs installed."""
    skills = _load_lock(repo_root)
    if not skills:
        console.print("[yellow]skills-lock.json not found or empty.[/yellow]")
        return

    console.print("\n[bold]Skills diff[/bold]\n")

    with console.status("[dim]Querying installed skills…[/dim]"):
        installed = _installed_skills()

    expected = set(skills.keys())
    missing = sorted(expected - installed)
    extra = sorted(installed - expected)
    ok_count = len(expected) - len(missing)

    table = Table(
        box=box.SIMPLE_HEAD, show_header=True, header_style="bold", padding=(0, 1)
    )
    table.add_column("Category", min_width=20)
    table.add_column("Status", min_width=14)
    table.add_column("Details")

    status_parts = []
    if ok_count:
        status_parts.append(f"[green]{ok_count} ✓[/green]")
    if missing:
        status_parts.append(f"[red]{len(missing)} ✗[/red]")
    status = "  ".join(status_parts) or "[dim]—[/dim]"

    details_parts = []
    if missing:
        details_parts.append("[red]missing: " + ", ".join(missing) + "[/red]")
    if not details_parts:
        details_parts.append("[dim]all present[/dim]")

    table.add_row(
        f"agent skills ({len(expected)})",
        status,
        "  ".join(details_parts),
    )
    console.print(table)

    if extra:
        console.print(
            f"[dim]{len(extra)} installed but not in lockfile: "
            + ", ".join(extra)
            + "[/dim]\n"
        )
