from __future__ import annotations

from pathlib import Path

from rich.console import Console
from rich.table import Table
from rich.text import Text

from ._types import FileState, FileStatus
from .sync import get_status, iter_tracked

console = Console()

_STATE_STYLE: dict[FileState, tuple[str, str]] = {
    FileState.LINKED_OK:       ("✓ linked-ok",       "green"),
    FileState.LINKED_WRONG:    ("⚡ linked-wrong",    "yellow"),
    FileState.BROKEN:          ("✗ broken",           "red bold"),
    FileState.RENDERED:        ("✓ rendered",         "blue"),
    FileState.RENDERED_STALE:  ("⚡ rendered-stale",  "yellow"),
    FileState.COPY_SAME:       ("~ copy-same",        "dim"),
    FileState.COPY_DIFFERENT:  ("⚡ copy-different",  "yellow"),
    FileState.MISSING:         ("✗ missing",          "red bold"),
}


def print_status(repo_root: Path, home: Path) -> None:
    tracked = iter_tracked(repo_root, home)
    results = [get_status(f) for f in tracked]

    table = Table(title="Dotfiles status", show_header=True, header_style="bold")
    table.add_column("File", no_wrap=True)
    table.add_column("State")
    table.add_column("Detail", style="dim")

    for r in sorted(results, key=lambda r: r.file.rel):
        label, style = _STATE_STYLE[r.state]
        name = r.file.rel + (" 🔑" if r.file.sensitive else "")
        table.add_row(name, Text(label, style=style), r.detail)

    console.print(table)

    counts: dict[FileState, int] = {}
    for r in results:
        counts[r.state] = counts.get(r.state, 0) + 1

    ok = counts.get(FileState.LINKED_OK, 0) + counts.get(FileState.RENDERED, 0)
    drift = sum(
        counts.get(s, 0)
        for s in (FileState.LINKED_WRONG, FileState.COPY_DIFFERENT, FileState.RENDERED_STALE)
    )
    err = counts.get(FileState.MISSING, 0) + counts.get(FileState.BROKEN, 0)

    console.print(
        f"\n[green]{ok}[/green] ok  "
        f"[yellow]{drift}[/yellow] drift  "
        f"[red]{err}[/red] missing/broken"
    )
