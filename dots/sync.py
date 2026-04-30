from __future__ import annotations

import tomllib
from pathlib import Path

from rich.console import Console

from ._types import FileState, FileStatus, TrackedFile
from .templates import render_file

console = Console()


def _load_manifest(repo_root: Path) -> dict:
    with open(repo_root / "manifest.toml", "rb") as f:
        return tomllib.load(f)


def iter_tracked(repo_root: Path, home: Path) -> list[TrackedFile]:
    """Walk *repo_root*/home/ and return a TrackedFile for every file."""
    home_src = repo_root / "home"
    manifest = _load_manifest(repo_root)

    sync_cfg = manifest.get("sync", {})
    exclude_globs: list[str] = sync_cfg.get("exclude", [])
    sensitive_set: set[str] = set(sync_cfg.get("sensitive", []))

    # Build template map: repo-relative source path -> home-relative target path
    template_map: dict[str, str] = {
        t["source"]: t["target"]
        for t in sync_cfg.get("templates", [])
    }

    files: list[TrackedFile] = []
    for src in sorted(home_src.rglob("*")):
        if not src.is_file():
            continue

        rel = src.relative_to(home_src)
        rel_str = str(rel)

        if any(src.match(g) for g in exclude_globs):
            continue

        is_template = src.suffix == ".tmpl"
        if is_template:
            target_rel = template_map.get(rel_str, rel_str.removesuffix(".tmpl"))
            dst = home / target_rel
        else:
            dst = home / rel

        dst_rel = str(dst.relative_to(home))
        files.append(TrackedFile(
            src=src,
            dst=dst,
            is_template=is_template,
            sensitive=rel_str in sensitive_set or dst_rel in sensitive_set,
        ))

    return files


def get_status(tracked: TrackedFile) -> FileStatus:
    """Return the current on-disk status of *tracked*."""
    dst = tracked.dst

    if tracked.is_template:
        if not dst.exists():
            return FileStatus(tracked, FileState.MISSING, "template not rendered")
        return FileStatus(tracked, FileState.RENDERED)

    if not dst.exists() and not dst.is_symlink():
        return FileStatus(tracked, FileState.MISSING)

    if dst.is_symlink():
        if not dst.exists():
            return FileStatus(tracked, FileState.BROKEN, f"→ {dst.readlink()}")
        if dst.resolve() == tracked.src.resolve():
            return FileStatus(tracked, FileState.LINKED_OK)
        return FileStatus(tracked, FileState.LINKED_WRONG, f"→ {dst.readlink()}")

    # Regular (non-symlink) file
    if dst.read_bytes() == tracked.src.read_bytes():
        return FileStatus(tracked, FileState.COPY_SAME)
    return FileStatus(tracked, FileState.COPY_DIFFERENT)


def sync_all(
    repo_root: Path,
    home: Path,
    *,
    force: bool = False,
    dry_run: bool = False,
) -> None:
    """Sync every tracked file: render templates, create symlinks."""
    tracked_files = iter_tracked(repo_root, home)
    results: list[FileStatus] = []

    for f in tracked_files:
        current = get_status(f)

        if current.state in (FileState.LINKED_OK, FileState.RENDERED):
            results.append(current)
            continue

        if f.is_template:
            _do_render(f, dry_run=dry_run)
            results.append(FileStatus(f, FileState.RENDERED))
        else:
            _do_link(f, force=force, dry_run=dry_run)
            if current.state not in (FileState.COPY_SAME,):
                results.append(FileStatus(f, FileState.LINKED_OK))
            else:
                results.append(current)

    _print_summary(results, dry_run=dry_run)


def _do_render(f: TrackedFile, *, dry_run: bool) -> None:
    verb = "[dim]would render[/dim]" if dry_run else "[blue]rendered  [/blue]"
    console.print(f"  {verb}  {f.rel}")
    if not dry_run:
        render_file(f.src, f.dst)


def _do_link(f: TrackedFile, *, force: bool, dry_run: bool) -> None:
    dst = f.dst

    if dst.exists() or dst.is_symlink():
        if not force:
            console.print(
                f"  [yellow]skip[/yellow]       {f.rel}"
                "  [dim](exists — use --force to overwrite)[/dim]"
            )
            return
        if not dry_run:
            dst.unlink()

    verb = "[dim]would link[/dim]" if dry_run else "[green]linked    [/green]"
    console.print(f"  {verb}  {f.rel}")
    if not dry_run:
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.symlink_to(f.src)


def _print_summary(results: list[FileStatus], *, dry_run: bool) -> None:
    counts: dict[FileState, int] = {}
    for r in results:
        counts[r.state] = counts.get(r.state, 0) + 1

    prefix = "[dim]Dry run —[/dim] " if dry_run else ""
    console.print(
        f"\n{prefix}"
        f"[green]{counts.get(FileState.LINKED_OK, 0)}[/green] linked  "
        f"[blue]{counts.get(FileState.RENDERED, 0)}[/blue] rendered  "
        f"[yellow]{counts.get(FileState.COPY_SAME, 0)}[/yellow] already in sync  "
        f"[red]{counts.get(FileState.MISSING, 0)}[/red] missing"
    )
