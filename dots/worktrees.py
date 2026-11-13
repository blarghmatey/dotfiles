"""Find git worktrees that can be cleaned out.

Worktrees pile up from agent sessions and PR branches, and most outlive their
PR. Squash merges and rebases mean "is HEAD an ancestor of main" misses most of
them, so a worktree also counts as merged when every local commit is
patch-equivalent to something in the base branch or in its merged PR's head:
nothing local is lost by removing it.
"""

from __future__ import annotations

import filecmp
import fnmatch
import json
import os
import subprocess
import time
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from dataclasses import asdict, dataclass
from enum import StrEnum
from pathlib import Path

from rich.console import Console
from rich.table import Table

from .repos import _parse_remote

console = Console()

MAX_DEPTH = 4
FETCH_WORKERS = 8
GITHUB_BATCH_SIZE = 40
PRS_PER_BRANCH = 20
ONE_DAY_SECONDS = 86400
MAX_LISTED_PATHS = 3

# Ignored paths a build, test run or install recreates. Any other ignored file
# (.env, *.local.env, settings.local.json, Pulumi.<stack>.yaml) keeps the worktree.
REGENERABLE_IGNORED = (
    "__pycache__",
    "*.pyc",
    ".venv",
    "venv",
    "*.egg-info",
    ".eggs",
    ".ruff_cache",
    ".pytest_cache",
    ".mypy_cache",
    ".hypothesis",
    ".tox",
    ".nox",
    ".coverage",
    "coverage",
    "coverage.xml",
    "htmlcov",
    "pytest_warnings*.json",
    ".drf_lint_cache.json",
    "node_modules",
    ".next",
    "next-env.d.ts",
    ".turbo",
    ".parcel-cache",
    "tsconfig.tsbuildinfo",
    "install-state.gz",
    ".yarn/cache",
    "webpack-stats",
    "build",
    "dist",
    "target",
    "dbt_packages",
    ".user.yml",
    "unit_test.duckdb",
    "logs",
)


class Category(StrEnum):
    """Cleanup verdict for one worktree, ordered roughly from safest to remove to keep."""

    PRUNABLE = "prunable"  # directory is gone, git still tracks it
    MERGED = "merged"  # all local commits are in the base branch or a merged PR
    CLOSED = "closed"  # PR closed unmerged; all local commits are in the PR head
    STALE = "stale"  # branch with no PR, clean, no activity for --stale-days
    DIRTY = "dirty"  # uncommitted, untracked, or non-regenerable ignored files
    UNANCHORED = "unanchored"  # detached HEAD with commits no ref holds
    MERGED_PLUS = "merged+local"  # PR merged or closed, but HEAD has commits beyond it
    OPEN = "open"  # open PR
    UNKNOWN = "unknown"  # PR state couldn't be looked up
    IN_USE = "in-use"  # a running process has its cwd inside
    LOCKED = "locked"
    ACTIVE = "active"  # anything else: recent, no PR yet


REMOVABLE = {Category.PRUNABLE, Category.MERGED, Category.CLOSED, Category.STALE}
# Branches in these categories are recoverable from the base branch or GitHub.
DELETE_BRANCH = {Category.MERGED, Category.CLOSED}


@dataclass
class Worktree:
    repo: str
    repo_path: str
    path: str
    branch: str | None
    head: str
    category: Category = Category.ACTIVE
    reason: str = ""
    age_days: float | None = None
    pr: int | None = None
    pr_url: str | None = None
    unpushed: int | None = None


def _git(cwd: Path | str, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", "-C", str(cwd), *args], capture_output=True, text=True, check=False
    )


def _out(cwd: Path | str, *args: str) -> str:
    r = _git(cwd, *args)
    if r.returncode:
        msg = f"git {' '.join(args)} in {cwd} failed: {r.stderr.strip()}"
        raise RuntimeError(msg)
    return r.stdout.strip()


def find_main_checkouts(root: Path) -> list[Path]:
    """Repo roots under `root` whose `.git` is a directory (worktrees have a `.git` file).

    Stops descending at any checkout, so it never walks node_modules or .venv trees.
    """
    found: list[Path] = []
    root_depth = len(root.parts)
    for dirpath, dirnames, filenames in os.walk(root):
        current = Path(dirpath)
        if ".git" in dirnames:
            found.append(current)
        if (
            ".git" in dirnames
            or ".git" in filenames
            or len(current.parts) - root_depth >= MAX_DEPTH
        ):
            dirnames.clear()
    return sorted(found)


def _process_cwds() -> set[Path]:
    cwds = set()
    for link in Path("/proc").glob("[0-9]*/cwd"):
        try:
            cwds.add(link.readlink())
        except OSError:
            continue  # process exited or belongs to another user
    return cwds


def _base_ref(repo: Path) -> str | None:
    r = _git(repo, "symbolic-ref", "--short", "refs/remotes/origin/HEAD")
    if r.returncode == 0:
        return r.stdout.strip()
    for ref in ("origin/main", "origin/master"):
        if _git(repo, "rev-parse", "--verify", "-q", ref).returncode == 0:
            return ref
    return None


def _list_worktrees(repo: Path) -> list[dict[str, str]]:
    """Parse `git worktree list --porcelain`, skipping the main checkout."""
    blocks = _out(repo, "worktree", "list", "--porcelain").split("\n\n")
    entries = []
    for block in blocks[1:]:
        entry = {}
        for line in block.splitlines():
            key, _, value = line.partition(" ")
            entry[key] = value
        entries.append(entry)
    return entries


def _fetch_prs(owner: str, name: str, branches: list[str]) -> dict[str, list[dict]] | None:
    """Map branch name -> PRs with that head branch, via batched GraphQL.

    Returns None if any batch fails, so callers can't mistake a failed lookup
    for "no PR".
    """
    result: dict[str, list[dict]] = {}
    for i in range(0, len(branches), GITHUB_BATCH_SIZE):
        batch = branches[i : i + GITHUB_BATCH_SIZE]
        fields = "\n".join(
            f"b{j}: pullRequests(headRefName: {json.dumps(b)}, first: {PRS_PER_BRANCH}, "
            "orderBy: {field: UPDATED_AT, direction: DESC}) "
            "{ nodes { number url state headRefOid } }"
            for j, b in enumerate(batch)
        )
        query = f"query {{ repository(owner: {json.dumps(owner)}, name: {json.dumps(name)}) {{ {fields} }} }}"
        r = subprocess.run(
            ["gh", "api", "graphql", "-f", f"query={query}"],
            capture_output=True,
            text=True,
            check=False,
        )
        if r.returncode:
            console.print(
                f"[yellow]warning:[/yellow] PR lookup for {owner}/{name} failed: {r.stderr.strip()}"
            )
            return None
        repo_node = json.loads(r.stdout)["data"]["repository"]
        for j, b in enumerate(batch):
            result[b] = repo_node[f"b{j}"]["nodes"]
    return result


def _pick_pr(prs: list[dict], head: str) -> dict | None:
    """An open PR wins; otherwise the one whose head is our HEAD, then the most recent."""
    for pr in prs:
        if pr["state"] == "OPEN":
            return pr
    for pr in prs:
        if pr["headRefOid"] == head:
            return pr
    return prs[0] if prs else None


def _unique_patches(wt_path: Path, upstream: str) -> set[str]:
    """Commits in HEAD with no patch-equivalent in `upstream` (survives rebases)."""
    lines = _out(wt_path, "cherry", upstream, "HEAD").splitlines()
    return {line[2:] for line in lines if line.startswith("+")}


def _commits_beyond_pr(wt_path: Path, base: str | None, pr: dict, *, fetch: bool) -> int:
    """How many local commits are in neither the base branch nor the PR's head.

    The PR head is usually gone from origin after merge, so fetch it from
    refs/pull/N/head when it isn't already in the object store. Returns -1 if
    the PR head isn't available.
    """
    pr_head = pr["headRefOid"]
    if _git(wt_path, "cat-file", "-e", f"{pr_head}^{{commit}}").returncode:
        if not fetch:
            return -1
        _git(wt_path, "fetch", "--quiet", "--no-tags", "origin", f"refs/pull/{pr['number']}/head")
        if _git(wt_path, "cat-file", "-e", f"{pr_head}^{{commit}}").returncode:
            return -1
    beyond = _unique_patches(wt_path, pr_head)
    if base:
        beyond &= _unique_patches(wt_path, base)
    return len(beyond)


def _anchored(repo: str, commit: str) -> bool:
    """True if some branch, remote-tracking branch or tag contains `commit`."""
    return bool(_out(repo, "for-each-ref", "--contains", commit, "--count=1"))


def _kept_ignored(wt_path: Path, repo: Path) -> list[str]:
    """Ignored paths in the worktree that would be lost by removing it.

    Skips regenerable build and cache output, and files byte-identical to the
    same path in the main checkout (e.g. a copied .env).
    """
    paths = _out(
        wt_path, "ls-files", "--others", "--ignored", "--exclude-standard", "--directory"
    ).splitlines()
    kept = []
    for p in paths:
        rel = p.rstrip("/")
        if any(
            fnmatch.fnmatch(rel, g) or fnmatch.fnmatch(rel.rsplit("/", 1)[-1], g)
            for g in REGENERABLE_IGNORED
        ):
            continue
        src, twin = wt_path / rel, repo / rel
        if src.is_file() and twin.is_file() and filecmp.cmp(src, twin, shallow=False):
            continue
        kept.append(p)
    return kept


def _classify(
    wt: Worktree,
    entry: dict[str, str],
    *,
    base: str | None,
    pr: dict | None,
    pr_lookup_ok: bool,
    cwds: set[Path],
    stale_days: int,
    fetch: bool,
) -> None:
    path = Path(wt.path)
    if "locked" in entry:
        wt.category, wt.reason = Category.LOCKED, entry["locked"] or "locked"
        return
    if "prunable" in entry:
        # Pruning drops the worktree's HEAD, the only ref a detached commit may have.
        if wt.branch or not wt.head or _anchored(wt.repo_path, wt.head):
            wt.category, wt.reason = Category.PRUNABLE, entry["prunable"] or "directory missing"
        else:
            wt.category, wt.reason = (
                Category.UNANCHORED,
                f"directory missing; detached HEAD {wt.head[:10]} is on no ref",
            )
        return
    if _git(path, "rev-parse", "-q", "--verify", "HEAD").returncode:
        wt.category, wt.reason = Category.ACTIVE, "no commits yet (unborn branch)"
        return

    wt.age_days = round(
        (time.time() - max(int(_out(path, "log", "-1", "--format=%ct")), int(path.stat().st_mtime)))
        / ONE_DAY_SECONDS,
        1,
    )
    if pr:
        wt.pr, wt.pr_url = pr["number"], pr["url"]
    if (
        wt.branch
        and _git(path, "rev-parse", "--verify", "-q", f"origin/{wt.branch}").returncode == 0
    ):
        wt.unpushed = int(_out(path, "rev-list", "--count", f"origin/{wt.branch}..HEAD"))

    if any(c == path or path in c.parents for c in cwds):
        wt.category, wt.reason = Category.IN_USE, "a process has its cwd here"
        return
    status = _out(path, "status", "--porcelain")
    if status:
        n = len(status.splitlines())
        wt.category, wt.reason = Category.DIRTY, f"{n} uncommitted/untracked path(s)"
        return
    if kept := _kept_ignored(path, Path(wt.repo_path)):
        listed = ", ".join(kept[:MAX_LISTED_PATHS]) + (
            ", ..." if len(kept) > MAX_LISTED_PATHS else ""
        )
        wt.category, wt.reason = Category.DIRTY, f"ignored files: {listed}"
        return
    if pr and pr["state"] == "OPEN":
        wt.category, wt.reason = Category.OPEN, "open PR"
        return
    if base:
        if _git(path, "merge-base", "--is-ancestor", "HEAD", base).returncode == 0:
            if not pr and wt.age_days < stale_days:
                # Branched off base with nothing committed yet: likely work about to start.
                wt.category, wt.reason = Category.ACTIVE, "no commits yet"
                return
            wt.category, wt.reason = Category.MERGED, f"HEAD is in {base}"
            return
        if not _unique_patches(path, base):
            wt.category, wt.reason = Category.MERGED, f"all patches are in {base}"
            return
    if not wt.branch:
        if _anchored(wt.repo_path, wt.head):
            wt.category, wt.reason = Category.STALE, "detached HEAD on an existing ref"
            if wt.age_days < stale_days:
                wt.category, wt.reason = Category.ACTIVE, "recent, detached HEAD"
        else:
            wt.category, wt.reason = Category.UNANCHORED, "detached HEAD with commits on no ref"
        return
    if not pr_lookup_ok:
        wt.category, wt.reason = Category.UNKNOWN, "PR lookup failed or not on GitHub"
        return
    if pr and pr["state"] in {"MERGED", "CLOSED"}:
        state = pr["state"].lower()
        beyond = _commits_beyond_pr(path, base, pr, fetch=fetch)
        if beyond == 0:
            cat = Category.MERGED if state == "merged" else Category.CLOSED
            wt.category, wt.reason = cat, f"PR #{pr['number']} {state}"
        elif beyond < 0:
            wt.category, wt.reason = (
                Category.MERGED_PLUS,
                f"PR #{pr['number']} {state}, its head commit isn't available locally",
            )
        else:
            wt.category, wt.reason = (
                Category.MERGED_PLUS,
                f"{beyond} commit(s) not in PR #{pr['number']} ({state}) or base",
            )
        return
    if wt.age_days >= stale_days:
        pushed = (
            "nothing unpushed"
            if wt.unpushed == 0
            else (
                "branch not on origin"
                if wt.unpushed is None
                else f"{wt.unpushed} unpushed commit(s)"
            )
        )
        wt.category, wt.reason = Category.STALE, f"no PR, idle {wt.age_days:.0f}d, {pushed}"
        return
    wt.category, wt.reason = Category.ACTIVE, "recent, no PR"


def scan_repo(repo: Path, *, fetch: bool, stale_days: int, cwds: set[Path]) -> list[Worktree]:
    entries = _list_worktrees(repo)
    if not entries:
        return []
    if fetch:
        # Branches only: with fetch.pruneTags set, a retagged upstream release
        # ("would clobber existing tag") otherwise fails the whole fetch.
        r = _git(
            repo,
            "fetch",
            "--prune",
            "--no-tags",
            "--quiet",
            "origin",
            "+refs/heads/*:refs/remotes/origin/*",
        )
        if r.returncode:
            console.print(
                f"[yellow]warning:[/yellow] fetch failed in {repo.name}: {r.stderr.strip()}"
            )
    base = _base_ref(repo)

    wts = [
        Worktree(
            repo=repo.name,
            repo_path=str(repo),
            path=e["worktree"],
            branch=e["branch"].removeprefix("refs/heads/") if "branch" in e else None,
            head=e.get("HEAD", ""),
        )
        for e in entries
    ]

    host, owner, name = _parse_remote(_git(repo, "remote", "get-url", "origin").stdout.strip())
    prs: dict[str, list[dict]] | None = None
    if host == "github.com" and owner:
        prs = _fetch_prs(owner, name, sorted({w.branch for w in wts if w.branch}))

    for wt, entry in zip(wts, entries, strict=True):
        pr = _pick_pr(prs.get(wt.branch, []), wt.head) if prs and wt.branch else None
        _classify(
            wt,
            entry,
            base=base,
            pr=pr,
            pr_lookup_ok=prs is not None,
            cwds=cwds,
            stale_days=stale_days,
            fetch=fetch,
        )
    return wts


def scan(root: Path, *, fetch: bool, stale_days: int) -> list[Worktree]:
    repos = find_main_checkouts(root)
    cwds = _process_cwds()
    with ThreadPoolExecutor(FETCH_WORKERS) as ex:
        results = ex.map(
            lambda r: scan_repo(r, fetch=fetch, stale_days=stale_days, cwds=cwds), repos
        )
        return [wt for wts in results for wt in wts]


def print_report(wts: list[Worktree], root: Path) -> None:
    by_cat: dict[Category, list[Worktree]] = defaultdict(list)
    for wt in wts:
        by_cat[wt.category].append(wt)
    for cat in Category:
        rows = sorted(by_cat.get(cat, []), key=lambda w: (w.repo, w.path))
        if not rows:
            continue
        style = "green" if cat in REMOVABLE else "yellow"
        table = Table(
            title=f"[{style}]{cat}[/{style}] ({len(rows)})",
            title_justify="left",
            show_edge=False,
            pad_edge=False,
        )
        table.add_column("worktree", overflow="fold")
        table.add_column("branch", overflow="fold")
        table.add_column("PR", justify="right")
        table.add_column("idle", justify="right")
        table.add_column("reason")
        for w in rows:
            rel = Path(w.path)
            rel = rel.relative_to(root) if rel.is_relative_to(root) else rel
            table.add_row(
                str(rel),
                w.branch or "(detached)",
                f"#{w.pr}" if w.pr else "",
                f"{w.age_days:.0f}d" if w.age_days is not None else "",
                w.reason,
            )
        console.print(table)
        console.print()
    counts = ", ".join(f"{c}: {len(by_cat[c])}" for c in Category if by_cat.get(c))
    console.print(f"[bold]{len(wts)}[/bold] worktrees. {counts}")


def _unchanged(wt: Worktree) -> str | None:
    """Why the worktree no longer matches the scan, or None if it still does."""
    head = _git(wt.path, "rev-parse", "HEAD").stdout.strip()
    if head != wt.head:
        return f"HEAD moved to {head[:10] or '?'} since the scan"
    branch = _git(wt.path, "symbolic-ref", "-q", "--short", "HEAD").stdout.strip() or None
    if branch != wt.branch:
        return f"now on {branch or 'a detached HEAD'} since the scan"
    return None


def remove(wts: list[Worktree], categories: set[Category]) -> None:
    """Remove worktrees in `categories`; delete the branch where it is recoverable.

    Each worktree's HEAD and branch are re-read first and must match the scan.
    `git worktree remove` (no --force) then refuses a tree that picked up
    modified or untracked files, and the branch is deleted only if it still
    points at the scanned commit.
    """
    for wt in wts:
        if wt.category not in categories or wt.category == Category.PRUNABLE:
            continue
        if why := _unchanged(wt):
            console.print(f"[yellow]skipped[/yellow] {wt.path}: {why}")
            continue
        r = _git(wt.repo_path, "worktree", "remove", wt.path)
        if r.returncode:
            console.print(f"[red]✗[/red] {wt.path}: {r.stderr.strip()}")
            continue
        msg = f"[green]✓[/green] removed {wt.path}"
        if wt.branch and wt.category in DELETE_BRANCH:
            b = _git(wt.repo_path, "update-ref", "-d", f"refs/heads/{wt.branch}", wt.head)
            msg += (
                f", deleted branch {wt.branch}"
                if b.returncode == 0
                else f" (branch kept: {b.stderr.strip()})"
            )
        console.print(msg)

    if Category.PRUNABLE not in categories:
        return
    # `git worktree prune` takes every prunable entry in a repo at once, so skip
    # any repo where one of them is a detached HEAD no ref holds.
    by_repo: dict[str, list[Worktree]] = defaultdict(list)
    for wt in wts:
        by_repo[wt.repo_path].append(wt)
    for repo, repo_wts in sorted(by_repo.items()):
        prunable = [w for w in repo_wts if w.category == Category.PRUNABLE]
        if not prunable:
            continue
        blockers = [
            w for w in repo_wts if w.category == Category.UNANCHORED and not Path(w.path).exists()
        ]
        if blockers:
            console.print(
                f"[yellow]skipped prune[/yellow] in {repo}: {blockers[0].path} has a detached "
                "HEAD on no ref; pin it with `git branch <name> <sha>` first"
            )
            continue
        _out(repo, "worktree", "prune")
        for w in prunable:
            console.print(f"[green]✓[/green] pruned {w.path}")


def to_json(wts: list[Worktree]) -> str:
    return json.dumps([asdict(w) for w in wts], indent=2)
