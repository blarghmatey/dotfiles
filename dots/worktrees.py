"""Find git worktrees that can be cleaned out.

Worktrees pile up from agent sessions and PR branches, and most outlive their
PR. Squash merges mean "is HEAD an ancestor of main" misses most of them, so a
worktree also counts as merged when its branch's PR merged and the local HEAD
is the PR head (or behind it): nothing local is lost by removing it.
"""

from __future__ import annotations

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
ONE_DAY_SECONDS = 86400


class Category(StrEnum):
    """Cleanup verdict for one worktree, ordered roughly from safest to remove to keep."""

    PRUNABLE = "prunable"  # directory is gone, git still tracks it
    MERGED = "merged"  # all local commits are in the base branch or a merged PR
    CLOSED = "closed"  # PR closed unmerged; HEAD matches the PR head on GitHub
    STALE = "stale"  # no PR, clean, no activity for --stale-days
    DIRTY = "dirty"  # uncommitted or untracked files
    MERGED_PLUS = "merged+local"  # PR merged, but HEAD has commits beyond it
    OPEN = "open"  # open PR
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


def _fetch_prs(owner: str, name: str, branches: list[str]) -> dict[str, list[dict]]:
    """Map branch name -> PRs with that head branch, via batched GraphQL."""
    result: dict[str, list[dict]] = {}
    for i in range(0, len(branches), GITHUB_BATCH_SIZE):
        batch = branches[i : i + GITHUB_BATCH_SIZE]
        fields = "\n".join(
            f"b{j}: pullRequests(headRefName: {json.dumps(b)}, first: 10, "
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
            continue
        repo_node = json.loads(r.stdout)["data"]["repository"]
        for j, b in enumerate(batch):
            result[b] = repo_node[f"b{j}"]["nodes"]
    return result


def _pick_pr(prs: list[dict], head: str) -> dict | None:
    """Prefer the PR whose head is our HEAD, then an open one, then the most recent."""
    for pr in prs:
        if pr["headRefOid"] == head:
            return pr
    for pr in prs:
        if pr["state"] == "OPEN":
            return pr
    return prs[0] if prs else None


def _unique_patches(wt_path: Path, upstream: str) -> set[str]:
    """Commits in HEAD with no patch-equivalent in `upstream` (survives rebases)."""
    lines = _out(wt_path, "cherry", upstream, "HEAD").splitlines()
    return {line[2:] for line in lines if line.startswith("+")}


def _commits_beyond_pr(wt_path: Path, base: str | None, pr: dict) -> int:
    """How many local commits are in neither the base branch nor the PR's head.

    The PR head is usually gone from origin after merge, so fetch it from
    refs/pull/N/head when it isn't already in the object store.
    """
    pr_head = pr["headRefOid"]
    if _git(wt_path, "cat-file", "-e", f"{pr_head}^{{commit}}").returncode:
        _git(wt_path, "fetch", "--quiet", "--no-tags", "origin", f"refs/pull/{pr['number']}/head")
        if _git(wt_path, "cat-file", "-e", f"{pr_head}^{{commit}}").returncode:
            return -1
    beyond = _unique_patches(wt_path, pr_head)
    if base:
        beyond &= _unique_patches(wt_path, base)
    return len(beyond)


def _classify(
    wt: Worktree,
    entry: dict[str, str],
    *,
    base: str | None,
    pr: dict | None,
    cwds: set[Path],
    stale_days: int,
) -> None:
    path = Path(wt.path)
    if "prunable" in entry:
        wt.category, wt.reason = Category.PRUNABLE, entry["prunable"] or "directory missing"
        return
    if "locked" in entry:
        wt.category, wt.reason = Category.LOCKED, entry["locked"]
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
    if pr and pr["state"] in {"MERGED", "CLOSED"}:
        state = pr["state"].lower()
        beyond = _commits_beyond_pr(path, base, pr)
        if beyond == 0:
            cat = Category.MERGED if state == "merged" else Category.CLOSED
            wt.category, wt.reason = cat, f"PR #{pr['number']} {state}"
        elif beyond < 0:
            wt.category, wt.reason = (
                Category.MERGED_PLUS,
                f"PR #{pr['number']} {state}, its head commit could not be fetched",
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
    prs: dict[str, list[dict]] = {}
    if host == "github.com" and owner:
        prs = _fetch_prs(owner, name, sorted({w.branch for w in wts if w.branch}))

    for wt, entry in zip(wts, entries, strict=True):
        pr = _pick_pr(prs.get(wt.branch, []), wt.head) if wt.branch else None
        _classify(wt, entry, base=base, pr=pr, cwds=cwds, stale_days=stale_days)
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


def remove(wts: list[Worktree], categories: set[Category]) -> None:
    """Remove worktrees in `categories`; delete the branch where it is recoverable.

    Uses plain `git worktree remove`, which refuses a tree with modified or
    untracked files, so a tree that changed since the scan is left alone.
    """
    prune_repos = set()
    for wt in wts:
        if wt.category not in categories:
            continue
        if wt.category == Category.PRUNABLE:
            prune_repos.add(wt.repo_path)
            continue
        r = _git(wt.repo_path, "worktree", "remove", wt.path)
        if r.returncode:
            console.print(f"[red]✗[/red] {wt.path}: {r.stderr.strip()}")
            continue
        msg = f"[green]✓[/green] removed {wt.path}"
        if wt.branch and wt.category in DELETE_BRANCH:
            b = _git(wt.repo_path, "branch", "-D", wt.branch)
            msg += (
                f", deleted branch {wt.branch}"
                if b.returncode == 0
                else f" (branch kept: {b.stderr.strip()})"
            )
        console.print(msg)
        prune_repos.add(wt.repo_path)
    for repo in sorted(prune_repos):
        _git(repo, "worktree", "prune")


def to_json(wts: list[Worktree]) -> str:
    return json.dumps([asdict(w) for w in wts], indent=2)
