"""Repo manifest generation.

Gives an agent (or you) the lay of the land for a directory tree of git
checkouts: what's there, who owns it, and what it's for. Pulls org/url/
worktree-count straight from git and description/topics from GitHub,
batched via one GraphQL query per ~50 repos rather than one REST call per
repo. A `note` field is sticky -- your own hand-written text, carried
forward across regenerations, for repos GitHub has no description for (or
context beyond what GitHub knows).
"""

from __future__ import annotations

import json
import os
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

import yaml
from rich.console import Console

console = Console()

MAX_DEPTH = 8
GITHUB_BATCH_SIZE = 50


@dataclass
class RepoInfo:
    """Everything known about one repo checkout."""

    name: str
    path: str
    org: str
    host: str
    url: str
    description: str = ""
    topics: list[str] = field(default_factory=list)
    worktrees: int = 0
    note: str | None = None


def _run(*args: str, cwd: Path | None = None) -> str:
    result = subprocess.run(args, cwd=cwd, capture_output=True, text=True, check=False)
    return result.stdout.strip() if result.returncode == 0 else ""


def find_repos(root: Path) -> list[Path]:
    """All git repo roots under `root`, up to MAX_DEPTH."""
    found: list[Path] = []
    root_depth = len(root.parts)
    for dirpath, dirnames, _ in os.walk(root):
        current = Path(dirpath)
        if len(current.parts) - root_depth >= MAX_DEPTH:
            dirnames.clear()
            continue
        if ".git" in dirnames:
            found.append(current)
    return sorted(found)


def _parse_remote(url: str) -> tuple[str, str, str]:
    """Return (host, org, repo_name) parsed from a git remote URL, or ("", "", "")."""
    if not url:
        return "", "", ""
    u = url.rstrip("/")
    if u.endswith(".git"):
        u = u[:-4]
    if u.startswith("git@"):
        host, _, path = u[len("git@") :].partition(":")
    else:
        host, _, path = u.split("://", 1)[-1].partition("/")
    parts = path.rsplit("/", 1)
    if len(parts) != 2:
        return host, "", ""
    return host, parts[0], parts[1]


def _worktree_count(repo_path: Path) -> int:
    out = _run("git", "worktree", "list", cwd=repo_path)
    return max(len(out.splitlines()) - 1, 0) if out else 0


def collect_repo_info(root: Path) -> list[RepoInfo]:
    """Scan `root` for git repos and gather local (git-only) info for each."""
    infos = []
    for repo_path in find_repos(root):
        url = _run("git", "remote", "get-url", "origin", cwd=repo_path)
        host, org, _name_from_url = _parse_remote(url)
        infos.append(
            RepoInfo(
                name=repo_path.name,
                path=str(repo_path.relative_to(root)),
                org=org or "local",
                host=host,
                url=url,
                worktrees=_worktree_count(repo_path),
            )
        )
    return infos


def _fetch_github_metadata(repos: list[RepoInfo]) -> int:
    """Fill description/topics for github.com repos via batched GraphQL. Mutates
    `repos` in place. Repos on other hosts or with no resolvable org are left
    untouched. Returns the number of repos successfully enriched."""
    candidates = [r for r in repos if r.host == "github.com" and r.org and r.org != "local"]
    enriched = 0
    for i in range(0, len(candidates), GITHUB_BATCH_SIZE):
        batch = candidates[i : i + GITHUB_BATCH_SIZE]
        fields = "\n".join(
            f'r{j}: repository(owner: "{r.org}", name: "{r.name}") '
            "{ description repositoryTopics(first: 20) { nodes { topic { name } } } }"
            for j, r in enumerate(batch)
        )
        result = subprocess.run(
            ["gh", "api", "graphql", "-f", f"query=query {{ {fields} }}"],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != 0:
            console.print(
                f"[yellow]warning:[/yellow] GitHub metadata batch failed: {result.stderr.strip()}"
            )
            continue
        data = json.loads(result.stdout).get("data", {})
        for j, r in enumerate(batch):
            node = data.get(f"r{j}")
            if not node:
                continue
            r.description = node.get("description") or ""
            r.topics = [
                t["topic"]["name"] for t in node.get("repositoryTopics", {}).get("nodes", [])
            ]
            enriched += 1
    return enriched


def _load_existing_notes(manifest_path: Path) -> dict[str, str | None]:
    """Map root-relative repo path -> sticky note from an existing manifest.

    Keyed on the entry's path rather than its basename: two checkouts sharing a
    folder name (personal/foo and work/foo) would otherwise share one note, and
    whichever sorted last would silently take the other's.
    """
    if not manifest_path.exists():
        return {}
    data = yaml.safe_load(manifest_path.read_text()) or {}
    return {
        entry.get("path", key): entry.get("note") for key, entry in data.get("repos", {}).items()
    }


def build_manifest(root: Path, manifest_path: Path) -> list[RepoInfo]:
    """Scan `root`, enrich from GitHub, preserve sticky notes, write `manifest_path`."""
    repos = collect_repo_info(root)
    _fetch_github_metadata(repos)

    notes = _load_existing_notes(manifest_path)
    for r in repos:
        r.note = notes.get(r.path)

    # Keyed by root-relative path, which is unique; a bare repo name is not.
    out = {
        r.path: {
            "name": r.name,
            "org": r.org,
            "url": r.url,
            "description": r.description,
            "topics": r.topics,
            "worktrees": r.worktrees,
            "note": r.note,
        }
        for r in sorted(repos, key=lambda r: r.path)
    }
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    with manifest_path.open("w") as f:
        yaml.safe_dump(
            {"repos": out}, f, sort_keys=False, default_flow_style=False, allow_unicode=True
        )
    return repos


def print_summary(repos: list[RepoInfo], manifest_path: Path) -> None:
    total = len(repos)
    described = sum(1 for r in repos if r.description or r.note)
    undescribed = [r.name for r in repos if not r.description and not r.note]
    non_github = [r.name for r in repos if r.host and r.host != "github.com"]

    console.print(f"Wrote [bold]{manifest_path}[/bold] ([green]{total}[/green] repos)")
    console.print(
        f"[green]{described}[/green] with description or note, "
        f"[yellow]{len(undescribed)}[/yellow] with neither"
    )
    if undescribed:
        console.print("\n[yellow]No description or note (agent has nothing to go on):[/yellow]")
        for name in undescribed:
            console.print(f"  - {name}")
    if non_github:
        console.print(
            f"\n[dim]{len(non_github)} repo(s) on non-github.com hosts, description not fetched:[/dim]"
        )
        for name in non_github:
            console.print(f"  - {name}")
