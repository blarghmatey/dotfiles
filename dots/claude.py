"""Claude Code configuration management.

Tracked files under home/.claude/:
    settings.json       — Global settings (model, MCP servers, hooks, worktree)
    CLAUDE.md           — Global instructions injected into every session
    keybindings.json    — Custom key bindings
    commands/           — Custom slash commands (one .md file per command)

``dots install claude``  — ensure the Claude Code CLI is installed globally
``dots sync``            — symlink all of the above into ~/.claude/
``dots diff``            — includes a Claude Code section
``dots upgrade``         — includes a claude version check

The commands/ directory is synced file-by-file: each .md inside
home/.claude/commands/ becomes a symlink at ~/.claude/commands/<name>.md.
Add a new file there to register a new /command in Claude Code.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

from rich import box
from rich.console import Console
from rich.table import Table

console = Console()

_CLAUDE_CLI_PKG = "@anthropic-ai/claude-code"
_CLAUDE_DIR = Path.home() / ".claude"

# Files tracked in the repo (relative to home/.claude/)
_TRACKED_FILES = [
    "settings.json",
    "CLAUDE.md",
    "keybindings.json",
]
# Directories whose *contents* are synced as individual file symlinks.
_TRACKED_DIRS = [
    "commands",
]


# ── helpers ───────────────────────────────────────────────────────────────────


def _claude_installed() -> bool:
    """Return True if the claude CLI is on PATH."""
    return subprocess.run(["which", "claude"], capture_output=True, check=False).returncode == 0


def _claude_version() -> str:
    """Return the installed claude version string, or '' if not found."""
    result = subprocess.run(["claude", "--version"], capture_output=True, text=True, check=False)
    return result.stdout.strip()


def _file_in_sync(repo_root: Path, rel: str) -> bool | None:
    """Compare a tracked file against the live ~/.claude/ copy.

    Returns True if symlinked to the repo copy or byte-identical,
    False if both exist but differ, None if either side is missing.
    """
    repo_copy = repo_root / "home" / ".claude" / rel
    live_copy = _CLAUDE_DIR / rel
    if not repo_copy.exists():
        return None
    if not live_copy.exists() and not live_copy.is_symlink():
        return None
    if live_copy.is_symlink() and live_copy.resolve() == repo_copy.resolve():
        return True
    return live_copy.exists() and live_copy.read_bytes() == repo_copy.read_bytes()


def _dir_contents_linked(repo_root: Path, rel: str) -> tuple[int, list[str]]:
    """Check per-file symlink status of a tracked commands directory.

    Returns (ok_count, missing_names) where missing_names are files that exist
    in the repo but are not yet symlinked in ~/.claude/<rel>/.
    """
    repo_dir = repo_root / "home" / ".claude" / rel
    live_dir = _CLAUDE_DIR / rel
    if not repo_dir.exists():
        return 0, []

    ok = 0
    missing: list[str] = []
    for src in sorted(repo_dir.iterdir()):
        if src.name.startswith("."):
            continue
        live = live_dir / src.name
        if live.is_symlink() and live.resolve() == src.resolve():
            ok += 1
        else:
            missing.append(src.name)
    return ok, missing


def _count_commands(repo_root: Path) -> int:
    """Count .md command files tracked in home/.claude/commands/."""
    commands_dir = repo_root / "home" / ".claude" / "commands"
    if not commands_dir.exists():
        return 0
    return sum(1 for f in commands_dir.iterdir() if f.suffix == ".md")


def _mcp_servers(repo_root: Path) -> dict[str, dict]:
    """Return the mcpServers dict from the tracked settings.json."""
    settings = repo_root / "home" / ".claude" / "settings.json"
    if not settings.exists():
        return {}
    try:
        with settings.open() as f:
            return json.load(f).get("mcpServers", {})
    except (json.JSONDecodeError, KeyError):
        return {}


def _lsp_servers(repo_root: Path) -> dict[str, dict]:
    """Return the lspServers dict from the tracked settings.json."""
    settings = repo_root / "home" / ".claude" / "settings.json"
    if not settings.exists():
        return {}
    try:
        with settings.open() as f:
            return json.load(f).get("lspServers", {})
    except (json.JSONDecodeError, KeyError):
        return {}


def _binary_on_path(command: str) -> bool:
    """Return True if *command* resolves on PATH."""
    return subprocess.run(["which", command], capture_output=True, check=False).returncode == 0


# ── public API ────────────────────────────────────────────────────────────────


def install_claude(_repo_root: Path) -> None:
    """Ensure the Claude Code CLI is installed globally via npm."""
    if _claude_installed():
        version = _claude_version()
        console.print(
            "[dim]Claude Code already installed[/dim]"
            + (f"  [dim]({version})[/dim]" if version else "")
        )
        return

    console.print(f"[bold]Installing Claude Code CLI[/bold]  ({_CLAUDE_CLI_PKG})")
    subprocess.run(["npm", "install", "-g", _CLAUDE_CLI_PKG], check=True)
    console.print("[green]✓[/green] Claude Code installed.")
    console.print("[dim]Run: dots sync  — to link config into ~/.claude/[/dim]")


def diff_claude(repo_root: Path) -> None:
    """Print a diff table for Claude Code configuration."""
    console.print("\n[bold]Claude Code diff[/bold]\n")

    table = Table(
        box=box.SIMPLE_HEAD,
        show_header=True,
        header_style="bold",
        padding=(0, 1),
    )
    table.add_column("Component", min_width=28)
    table.add_column("Status", min_width=16)
    table.add_column("Details")

    # ── claude CLI ────────────────────────────────────────────────────────────
    cli_ok = _claude_installed()
    version = _claude_version() if cli_ok else ""
    cli_detail = (
        (f"[dim]{version}[/dim]" if version else "[dim]installed[/dim]")
        if cli_ok
        else f"[red]missing — run: npm install -g {_CLAUDE_CLI_PKG}[/red]"
    )
    table.add_row(
        "claude CLI",
        "[green]✓[/green]" if cli_ok else "[red]✗[/red]",
        cli_detail,
    )

    # ── tracked files ─────────────────────────────────────────────────────────
    for rel in _TRACKED_FILES:
        repo_copy = repo_root / "home" / ".claude" / rel
        in_sync = _file_in_sync(repo_root, rel)
        label = f".claude/{rel}"

        if in_sync is None:
            if not repo_copy.exists():
                status, detail = "[dim]—[/dim]", "[dim]not tracked in dotfiles[/dim]"
            else:
                status = "[yellow]?[/yellow]"
                detail = "[yellow]not linked — run: dots sync[/yellow]"
        elif in_sync:
            status, detail = "[green]✓[/green]", "[dim]in sync[/dim]"
        else:
            status = "[yellow]≠[/yellow]"
            detail = (
                f"[yellow]live copy differs — run: cp ~/.claude/{rel} home/.claude/{rel}[/yellow]"
            )
        table.add_row(label, status, detail)

    # ── tracked directories ───────────────────────────────────────────────────
    for rel in _TRACKED_DIRS:
        repo_dir = repo_root / "home" / ".claude" / rel
        if not repo_dir.exists():
            table.add_row(
                f".claude/{rel}/",
                "[dim]—[/dim]",
                "[dim]not tracked in dotfiles[/dim]",
            )
            continue

        ok, missing = _dir_contents_linked(repo_root, rel)
        total = ok + len(missing)
        label = f".claude/{rel}/ ({total} files)"

        if not missing:
            status = "[green]✓[/green]"
            detail = "[dim]all symlinked[/dim]"
        else:
            status = f"[green]{ok} ✓[/green]  [yellow]{len(missing)} ?[/yellow]"
            detail = f"[yellow]not linked: {', '.join(missing)} — run: dots sync[/yellow]"
        table.add_row(label, status, detail)

    # ── MCP servers summary ───────────────────────────────────────────────────
    servers = _mcp_servers(repo_root)
    server_names = ", ".join(sorted(servers)) if servers else "none"
    table.add_row(
        f"MCP servers ({len(servers)})",
        f"[cyan]{len(servers)}[/cyan]" if servers else "[dim]—[/dim]",
        f"[dim]{server_names}[/dim]",
    )

    # ── LSP servers ───────────────────────────────────────────────────────────
    lsp = _lsp_servers(repo_root)
    if lsp:
        ok_lsp: list[str] = []
        missing_lsp: list[str] = []
        for name, cfg in lsp.items():
            cmd = cfg.get("command", name)
            if _binary_on_path(cmd):
                ok_lsp.append(name)
            else:
                missing_lsp.append(f"{name} ({cmd})")

        lsp_status_parts: list[str] = []
        if ok_lsp:
            lsp_status_parts.append(f"[green]{len(ok_lsp)} ✓[/green]")
        if missing_lsp:
            lsp_status_parts.append(f"[red]{len(missing_lsp)} ✗[/red]")

        lsp_detail = (
            "[red]binary missing: " + ", ".join(missing_lsp) + "[/red]"
            if missing_lsp
            else "[dim]all binaries found[/dim]"
        )
        table.add_row(
            f"LSP servers ({len(lsp)})",
            "  ".join(lsp_status_parts) or "[dim]—[/dim]",
            lsp_detail,
        )
    else:
        table.add_row("LSP servers", "[dim]—[/dim]", "[dim]none configured[/dim]")

    console.print(table)


def upgrade_claude() -> None:
    """Upgrade Claude Code to the latest version via npm."""
    if not _claude_installed():
        console.print("[yellow]Claude Code not installed — run: dots install claude[/yellow]")
        return
    console.print(f"[bold]Upgrading Claude Code CLI[/bold]  ({_CLAUDE_CLI_PKG})")
    subprocess.run(["npm", "install", "-g", _CLAUDE_CLI_PKG], check=True)
