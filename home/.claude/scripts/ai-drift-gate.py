#!/usr/bin/env python3
"""PreToolUse gate: flag AI-drift markers in text headed for GitHub or git history.

The markers are the ones that first appeared in my writing in 2025 and never in
the 2016-2024 baseline (see ~/Documents/style-analysis/personal-style-profile.md,
Part 5). Like pr-claim-audit-gate.sh, this is a one-time speed bump: the first
attempt is blocked with the findings, and re-running the identical command goes
through, so a legitimate quote of someone else's em dash never hard-blocks.
"""

import hashlib
import json
import re
import shlex
import subprocess
import sys
import tempfile
from pathlib import Path

GH_WRITE = re.compile(r"\bgh\s+(pr|issue)\s+(create|edit|comment|review)\b")
GIT_COMMIT = re.compile(r"\bgit\s+(-C\s+\S+\s+)?commit\b")
MCP_WRITE_TOOLS = {
    "mcp__github__create_pull_request",
    "mcp__github__create_issue",
    "mcp__github__add_issue_comment",
    "mcp__github__create_pull_request_review",
    "mcp__github__update_issue",
}
MARKERS = [
    ("em/en dash", re.compile("[—–]")),
    (
        "report-style header",
        re.compile(
            r"(?:^|(?<=['\"]))#{1,4} (Summary|Root Cause|Key Changes|Changes Made|Problem|Solution"
            r"|Overview|Impact|Key Findings?|Executive Summary)\b",
            re.MULTILINE | re.IGNORECASE,
        ),
    ),
    ("status emoji", re.compile("[✅❌⚠\U0001f680\U0001f389✨]")),
    ("bold-label bullet", re.compile(r"^\s*[-*] \*\*[^*\n]+\*\*:", re.MULTILINE)),
    ("bold field label", re.compile(r"^\*\*[A-Z][\w ]+\*\*:", re.MULTILINE)),
    (
        "inflated vocabulary",
        re.compile(
            r"\b(comprehensive|seamless(ly)?|robust|leverag(e|es|ed|ing)|utiliz(e|es|ed|ing)"
            r"|cutting-edge|critical(ly)? important)\b",
            re.IGNORECASE,
        ),
    ),
]
TYPED_SUBJECT = re.compile(r"^([a-z]+(,[a-z]+)*)(\([^)]*\))?!?: (.)")


def _strip_code(text: str) -> str:
    """Drop fenced blocks and inline code so quoted code/output isn't judged as prose."""
    text = re.sub(r"```.*?```", "", text, flags=re.DOTALL)
    return re.sub(r"`[^`\n]*`", "", text)


def _tokens(command: str) -> list[str]:
    try:
        return shlex.split(command, posix=True)
    except ValueError:
        return command.split()


def _file_args(tokens: list[str], flags: set[str]) -> str:
    """Read the contents of any file passed via the given flags (--body-file, -F)."""
    contents = []
    for index, token in enumerate(tokens):
        value = None
        if token in flags and index + 1 < len(tokens):
            value = tokens[index + 1]
        elif "=" in token and token.split("=", 1)[0] in flags:
            value = token.split("=", 1)[1]
        if value and value != "-":
            path = Path(value).expanduser()
            if path.is_file():
                contents.append(path.read_text(errors="replace"))
    return "\n".join(contents)


def _commit_subject(tokens: list[str]) -> str | None:
    for index, token in enumerate(tokens):
        if token in {"-m", "--message"} and index + 1 < len(tokens):
            return tokens[index + 1].split("\n", 1)[0]
        if token.startswith("--message="):
            return token.split("=", 1)[1].split("\n", 1)[0]
    return None


def _repo_dir(command: str, cwd: str) -> str:
    match = re.match(r"\s*cd\s+(\S+)\s*&&", command)
    if match:
        return str(Path(match.group(1)).expanduser())
    match = re.search(r"\bgit\s+-C\s+(\S+)\s+commit", command)
    return str(Path(match.group(1)).expanduser()) if match else cwd


def _repo_commit_convention(repo: str) -> dict | None:
    """The typed-commit convention recent history follows, or None if it isn't typed.

    Scope and capitalization are only enforced when most of the repo's own typed
    subjects agree, so a repo that uses `feat(skills): add ...` isn't pushed toward
    the unscoped, capitalized form used elsewhere.
    """
    result = subprocess.run(
        ["git", "-C", repo, "log", "--no-merges", "-50", "--format=%s"],
        capture_output=True,
        text=True,
        check=False,
    )
    subjects = [s for s in result.stdout.splitlines() if s]
    typed = [m for s in subjects if (m := TYPED_SUBJECT.match(s))]
    if len(subjects) < 10 or len(typed) / len(subjects) < 0.6:
        return None
    return {
        "unscoped": sum(m.group(3) is None for m in typed) / len(typed) >= 0.8,
        "capitalized": sum(m.group(4).isupper() for m in typed) / len(typed) >= 0.8,
    }


def _findings(payload: dict) -> list[str]:
    tool = payload.get("tool_name", "")
    tool_input = payload.get("tool_input") or {}
    found: list[str] = []
    if tool == "Bash":
        command = tool_input.get("command") or ""
        is_gh, is_commit = bool(GH_WRITE.search(command)), bool(GIT_COMMIT.search(command))
        if not (is_gh or is_commit):
            return []
        tokens = _tokens(command)
        text = command + "\n" + _file_args(tokens, {"--body-file", "-F", "--file"})
        if is_commit:
            subject = _commit_subject(tokens)
            repo = _repo_dir(command, payload.get("cwd") or ".")
            convention = _repo_commit_convention(repo) if subject else None
            if convention:
                match = TYPED_SUBJECT.match(subject)
                if not match:
                    found.append(
                        f"commit subject {subject!r}: this repo uses typed commits (`type: Subject`)"
                    )
                else:
                    if convention["unscoped"] and match.group(3):
                        found.append(
                            f"commit subject {subject!r}: this repo's typed commits don't use scopes"
                        )
                    if convention["capitalized"] and not match.group(4).isupper():
                        found.append(
                            f"commit subject {subject!r}: capitalize the description after the type"
                        )
            if subject and len(subject) > 72:
                found.append(f"commit subject is {len(subject)} chars (max 72)")
            if subject and subject.rstrip().endswith("."):
                found.append("commit subject ends with a period")
    elif tool in MCP_WRITE_TOOLS:
        text = "\n".join(str(tool_input.get(key) or "") for key in ("title", "body"))
    else:
        return []
    prose = _strip_code(text)
    for label, pattern in MARKERS:
        hits = sorted({m.group(0) for m in pattern.finditer(prose)})
        if hits:
            found.append(f"{label}: {', '.join(repr(h) for h in hits[:5])}")
    return found


def main() -> int:
    try:
        payload = json.load(sys.stdin)
    except json.JSONDecodeError:
        return 0
    found = _findings(payload)
    if not found:
        return 0
    key = json.dumps([payload.get("tool_name"), payload.get("tool_input")], sort_keys=True).encode()
    marker = (
        Path(tempfile.gettempdir()) / f"claude-ai-drift-seen-{hashlib.sha256(key).hexdigest()[:16]}"
    )
    if marker.exists():
        marker.unlink()
        return 0
    marker.touch()
    print(
        "Style check (pre-AI baseline profile) flagged this text:\n  - "
        + "\n  - ".join(found)
        + "\nRewrite it (commas/parentheses instead of dashes, prose instead of report headers "
        "and bold labels, concrete words instead of inflated ones). If a hit is a legitimate "
        "quote or required by a repo template, re-run the identical command to proceed.",
        file=sys.stderr,
    )
    return 2


if __name__ == "__main__":
    sys.exit(main())
