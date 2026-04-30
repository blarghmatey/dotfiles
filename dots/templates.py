from __future__ import annotations

import os
import re
import subprocess
from pathlib import Path

_PASS_RE = re.compile(r"\{\{\s*pass:([^\}]+?)\s*\}\}")
_ENV_RE = re.compile(r"\{\{\s*env:([^\}]+?)\s*\}\}")


def _resolve_pass(secret: str) -> str:
    result = subprocess.run(
        ["pass", "show", secret.strip()],
        capture_output=True,
        text=True,
        check=True,
    )
    # `pass show` prints the secret on the first line
    return result.stdout.split("\n")[0].strip()


def _resolve_env(var: str) -> str:
    value = os.environ.get(var.strip())
    if value is None:
        raise KeyError(f"Environment variable not set: {var!r}")
    return value


def render(content: str) -> str:
    """Substitute {{ pass:name }} and {{ env:VAR }} markers in *content*."""

    def replace_pass(m: re.Match[str]) -> str:
        return _resolve_pass(m.group(1))

    def replace_env(m: re.Match[str]) -> str:
        return _resolve_env(m.group(1))

    content = _PASS_RE.sub(replace_pass, content)
    content = _ENV_RE.sub(replace_env, content)
    return content


def render_file(src: Path, dst: Path, *, dry_run: bool = False) -> None:
    """Read *src*, render all template markers, and write rendered output to *dst*."""
    raw = src.read_text()
    rendered = render(raw)
    if dry_run:
        return
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_text(rendered)
