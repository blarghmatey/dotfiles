from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from pathlib import Path


class FileState(str, Enum):
    LINKED_OK = "linked-ok"
    LINKED_WRONG = "linked-wrong"
    BROKEN = "broken"
    RENDERED = "rendered"
    RENDERED_STALE = "rendered-stale"
    COPY_SAME = "copy-same"
    COPY_DIFFERENT = "copy-different"
    MISSING = "missing"


@dataclass
class TrackedFile:
    """A file tracked in the dotfiles repo."""

    src: Path          # Absolute path within repo home/
    dst: Path          # Absolute path within ~/
    is_template: bool  # True when src has a .tmpl extension
    sensitive: bool = False

    @property
    def rel(self) -> str:
        """Path relative to the home directory."""
        return str(self.dst.relative_to(Path.home()))


@dataclass
class FileStatus:
    """Current on-disk status of a tracked file."""

    file: TrackedFile
    state: FileState
    detail: str = ""
