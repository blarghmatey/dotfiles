from __future__ import annotations

import tomllib
from functools import lru_cache
from pathlib import Path

# deploy/ lives one level below the repo root
_MANIFEST_PATH = Path(__file__).parent.parent / "manifest.toml"


@lru_cache(maxsize=1)
def load() -> dict:
    """Return the parsed manifest.toml (cached for the lifetime of the deploy run)."""
    with open(_MANIFEST_PATH, "rb") as f:
        return tomllib.load(f)


def profile(name: str) -> dict:
    """Return the manifest section for *name*, raising KeyError if absent."""
    manifest = load()
    try:
        return manifest["profiles"][name]
    except KeyError:
        available = list(manifest.get("profiles", {}).keys())
        raise KeyError(
            f"Profile {name!r} not found in manifest.toml. "
            f"Available: {available}"
        ) from None
