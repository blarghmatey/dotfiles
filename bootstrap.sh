#!/usr/bin/env bash
# bootstrap.sh — set up a fresh Arch/Manjaro machine from scratch.
#
# This script has zero third-party dependencies. It installs:
#   1. base-devel + git (system packages)
#   2. yay (AUR helper)
#   3. uv (Python package/toolchain manager)
#   4. uvenv (pipx-like Python tool manager)
#   5. dots CLI (this repo, as an editable install)
#
# After bootstrap, run:
#   dots sync              # symlink dotfiles
#   dots install all       # install all packages and tools
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GREEN='\033[0;32m'
DIM='\033[2m'
RESET='\033[0m'

step() { echo -e "\n${GREEN}▶ $*${RESET}"; }
info() { echo -e "  ${DIM}$*${RESET}"; }

# ── 1. System base ────────────────────────────────────────────────────────────
step "Ensuring base-devel and git are installed"
sudo pacman -S --needed --noconfirm base-devel git

# ── 2. yay (AUR helper) ───────────────────────────────────────────────────────
if ! command -v yay &>/dev/null; then
    step "Installing yay (AUR helper)"
    _tmpdir="$(mktemp -d)"
    git clone --depth=1 https://aur.archlinux.org/yay.git "$_tmpdir/yay"
    (cd "$_tmpdir/yay" && makepkg -si --noconfirm)
    rm -rf "$_tmpdir"
else
    info "yay already installed — skipping"
fi

# ── 3. uv ─────────────────────────────────────────────────────────────────────
if ! command -v uv &>/dev/null; then
    step "Installing uv (Python package manager)"
    curl -LsSf https://astral.sh/uv/install.sh | sh
    # Make uv available for the rest of this script
    export PATH="$HOME/.local/bin:$PATH"
else
    info "uv already installed — skipping"
fi

# ── 4. uvenv ──────────────────────────────────────────────────────────────────
if ! command -v uvenv &>/dev/null; then
    step "Installing uvenv"
    uv tool install uvenv
    export PATH="$(uv tool dir)/bin:$PATH"
else
    info "uvenv already installed — skipping"
fi

# ── 5. dots CLI ───────────────────────────────────────────────────────────────
# Use uv tool install directly (not uvenv) — uvenv install has a bug where it
# tries to parse the existing uvenv.lock before writing and fails if any entry
# is missing the `classifier` field added in newer uvenv versions.
step "Installing dots CLI (editable install from repo)"
cd "$REPO_DIR"
uv tool install --editable .

echo ""
echo -e "${GREEN}Bootstrap complete.${RESET}"
echo ""
echo "  Next steps:"
echo "    dots sync              # symlink dotfiles into ~/"
echo "    dots install all       # install system packages, Python tools, and npm globals"
echo "    dots status            # verify everything is in sync"
