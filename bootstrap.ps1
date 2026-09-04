# bootstrap.ps1 - set up a fresh Windows machine to run the `dots` CLI natively.
#
# This script has zero third-party dependencies. It installs:
#   1. Scoop (package manager for the windows profile)
#   2. uv (Python package/toolchain manager)
#   3. dots CLI (this repo, as an editable install)
#
# After bootstrap, run:
#   dots bootstrap --profile windows   # WSL2 feature/kernel, Scoop sanity check
#   dots install packages --profile windows
#
# This is the Windows counterpart to bootstrap.sh, which does the same for
# Arch/WSL2. The two are independent -- this script never touches WSL.

$ErrorActionPreference = 'Stop'

function Step($msg) { Write-Host "`n==> $msg" -ForegroundColor Green }
function Info($msg) { Write-Host "    $msg" -ForegroundColor DarkGray }

function Sync-Path {
    # Re-reads PATH from the registry so binaries an installer just added
    # (Scoop shims, uv, dots) resolve in *this* session without a new shell.
    $machine = [System.Environment]::GetEnvironmentVariable('Path', 'Machine')
    $user = [System.Environment]::GetEnvironmentVariable('Path', 'User')
    $env:Path = "$machine;$user"
}

$RepoDir = Split-Path -Parent $MyInvocation.MyCommand.Path

# -- 1. Scoop -----------------------------------------------------------------
if (Get-Command scoop -ErrorAction SilentlyContinue) {
    Info 'Scoop already installed - skipping'
} else {
    Step 'Installing Scoop'
    Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
    Invoke-Expression (Invoke-RestMethod get.scoop.sh)
    Sync-Path
}

# -- 2. uv ----------------------------------------------------------------------
if (Get-Command uv -ErrorAction SilentlyContinue) {
    Info 'uv already installed - skipping'
} else {
    Step 'Installing uv (Python package manager)'
    Invoke-Expression (Invoke-RestMethod https://astral.sh/uv/install.ps1)
    Sync-Path
}

# -- 3. dots CLI ------------------------------------------------------------
Step 'Installing dots CLI (editable install from repo)'
Push-Location $RepoDir
try {
    uv tool install --editable .
    if ($LASTEXITCODE -ne 0) {
        throw "uv tool install failed (exit $LASTEXITCODE)"
    }
} finally {
    Pop-Location
}
Sync-Path

Write-Host ''
Write-Host 'Bootstrap complete.' -ForegroundColor Green
Write-Host ''
Write-Host '  Next steps:'
Write-Host '    dots bootstrap --profile windows   # WSL2 feature/kernel, Scoop sanity check'
Write-Host '    dots install packages --profile windows'
Write-Host '    dots diff --profile windows'
