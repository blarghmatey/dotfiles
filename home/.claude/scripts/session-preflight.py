#!/usr/bin/env python3
"""SessionStart hook: report credentials that will expire mid-session and a stale checkout.

Long autonomous sessions kept failing on a credential that had expired
(Vault-backed kubectl most often, also gcloud and fly), on a witan outage, or
on a worktree based on an old commit. A validity check at startup misses a
credential with an hour left, so this reports time remaining instead, and
tells the agent to ask for a re-auth before starting rather than work around
it later.

SessionStart stdout is added to the session context and exit code 2 is not
supported for this event, so this only ever informs. Every check is
read-only: no token renewal, no `git fetch`, no kubeconfig writes.
"""

import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import urllib.error
import urllib.request
from collections.abc import Callable
from concurrent.futures import ThreadPoolExecutor, wait
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from pathlib import Path

ONE_HOUR_SECONDS = 60 * 60
# A credential with less than this left will likely die before a long run ends.
CREDENTIAL_WARN_SECONDS = 2 * ONE_HOUR_SECONDS
NETWORK_TIMEOUT_SECONDS = 6
# Under the 20s hook timeout, so a hung DNS lookup costs one check's result
# instead of everything the other checks found.
DEADLINE_SECONDS = 15
HTTP_FORBIDDEN = 403
NO_EXPIRY = datetime.max.replace(tzinfo=UTC)

MIT_CODE_ROOT = Path.home() / "code" / "mit"
EKS_CACHE_DIR = Path.home() / ".cache" / "ol-infrastructure" / "eks"
# Admin contexts authenticate with ambient AWS credentials, not the Vault cache.
EKS_VAULT_MODES = ("readonly", "developer")
# The targets sessions actually use, by count of `fly -t` in transcripts.
FLY_TARGETS = ("odl-inf", "odl-qa", "odl-prod")
RUFF_PRECOMMIT_REV = re.compile(
    r"astral-sh/ruff-pre-commit['\"]?\s*\n(?:\s*#.*\n)*\s*rev:\s*['\"]?([^\s'\"]+)"
)
VAULT_ADDRESS = re.compile(r'^PRODUCTION_VAULT_ADDRESS = "([^"]+)"', re.MULTILINE)


@dataclass
class Finding:
    text: str
    ok: bool = False


def run(cmd: list[str], cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    # No check here may stop to ask for input: an HTTPS git remote would
    # otherwise prompt for credentials and hold the session start for the
    # whole timeout.
    return subprocess.run(
        cmd,
        cwd=cwd,
        env={**os.environ, "GIT_TERMINAL_PROMPT": "0"},
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=NETWORK_TIMEOUT_SECONDS,
        check=False,
    )


def local_time(moment: datetime) -> str:
    return moment.astimezone().strftime("%H:%M")


def remaining(seconds: float) -> str:
    hours, rem = divmod(int(seconds), ONE_HOUR_SECONDS)
    return f"{hours}h{rem // 60:02d}m"


def kubeconfig_eks_modes() -> tuple[Path, set[str]] | None:
    """Locate eks.py and the Vault-backed modes the current kubeconfig uses."""
    result = run(["kubectl", "config", "view", "-o", "json"])
    if result.returncode != 0:
        return None
    # A missing kubeconfig still exits 0, with "users": null.
    users = json.loads(result.stdout)["users"] or []
    eks_py = None
    modes = set()
    for user in users:
        args = (user["user"].get("exec") or {}).get("args") or []
        script = next((arg for arg in args if arg.endswith("eks.py")), None)
        if script is None:
            continue
        eks_py = Path(script)
        modes.add(args[args.index("--mode") + 1])
    if eks_py is None:
        return None
    return eks_py, modes & set(EKS_VAULT_MODES)


def eks_reauth(eks_py: Path, mode: str) -> str:
    """Build a command that forces a fresh Vault OIDC login for one mode.

    eks.py reuses any cached AWS credentials with more than five minutes left
    and any Vault token that still authenticates, so `setup` run early would
    rewrite the kubeconfig and leave the expiry where it was. Dropping the two
    cache files and calling the loader eks.py's exec plugin uses forces the
    login without touching the kubeconfig.
    """
    caches = " ".join(
        shlex.quote(str(EKS_CACHE_DIR / f"{kind}-{mode}.json"))
        for kind in ("vault-token", "aws-creds")
    )
    login = (
        'import sys; sys.path.insert(0, "scripts/eks"); import eks; '
        f'eks.load_valid_aws_credentials(eks.AccessMode("{mode}"))'
    )
    return (
        f"cd {shlex.quote(str(eks_py.parents[2]))} && rm -f {caches} && "
        f"uv run python -c {shlex.quote(login)}"
    )


def https_get(url: str, headers: dict[str, str] | None = None) -> bytes:
    if not url.startswith("https://"):
        msg = f"refusing non-https URL {url!r}"
        raise ValueError(msg)
    request = urllib.request.Request(url, headers=headers or {})  # noqa: S310
    with urllib.request.urlopen(request, timeout=NETWORK_TIMEOUT_SECONDS) as response:  # noqa: S310
        return response.read()


def vault_token_expiry(vault_address: str, token: str, now: datetime) -> datetime | None:
    """Return when a Vault token expires, or None when Vault rejects it."""
    try:
        body = https_get(f"{vault_address}/v1/auth/token/lookup-self", {"X-Vault-Token": token})
    except urllib.error.HTTPError as exc:
        if exc.code == HTTP_FORBIDDEN:
            return None
        raise
    ttl = json.loads(body)["data"]["ttl"]
    # A ttl of 0 is a token with no expiry.
    return NO_EXPIRY if ttl == 0 else now + timedelta(seconds=ttl)


def check_kubectl(now: datetime) -> list[Finding]:
    """Report how long kubectl stays usable for each Vault-backed mode the kubeconfig uses.

    eks.py renews the cached AWS credentials from the cached Vault token, and
    the STS credentials outlive the token, so kubectl works until the later of
    the two. After that it needs a browser OIDC login, which a non-interactive
    kubectl call refuses to start.
    """
    located = kubeconfig_eks_modes()
    if located is None:
        return []
    eks_py, modes = located
    match = VAULT_ADDRESS.search(eks_py.read_text())
    if match is None:
        return [
            Finding(
                f"kubectl: could not read PRODUCTION_VAULT_ADDRESS from {eks_py}; update session-preflight.py."
            )
        ]
    vault_address = match[1]

    findings = []
    for mode in sorted(modes):
        label = f"kubectl {mode} contexts (Vault-backed)"
        reauth = eks_reauth(eks_py, mode)
        aws_cache = EKS_CACHE_DIR / f"aws-creds-{mode}.json"
        token_cache = EKS_CACHE_DIR / f"vault-token-{mode}.json"
        if not aws_cache.exists():
            findings.append(Finding(f"{label}: no cached login. Re-auth: `! {reauth}`"))
            continue
        aws_expiry = datetime.fromisoformat(json.loads(aws_cache.read_text())["expires_at"])
        vault_expiry = None
        if token_cache.exists():
            token = json.loads(token_cache.read_text())["token"]
            vault_expiry = vault_token_expiry(vault_address, token, now)
        usable_until = max(aws_expiry, vault_expiry or aws_expiry)
        left = (usable_until - now).total_seconds()
        if usable_until == NO_EXPIRY:
            findings.append(Finding(f"kubectl {mode} (no expiry)", ok=True))
        elif left <= 0:
            findings.append(Finding(f"{label}: expired. Re-auth: `! {reauth}`"))
        elif left < CREDENTIAL_WARN_SECONDS:
            findings.append(
                Finding(
                    f"{label}: expire at {local_time(usable_until)}, {remaining(left)} from now. "
                    f"Re-auth before long cluster work: `! {reauth}`",
                )
            )
        else:
            findings.append(Finding(f"kubectl {mode} until {local_time(usable_until)}", ok=True))
    return findings


def check_gcloud() -> list[Finding]:
    # print-access-token refreshes from the stored refresh token, so a failure
    # means a browser login is needed, not that the access token aged out.
    result = run(["gcloud", "auth", "print-access-token"])
    if result.returncode != 0:
        return [Finding("gcloud: token refresh failed. Re-auth: `! gcloud auth login`")]
    return [Finding("gcloud", ok=True)]


def check_fly() -> list[Finding]:
    with ThreadPoolExecutor(max_workers=len(FLY_TARGETS)) as pool:
        statuses = pool.map(lambda target: run(["fly", "-t", target, "status"]), FLY_TARGETS)
        logged_out = [
            target
            for target, status in zip(FLY_TARGETS, statuses, strict=True)
            if status.returncode != 0
        ]
    if logged_out:
        logins = " && ".join(f"fly -t {target} login" for target in logged_out)
        return [Finding(f"fly {', '.join(logged_out)}: logged out. Re-auth: `! {logins}`")]
    return [Finding(f"fly {', '.join(FLY_TARGETS)}", ok=True)]


def check_witan(cwd: Path) -> list[Finding]:
    """Probe the remote witan target this directory resolves to, if any.

    `witan whoami` reads local claims only and exits non-zero for a local-store
    target, so it is used to resolve the endpoint, and /health (unauthenticated,
    answers from process state) to see whether the service is up.
    """
    whoami = run(["witan", "whoami"], cwd=cwd)
    if whoami.returncode != 0:
        return []
    endpoint = next(
        line.split(maxsplit=1)[1]
        for line in whoami.stdout.splitlines()
        if line.startswith("Endpoint")
    )
    base = endpoint.removesuffix("/mcp")
    try:
        https_get(f"{base}/health")
    except (urllib.error.URLError, TimeoutError) as exc:
        return [
            Finding(
                f"witan {base}: /health failed ({exc}). Task claims, comments and closes will fail; "
                "tell the user before starting tracked work.",
            )
        ]
    return [Finding("witan reachable", ok=True)]


def git(cwd: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return run(["git", "-C", str(cwd), *args])


def check_checkout(cwd: Path) -> list[Finding]:
    """Compare HEAD against the remote default branch without fetching.

    `ls-remote` reads the remote's current tip, so a stale local origin/<default>
    can't hide how far behind the checkout is.
    """
    if git(cwd, "rev-parse", "--git-dir").returncode != 0:
        return []
    remote = git(cwd, "ls-remote", "--symref", "origin", "HEAD")
    if remote.returncode != 0:
        return []
    default_ref = re.search(r"^ref: refs/heads/(\S+)\s+HEAD", remote.stdout, re.MULTILINE)
    tip = re.search(r"^([0-9a-f]{40})\s+HEAD", remote.stdout, re.MULTILINE)
    if default_ref is None or tip is None:
        return []
    default, tip_sha = default_ref[1], tip[1]

    if git(cwd, "cat-file", "-e", f"{tip_sha}^{{commit}}").returncode != 0:
        return [
            Finding(
                f"checkout: origin/{default} has commits this clone hasn't fetched. "
                f"Run `git fetch origin` and check whether HEAD should be rebased before starting.",
            )
        ]
    findings = []
    behind = int(git(cwd, "rev-list", "--count", f"HEAD..{tip_sha}").stdout)
    if behind:
        findings.append(Finding(f"checkout: HEAD is {behind} commit(s) behind origin/{default}."))

    head_config = git(cwd, "show", "HEAD:.pre-commit-config.yaml")
    tip_config = git(cwd, "show", f"{tip_sha}:.pre-commit-config.yaml")
    if head_config.returncode == 0 and tip_config.returncode == 0:
        head_rev = RUFF_PRECOMMIT_REV.search(head_config.stdout)
        tip_rev = RUFF_PRECOMMIT_REV.search(tip_config.stdout)
        if head_rev and tip_rev and head_rev[1] != tip_rev[1]:
            findings.append(
                Finding(
                    f"ruff: this branch pins ruff-pre-commit {head_rev[1]}, origin/{default} pins {tip_rev[1]}. "
                    "CI lints the merge result with the newer pin, so lint with that before calling a failure flaky.",
                )
            )
    return findings


def main() -> None:
    hook_input = json.load(sys.stdin)
    cwd = Path(hook_input["cwd"])
    now = datetime.now(UTC)

    # Each check runs only when its tool is installed: a missing binary is not
    # an expired credential and shouldn't tell the agent to stop.
    checks: dict[str, tuple[str, Callable[[], list[Finding]]]] = {
        "checkout": ("git", lambda: check_checkout(cwd)),
    }
    if cwd.is_relative_to(MIT_CODE_ROOT):
        checks |= {
            "kubectl": ("kubectl", lambda: check_kubectl(now)),
            "gcloud": ("gcloud", check_gcloud),
            "witan": ("witan", lambda: check_witan(cwd)),
            "fly": ("fly", check_fly),
        }
    checks = {
        name: (binary, check) for name, (binary, check) in checks.items() if shutil.which(binary)
    }
    if not checks:
        return

    pool = ThreadPoolExecutor(max_workers=len(checks))
    futures = {name: pool.submit(check) for name, (_, check) in checks.items()}
    wait(futures.values(), timeout=DEADLINE_SECONDS)

    findings: list[Finding] = []
    for name, future in futures.items():
        if not future.done():
            findings.append(
                Finding(f"{name}: no answer within {DEADLINE_SECONDS}s, status unknown.")
            )
            continue
        # One failing check must not hide what the others found.
        try:
            findings += future.result()
        except Exception as exc:  # noqa: BLE001
            findings.append(Finding(f"{name} check error: {type(exc).__name__}: {exc}"))

    problems = [f.text for f in findings if not f.ok]
    passing = [f.text for f in findings if f.ok]
    if problems or passing:
        print(f"Session preflight at {local_time(now)}:")
        for problem in problems:
            print(f"- {problem}")
        if passing:
            print(f"OK: {'; '.join(passing)}.")
        if problems:
            print(
                "If this session's work depends on a failing item, stop and ask the user to fix it "
                "before starting. Don't work around an expired credential."
            )
    sys.stdout.flush()
    # Skip the executor's exit-time join, which would wait on a check still
    # stuck in DNS and push the hook past its timeout.
    os._exit(0)


if __name__ == "__main__":
    main()
