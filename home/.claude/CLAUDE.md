# Global Claude Code Instructions

## Environment
- OS: Arch Linux on WSL2 (Manjaro)
- Shell: zsh with starship prompt, atuin history, zoxide navigation
- Preferred editor: Neovim / Emacs
- Python: managed via `uv` (never system pip for tooling)
- Node: npm globals; bun available

## Communication Style
- Terse, direct responses — no trailing summaries of what was just done
- No emojis unless explicitly requested
- Reference code as `file_path:line_number` format
- One-sentence updates while working; brief is good, silent is not

## Writing Style (all written artifacts)
Applies everywhere text gets written, not just chat replies: code comments,
commit messages, PR/issue bodies, RFCs, review replies, docs.
- Say the thing directly. No throat-clearing openers ("I'll go ahead and...",
  "Let's dive into...", "Great question!", "Sure, here's...").
- Cut hedge padding: "it's worth noting that", "in order to", "essentially",
  "basically", "simply", "just". Delete it or say the thing plainly instead.
- No inflated adjectives: robust, seamless, powerful, comprehensive, elegant,
  cutting-edge, significant. Use a concrete description or drop the word.
- No corporate transitions ("Furthermore", "Moreover", "Additionally"). Start
  the next sentence directly.
- Don't restate the request or narrate what you're about to do before doing
  it ("Now let's...", "Next, I will..."). Just do it.
- Don't explain what code obviously does. Same rule as inline comments,
  applied to PR/commit prose: state the non-obvious why, skip the what.
- Short declarative sentences over hedged, qualifier-stacked ones.
- Don't inflate importance ("this is a critical fix", "this dramatically
  improves..."). See Claims Must Be Evidence-Backed below; an unverified
  claim of impact is still unverified.
- No em dashes. Use a period, comma, or parentheses instead.
- Don't overuse "load-bearing." It's a specific, useful metaphor (a piece
  nothing else can safely be removed out from under). Reach for it only
  when that's actually the claim, not as generic emphasis.

## Code Preferences
- Python: use `uv` for environments, `cyclopts` for CLIs, `ruff` for linting
- Avoid premature abstractions — solve exactly what's needed, no more
- No defensive error handling for internal invariants
- Default to no comments; add only when the WHY is non-obvious
- No backwards-compatibility shims when you can just change the code

## Shell & Tools
- Prefer `rg` (ripgrep) over `grep` for code search
- `bat` for viewing files when suggesting terminal commands
- `eza` instead of `ls`, `fd` instead of `find` where appropriate
- `delta` for git diffs
- `just` for project task runners (prefer over Makefile)

## Repo Context Files
- Always check the repo root for an `AGENTS.md` before relying on these global
  defaults — it takes precedence for anything it covers.
- When a repo has no context file yet and one is needed, create `AGENTS.md`, not
  `CLAUDE.md`. Exception: this file itself (`~/.claude/CLAUDE.md`, synced from
  `home/.claude/CLAUDE.md` in the dotfiles repo) stays `CLAUDE.md` — Claude Code
  loads that specific name for global config, so it isn't a per-repo context file.

## Exploring Unfamiliar Repos
- Check first whether the repo is already checked out locally (e.g. under
  `~/code/`) — a local checkout beats both cloning and fetching.
- Otherwise, for read-only exploration of a repo's file contents (not
  GitHub/GitLab-side state like PRs, reviews, checks, or issues — use the API
  for those), clone it rather than fetching files one at a time via `gh api`,
  raw.githubusercontent.com, WebFetch, or MCP file-getters. A sequence of
  per-file fetches is slow, burns rate limit, and can't grep across the tree.
- Shallow-clone into `/tmp` or the session scratchpad, never into a code
  directory where it would look like a real checkout: `git clone --depth 1`,
  adding `--filter=blob:none` for a large repo.
- A depth-1 clone only has the default branch. If the question is about a
  PR's contents, fetch that ref explicitly, e.g.
  `git fetch origin pull/<n>/head:<local-branch>` (or the branch name) after
  the initial clone.
- Delete the clone when done exploring; don't leave it behind in /tmp.

## Git
- Commits focus on "why" not "what"
- Prefer creating new commits over amending published ones
- GPG signing is enabled; never skip hooks with `--no-verify`
- Worktree workflow supported (`git wt`, `pi-worktrees`)
- Before making code changes in any git repo, call `EnterWorktree` first so concurrent
  work doesn't stomp on the same files. Skip only when the user explicitly says to edit
  in place. Prefer basing the worktree off of the latest commit of the default branch.
- If `EnterWorktree` isn't available, check manually before the first edit: compare
  `git rev-parse --show-toplevel` to `git rev-parse --git-common-dir` (stripped of
  `/.git`) — if they match, you're in the shared checkout, not a worktree. Other
  concurrent sessions can switch branches or commit there while you're mid-task, so
  create one (`git worktree add ../wt-<slug> -b <branch>`) before touching files.

## Claims Must Be Evidence-Backed
- Before stating a factual claim in a PR body, code comment, commit message, or
  review reply — "prod never showed this", "the library defaults to X", "this
  fixed the leak" — verify it against the live source: a Prometheus/Grafana query,
  the actual library source, or the running infra definition
  (Pulumi/Terraform/K8s manifest as deployed, not as written).
  Don't assert framework or library default behavior from memory.
- Size the query window to the claim. A claim about a period ("prod never
  showed this", "broken since the July deploy") is only supported by a window
  covering that whole period; if retention won't reach back that far, narrow
  the claim to what was actually queried ("no occurrences in the last 30
  days") instead of asserting it whole. At least 7 days is the floor for
  *trend* claims, so a short blip doesn't read as a trend — a minimum, never
  sufficient on its own for an absence claim.
- A config/manifest change is not verified by merging. Confirm the rollout
  actually happened — pods restarted, the new value is live in the running
  process — before claiming the change took effect, and confirm it's scoped to
  the intended environment only.
- If a claim can't be verified before shipping, flag it as unverified or drop
  it rather than stating it with unwarranted confidence.
