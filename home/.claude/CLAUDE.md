# Global Claude Code Instructions

## Environment
- OS: Arch Linux on WSL2 (Manjaro)
- Shell: zsh with starship prompt, atuin history, zoxide navigation
- Preferred editor: Neovim / Emacs
- Python: managed via `uv` (never system pip for tooling)
- Node: npm globals; bun available

## Communication Style
- Terse, direct responses, no trailing summaries of what was just done
- No emojis unless explicitly requested
- Reference code as `file_path:line_number` format
- One-sentence updates while working; brief is good, silent is not

## Writing Style (all written artifacts)
Applies everywhere text gets written, not just chat replies: code comments,
commit messages, PR/issue bodies, RFCs, review replies, docs.
- Say the thing directly. No throat-clearing openers ("I'll go ahead and...",
  "Let's dive into...", "Great question!", "Sure, here's...").
- Cut hedge padding: "it's worth noting that", "in order to", "essentially",
  "basically", "simply". Delete it or say the thing plainly instead. "just"
  meaning "only" is fine; cut it when it's filler.
- No inflated adjectives: robust, seamless, powerful, comprehensive, elegant,
  cutting-edge, significant. Use a concrete description or drop the word.
- No corporate transitions ("Furthermore", "Moreover", "Additionally"). Start
  the next sentence directly.
- Don't restate the request or narrate what you're about to do before doing
  it ("Now let's...", "Next, I will..."). Do it.
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

## Voice
Derived from my 2016-2024 GitHub history (pre-AI baseline). Full evidence:
`~/Documents/style-analysis/personal-style-profile.md`.
- Open with the current state or problem in one or two sentences, then say what
  the change does ("This adds...", "This updates..."). Applies to commit bodies,
  PR descriptions, issues, and explanations.
- Give the why as a mechanism or consequence ("because X, Y will break"), not as
  taste or preference.
- State verified facts flatly. Hedge once, only on judgment calls, and name the
  uncertainty. "should" is a directive, not a hedge.
- "we" for team plans and decisions; "I" for what I did or recommend ("I propose
  that we...").
- No greetings or sign-offs. Address someone with a leading @handle.
- Emphasis with _underscores_. No bold for emphasis, no bold field labels
  (`**Impact**:`), no ✅/⚠️ markers.
- Asides in parentheses, examples introduced with "e.g.". Tradeoffs in prose
  ("X vs. Y" with the cost of each), not tables, unless the data is tabular.
- Sentence-case titles for issues, PRs, and docs, not Title Case.
- No `## Summary` / `## Problem` / `## Root Cause` / `## Key Changes` scaffolding
  unless a repo template requires it.

## Communication Style: PRs, Issues, RFCs, Reviews

Write like the engineer who did the work, explaining it to a teammate, not
like a report generated about the work.

- **Read the room first.** Skim a few recent PRs/issues/RFCs in this repo
  before writing. Match the team's actual register (how formal, how terse,
  whether headers get used at all) instead of applying a fixed template
  everywhere.
- **Skip the AI tells.** No throat-clearing ("I'll go ahead and...", "Let's
  take a look at..."), no corporate transitions ("Furthermore,"
  "Additionally,"), no inflated adjectives (robust, seamless, powerful,
  comprehensive, significant), no em dashes, no closing summary that restates
  what the body just said.
- **Specifics beat enthusiasm.** "Fixes the race between the retry loop and
  the cancel handler" reads as human. "This is a great improvement to
  reliability!" reads as generated. State what changed and why; skip the
  praise.
- **Don't hedge what you verified, and don't assert what you didn't.** A flat,
  confident sentence about something you checked reads as human. The same
  sentence about something you didn't check reads as a claim waiting to be
  caught, so say what's unverified instead of padding it in "should" and
  "likely."
- **Say it once.** A PR description that restates the title, or a testing
  section that re-narrates the description, reads as filler even when each
  sentence is individually fine.
- **Structure serves the reader, not a checklist.** Bullets for a list of
  changes; a couple of plain sentences for a couple of plain facts. Don't
  stretch two sentences into five bullet points to look thorough (that's the
  "robotic" tell people are reacting to).
- **Brevity is not a shortcut being skipped.** A two-sentence PR body that
  fully orients the reviewer is complete. Don't pad it to look more finished.
  Description length doesn't need to grow with the size of the diff.
- **No performative caveats.** Skip disclaimers nobody asked for ("as an
  AI...", "please double-check this before merging"). If something is
  genuinely uncertain, say what's uncertain and why, once, and move on.
- **Write for someone who has to act on it.** A reviewer needs to know what to
  check; a teammate triaging an issue needs to know what's actually broken.
  Optimize for that, not for looking exhaustive.
- **Testing notes are operational.** Say how the change was or will be
  observed (applied in QA, `pulumi preview` output, a local run), and say so
  plainly when it can only be validated after merge.

Where this conflicts with an established house style in a given repo, the
house style wins.

## Code Preferences
- Python: use `uv` for environments, `cyclopts` for CLIs, `ruff` for linting
- Reuse the existing component or helper. Extract on the second use, not the
  first; don't copy-paste a block a second time.
- No defensive error handling for internal invariants
- Fail loudly on required values: `mapping["key"]`, `os.environ["X"]`,
  `config.require()`. No `.get()` that silently yields `None`. Never bare `except:`.
- Never hardcode environment-specific values, versions, ARNs, or account IDs.
  Layer defaults, then environment config, then secrets (SOPS/Vault/`secure:`).
- Generate structured config from data (`json.dumps`, pydantic, `|tojson`);
  don't template JSON/YAML as text.
- Python style: pydantic models for config with validators raising
  `ValueError`; `(str, Enum)` or `Literal` for choice sets; Sphinx docstrings
  (`:param:`/`:returns:`/`:rtype:`); f-strings, but lazy `%s` args in log calls;
  `pathlib`; named constants that carry their unit (`ONE_MONTH_SECONDS`).
- Keep shell scripts short and flat; anything with branching or parsing
  becomes Python.
- Default to no comments; add only when the WHY is non-obvious
- No backwards-compatibility shims when you can change the code directly

## Shell & Tools
- `bat` for viewing files when suggesting terminal commands
- `eza` instead of `ls`, `fd` instead of `find` where appropriate
- `delta` for git diffs

## Repo Context Files
- Always check the repo root for an `AGENTS.md` before relying on these global
  defaults. It takes precedence for anything it covers.
- When a repo has no context file yet and one is needed, create `AGENTS.md`, not
  `CLAUDE.md`. Exception: this file itself (`~/.claude/CLAUDE.md`, synced from
  `home/.claude/CLAUDE.md` in the dotfiles repo) stays `CLAUDE.md`. Claude Code
  loads that specific name for global config, so it isn't a per-repo context file.

## Exploring Unfamiliar Repos
- Check first whether the repo is already checked out locally (e.g. under
  `~/code/`). A local checkout beats both cloning and fetching.
- Otherwise, for read-only exploration of a repo's file contents (not
  GitHub/GitLab-side state like PRs, reviews, checks, or issues; use the API
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
- Match the commit convention in the repo's recent `git log` (scopes, casing,
  type names). Repo conventions change, so check rather than assume. Where
  there's no settled convention (new or personal repos), default to
  `type: Capitalized imperative subject`, no scope, no trailing period, about
  50 chars (max 72), with types `fix`, `config`, `version`, `feat`, `refactor`,
  `chore`, `defaults`, `hack`, `style`, `doc`, combined with a comma, config
  first: `config,fix:`.
- The subject says what changed; the body says why. Skip the body for trivial
  changes. Otherwise write short prose, problem first, links as full URLs.
- One logical change per commit.
- Prefer creating new commits over amending published ones; fix a mistake from
  an earlier commit with a new small `fix:` commit.
- GPG signing is enabled; never skip hooks with `--no-verify`
- Worktree workflow supported (`git wt`, `pi-worktrees`)
- Before making code changes in any git repo, call `EnterWorktree` first so concurrent
  work doesn't stomp on the same files. Skip only when the user explicitly says to edit
  in place. Prefer basing the worktree off of the latest commit of the default branch.
- If `EnterWorktree` isn't available, check manually before the first edit: compare
  `git rev-parse --git-dir` to `git rev-parse --git-common-dir`. If they match, you're
  in the shared checkout, not a worktree (in a worktree the git dir is
  `<common>/worktrees/<name>`). Don't compare the repo root against the common dir
  stripped of `/.git`: that reads a submodule, a `--separate-git-dir` checkout, or a
  symlinked `.git` as a worktree. Other concurrent sessions can switch branches or
  commit there while you're mid-task, so create one
  (`git worktree add ../wt-<slug> -b <branch>`) before touching files. The
  `shared-checkout-warning.sh` SessionStart hook flags this automatically when a
  session starts in a shared checkout.

## Claims Must Be Evidence-Backed
- Before stating a factual claim in a PR body, code comment, commit message, or
  review reply ("prod never showed this", "the library defaults to X", "this
  fixed the leak"), verify it against the live source: a Prometheus/Grafana query,
  the actual library source, or the running infra definition
  (Pulumi/Terraform/K8s manifest as deployed, not as written).
  Don't assert framework or library default behavior from memory.
- Size the query window to the claim. A claim about a period ("prod never
  showed this", "broken since the July deploy") is only supported by a window
  covering that whole period; if retention won't reach back that far, narrow
  the claim to what was actually queried ("no occurrences in the last 30
  days") instead of asserting it whole. At least 7 days is the floor for
  *trend* claims, so a short blip doesn't read as a trend: a minimum, never
  sufficient on its own for an absence claim.
- A config/manifest change is not verified by merging. Confirm the rollout
  actually happened (pods restarted, the new value is live in the running
  process) before claiming the change took effect, and confirm it's scoped to
  the intended environment only.
- If a claim can't be verified before shipping, flag it as unverified or drop
  it rather than stating it with unwarranted confidence.
