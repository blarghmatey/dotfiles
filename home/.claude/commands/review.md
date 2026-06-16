---
description: Quick review of staged or recent changes for bugs and issues
allowed-tools:
  - Bash
  - Read
---

Review the current changes for correctness issues only — not style, not refactoring opportunities.

Steps:
1. Run `git diff --staged` (fall back to `git diff HEAD~1` if nothing staged)
2. Read any relevant files that provide context for the changed lines
3. Report only real bugs, logic errors, or security issues — not suggestions

Format:
- One finding per bullet, with file:line reference
- If nothing is wrong, say so in one sentence

Do not suggest refactors, renames, or improvements unless they directly relate to a correctness bug.
