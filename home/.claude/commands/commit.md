---
description: Compose a commit message for staged changes
allowed-tools:
  - Bash
---

Look at the staged changes and write a commit message for them.

Run `git diff --staged` and `git log --oneline -10` to understand the context and style.

Rules:
- Subject line: ≤72 chars, imperative mood, no period
- Focus on WHY the change was made, not WHAT changed (the diff shows what)
- If conventional commits are used in this repo, match the prefix style (feat/fix/refactor/etc.)
- Body only if the motivation needs more than one sentence
- No co-author or generated-by footers unless asked

Output ONLY the commit message text, nothing else.
