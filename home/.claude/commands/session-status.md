---
description: Close out the session with DONE / BLOCKED / DEFERRED
allowed-tools:
  - Bash
  - Read
---

Read `~/.claude/session-summary-template.md` and follow it. That file is the
wording the Stop hook uses too, so the two stay in step; if it is missing, close
out with DONE (each line naming its evidence), BLOCKED (the external blocker and
who clears it), and DEFERRED (with a filed witan task id), leaving out any empty
bucket.

Work out what happened from the session itself, not from memory of what was
intended:

- `git log --oneline` on the branches touched, and `git status --short`
- `gh pr list --author @me --state all --limit 10` for anything opened or merged
- `witan tasks` for tasks claimed in this session

File a witan task for each DEFERRED item now (`task_create`) and give the id. If
an existing task already covers it, cite that id instead of filing a duplicate.
