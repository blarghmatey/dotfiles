---
description: Summarise recent git activity as a standup update
allowed-tools:
  - Bash
---

Generate a brief standup update based on recent git activity in this repo.

Steps:
1. Run `git log --oneline --since="2 days ago" --author="$(git config user.name)"` to get recent commits
2. Run `git status` to see in-progress work
3. Check open PRs with `gh pr list --author @me --state open` if gh is available

Format (keep it short — this is a standup, not a report):
- **Done**: bullet per meaningful commit or PR merged (group closely related commits)
- **Today**: what you're working on now (from git status + any in-progress branches)
- **Blockers**: omit the section entirely if there are none

No padding, no meta-commentary. Write it like you'd actually say it in a meeting.
