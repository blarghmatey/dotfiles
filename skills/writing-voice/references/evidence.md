# Evidence

Measured from GitHub activity dated 2016-2024 (the pre-AI baseline), with 2025
used only for comparison. Quotes here come from public repositories. The full
analysis, including private-repo evidence, is local to the author and not
published.

## Numbers behind the rules

| Rule | Measurement |
|---|---|
| No em/en dashes | 0 authored uses in 2012-2024 comments, issues, discussions and commits. Every raw hit was a GitHub email-reply footer or quoted text |
| Context first, then "This ..." | "`. This <verb>`" after a context sentence in 32-35% of non-empty PR bodies in 2020-2024; causal connectives in 21-24% in every era |
| Body length flat vs diff size | 2023-24 own-words median 296 chars for PRs of 20 lines or fewer, 330-358 for everything larger |
| Hedging | 9.4 hedges per 1k words (2016-19) falling to 4.0 (2023-24); review comments hedge design opinions, never factual corrections |
| No greetings or sign-offs | Greetings open 7 of 3,308 entries (0.2%); 0 sign-offs outside mail-client signatures |
| Emphasis | Bold in 0-1% of comments; underscores used for emphasis instead |
| Tables | 0-1% of issues and discussions |
| Review comment shape | Median 12-17 words; `suggestion` blocks in 16-18% of comments since mid-2022; "nit" in 13 of 763; "LGTM" 0 |
| Approvals | 83% of 2023-24 reviews carry no text |
| AI-era markers | 13 of 146 PRs from 2025 (all dated 2025-06-30 or later) combine `## Summary`/`## Root Cause` headers, bold-label bullets, ✅ or "comprehensive/critical". Before 2025, bold-label bullets appear in 1 of 911 PRs and self-chosen report headers in none |
| Title Case issue titles | 4-10% in 2016-2024, 38% in 2025 |

## Public examples

- Context, then change: https://github.com/mitodl/ol-infrastructure/pull/2461
  ("Files uploaded by instructors in edX do not get explicitly expired from the
  cache when they are updated. This explicitly limits the TTL for those files to
  30 seconds...")
- Short PR body with operational testing note:
  https://github.com/mitodl/ol-data-platform/pull/1244
- Testing note that says what to look for:
  https://github.com/mitodl/ol-infrastructure/pull/2502
- Honest uncertainty, stated once: https://github.com/mitodl/ol-infrastructure/pull/7
- Review approval with a caveat ("Minor typo, otherwise :+1:"):
  https://github.com/mitodl/ol-infrastructure/pull/985
- Inviting pushback on a design call:
  https://github.com/mitodl/ol-infrastructure/pull/666
- Flat factual correction with mechanism: https://github.com/mitodl/salt-ops/pull/843
- Commit body, problem first: mitodl/ol-infrastructure commits `7902a00b`
  (2023-03-14) and `77bf9a48` (2022-06-16)
- Upstream PR in the same register: https://github.com/airbytehq/airbyte/pull/36320
