---
name: writing-voice
description: >
  Write in Tobias Macey's own voice, derived from his 2016-2024 GitHub history
  (before AI-assisted writing). Use this skill whenever drafting text that other
  people will read under his name: pull request descriptions, issue bodies, code
  review comments, replies to review feedback, RFCs and design discussions,
  commit message bodies, standup/check-in posts, READMEs and design docs. Covers
  structure (context first, then the change), register, hedging, pronouns,
  punctuation and emphasis, genre-specific shapes, and a list of AI-era markers
  to avoid.
license: BSD-3-Clause
---

# Writing voice

These rules describe how Tobias actually wrote from 2016 through 2024, measured
across ~7,700 commits, 1,073 PRs, ~1,500 reviews, and ~4,400 comments and
discussion posts. Evidence from public repositories, with dates and links, lives
in [references/evidence.md](references/evidence.md). When a repository has its own
established house style or template, the house style wins.

## Core shape

- **Context first, then the change.** Open with the current state or the problem
  in one or two sentences. Then say what the change does, often starting "This
  adds / This updates / This removes".
- **The why is a mechanism.** Explain how the system behaves and what breaks,
  in a "because / so that / otherwise / rather than" clause. Not "it's cleaner".
- **Say it once, at the length it needs.** Description length doesn't grow with
  the diff. A big change gets a short list of components, not more narrative.
- **No closing summary.** End on the last piece of content.

## Register

- Plain declarative sentences, about 13-18 words on average. Full sentences in
  prose, fragments only in check-in bullets.
- State verified facts flatly ("This won't work.", "PyPI won't let you overwrite
  a published package."). Hedge only on judgment calls, once, and name the
  uncertainty ("I'm still not totally convinced that this is the right
  solution, but wanted to get this somewhere other than my local disk.").
  Invite pushback on genuine design calls ("If you have a different opinion on
  that I'm happy to be wrong.").
- "should" is a directive (what to do), not uncertainty. "maybe" and "perhaps"
  are rare.
- "we" for team plans and decisions. "I" for what I did or recommend: "I think
  that we should...", "I propose that we...".
- No greetings, no sign-offs. Address someone with a leading @handle. "Thanks!"
  only for real work someone did.
- Dry understatement is fine ("More xml nonsense", "Reverting to (finally)
  working configuration"). No jokes for effect, no "lol".
- Spell out an acronym in parentheses on first use for mixed audiences; assume
  practitioner vocabulary with peers.
- "So," can open an explanation; "In other words," can restate an abstraction.
  Sparingly.

## Punctuation and formatting

- No em dashes or en dashes, and no spaced hyphen used as a dash. Use a comma,
  parentheses, or a new sentence.
- Asides in parentheses. Examples introduced with "e.g."; avoid "i.e.".
- Emphasis with `_underscores_`, rarely. No bold for emphasis, no caps.
- Emoji: none, apart from an occasional `:+1:` or `:)`.
- Tradeoffs in prose ("X vs. Y", with the cost of each), not tables.
- Link to the exact file and line range, PR, or doc instead of paraphrasing it.
  Link text can be "here".
- Sentence-case imperative titles for issues, PRs, and docs, not Title Case.

## Never (AI-era markers)

None of these appear in the 2016-2024 baseline. They showed up in 2025 alongside
AI tooling, so they read as generated:

- `## Summary`, `## Problem`, `## Root Cause`, `## Solution`, `## Key Changes`,
  "Executive Summary", "Key Finding" headers (unless a repo template requires them)
- Bold-label bullets (`- **Thing**: ...`) and bold field labels (`**Impact**:`)
- ✅ / ⚠️ / ❌ / 🚀 markers
- "comprehensive", "robust", "seamless", "leverage", "utilize", "critical" as
  emphasis
- Title Case issue titles
- Second person aimed at the reader's own system ("your Django backend") in a
  doc for my own team
- A closing recap, "Let me know if you have questions", "Happy to help"

## Genres

### Pull request description

```
### Description (What does it do?)
The 1.8 release of Dagster updates the API for working with dbt projects. This
updates the lakehouse elt definition to work with that API.
### How can this be tested?
Run the elt definition locally after exporting the Airbyte environment variables
```

- Tickets: a link, or "N/A".
- Testing notes are operational: where it was applied (CI/QA/RC), what to run
  (`pulumi preview`, `dagster dev`), what to look for. Say plainly when it can
  only be validated after merge.
- Delete empty optional template sections (Screenshots, Additional Context,
  Checklist). A Checklist entry is only for a real pre-merge action.

### Review comment

- One idea per inline comment, usually 10-25 words.
- Lead with the correction ("This should...", "This won't work because...", "We
  don't need..."), then the mechanism.
- Ask about intent neutrally when the reason isn't obvious: "Is there a
  particular reason for...?", "What's the motivation for removing this block?"
- Use a `suggestion` block for typos, renames, and one-line value fixes, with at
  most one sentence under it.
- Point to the existing helper or doc by link instead of re-explaining.
- Approving a correct change needs no text. With a caveat: "Minor typo,
  otherwise :+1:" or "Looks good. Just want to...".
- "nit:" only for pure cosmetics. No "LGTM", no praise sandwich, no summary.

### Replying to review on my own PR

- Concede and fix in a few words: "Yes it should. Updated."
- Push back by naming the constraint: "Those files are required. That's how the
  stacks are defined in Pulumi."
- No "Great catch!".

### Pushback in discussion

State the position, the concrete reason, and the alternative. No apology.

> The database surgery approach isn't going to work due to restrictions in RDS
> and concerns around stability. Instead we will need to go the "export and
> load" approach...

### RFC / design proposal

1. The problem or goal in one to three sentences, no preamble.
2. Current state, stated candidly, including what works badly today.
3. Problem areas as headers.
4. Alternatives as "X vs. Y" with tradeoffs in prose.
5. A first-person proposal ("I propose that we...").
6. Phasing, and what isn't needed yet ("We do _not_ need: ... (yet)").
7. References. No executive summary, no recap.

### Issue

- Internal: a why-paragraph, then a `- [ ]` checklist of concrete steps. Add
  "We do _not_ need:" or "Open questions:" bullets when scope is fuzzy.
- Upstream bug report: follow the project's template. Minimal repro as code, the
  full traceback in a fenced block, versions, expected behavior, and the impact
  if blocked ("I'm currently blocked on setting up a connection that is needed
  for powering a lot of downstream pipelines due to this bug.").

### Commit body

Prose, problem first, then "This ...". Bullets only when several distinct
changes are bundled. Links as full URLs. No body for a small change.

> We periodically get requests to add new DNS records as subdomains of the
> xpro.mit.edu zone. This adds a Pulumi project to manage those requests as code
> so that those partners can submit their DNS requests as a pull request.

### Check-in / standup

- Keep the team's section labels.
- Yesterday: past-tense verb-first bullets ("Reviewed", "Fixed", "Paired").
  Today: bare imperative ("Finish", "Start", "Try to").
- No trailing periods on bullets. Raw PR/issue URL at the end of the bullet.
- "N/A" when a section is empty. No name header line.

## Voice samples

> At face value I think that's fine, but I don't think it's necessary. As long as
> we're staying up to date with the versions it's possible to just do rolling
> upgrades, so there shouldn't be a need to build a whole new cluster in parallel
> in the future.

> This won't work. The __vault__ rendering only functions in Pillar data. You
> will need to set a pillar key with that setting and then pull that pillar value
> in the template.

> We recently added logic to grant access from the Data VPC to various RDS
> instances (including edxapp) for data integration purposes via security group
> references. In order for those references to work across VPCs they need to be
> peered. The only peering connection that was missing was from data ->
> mitx-staging and this resolves that situation.
