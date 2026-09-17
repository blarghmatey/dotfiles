---
description: Compose a commit message for staged changes
allowed-tools:
  - Bash
---

Look at the staged changes and write a commit message for them.

Run `git diff --staged` and `git log --no-merges --format=%s -30` to see the change
and the repo's current subject convention.

Subject:
- Follow the convention in that log: whether subjects are typed, whether types
  carry scopes, and whether the description after the type is capitalized.
- If the repo has no settled convention, use `type: Capitalized imperative
  subject` with no scope. Types: `fix` (broken behavior), `config` (settings,
  stack values, secret references, sizing, CI/lint config), `version` (version
  bumps and releases), `feat` (new capability), `refactor` (restructuring or
  removing dead code), `chore` (cleanup), `defaults` (changing a default),
  `hack` (temporary workaround), `style` (formatter output), `doc` (docs).
  Combine with a comma, config first: `config,fix:`.
- State what changed, imperative mood, about 50 chars, max 72, no period.

Body:
- None for a small, self-explanatory change.
- Otherwise one to three short prose paragraphs: the situation or problem first
  (what was broken, what constraint exists, who needed it), then what the change
  does, often starting "This adds/updates/removes...". Use `-` bullets only when
  several distinct changes are bundled.
- Links as full URLs in the body, not `#N` in the subject.
- No em dashes, no bold, no headers, no emoji.
- No co-author or generated-by footers unless asked.

If the staged changes contain more than one logical change, say so and suggest
how to split them instead of writing one message.

Output ONLY the commit message text, nothing else.
