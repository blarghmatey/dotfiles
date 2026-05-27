---
name: planning-mode
description: >
  Collaborative planning workflow: Explore → Frame → Diverge → Converge → Artifact.
  Use when the user wants to plan before coding, says "planning mode", "plan this out",
  "let's think through", "explore options", or asks to produce an RFC, implementation
  guide, or GitHub issues. Also activate via /plan command or Ctrl+Alt+P. The skill
  defines the workflow; the companion extension manages mode state, UI status, and
  artifact export.
license: BSD-3-Clause
metadata:
  category: workflow
---

# Planning Mode

A structured workflow for collaborative exploration and iteration on a plan — use it
before any significant implementation to arrive at a well-reasoned artifact.

## Activating

| Method | Effect |
|--------|--------|
| `/plan` | Toggle planning mode on/off |
| `Ctrl+Alt+P` | Keyboard shortcut toggle |
| `pi --plan` | Start a session already in planning mode |
| `/plan readonly` | Enter planning mode + block file writes |
| `/plan export` | Prompt for artifact format at any time |

## The workflow

```
Explore → Frame → Diverge → Converge → Artifact
```

### 1 · Explore

Before proposing anything, read relevant code, docs, and existing plans.
Surface **facts, not assumptions**. Ask for any context you're missing.

### 2 · Frame

Agree on the problem statement and success criteria **before** generating options.
Ask **one focused question at a time**. Do not propose solutions until framing is
complete.

### 3 · Diverge

Generate 2–4 distinct approaches with honest tradeoffs — cost, risk, complexity,
reversibility. The user can push back, adjust scope, or pick a direction.

### 4 · Converge

Narrow to a concrete plan with ordered, actionable steps.
Flag open questions with ❓ — they stay visible until resolved.

When the plan is solid, close with:

```
## Plan
1. First step
2. Second step
…

PLAN READY
```

### 5 · Artifact

The extension prompts for output format on detecting `PLAN READY`. You can also
trigger this at any time with `/plan export`.

| Format | Purpose |
|--------|---------|
| **Implementation guide** | Ordered tasks, each sized for one coding session (≤ ~2 h) |
| **RFC** | Problem / options considered / decision / consequences |
| **GitHub issues** | One issue per deliverable with acceptance criteria |

## Ground rules

- **One question per response** — never a barrage
- **Flag assumptions explicitly**: *"I'm assuming X — is that right?"*
- **Keep ❓ open questions visible** until explicitly resolved
- User can say **"too deep"** (zoom out) or **"go deeper"** (add detail) at any point
- In planning mode: *describe* what would be done, don't do it
