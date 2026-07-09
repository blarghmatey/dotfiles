/**
 * Planning mode utilities: instructions injected into context, artifact export prompts,
 * and convergence detection.
 */

// ---------------------------------------------------------------------------
// Context injection
// ---------------------------------------------------------------------------

export const PLANNING_INSTRUCTIONS = `\
[PLANNING MODE ACTIVE]

You are in collaborative planning mode. Explore and iterate on a plan with the user
— do NOT implement anything yet.

Follow this workflow in order:

## 1 · Explore
Before proposing anything, read relevant code, docs, and existing plans.
Surface facts, not assumptions. Ask for clarification if you're missing context.

## 2 · Frame
Agree on the problem statement and success criteria before generating options.
Ask one focused question at a time. Do not propose solutions until framing is complete.

## 3 · Diverge
Generate 2–4 distinct approaches with honest tradeoffs (cost, risk, complexity,
reversibility). The user may push back, adjust scope, or pick a direction.

## 4 · Converge
Narrow to a concrete plan with ordered, actionable steps.
Flag open questions explicitly with ❓ — they stay visible until resolved.

When the plan is solid, close your response with a section headed exactly:

  ## Plan
  1. First step
  2. Second step
  …

Then include the marker PLAN READY on its own line.

## Ground rules
- One focused question per response — never a barrage.
- Flag assumptions: "I'm assuming X — is that right?"
- Keep ❓ open questions visible until explicitly resolved.
- If the user says "too deep", zoom out; "go deeper" means add detail.
- Do not start implementing — describe what would be done, not do it.
`;

export const PLANNING_INSTRUCTIONS_READONLY = `${PLANNING_INSTRUCTIONS}
Note: Read-only mode is active. Do not attempt to write or edit files.
`;

// ---------------------------------------------------------------------------
// Artifact export prompts
// ---------------------------------------------------------------------------

export const ARTIFACT_PROMPTS = {
  implementationGuide: `\
The planning phase is complete. Produce an **implementation guide** from the plan above.

Format:

# Implementation: <title>

## Context
One paragraph: what we're building and why.

## Prerequisites
Bullet list of anything that must be true before starting.

## Tasks
Ordered numbered list. Each task:
- Small enough to complete in one coding session (≤ ~2 h)
- Has a clear done condition
- Notes any dependencies on previous tasks

## Open questions
Any ❓ items that must be resolved before or during implementation.
`,

  rfc: `\
The planning phase is complete. Produce an **RFC document** from the plan above.

Format:

# RFC: <title>

## Status
Draft

## Problem
What problem are we solving and why does it matter now?

## Options considered
For each option explored during planning:

### Option N: <name>
**Tradeoffs:** pros / cons / risks

## Decision
Which option we're going with and why.

## Implementation plan
Ordered steps.

## Consequences
What changes downstream, what we give up, what we gain.

## Open questions
Any ❓ items not yet resolved.
`,

  githubIssues: `\
The planning phase is complete. Produce **GitHub issues** from the plan above.

For each deliverable (aim for 3–8 issues, right-sized for 1–3 days each):

---

**Issue N: <title>**
Labels: (suggest relevant labels)

### Context
Why this issue exists and what it depends on.

### Acceptance criteria
- [ ] Specific, testable condition 1
- [ ] Specific, testable condition 2

### Out of scope
What this issue deliberately does NOT cover.

---

After the issues, add a **Milestone** section suggesting how to group them.
`,

  omnigraphProject: `\
The planning phase is complete. Register this plan in the omnigraph
work-coordination graph using the **omnigraph-memory MCP tools**, so it can be
tracked and picked up across sessions and agents. Do NOT implement anything —
only create the project and tasks.

1. Create one project for the overall plan and note its returned \`wp-\` slug:
   \`workflow_project_create(title="<concise plan title>",
   description="<1–2 sentence goal>", phase="implementation")\`

2. Create one task per actionable step in the \`## Plan\`, each linked to the
   project via \`project_slug="<wp- slug>"\`:
   \`task_create(title="<step>", description="<what to do + done condition>",
   project_slug="<wp- slug>", priority="<p0|p1|p2|p3>")\`
   - **Ordering → dependencies:** if a step must follow earlier ones, pass
     \`blocked_by=["<tk- slug of the prerequisite>", ...]\`.
   - **Grouping → hierarchy:** for a step that decomposes into sub-steps, create
     it as \`type="epic"\` and create the sub-steps with \`parent="<epic tk- slug>"\`.
   - Pass \`external_uri\` when a step references a GitHub issue/PR.
   - Turn each open question (❓) into a task whose title starts with
     "[open question]" so it stays visible.

3. Finish with a short summary: the project slug and the created task slugs/titles,
   and note the user can triage with the \`/task\` skill or
   \`omnigraph-explore tasks --ready\`.

If the omnigraph-memory tools are not available, say so instead of guessing.
`,
} as const;

// ---------------------------------------------------------------------------
// Convergence detection
// ---------------------------------------------------------------------------

/**
 * Returns the `## Plan` section text if the assistant has signalled convergence
 * (the response contains both a `## Plan` heading and the `PLAN READY` marker),
 * otherwise null.
 */
export function extractPlanReady(text: string): string | null {
  if (!text.includes("PLAN READY")) return null;
  const match = text.match(/##\s+Plan\b[\s\S]*/i);
  return match ? match[0].replace(/PLAN READY\s*$/i, "").trim() : text;
}
