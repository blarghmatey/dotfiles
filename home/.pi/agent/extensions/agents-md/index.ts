/**
 * agents-md Extension
 *
 * Scaffolds or updates AGENTS.md files following the open spec at https://agents.md/
 * and effective context-engineering best practices from the wider community.
 *
 * Commands:
 *   /agents-init [path]   Create or update AGENTS.md in a directory
 *   /agents-scan [path]   Scan a monorepo for submodules / workspace packages
 *                         that are missing AGENTS.md
 *
 * Best-practice principles baked into the generated prompt:
 *   - Less is more: target ≤ 200 lines; shorter is better
 *   - Cover WHY (purpose), WHAT (structure), HOW (commands/conventions)
 *   - Universally applicable instructions only — task-specific guidance belongs
 *     in separate docs referenced via progressive disclosure
 *   - Prefer pointers to files over copying content inline
 *   - Linters own code style; AGENTS.md does not
 */

import { execSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { isAbsolute, join, relative, resolve } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { AutocompleteItem } from "@earendil-works/pi-tui";

// ── filesystem helpers ────────────────────────────────────────────────────────

const SKIP_DIRS = new Set([
  ".git",
  "node_modules",
  "__pycache__",
  ".venv",
  "venv",
  ".tox",
  "dist",
  "build",
  "target",
  ".idea",
  ".vscode",
  ".mypy_cache",
  ".pytest_cache",
  ".ruff_cache",
  "coverage",
  ".turbo",
  ".next",
  ".nuxt",
]);

function tryRead(filePath: string, maxBytes = 4096): string | null {
  try {
    const buf = readFileSync(filePath);
    const text = buf.toString("utf8", 0, maxBytes);
    return buf.length > maxBytes ? `${text}\n…(truncated)` : text;
  } catch {
    return null;
  }
}

function tryExec(cmd: string, cwd: string): string | null {
  try {
    return execSync(cmd, {
      cwd,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
  } catch {
    return null;
  }
}

function listDir(dir: string): string {
  try {
    const entries = readdirSync(dir, { withFileTypes: true });
    return entries
      .filter((e) => !SKIP_DIRS.has(e.name))
      .sort((a, b) => {
        if (a.isDirectory() && !b.isDirectory()) return -1;
        if (!a.isDirectory() && b.isDirectory()) return 1;
        return a.name.localeCompare(b.name);
      })
      .map((e) => (e.isDirectory() ? `  ${e.name}/` : `  ${e.name}`))
      .join("\n");
  } catch {
    return "(could not read directory)";
  }
}

function gatherProjectFacts(targetDir: string): string[] {
  const sections: string[] = [];

  // Language / package manager manifests — read up to 2 KB each
  for (const f of [
    "package.json",
    "pyproject.toml",
    "Cargo.toml",
    "go.mod",
    "pom.xml",
    "build.gradle",
    "build.gradle.kts",
    "mix.exs",
    "Gemfile",
    "composer.json",
    "deno.json",
    "deno.jsonc",
  ]) {
    const content = tryRead(join(targetDir, f), 2048);
    if (content) sections.push(`### ${f}\n\`\`\`\n${content}\n\`\`\``);
  }

  // Task runners — reveal build targets
  for (const f of ["Makefile", "justfile", "Justfile", "taskfile.yml", "Taskfile.yml"]) {
    const content = tryRead(join(targetDir, f), 1024);
    if (content) sections.push(`### ${f}\n\`\`\`\n${content}\n\`\`\``);
  }

  // README — project description and context
  for (const f of ["README.md", "README.rst", "README.txt", "README"]) {
    const content = tryRead(join(targetDir, f), 3000);
    if (content) {
      sections.push(`### ${f}\n${content}`);
      break;
    }
  }

  return sections;
}

// ── prose guide injected into every agents-init prompt ────────────────────────

const AGENTSMD_GUIDE = `
## What makes a great AGENTS.md

AGENTS.md is a README for coding agents — loaded into every agent session context.
Because it competes with all other context for the model's attention, it must be:

- **Concise** — target ≤ 200 lines (shorter is better; AGENTS.md is NOT a wiki)
- **Universally applicable** — if an instruction only matters for a specific type of task, omit it
- **Instruction-minimal** — every extra instruction dilutes all others; include only broadly necessary guidance
- **WHY + WHAT + HOW** — project purpose, directory map, and enough operational detail to run the project
- **Prefer pointers over copies** — reference other docs/files rather than embedding their content inline

Do NOT include:
- Code style rules enforced by linters (agents follow existing code patterns in context)
- Exhaustive command references (keep only the essential 3–8 commands)
- Subsystem-specific guides that only apply to some tasks (point to them via progressive disclosure instead)
- Auto-generated boilerplate that says nothing meaningful

## Recommended structure

\`\`\`markdown
# AGENTS.md

## Project Overview
One or two sentences on what the project does and why it exists.

## Repository Layout
Brief map of top-level directories and their purpose.
One line per directory is usually enough.

## Dev Setup
Minimal steps to bootstrap a working dev environment.
(e.g. "Run \`make dev\` after cloning — requires Python 3.12+ and Docker.")

## Key Commands
Commands for build / test / lint. Aim for 3–8 entries.
| Command | Purpose |
|---------|---------|
| \`make test\` | Run the test suite |
| \`make lint\` | Run linters |

## Conventions & Gotchas  ← omit if nothing non-obvious
Only things that are easy to get wrong and not enforced by tooling.

## Security Notes  ← omit unless genuinely important
Auth patterns, secrets handling, anything an agent could easily mishandle.

## Further Reading  ← use for progressive disclosure
Pointers to detailed docs the agent should read for specific tasks.
\`\`\`

Omit empty sections. Every section must earn its place.
`.trim();

// ── prompt builder ─────────────────────────────────────────────────────────────

interface PromptOpts {
  mode: "create" | "update";
  targetDir: string;
  relTarget: string;
  outputPath: string;
  dirListing: string;
  projectFacts: string[];
  existingContent: string | null;
  gitRoot: string | null;
  gitmodules: string | null;
  ciHints: string[];
}

function buildInitPrompt(opts: PromptOpts): string {
  const {
    mode,
    targetDir,
    relTarget,
    outputPath,
    dirListing,
    projectFacts,
    existingContent,
    gitRoot,
    gitmodules,
    ciHints,
  } = opts;

  const gitNote =
    gitRoot && gitRoot !== targetDir
      ? `\n> ℹ️  Git root is at \`${gitRoot}\`. A root-level AGENTS.md there would apply to the whole repo.`
      : "";

  const ciSection =
    ciHints.length > 0
      ? `\n### CI/CD pipelines detected\n${ciHints.map((h) => `- \`${h}\``).join("\n")}`
      : "";

  const existingSection = existingContent
    ? `\n### Current AGENTS.md (the file you are updating)\n\`\`\`markdown\n${existingContent}\n\`\`\``
    : "";

  const task =
    mode === "update"
      ? `The AGENTS.md already exists (shown above in "Current AGENTS.md"). Update it to improve accuracy and conciseness. Preserve intentional custom sections that look hand-written. Remove or tighten anything stale, verbose, or task-specific.`
      : `No AGENTS.md exists yet. Create one from scratch.`;

  return `
You are ${mode === "update" ? "updating" : "creating"} \`AGENTS.md\` for \`${relTarget}\`.
Output file: \`${outputPath}\`${gitNote}

---

${AGENTSMD_GUIDE}

---

## Project facts gathered from the filesystem

### Directory listing of \`${relTarget}\`
\`\`\`
${dirListing}
\`\`\`

${projectFacts.join("\n\n")}
${gitmodules ? `\n### .gitmodules\n\`\`\`\n${gitmodules}\n\`\`\`` : ""}
${ciSection}
${existingSection}

---

## Task

${task}

Explore the codebase further as needed — read key source files, run \`git log --oneline -10\`,
look at test directories, check CI config, etc. — to fill in any gaps.
Then write the final AGENTS.md to \`${outputPath}\` using the \`write\` tool.

Keep the result **under 200 lines**. Aim for a file a new engineer or AI agent could
read in under 2 minutes and immediately know how to work on the project.
`.trim();
}

// ── extension ─────────────────────────────────────────────────────────────────

export default function agentsMdExtension(pi: ExtensionAPI): void {
  // ── /agents-init [path] ─────────────────────────────────────────────────────

  pi.registerCommand("agents-init", {
    description:
      "Create or update AGENTS.md for the current repo or a subdirectory (/agents-init [path])",

    getArgumentCompletions(prefix: string): AutocompleteItem[] | null {
      try {
        const entries = readdirSync(process.cwd(), { withFileTypes: true });
        const dirs = entries
          .filter((e) => e.isDirectory() && !e.name.startsWith(".") && !SKIP_DIRS.has(e.name))
          .map((e) => ({ value: `${e.name}/`, label: `${e.name}/` }))
          .filter((i) => i.value.startsWith(prefix));
        return dirs.length > 0 ? dirs : null;
      } catch {
        return null;
      }
    },

    handler: async (args, ctx) => {
      const launchCwd = ctx.cwd;
      const rawPath = args?.trim() ?? "";
      const targetDir = rawPath
        ? isAbsolute(rawPath)
          ? rawPath
          : resolve(launchCwd, rawPath)
        : launchCwd;

      if (!existsSync(targetDir)) {
        ctx.ui.notify(`Directory not found: ${targetDir}`, "error");
        return;
      }

      const agentsMdPath = join(targetDir, "AGENTS.md");
      const exists = existsSync(agentsMdPath);
      let mode: "create" | "update" = "create";

      if (exists) {
        const relDisplay = relative(launchCwd, targetDir) || ".";
        const choice = await ctx.ui.select(`AGENTS.md already exists in \`${relDisplay}\``, [
          "Update — improve while preserving custom sections",
          "Overwrite — fresh start",
          "Cancel",
        ]);
        if (!choice || choice.startsWith("Cancel")) {
          ctx.ui.notify("Cancelled.", "info");
          return;
        }
        mode = choice.startsWith("Update") ? "update" : "create";
      }

      // Gather project facts synchronously before sending the prompt
      const projectFacts = gatherProjectFacts(targetDir);
      const dirListing = listDir(targetDir);
      const existingContent = mode === "update" ? tryRead(agentsMdPath) : null;
      const gitRoot = tryExec("git rev-parse --show-toplevel", targetDir);
      const gitmodules = tryRead(join(targetDir, ".gitmodules"));

      const ciHints: string[] = [];
      for (const f of [
        ".github/workflows",
        ".circleci/config.yml",
        ".travis.yml",
        "Jenkinsfile",
        ".gitlab-ci.yml",
        "azure-pipelines.yml",
      ]) {
        if (existsSync(join(targetDir, f))) ciHints.push(f);
      }

      const relTarget = relative(launchCwd, targetDir) || "(repo root)";
      const outputPath = relative(launchCwd, agentsMdPath) || "AGENTS.md";

      const prompt = buildInitPrompt({
        mode,
        targetDir,
        relTarget,
        outputPath,
        dirListing,
        projectFacts,
        existingContent,
        gitRoot,
        gitmodules,
        ciHints,
      });

      ctx.ui.notify(`${mode === "update" ? "Updating" : "Creating"} ${outputPath}…`, "info");

      pi.sendUserMessage(prompt);
    },
  });

  // ── /agents-scan [path] ─────────────────────────────────────────────────────

  pi.registerCommand("agents-scan", {
    description:
      "Scan a repo for submodules or workspace packages that need AGENTS.md (/agents-scan [path])",

    getArgumentCompletions(prefix: string): AutocompleteItem[] | null {
      try {
        const entries = readdirSync(process.cwd(), { withFileTypes: true });
        const dirs = entries
          .filter((e) => e.isDirectory() && !e.name.startsWith(".") && !SKIP_DIRS.has(e.name))
          .map((e) => ({ value: `${e.name}/`, label: `${e.name}/` }))
          .filter((i) => i.value.startsWith(prefix));
        return dirs.length > 0 ? dirs : null;
      } catch {
        return null;
      }
    },

    handler: async (args, ctx) => {
      const launchCwd = ctx.cwd;
      const rawPath = args?.trim() ?? "";
      const targetDir = rawPath
        ? isAbsolute(rawPath)
          ? rawPath
          : resolve(launchCwd, rawPath)
        : launchCwd;

      if (!existsSync(targetDir)) {
        ctx.ui.notify(`Directory not found: ${targetDir}`, "error");
        return;
      }

      const gitRoot = tryExec("git rev-parse --show-toplevel", targetDir) ?? targetDir;
      const relTarget = relative(launchCwd, gitRoot) || "(repo root)";

      const prompt = `
Scan the repository at \`${gitRoot}\` (\`${relTarget}\` relative to cwd) and produce
a report of where AGENTS.md files should be added or improved.

Do the following using \`bash\` and \`read\` tools:

1. **Root** — check whether \`${gitRoot}/AGENTS.md\` exists. If it does, briefly assess
   its quality (concise? covers WHY/WHAT/HOW? any obvious gaps?).

2. **Submodules** — read \`.gitmodules\` if present. For each submodule, note its path
   and whether an AGENTS.md exists there.

3. **Monorepo workspaces** — look for workspace configs and list packages/apps missing AGENTS.md:
   - \`package.json\` → \`workspaces\` field
   - \`pnpm-workspace.yaml\`
   - \`pyproject.toml\` → \`[tool.poetry.packages]\` or \`[tool.hatch.build.targets]\`
   - \`Cargo.toml\` → \`[workspace]\` members
   - \`go.work\` → \`use\` directives

4. **Notable subdirectories** — flag large top-level dirs that look self-contained
   (e.g. \`packages/\`, \`apps/\`, \`services/\`, \`dags/\`, \`modules/\`) and check each
   for an AGENTS.md.

Present findings as a Markdown table:

| Location | Has AGENTS.md | Assessment |
|----------|--------------|------------|

Then list the top 3 most impactful places to add or improve AGENTS.md, and ask the user
which location(s) to initialize (so you can follow up with /agents-init).
      `.trim();

      ctx.ui.notify(`Scanning ${relTarget} for AGENTS.md opportunities…`, "info");
      pi.sendUserMessage(prompt);
    },
  });
}
