/**
 * Virtual CWD Extension
 *
 * Adds a /cd command to change the working directory for the current pi session
 * without restarting. All relative paths in tool calls and user bash commands
 * are resolved against the virtual cwd.
 *
 * Usage:
 *   /cd <path>              Change virtual cwd (relative or absolute)
 *   /cd worktree <name>     Change to a git worktree by name or branch
 *   /cd                     Show current virtual cwd
 *   /cd -                   Reset to pi's original launch directory
 *
 * The active directory is shown as a status indicator in the footer when it
 * differs from the launch directory.
 */

import { execSync } from "node:child_process";
import { existsSync, statSync } from "node:fs";
import { basename, isAbsolute, resolve } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { createLocalBashOperations, isToolCallEventType } from "@earendil-works/pi-coding-agent";
import type { AutocompleteItem } from "@earendil-works/pi-tui";

function shellQuote(s: string): string {
  return `'${s.replace(/'/g, "'\\''")}'`;
}

interface Worktree {
  path: string;
  branch: string;
  head: string;
}

function listWorktrees(cwd: string): Worktree[] {
  try {
    const output = execSync("git worktree list --porcelain", {
      cwd,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    });
    const worktrees: Worktree[] = [];
    let current: Partial<Worktree> = {};
    for (const line of output.split("\n")) {
      if (line.startsWith("worktree ")) {
        current.path = line.slice(9);
      } else if (line.startsWith("HEAD ")) {
        current.head = line.slice(5);
      } else if (line.startsWith("branch ")) {
        current.branch = line.slice(7).replace("refs/heads/", "");
      } else if (line === "detached") {
        current.branch = "HEAD (detached)";
      } else if (line === "" && current.path) {
        worktrees.push(current as Worktree);
        current = {};
      }
    }
    if (current.path) worktrees.push(current as Worktree);
    return worktrees;
  } catch {
    return [];
  }
}

function findWorktree(name: string, cwd: string): Worktree | undefined {
  const worktrees = listWorktrees(cwd);
  return worktrees.find(
    (wt) => basename(wt.path) === name || wt.path === name || wt.branch === name
  );
}

export default function (pi: ExtensionAPI) {
  let virtualCwd: string | null = null;

  function getVirtualCwd(launchCwd: string): string {
    return virtualCwd ?? launchCwd;
  }

  function isChanged(launchCwd: string): boolean {
    return virtualCwd !== null && virtualCwd !== launchCwd;
  }

  // biome-ignore lint/suspicious/noExplicitAny: ctx.ui type is not exported by pi
  function updateStatus(ctx: { cwd: string; hasUI: boolean; ui: any }) {
    if (!ctx.hasUI) return;
    const vCwd = getVirtualCwd(ctx.cwd);
    if (isChanged(ctx.cwd)) {
      ctx.ui.setStatus("virtual-cwd", ctx.ui.theme.fg("accent", `⌂ ${vCwd}`));
    } else {
      ctx.ui.setStatus("virtual-cwd", undefined);
    }
  }

  // Initialize on session start (handles resume/fork too)
  pi.on("session_start", async (_event, ctx) => {
    virtualCwd = null;
    updateStatus(ctx);
  });

  // /cd command
  pi.registerCommand("cd", {
    description: "Change virtual cwd (/cd <path> | /cd worktree <name> | /cd - to reset)",

    getArgumentCompletions(prefix: string): AutocompleteItem[] | null {
      // "worktree " prefix: complete worktree names
      const worktreePrefix = "worktree ";
      if (prefix.startsWith(worktreePrefix)) {
        const _namePart = prefix.slice(worktreePrefix.length);
        // Use process.cwd() as the best available cwd at completion time
        const worktrees = listWorktrees(process.cwd());
        const items = worktrees.flatMap((wt) => {
          const name = basename(wt.path);
          const label = wt.branch !== "HEAD (detached)" ? `${name} (${wt.branch})` : name;
          return [{ value: `worktree ${name}`, label }];
        });
        return items.filter((i) => i.value.startsWith(prefix)) || null;
      }

      // Offer "worktree" as a subcommand completion
      if ("worktree".startsWith(prefix)) {
        return [{ value: "worktree ", label: "worktree <name>  Change to a git worktree" }];
      }

      return null;
    },

    handler: async (args, ctx) => {
      const launchCwd = ctx.cwd;
      const currentVCwd = getVirtualCwd(launchCwd);
      const target = args?.trim();

      // No arg: show current
      if (!target) {
        const label = isChanged(launchCwd)
          ? `Virtual cwd: ${currentVCwd}\nLaunch cwd:  ${launchCwd}`
          : `Cwd: ${currentVCwd} (same as launch directory)`;
        ctx.ui.notify(label, "info");
        return;
      }

      // - : reset to launch dir
      if (target === "-") {
        virtualCwd = null;
        ctx.ui.notify(`Reset to launch directory: ${launchCwd}`, "info");
        updateStatus(ctx);
        return;
      }

      // worktree <name>: resolve via git worktree list
      const worktreeMatch = target.match(/^worktree\s+(.+)$/);
      if (worktreeMatch) {
        const name = worktreeMatch[1].trim();
        const wt = findWorktree(name, currentVCwd);
        if (!wt) {
          const available = listWorktrees(currentVCwd)
            .map((w) => basename(w.path))
            .join(", ");
          ctx.ui.notify(
            `Worktree not found: ${name}${available ? `\nAvailable: ${available}` : ""}`,
            "error"
          );
          return;
        }
        virtualCwd = wt.path;
        ctx.ui.notify(`Working directory: ${virtualCwd}`, "info");
        updateStatus(ctx);
        return;
      }

      // Plain path
      const resolved = isAbsolute(target) ? target : resolve(currentVCwd, target);

      if (!existsSync(resolved)) {
        ctx.ui.notify(`Directory not found: ${resolved}`, "error");
        return;
      }
      if (!statSync(resolved).isDirectory()) {
        ctx.ui.notify(`Not a directory: ${resolved}`, "error");
        return;
      }

      virtualCwd = resolved;
      ctx.ui.notify(`Working directory: ${virtualCwd}`, "info");
      updateStatus(ctx);
    },
  });

  // Inject cwd into the system prompt so the model knows where it is
  pi.on("before_agent_start", async (event, ctx) => {
    if (!isChanged(ctx.cwd)) return;
    const vCwd = getVirtualCwd(ctx.cwd);
    return {
      systemPrompt:
        event.systemPrompt +
        `\n\n## Working Directory\nThe current working directory has been set to \`${vCwd}\` for this session (pi was launched from \`${ctx.cwd}\`). Treat all relative paths as relative to \`${vCwd}\`. When asked to work in "the current directory", use \`${vCwd}\`.`,
    };
  });

  // Intercept agent tool calls to rewrite relative paths
  pi.on("tool_call", (event, ctx) => {
    if (!isChanged(ctx.cwd)) return;
    const vCwd = getVirtualCwd(ctx.cwd);

    if (isToolCallEventType("bash", event)) {
      event.input.command = `cd ${shellQuote(vCwd)} && ${event.input.command}`;
      return;
    }

    // For file tools: resolve relative path against virtualCwd
    const input = event.input as Record<string, unknown>;

    if (typeof input.path === "string" && !isAbsolute(input.path)) {
      input.path = resolve(vCwd, input.path);
    }

    // grep/find may pass an array of paths
    if (Array.isArray(input.paths)) {
      input.paths = (input.paths as string[]).map((p) =>
        typeof p === "string" && !isAbsolute(p) ? resolve(vCwd, p) : p
      );
    }
  });

  // Intercept user ! commands to run in the virtual cwd
  pi.on("user_bash", (_event, ctx) => {
    if (!isChanged(ctx.cwd)) return;
    const vCwd = getVirtualCwd(ctx.cwd);
    const local = createLocalBashOperations();
    return {
      operations: {
        exec(command: string, _cwd: string, options: unknown) {
          return local.exec(command, vCwd, options as never);
        },
      },
    };
  });
}
