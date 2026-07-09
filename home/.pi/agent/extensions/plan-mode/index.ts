/**
 * Plan Mode Extension
 *
 * Collaborative planning mode: structured Explore → Frame → Diverge → Converge → Artifact
 * workflow. When active, the LLM is instructed to iterate on a plan with the user before
 * implementing anything. On convergence the user is prompted to export as an implementation
 * guide, RFC, set of GitHub issues, or a tracked project + tasks in the omnigraph MCP.
 *
 * Features:
 * - /plan command and Ctrl+Alt+P to toggle
 * - /plan readonly  blocks file writes during exploration
 * - /plan export    prompt for artifact format at any time
 * - pi --plan       start a session already in planning mode
 * - Footer status: "⏸ plan" (or "⏸ plan 🔒" in read-only mode)
 * - Auto-detects PLAN READY marker and offers artifact menu
 * - State persists across /fork and session restores
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Key } from "@earendil-works/pi-tui";
import {
  ARTIFACT_PROMPTS,
  extractPlanReady,
  PLANNING_INSTRUCTIONS,
  PLANNING_INSTRUCTIONS_READONLY,
} from "./utils.ts";

// Tool sets ----------------------------------------------------------------

/** Read-only tool set used when /plan readonly is active. */
const READONLY_TOOLS = ["read", "bash", "ast_grep_search", "lsp_diagnostics", "lsp_navigation"];

/** Full tool set restored when exiting plan mode or disabling read-only. */
const ALL_TOOLS = [
  "read",
  "bash",
  "edit",
  "write",
  "ast_grep_search",
  "lsp_diagnostics",
  "lsp_navigation",
];

// State -------------------------------------------------------------------

interface PlanState {
  enabled: boolean;
  readonly: boolean;
  /** Last extracted ## Plan content; kept for /plan export after mode exits. */
  planContent: string;
}

function emptyState(): PlanState {
  return { enabled: false, readonly: false, planContent: "" };
}

// Extension ---------------------------------------------------------------

export default function planModeExtension(pi: ExtensionAPI): void {
  let state: PlanState = emptyState();

  // --plan CLI flag: start pi already in planning mode
  pi.registerFlag("plan", {
    description: "Start in planning mode (collaborative exploration → artifact)",
    type: "boolean",
    default: false,
  });

  // ── UI helpers ──────────────────────────────────────────────────────────

  function updateStatus(ctx: ExtensionContext): void {
    if (state.enabled) {
      const label = state.readonly ? "⏸ plan 🔒" : "⏸ plan";
      ctx.ui.setStatus("plan-mode", ctx.ui.theme.fg("warning", label));
    } else {
      ctx.ui.setStatus("plan-mode", undefined);
    }
  }

  // ── Mode transitions ────────────────────────────────────────────────────

  function enable(ctx: ExtensionContext, readonly = false): void {
    state = { ...state, enabled: true, readonly };
    if (readonly) pi.setActiveTools(READONLY_TOOLS);
    ctx.ui.notify(
      readonly
        ? "Planning mode enabled (read-only — writes blocked). /plan to exit."
        : "Planning mode enabled. /plan to exit · /plan export for an artifact."
    );
    updateStatus(ctx);
    persist();
  }

  function disable(ctx: ExtensionContext): void {
    state = { ...state, enabled: false, readonly: false };
    pi.setActiveTools(ALL_TOOLS);
    ctx.ui.notify("Planning mode off.");
    updateStatus(ctx);
    persist();
  }

  function persist(): void {
    pi.appendEntry("plan-mode-state", state);
  }

  // ── Artifact export ──────────────────────────────────────────────────────

  async function promptArtifact(ctx: ExtensionContext): Promise<void> {
    const choice = await ctx.ui.select("Export plan as:", [
      "Implementation guide — ordered tasks for a coding session",
      "RFC — problem / options considered / decision / consequences",
      "GitHub issues — one issue per deliverable with acceptance criteria",
      "Omnigraph — register as a tracked project + tasks (omnigraph MCP)",
      "Keep planning",
    ]);

    if (!choice || choice.startsWith("Keep")) return;

    const prompt = choice.startsWith("Implementation")
      ? ARTIFACT_PROMPTS.implementationGuide
      : choice.startsWith("RFC")
        ? ARTIFACT_PROMPTS.rfc
        : choice.startsWith("GitHub")
          ? ARTIFACT_PROMPTS.githubIssues
          : ARTIFACT_PROMPTS.omnigraphProject;

    // Exit plan mode first so the LLM generates freely
    disable(ctx);
    pi.sendMessage(
      { customType: "plan-export", content: prompt, display: true },
      { triggerTurn: true }
    );
  }

  // ── Commands ─────────────────────────────────────────────────────────────

  pi.registerCommand("plan", {
    description: "Toggle planning mode — /plan · /plan readonly · /plan export",
    handler: async (args, ctx) => {
      const arg = (args ?? "").trim().toLowerCase();

      if (arg === "export") {
        if (!state.enabled && !state.planContent) {
          ctx.ui.notify("No active plan. Start with /plan.", "info");
          return;
        }
        await promptArtifact(ctx);
        return;
      }

      if (arg === "readonly" || arg === "ro") {
        if (!state.enabled) {
          enable(ctx, true);
        } else {
          state = { ...state, readonly: !state.readonly };
          if (state.readonly) {
            pi.setActiveTools(READONLY_TOOLS);
            ctx.ui.notify("Read-only enabled — writes blocked.");
          } else {
            pi.setActiveTools(ALL_TOOLS);
            ctx.ui.notify("Read-only disabled.");
          }
          updateStatus(ctx);
          persist();
        }
        return;
      }

      // Default: toggle on/off
      if (state.enabled) {
        disable(ctx);
      } else {
        enable(ctx);
      }
    },
  });

  pi.registerShortcut(Key.ctrlAlt("p"), {
    description: "Toggle planning mode",
    handler: async (ctx) => {
      if (state.enabled) disable(ctx);
      else enable(ctx);
    },
  });

  // ── Tool gating (read-only mode) ─────────────────────────────────────────

  pi.on("tool_call", async (event) => {
    if (!state.enabled || !state.readonly) return;
    if (event.toolName === "write" || event.toolName === "edit") {
      return {
        block: true,
        reason:
          "Planning mode (read-only): file writes are blocked. " +
          "Use /plan readonly to toggle, or /plan to exit.",
      };
    }
  });

  // ── Context injection ────────────────────────────────────────────────────

  pi.on("before_agent_start", async () => {
    if (!state.enabled) return;
    return {
      message: {
        customType: "plan-mode-context",
        content: state.readonly ? PLANNING_INSTRUCTIONS_READONLY : PLANNING_INSTRUCTIONS,
        display: false,
      },
    };
  });

  // Prune stale plan-mode messages when mode is off (keep context window clean)
  pi.on("context", async (event) => {
    if (state.enabled) return;
    return {
      messages: event.messages.filter((m) => {
        const msg = m as { customType?: string };
        return msg.customType !== "plan-mode-context";
      }),
    };
  });

  // ── Convergence detection ────────────────────────────────────────────────

  pi.on("agent_end", async (event, ctx) => {
    if (!state.enabled || !ctx.hasUI) return;

    // Find the last assistant message from this turn
    const last = [...event.messages].reverse().find((m) => m.role === "assistant");
    if (!last || !("content" in last) || !Array.isArray(last.content)) return;

    const text = (last.content as Array<{ type: string; text?: string }>)
      .filter((b) => b.type === "text")
      .map((b) => b.text ?? "")
      .join("\n");

    const planContent = extractPlanReady(text);
    if (planContent) {
      state = { ...state, planContent };
      persist();
      await promptArtifact(ctx);
    }
  });

  // ── Session restore ───────────────────────────────────────────────────────

  pi.on("session_start", async (_event, ctx) => {
    // --plan flag wins on fresh start
    if (pi.getFlag("plan") === true) {
      state = { ...state, enabled: true };
    }

    // Restore the most recent persisted state entry
    const entries = ctx.sessionManager.getEntries();
    const saved = entries
      .filter(
        (e) =>
          (e as { type: string; customType?: string }).type === "custom" &&
          (e as { customType?: string }).customType === "plan-mode-state"
      )
      .pop() as { data?: PlanState } | undefined;

    if (saved?.data) {
      state = { ...emptyState(), ...saved.data };
    }

    if (state.enabled && state.readonly) {
      pi.setActiveTools(READONLY_TOOLS);
    }

    updateStatus(ctx);
  });
}
