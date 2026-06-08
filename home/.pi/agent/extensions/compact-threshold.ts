/**
 * Compact Threshold Extension
 *
 * Automatically triggers context compaction when context usage exceeds a
 * configurable threshold, reducing rate-limit consumption by compacting early
 * instead of letting the context window fill up.
 *
 * Configuration is loaded from (project overrides global):
 *   ~/.pi/agent/compact-thresholds.json
 *   <cwd>/.pi/compact-thresholds.json
 *
 * Config format:
 * {
 *   "enabled": true,
 *   "showStatus": true,
 *   "thresholds": {
 *     "default": 80,
 *     "anthropic": 70,
 *     "claude-sonnet-4-6": 65,
 *     "claude-opus-4-7": 60
 *   }
 * }
 *
 * Threshold keys are matched in order: exact model ID → provider name → "default".
 * Values are percentages (0–100). Omit a key to fall through to the next level.
 * Set "enabled": false to disable auto-compact globally (can still use /compact).
 *
 * Commands:
 *   /threshold          Show current context usage, active threshold, and config
 *   /threshold status   Toggle footer status indicator on/off
 */

import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

interface ThresholdConfig {
  enabled?: boolean;
  showStatus?: boolean;
  thresholds?: Record<string, number>;
}

function loadConfig(cwd: string): ThresholdConfig {
  function tryParse(path: string): ThresholdConfig {
    if (!existsSync(path)) return {};
    try {
      return JSON.parse(readFileSync(path, "utf-8"));
    } catch (e) {
      console.error(`compact-threshold: failed to parse ${path}: ${e}`);
      return {};
    }
  }

  const global = tryParse(join(homedir(), ".pi", "agent", "compact-thresholds.json"));
  const project = tryParse(join(cwd, ".pi", "compact-thresholds.json"));

  return {
    enabled: project.enabled ?? global.enabled ?? true,
    showStatus: project.showStatus ?? global.showStatus ?? true,
    thresholds: { ...global.thresholds, ...project.thresholds },
  };
}

function resolveThreshold(
  config: ThresholdConfig,
  modelId: string | undefined,
  provider: string | undefined
): number | null {
  const thresholds = config.thresholds ?? {};

  // Exact model ID match
  if (modelId && modelId in thresholds) return thresholds[modelId];

  // Provider match
  if (provider && provider in thresholds) return thresholds[provider];

  // Default
  if ("default" in thresholds) return thresholds.default;

  return null;
}

function formatPercent(percent: number | null): string {
  return percent === null ? "?" : `${Math.round(percent)}%`;
}

export default function compactThresholdExtension(pi: ExtensionAPI): void {
  let showStatus = true;
  let lastCompactPercent: number | null = null;

  // biome-ignore lint/suspicious/noExplicitAny: ctx.ui type not exported by pi
  // biome-ignore lint/correctness/noUnusedVariables: used in handler
  function updateFooter(
    // biome-ignore lint/suspicious/noExplicitAny: ctx.ui type not exported by pi
    ctx: { hasUI: boolean; ui: any },
    percent: number | null,
    threshold: number | null
  ): void {
    if (!ctx.hasUI || !showStatus) return;

    if (percent === null) {
      ctx.ui.setStatus("compact-threshold", undefined);
      return;
    }

    const label =
      threshold !== null
        ? `ctx ${formatPercent(percent)}/${threshold}%`
        : `ctx ${formatPercent(percent)}`;

    const color =
      threshold !== null && percent >= threshold * 0.9
        ? "warning"
        : percent >= 50
          ? "accent"
          : "fg";

    ctx.ui.setStatus("compact-threshold", ctx.ui.theme.fg(color, label));
  }

  pi.on("session_start", async (_event, ctx) => {
    const config = loadConfig(ctx.cwd);
    showStatus = config.showStatus ?? true;
    lastCompactPercent = null;
    updateFooter(ctx, null, null);
  });

  pi.on("turn_end", async (_event, ctx) => {
    const config = loadConfig(ctx.cwd);
    if (!config.enabled) return;

    const usage = ctx.getContextUsage();
    const percent = usage?.percent ?? null;
    const modelId = ctx.model?.id;
    const provider = (ctx.model as { provider?: string } | undefined)?.provider;
    const threshold = resolveThreshold(config, modelId, provider);

    updateFooter(ctx, percent, threshold);

    if (threshold === null || percent === null) return;

    if (percent >= threshold) {
      const prevPercent = lastCompactPercent;
      // Guard against compact loops: don't trigger again if we just compacted
      // and usage is still high (model was already minimal, nothing to compact)
      if (prevPercent !== null && prevPercent >= threshold) return;

      lastCompactPercent = percent;
      ctx.compact();

      if (ctx.hasUI) {
        ctx.ui.notify(
          `Auto-compact triggered: context at ${formatPercent(percent)} (threshold ${threshold}%)`,
          "info"
        );
      }
    } else {
      lastCompactPercent = null;
    }
  });

  pi.registerCommand("threshold", {
    description: "Show context usage and auto-compact threshold config",
    handler: async (args, ctx) => {
      const arg = (args ?? "").trim().toLowerCase();

      if (arg === "status") {
        showStatus = !showStatus;
        if (!showStatus) ctx.ui.setStatus("compact-threshold", undefined);
        ctx.ui.notify(`Context usage indicator ${showStatus ? "enabled" : "disabled"}.`, "info");
        return;
      }

      const config = loadConfig(ctx.cwd);
      const usage = ctx.getContextUsage();
      const modelId = ctx.model?.id;
      const provider = (ctx.model as { provider?: string } | undefined)?.provider;
      const threshold = resolveThreshold(config, modelId, provider);

      const lines: string[] = [
        `Auto-compact: ${config.enabled ? "enabled" : "disabled"}`,
        `Model:        ${modelId ?? "(unknown)"}`,
        `Provider:     ${provider ?? "(unknown)"}`,
        `Threshold:    ${threshold !== null ? `${threshold}%` : "none configured"}`,
        `Context:      ${usage ? `${formatPercent(usage.percent)} (${usage.tokens?.toLocaleString() ?? "?"} / ${usage.contextWindow.toLocaleString()} tokens)` : "unavailable"}`,
        "",
        "Config files (project overrides global):",
        `  Global:  ~/.pi/agent/compact-thresholds.json`,
        `  Project: <cwd>/.pi/compact-thresholds.json`,
      ];

      if (config.thresholds && Object.keys(config.thresholds).length > 0) {
        lines.push("", "Configured thresholds:");
        for (const [key, val] of Object.entries(config.thresholds)) {
          const active = key === modelId || key === provider || key === "default";
          lines.push(`  ${active ? "→" : " "} ${key}: ${val}%`);
        }
      } else {
        lines.push("", "No thresholds configured. Create compact-thresholds.json to enable.");
      }

      ctx.ui.notify(lines.join("\n"), "info");
    },
  });
}
