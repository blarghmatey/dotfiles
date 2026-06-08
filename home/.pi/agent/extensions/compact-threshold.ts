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
 *   },
 *   "contextWindows": {
 *     "claude-bridge": 200000,
 *     "my-custom-model": 128000
 *   }
 * }
 *
 * Threshold keys are matched in order: exact model ID → provider name → "default".
 * contextWindow keys are matched in the same order and override the value reported
 * by getContextUsage() — useful when a provider reports an incorrect limit (e.g. 1M).
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
  contextWindows?: Record<string, number>;
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
    contextWindows: { ...global.contextWindows, ...project.contextWindows },
  };
}

function resolveByKey<T>(
  map: Record<string, T> | undefined,
  modelId: string | undefined,
  provider: string | undefined
): T | null {
  if (!map) return null;
  if (modelId && modelId in map) return map[modelId];
  if (provider && provider in map) return map[provider];
  if ("default" in map) return (map as Record<string, T>).default;
  return null;
}

function resolveContextUsage(
  config: ThresholdConfig,
  modelId: string | undefined,
  provider: string | undefined,
  reportedTokens: number | null,
  reportedWindow: number
): { tokens: number | null; contextWindow: number; percent: number | null } {
  const overrideWindow = resolveByKey(config.contextWindows, modelId, provider);
  const contextWindow = overrideWindow ?? reportedWindow;
  const tokens = reportedTokens;
  const percent = tokens !== null && contextWindow > 0 ? (tokens / contextWindow) * 100 : null;
  return { tokens, contextWindow, percent };
}

function formatPercent(percent: number | null): string {
  return percent === null ? "?" : `${Math.round(percent)}%`;
}

export default function compactThresholdExtension(pi: ExtensionAPI): void {
  let showStatus = true;
  let lastCompactPercent: number | null = null;
  let lastKnownPercent: number | null = null;
  let lastKnownThreshold: number | null = null;

  function updateFooter(
    // biome-ignore lint/suspicious/noExplicitAny: ctx.ui type not exported by pi
    ctx: { hasUI: boolean; ui: any },
    percent: number | null,
    threshold: number | null
  ): void {
    if (!ctx.hasUI || !showStatus) return;

    // Persist last known values so the indicator stays visible when tokens are
    // temporarily unavailable (e.g. right after compaction).
    if (percent !== null) lastKnownPercent = percent;
    if (threshold !== null) lastKnownThreshold = threshold;

    const displayPercent = lastKnownPercent;
    const displayThreshold = lastKnownThreshold;

    const label =
      displayThreshold !== null
        ? `ctx ${formatPercent(displayPercent)}/${displayThreshold}%`
        : `ctx ${formatPercent(displayPercent)}`;

    const color =
      displayPercent !== null &&
      displayThreshold !== null &&
      displayPercent >= displayThreshold * 0.9
        ? "warning"
        : displayPercent !== null && displayPercent >= 50
          ? "accent"
          : "muted";

    ctx.ui.setStatus("compact-threshold", ctx.ui.theme.fg(color, label));
  }

  pi.on("session_start", async (_event, ctx) => {
    const config = loadConfig(ctx.cwd);
    showStatus = config.showStatus ?? true;
    lastCompactPercent = null;
    lastKnownPercent = null;
    lastKnownThreshold = null;
    if (ctx.hasUI && showStatus) {
      ctx.ui.setStatus("compact-threshold", ctx.ui.theme.fg("muted", "ctx --"));
    }
  });

  pi.on("turn_end", async (_event, ctx) => {
    const config = loadConfig(ctx.cwd);
    if (!config.enabled) return;

    const raw = ctx.getContextUsage();
    const modelId = ctx.model?.id;
    const provider = (ctx.model as { provider?: string } | undefined)?.provider;

    const { contextWindow, percent } = resolveContextUsage(
      config,
      modelId,
      provider,
      raw?.tokens ?? null,
      raw?.contextWindow ?? 0
    );

    const threshold = resolveByKey(config.thresholds, modelId, provider);

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
          `Auto-compact triggered: context at ${formatPercent(percent)} of ${contextWindow.toLocaleString()} tokens (threshold ${threshold}%)`,
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
      const raw = ctx.getContextUsage();
      const modelId = ctx.model?.id;
      const provider = (ctx.model as { provider?: string } | undefined)?.provider;
      const threshold = resolveByKey(config.thresholds, modelId, provider);
      const windowOverride = resolveByKey(config.contextWindows, modelId, provider);

      const { tokens, contextWindow, percent } = resolveContextUsage(
        config,
        modelId,
        provider,
        raw?.tokens ?? null,
        raw?.contextWindow ?? 0
      );

      const windowNote = windowOverride
        ? ` (overridden from ${raw?.contextWindow.toLocaleString() ?? "?"})`
        : raw && raw.contextWindow !== contextWindow
          ? ` (reported: ${raw.contextWindow.toLocaleString()})`
          : "";

      const lines: string[] = [
        `Auto-compact: ${config.enabled ? "enabled" : "disabled"}`,
        `Model:        ${modelId ?? "(unknown)"}`,
        `Provider:     ${provider ?? "(unknown)"}`,
        `Threshold:    ${threshold !== null ? `${threshold}%` : "none configured"}`,
        `Context:      ${raw ? `${formatPercent(percent)} (${tokens?.toLocaleString() ?? "?"} / ${contextWindow.toLocaleString()} tokens${windowNote})` : "unavailable"}`,
        "",
        "Config files (project overrides global):",
        "  Global:  ~/.pi/agent/compact-thresholds.json",
        "  Project: <cwd>/.pi/compact-thresholds.json",
      ];

      if (config.thresholds && Object.keys(config.thresholds).length > 0) {
        lines.push("", "Configured thresholds:");
        for (const [key, val] of Object.entries(config.thresholds)) {
          const active = key === modelId || key === provider || key === "default";
          lines.push(`  ${active ? "→" : " "} ${key}: ${val}%`);
        }
      } else {
        lines.push("", "No thresholds configured.");
      }

      if (config.contextWindows && Object.keys(config.contextWindows).length > 0) {
        lines.push("", "Configured context window overrides:");
        for (const [key, val] of Object.entries(config.contextWindows)) {
          const active = key === modelId || key === provider || key === "default";
          lines.push(`  ${active ? "→" : " "} ${key}: ${val.toLocaleString()} tokens`);
        }
      }

      ctx.ui.notify(lines.join("\n"), "info");
    },
  });
}
