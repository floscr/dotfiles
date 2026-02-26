/**
 * Permission Gate Extension
 *
 * - Confirms before running dangerous bash commands (rm -rf, sudo, chmod 777, etc.)
 * - Blocks writes to protected paths (.env, .git/, node_modules/)
 * - Auto-blocks dangerous commands in non-interactive mode
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  // --- Dangerous bash commands ---
  const dangerousPatterns = [
    /\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|--recursive)\b/i,
    /\bsudo\b/i,
    /\b(chmod|chown)\b.*777/i,
    /\bmkfs\b/i,
    /\bdd\b.*of=\/dev\//i,
    />\s*\/dev\/sd/i,
    /\bsystemctl\s+(stop|disable|mask)\b/i,
    /\bnixos-rebuild\s+switch\b/i,
  ];

  // --- Protected paths (block writes/edits) ---
  const protectedPaths = [
    ".env",
    ".git/",
    "node_modules/",
    "flake.lock",
  ];

  pi.on("tool_call", async (event, ctx) => {
    // Dangerous bash commands
    if (event.toolName === "bash") {
      const command = event.input.command as string;
      const matched = dangerousPatterns.find((p) => p.test(command));

      if (matched) {
        if (!ctx.hasUI) {
          return { block: true, reason: "Dangerous command blocked (non-interactive)" };
        }

        const choice = await ctx.ui.select(
          `⚠️  Dangerous command:\n\n  ${command}\n\nAllow?`,
          ["Yes", "No"],
        );

        if (choice !== "Yes") {
          return { block: true, reason: "Blocked by user" };
        }
      }
    }

    // Protected path writes
    if (event.toolName === "write" || event.toolName === "edit") {
      const path = event.input.path as string;
      const match = protectedPaths.find((p) => path.includes(p));

      if (match) {
        if (!ctx.hasUI) {
          return { block: true, reason: `Protected path: ${path}` };
        }

        const choice = await ctx.ui.select(
          `🔒 Write to protected path:\n\n  ${path}\n\nAllow?`,
          ["Yes", "No"],
        );

        if (choice !== "Yes") {
          ctx.ui.notify(`Blocked write to: ${path}`, "warning");
          return { block: true, reason: `Blocked write to protected path: ${path}` };
        }
      }
    }

    return undefined;
  });
}
