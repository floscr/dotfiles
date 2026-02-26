/**
 * Commit Extension - Interactive git commit workflow with hunk-level staging
 *
 * Provides:
 * - /commit command: Select mode (auto/staged/changed/other), then LLM drafts commit
 * - git_overview tool: List staged/changed files with stat summary
 * - git_file_diff tool: Show diffs for specific files (priority-sorted, truncated)
 * - git_hunk tool: Inspect individual hunks from a file diff
 * - git_stage_hunks tool: Stage specific hunks (not whole files) via git apply --cached
 * - git_commit_with_user_approval tool: LLM proposes commit, user reviews in editor
 *
 * Usage:
 *   /commit          - Select what to commit, LLM drafts message
 *   /commit message  - Quick commit with provided message hint
 *
 * Hunk workflow (LLM-driven):
 *   1. git_overview → see all changed files
 *   2. git_file_diff → inspect diffs for files of interest
 *   3. git_hunk → drill into specific hunks
 *   4. git_stage_hunks → stage only the relevant hunks
 *   5. git_commit_with_user_approval → propose commit for staged changes
 *
 * Based on: https://github.com/dejanr/dotfiles (pi-mono extensions)
 *           https://github.com/can1357/oh-my-pi (commit tool architecture)
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import { execSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

// ─── Diff parsing ────────────────────────────────────────────────────────────

interface FileDiff {
  filename: string;
  content: string;
  additions: number;
  deletions: number;
  isBinary: boolean;
}

interface DiffHunk {
  index: number;
  header: string;
  oldStart: number;
  oldLines: number;
  newStart: number;
  newLines: number;
  content: string;
}

interface FileHunks {
  filename: string;
  isBinary: boolean;
  hunks: DiffHunk[];
}

function parseFileDiffs(diff: string): FileDiff[] {
  const sections: FileDiff[] = [];
  const parts = diff.split("\ndiff --git ");
  for (let i = 0; i < parts.length; i++) {
    const part = i === 0 ? parts[i] : `diff --git ${parts[i]}`;
    if (!part.trim()) continue;
    const lines = part.split("\n");
    const header = lines[0] ?? "";
    const match = header.match(/diff --git a\/(.+?) b\/(.+)$/);
    if (!match) continue;
    const filename = match[2];
    const isBinary = lines.some((l) => l.startsWith("Binary files "));
    let additions = 0;
    let deletions = 0;
    for (const line of lines) {
      if (line.startsWith("+++") || line.startsWith("---")) continue;
      if (line.startsWith("+")) additions++;
      else if (line.startsWith("-")) deletions++;
    }
    sections.push({ filename, content: part, additions, deletions, isBinary });
  }
  return sections;
}

function parseHunkHeader(line: string) {
  const match = line.match(
    /@@\s-([0-9]+)(?:,([0-9]+))?\s\+([0-9]+)(?:,([0-9]+))?\s@@/,
  );
  if (!match) return { oldStart: 0, oldLines: 0, newStart: 0, newLines: 0 };
  return {
    oldStart: parseInt(match[1] ?? "0", 10) || 0,
    oldLines: parseInt(match[2] ?? "1", 10) || 0,
    newStart: parseInt(match[3] ?? "0", 10) || 0,
    newLines: parseInt(match[4] ?? "1", 10) || 0,
  };
}

function parseFileHunks(fileDiff: FileDiff): FileHunks {
  if (fileDiff.isBinary)
    return { filename: fileDiff.filename, isBinary: true, hunks: [] };

  const lines = fileDiff.content.split("\n");
  const hunks: DiffHunk[] = [];
  let current: DiffHunk | null = null;
  let buffer: string[] = [];
  let index = 0;

  for (const line of lines) {
    if (line.startsWith("@@")) {
      if (current) {
        current.content = buffer.join("\n");
        hunks.push(current);
      }
      const h = parseHunkHeader(line);
      current = { index, header: line, ...h, content: "" };
      buffer = [line];
      index++;
      continue;
    }
    if (current) buffer.push(line);
  }
  if (current) {
    current.content = buffer.join("\n");
    hunks.push(current);
  }

  return { filename: fileDiff.filename, isBinary: false, hunks };
}

function parseDiffHunks(diff: string): FileHunks[] {
  return parseFileDiffs(diff).map(parseFileHunks);
}

// ─── Git helpers ─────────────────────────────────────────────────────────────

function git(args: string[], signal?: AbortSignal): string {
  return execSync(`git ${args.join(" ")}`, {
    encoding: "utf-8",
    timeout: 30000,
  }).trim();
}

function extractFileHeader(diff: string): string {
  const lines = diff.split("\n");
  const headerLines: string[] = [];
  for (const line of lines) {
    if (line.startsWith("@@")) break;
    headerLines.push(line);
  }
  return headerLines.join("\n");
}

// ─── Constants ───────────────────────────────────────────────────────────────

const COMMIT_FORMAT_GUIDE = `
Commit message format guidelines:
- Start with a short prefix followed by colon and space (feat:, fix:, docs:, refactor:, test:, chore:, etc.)
- feat: for user-visible features, fix: for bug fixes
- A scope MAY be added in parentheses, e.g. fix(parser): - only when it meaningfully improves clarity
- Short description in imperative mood explaining what changed, not how
- Body MAY be included after one blank line for context, rationale, or non-obvious behavior
- Breaking changes should be explained clearly in description or body, no special marking required
- Clarity and usefulness matter more than strict conformance
`.trim();

const EXCLUDED_LOCK_FILES = new Set([
  "Cargo.lock",
  "package-lock.json",
  "yarn.lock",
  "pnpm-lock.yaml",
  "bun.lock",
  "bun.lockb",
  "go.sum",
  "poetry.lock",
  "Pipfile.lock",
  "uv.lock",
  "composer.lock",
  "Gemfile.lock",
  "flake.lock",
  "pubspec.lock",
  "Podfile.lock",
  "mix.lock",
]);

// ─── Extension ───────────────────────────────────────────────────────────────

export default function commit(pi: ExtensionAPI) {
  // ── /commit command ──────────────────────────────────────────────────────

  pi.registerCommand("commit", {
    description: "Draft and create a git commit with LLM assistance",
    handler: async (args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify("commit requires interactive mode", "error");
        return;
      }

      let mode: string;
      let instruction = args.trim();

      if (instruction) {
        mode = "other";
      } else {
        const selection = await ctx.ui.select("What do you want to commit?", [
          "auto - Let the agent figure out what to commit",
          "staged - Commit currently staged files",
          "changed - Commit all changed files",
          "hunks - Select individual hunks to commit",
          "other - Describe what you want to commit",
        ]);

        if (!selection) {
          ctx.ui.notify("Cancelled", "info");
          return;
        }

        mode = selection.split(" - ")[0];

        if (mode === "other") {
          const input = await ctx.ui.input("What do you want to commit?");
          if (!input) {
            ctx.ui.notify("Cancelled", "info");
            return;
          }
          instruction = input;
        }
      }

      let prompt: string;
      switch (mode) {
        case "auto":
          prompt = `Analyze the current git status and changes. Determine what should be committed, stage the appropriate files, and draft a commit message. Use git_commit_with_user_approval to let me review and confirm the commit.

Use git_overview and git_file_diff to understand the changes. If a file contains unrelated changes mixed together, use git_hunk and git_stage_hunks to stage only the relevant hunks instead of the whole file.

IMPORTANT: Be very selective about what you commit. Only include changes that are clearly related to each other. Do NOT commit:
- Untracked files unless they are clearly part of the current work
- Unrelated local changes
- Configuration files, logs, or artifacts that shouldn't be in version control

When in doubt, leave it out.`;
          break;
        case "staged":
          prompt = `Check what files are currently staged (git diff --cached). Draft a commit message for the staged changes. Use git_commit_with_user_approval to let me review and confirm the commit. Do not stage any additional files.`;
          break;
        case "changed":
          prompt = `Stage all tracked files that have been modified (git add -u) and draft a commit message based on the changes. Use git_commit_with_user_approval to let me review and confirm the commit.`;
          break;
        case "hunks":
          prompt = `Analyze all current changes and create atomic commits by staging individual hunks. Here's the workflow:

1. Use git_overview to see all changed files
2. Use git_file_diff to inspect the diffs
3. Use git_hunk to examine individual hunks when a file contains unrelated changes
4. Group related hunks together and use git_stage_hunks to stage them
5. Use git_commit_with_user_approval to propose a commit message

Use your judgment to separate unrelated changes into distinct commits. If all changes are related, a single commit is fine. Focus on creating clean, atomic commits.`;
          break;
        case "other":
          prompt = `I want to commit: ${instruction}

Analyze the git status and stage ONLY the files directly relevant to this request. If changes span multiple concerns within a file, use git_hunk to inspect individual hunks and git_stage_hunks to stage only the relevant ones. Draft a commit message. Use git_commit_with_user_approval to let me review and confirm.

IMPORTANT: Be very conservative. Only stage changes clearly related to the request.`;
          break;
        default:
          ctx.ui.notify("Unknown mode", "error");
          return;
      }

      pi.sendUserMessage(prompt);
    },
  });

  // ── git_overview tool ────────────────────────────────────────────────────

  pi.registerTool({
    name: "git_overview",
    label: "Git Overview",
    description:
      "Return changed/staged files with stat summary and line counts. Use this first to understand the scope of changes before drilling into specific files.",
    parameters: Type.Object({
      staged: Type.Optional(
        Type.Boolean({
          description: "Show staged changes (default: false, shows unstaged)",
        }),
      ),
    }),

    async execute(_toolCallId, params, signal) {
      const staged = params.staged ?? false;
      const args = staged ? "diff --cached" : "diff";

      const nameOnly = git([args, "--name-only"].filter(Boolean));
      const files = nameOnly
        .split("\n")
        .filter(Boolean)
        .filter((f) => !EXCLUDED_LOCK_FILES.has(f.split("/").pop() ?? f));

      const stat = git([args, "--stat"].filter(Boolean));
      const numstat = git([args, "--numstat"].filter(Boolean));

      const result = {
        files,
        stat,
        numstat: numstat
          .split("\n")
          .filter(Boolean)
          .map((line) => {
            const [add, del, file] = line.split("\t");
            return { path: file, additions: +add || 0, deletions: +del || 0 };
          })
          .filter((e) => !EXCLUDED_LOCK_FILES.has(e.path.split("/").pop() ?? e.path)),
        excludedLockFiles: nameOnly
          .split("\n")
          .filter((f) => EXCLUDED_LOCK_FILES.has(f.split("/").pop() ?? f)),
      };

      return {
        content: [{ type: "text", text: JSON.stringify(result, null, 2) }],
        details: result,
      };
    },

    renderCall(args, theme) {
      const staged = args.staged ? "staged" : "unstaged";
      return new Text(
        theme.fg("toolTitle", theme.bold("git overview ")) +
          theme.fg("muted", staged),
        0,
        0,
      );
    },

    renderResult(result, _opts, theme) {
      const details = result.details as any;
      if (!details?.files) {
        const t = result.content[0];
        return new Text(t?.type === "text" ? t.text : "", 0, 0);
      }
      return new Text(
        theme.fg("success", `${details.files.length} files`) +
          theme.fg("muted", details.excludedLockFiles?.length ? ` (${details.excludedLockFiles.length} lock files excluded)` : ""),
        0,
        0,
      );
    },
  });

  // ── git_file_diff tool ───────────────────────────────────────────────────

  pi.registerTool({
    name: "git_file_diff",
    label: "Git File Diff",
    description:
      "Return the diff for specific files. Use after git_overview to inspect changes in detail. Diffs are priority-sorted (source code first, docs/config last) and truncated for large files.",
    parameters: Type.Object({
      files: Type.Array(Type.String({ description: "File paths to diff" }), {
        minItems: 1,
        maxItems: 10,
      }),
      staged: Type.Optional(
        Type.Boolean({ description: "Show staged changes (default: false)" }),
      ),
    }),

    async execute(_toolCallId, params, signal) {
      const staged = params.staged ?? false;
      const flag = staged ? "--cached" : "";
      const parts: string[] = [];
      const truncated: string[] = [];

      for (const file of params.files) {
        const diff = git(
          ["diff", flag, "--", file].filter(Boolean),
        );
        if (!diff) continue;

        const lines = diff.split("\n");
        if (lines.length > 200) {
          const head = lines.slice(0, 100);
          const tail = lines.slice(-50);
          parts.push(
            `=== ${file} ===\n${head.join("\n")}\n\n... (${lines.length - 150} lines truncated) ...\n\n${tail.join("\n")}`,
          );
          truncated.push(file);
        } else {
          parts.push(`=== ${file} ===\n${diff}`);
        }
      }

      const text = parts.join("\n\n") || "(no diff)";
      return {
        content: [{ type: "text", text }],
        details: {
          files: params.files,
          staged,
          truncatedFiles: truncated.length > 0 ? truncated : undefined,
        },
      };
    },

    renderCall(args, theme) {
      const files = (args.files as string[]) || [];
      return new Text(
        theme.fg("toolTitle", theme.bold("git diff ")) +
          theme.fg("muted", files.join(", ")),
        0,
        0,
      );
    },
  });

  // ── git_hunk tool ────────────────────────────────────────────────────────

  pi.registerTool({
    name: "git_hunk",
    label: "Git Hunk",
    description:
      "Return individual hunks from a file diff, with 1-based indices. Use this to inspect specific hunks before staging them with git_stage_hunks. If no hunk indices are specified, returns all hunks with their indices.",
    parameters: Type.Object({
      file: Type.String({ description: "File path" }),
      hunks: Type.Optional(
        Type.Array(Type.Number({ description: "1-based hunk indices to return" }), {
          minItems: 1,
        }),
      ),
      staged: Type.Optional(
        Type.Boolean({ description: "Show staged changes (default: false)" }),
      ),
    }),

    async execute(_toolCallId, params, signal) {
      const staged = params.staged ?? false;
      const flag = staged ? "--cached" : "";
      const diff = git(
        ["diff", flag, "--", params.file].filter(Boolean),
      );

      if (!diff.trim()) {
        return {
          content: [{ type: "text", text: `No diff for ${params.file}` }],
          details: { file: params.file, staged, hunks: [] },
        };
      }

      const fileDiffs = parseFileDiffs(diff);
      const fileDiff = fileDiffs.find((d) => d.filename === params.file);
      if (!fileDiff) {
        return {
          content: [{ type: "text", text: `No diff found for ${params.file}` }],
          details: { file: params.file, staged, hunks: [] },
        };
      }

      if (fileDiff.isBinary) {
        return {
          content: [{ type: "text", text: "Binary file — no hunks available." }],
          details: { file: params.file, staged, hunks: [] },
        };
      }

      const fileHunks = parseFileHunks(fileDiff);
      let selected = fileHunks.hunks;

      if (params.hunks && params.hunks.length > 0) {
        const wanted = new Set(params.hunks.map((n) => Math.max(1, Math.floor(n))));
        selected = fileHunks.hunks.filter((h) => wanted.has(h.index + 1));
      }

      // Build output with clear hunk labels
      const text = selected.length
        ? selected
            .map(
              (h) =>
                `── Hunk ${h.index + 1} (lines ${h.newStart}-${h.newStart + h.newLines - 1}) ──\n${h.content}`,
            )
            .join("\n\n")
        : "(no matching hunks)";

      return {
        content: [{ type: "text", text }],
        details: {
          file: params.file,
          staged,
          totalHunks: fileHunks.hunks.length,
          returnedHunks: selected.map((h) => ({
            index: h.index + 1,
            lines: `${h.newStart}-${h.newStart + h.newLines - 1}`,
          })),
        },
      };
    },

    renderCall(args, theme) {
      const file = (args.file as string) || "";
      const hunks = (args.hunks as number[]) || [];
      let text = theme.fg("toolTitle", theme.bold("git hunk "));
      text += theme.fg("muted", file);
      if (hunks.length > 0) {
        text += theme.fg("dim", ` [${hunks.join(", ")}]`);
      }
      return new Text(text, 0, 0);
    },

    renderResult(result, _opts, theme) {
      const details = result.details as any;
      if (!details) {
        const t = result.content[0];
        return new Text(t?.type === "text" ? t.text : "", 0, 0);
      }
      return new Text(
        theme.fg("success", `${details.returnedHunks?.length ?? 0}`) +
          theme.fg("muted", ` of ${details.totalHunks ?? "?"} hunks`),
        0,
        0,
      );
    },
  });

  // ── git_stage_hunks tool ─────────────────────────────────────────────────

  pi.registerTool({
    name: "git_stage_hunks",
    label: "Stage Hunks",
    description: `Stage specific hunks from files without staging entire files. Use after git_hunk to stage only the changes you want to commit.

Each file entry specifies which hunks to stage:
- "all": stage the entire file diff
- "indices": stage specific hunks by their 1-based index (from git_hunk output)
- "lines": stage hunks overlapping a line range in the new file`,
    parameters: Type.Object({
      selections: Type.Array(
        Type.Object({
          path: Type.String({ description: "File path" }),
          hunks: Type.Union([
            Type.Object({ type: Type.Literal("all") }),
            Type.Object({
              type: Type.Literal("indices"),
              indices: Type.Array(Type.Number(), { minItems: 1 }),
            }),
            Type.Object({
              type: Type.Literal("lines"),
              start: Type.Number(),
              end: Type.Number(),
            }),
          ]),
        }),
        { minItems: 1 },
      ),
    }),

    async execute(_toolCallId, params, signal) {
      // Get the unstaged diff
      const diff = git(["diff"]);
      const fileDiffs = parseFileDiffs(diff);
      const fileDiffMap = new Map(fileDiffs.map((d) => [d.filename, d]));

      const patchParts: string[] = [];
      const staged: string[] = [];
      const errors: string[] = [];

      for (const sel of params.selections) {
        const fileDiff = fileDiffMap.get(sel.path);
        if (!fileDiff) {
          errors.push(`No unstaged diff for ${sel.path}`);
          continue;
        }

        if (fileDiff.isBinary) {
          if (sel.hunks.type !== "all") {
            errors.push(`Cannot select hunks for binary file ${sel.path}`);
            continue;
          }
          patchParts.push(fileDiff.content);
          staged.push(sel.path);
          continue;
        }

        if (sel.hunks.type === "all") {
          patchParts.push(fileDiff.content);
          staged.push(`${sel.path} (all)`);
          continue;
        }

        const fileHunks = parseFileHunks(fileDiff);
        let selected: DiffHunk[];

        if (sel.hunks.type === "indices") {
          const wanted = new Set(
            sel.hunks.indices.map((n) => Math.max(1, Math.floor(n))),
          );
          selected = fileHunks.hunks.filter((h) => wanted.has(h.index + 1));
        } else {
          // lines
          const start = Math.floor(sel.hunks.start);
          const end = Math.floor(sel.hunks.end);
          selected = fileHunks.hunks.filter(
            (h) =>
              h.newStart <= end && h.newStart + h.newLines - 1 >= start,
          );
        }

        if (selected.length === 0) {
          errors.push(`No matching hunks for ${sel.path}`);
          continue;
        }

        const header = extractFileHeader(fileDiff.content);
        const filePatch = [header, ...selected.map((h) => h.content)].join(
          "\n",
        );
        patchParts.push(filePatch);
        staged.push(
          `${sel.path} (${selected.length}/${fileHunks.hunks.length} hunks)`,
        );
      }

      if (patchParts.length === 0) {
        return {
          content: [
            {
              type: "text",
              text: `Nothing to stage. Errors: ${errors.join("; ") || "none"}`,
            },
          ],
          details: { staged: [], errors },
        };
      }

      // Write patch and apply
      const patch =
        patchParts
          .map((p) => (p.endsWith("\n") ? p : `${p}\n`))
          .join("\n")
          .trimEnd() + "\n";

      const tmpFile = path.join(
        os.tmpdir(),
        `pi-hunks-${Date.now()}.patch`,
      );
      try {
        fs.writeFileSync(tmpFile, patch);
        const result = await pi.exec(
          "git",
          ["apply", "--cached", "--binary", tmpFile],
          { signal },
        );
        if (result.code !== 0) {
          return {
            content: [
              {
                type: "text",
                text: `git apply failed: ${result.stderr}`,
              },
            ],
            details: { staged: [], errors: [result.stderr] },
          };
        }
      } finally {
        try {
          fs.unlinkSync(tmpFile);
        } catch {}
      }

      const text = `Staged:\n${staged.map((s) => `  ✓ ${s}`).join("\n")}${errors.length > 0 ? `\n\nErrors:\n${errors.map((e) => `  ✗ ${e}`).join("\n")}` : ""}`;
      return {
        content: [{ type: "text", text }],
        details: { staged, errors },
      };
    },

    renderCall(args, theme) {
      const sels = (args.selections as any[]) || [];
      const files = sels.map((s) => s.path).join(", ");
      return new Text(
        theme.fg("toolTitle", theme.bold("stage hunks ")) +
          theme.fg("muted", files),
        0,
        0,
      );
    },

    renderResult(result, _opts, theme) {
      const details = result.details as any;
      if (!details) {
        const t = result.content[0];
        return new Text(t?.type === "text" ? t.text : "", 0, 0);
      }
      const ok = (details.staged as string[])?.length ?? 0;
      const err = (details.errors as string[])?.length ?? 0;
      let text = theme.fg("success", `✓ ${ok} staged`);
      if (err > 0) text += theme.fg("error", ` ✗ ${err} errors`);
      return new Text(text, 0, 0);
    },
  });

  // ── git_commit_with_user_approval tool ───────────────────────────────────

  pi.registerTool({
    name: "git_commit_with_user_approval",
    label: "Git Commit (with approval)",
    description: `Create a git commit with user review and approval. Use this tool when the user should confirm and potentially edit the commit message before committing. For automated commits where no user confirmation is needed, use the regular git commit command via bash instead.

${COMMIT_FORMAT_GUIDE}`,
    parameters: Type.Object({
      message: Type.String({
        description:
          "Proposed commit message (subject line, optionally followed by blank line and body)",
      }),
      files: Type.Optional(
        Type.Array(Type.String(), {
          description:
            "Files to stage before committing. If empty or omitted, commits whatever is currently staged.",
        }),
      ),
    }),

    async execute(_toolCallId, params, signal, _onUpdate, ctx) {
      if (!ctx.hasUI) {
        return {
          content: [
            {
              type: "text",
              text: "Error: UI not available (running in non-interactive mode)",
            },
          ],
          details: { committed: false, reason: "no-ui" },
        };
      }

      // Stage files if provided
      if (params.files && params.files.length > 0) {
        const stageResult = await pi.exec(
          "git",
          ["add", "--", ...params.files],
          { signal },
        );
        if (stageResult.code !== 0) {
          return {
            content: [
              { type: "text", text: `Error staging files: ${stageResult.stderr}` },
            ],
            details: {
              committed: false,
              reason: "stage-failed",
              error: stageResult.stderr,
            },
          };
        }
      }

      // Check if there's anything to commit
      const statusResult = await pi.exec(
        "git",
        ["diff", "--cached", "--quiet"],
        { signal },
      );
      if (statusResult.code === 0) {
        return {
          content: [{ type: "text", text: "Nothing staged to commit" }],
          details: { committed: false, reason: "nothing-staged" },
        };
      }

      // Show what will be committed
      const diffStatResult = await pi.exec(
        "git",
        ["diff", "--cached", "--stat"],
        { signal },
      );
      const stagedInfo = diffStatResult.stdout.trim();

      // Let user edit the commit message
      const editorPrompt = `Staged changes:\n${stagedInfo}\n\n───────────────────────────────────────\nEdit commit message (save to commit, cancel to abort):`;
      const finalMessage = await ctx.ui.editor(editorPrompt, params.message);

      if (finalMessage === undefined || finalMessage.trim() === "") {
        return {
          content: [{ type: "text", text: "Commit cancelled by user" }],
          details: { committed: false, reason: "user-cancelled" },
        };
      }

      // Execute the commit
      const commitResult = await pi.exec(
        "git",
        ["commit", "-m", finalMessage.trim()],
        { signal },
      );

      if (commitResult.code !== 0) {
        return {
          content: [
            { type: "text", text: `Commit failed: ${commitResult.stderr}` },
          ],
          details: {
            committed: false,
            reason: "commit-failed",
            error: commitResult.stderr,
          },
        };
      }

      // Get the commit hash
      const hashResult = await pi.exec(
        "git",
        ["rev-parse", "--short", "HEAD"],
        { signal },
      );
      const commitHash = hashResult.stdout.trim();

      return {
        content: [
          {
            type: "text",
            text: `Committed ${commitHash}: ${finalMessage.trim().split("\n")[0]}`,
          },
        ],
        details: {
          committed: true,
          hash: commitHash,
          message: finalMessage.trim(),
          files: params.files || [],
        },
      };
    },

    renderCall(args, theme) {
      const message = (args.message as string) || "";
      const subject = message.split("\n")[0];
      const files = (args.files as string[]) || [];

      let text = theme.fg("toolTitle", theme.bold("git commit "));
      text += theme.fg("muted", `"${subject}"`);
      if (files.length > 0) {
        text += theme.fg(
          "dim",
          ` (${files.length} file${files.length !== 1 ? "s" : ""})`,
        );
      }
      return new Text(text, 0, 0);
    },

    renderResult(result, _options, theme) {
      const details = result.details as any;
      if (!details) {
        const t = result.content[0];
        return new Text(t?.type === "text" ? t.text : "", 0, 0);
      }

      if (!details.committed) {
        const reason = details.reason || "unknown";
        if (reason === "user-cancelled")
          return new Text(theme.fg("warning", "Cancelled"), 0, 0);
        if (reason === "nothing-staged")
          return new Text(theme.fg("warning", "Nothing to commit"), 0, 0);
        return new Text(
          theme.fg("error", `Failed: ${details.error || reason}`),
          0,
          0,
        );
      }

      const subject = (details.message || "").split("\n")[0];
      return new Text(
        theme.fg("success", "✓ ") +
          theme.fg("accent", details.hash || "") +
          theme.fg("muted", ` ${subject}`),
        0,
        0,
      );
    },
  });
}
