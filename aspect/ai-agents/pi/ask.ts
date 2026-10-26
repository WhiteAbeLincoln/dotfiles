import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

// Custom/MCP tools require explicit opt-in because their effects are unknown.
const READ_ONLY_TOOLS = new Set(["read", "grep", "find", "ls"]);
const FORBIDDEN_TOOLS = new Set(["bash", "powershell", "edit", "write"]);
const QUESTION_TYPE = "dotfiles-ask";

export default function askExtension(pi: ExtensionAPI): void {
  let asking = false;
  let allowedTools = new Set(READ_ONLY_TOOLS);

  function configuredTools(): Set<string> {
    // Pi preserves extension-specific keys and merges trusted project settings.
    const { ask } = pi.getSettings() as { ask?: unknown };
    if (ask === undefined) return new Set(READ_ONLY_TOOLS);
    if (ask === null || typeof ask !== "object" || Array.isArray(ask)) {
      throw new Error("ask must be an object in Pi's settings.json.");
    }
    const { additionalTools } = ask as { additionalTools?: unknown };
    if (additionalTools === undefined) return new Set(READ_ONLY_TOOLS);
    if (!Array.isArray(additionalTools) || additionalTools.some(
      (name) => typeof name !== "string" || !name.trim() || name !== name.trim(),
    )) {
      throw new Error("ask.additionalTools must be an array of exact, non-empty tool names.");
    }
    const forbidden = additionalTools.filter((name) => FORBIDDEN_TOOLS.has(name));
    if (forbidden.length > 0) {
      throw new Error(`ask.additionalTools cannot enable shell or file-write tools: ${forbidden.join(", ")}`);
    }
    return new Set([...READ_ONLY_TOOLS, ...additionalTools]);
  }

  function finishAsk(ctx: ExtensionContext): void {
    asking = false;
    allowedTools = new Set(READ_ONLY_TOOLS);
    if (ctx.hasUI) ctx.ui.setStatus("ask", undefined);
  }

  pi.registerCommand("ask", {
    description: "Answer a question with file inspection only (no bash or writes)",
    handler: async (args, ctx) => {
      const question = args.trim();
      if (!question) {
        ctx.ui.notify("Usage: /ask <question>", "warning");
        return;
      }
      if (!ctx.isIdle() || asking) {
        ctx.ui.notify("Wait for the current answer to finish, or abort it first.", "warning");
        return;
      }

      try {
        allowedTools = configuredTools();
      } catch (error) {
        ctx.ui.notify(error instanceof Error ? error.message : String(error), "error");
        return;
      }

      const available = new Set(pi.getAllTools().map((tool) => tool.name));
      const unavailable = [...allowedTools].filter((name) => !available.has(name) && !READ_ONLY_TOOLS.has(name));
      if (unavailable.length > 0 && ctx.hasUI) {
        ctx.ui.notify(`/ask: configured tools are not loaded: ${unavailable.join(", ")}`, "warning");
      }
      const instructions =
        "For this /ask run, answer the question rather than implementing changes. " +
        `Permitted tools, if already available: ${[...allowedTools].join(", ")}. ` +
        "Use these tools only for read-only inspection. " +
        "Do not execute shell commands, modify files, or use other tools. " +
        "If inspection is insufficient, explain the limitation. " +
        "These restrictions apply only to this run, not subsequent requests.";

      // Prefix the question instead of changing cached system/tool declarations.
      // Custom messages become user messages on the wire; this trigger marks the
      // run busy synchronously, so waitForIdle also covers preparation failures.
      asking = true;
      try {
        if (ctx.hasUI) ctx.ui.setStatus("ask", "ask: read-only");
        pi.sendMessage(
          { customType: QUESTION_TYPE, content: `${instructions}\n\nQuestion:\n${question}`, display: true },
          { triggerTurn: true },
        );
        await ctx.waitForIdle();
      } finally {
        // Also clean up if preparing the run fails before agent_settled can fire.
        finishAsk(ctx);
      }
    },
  });

  // Enforce the boundary even for stale calls or tools activated by another extension.
  pi.on("tool_call", async (event) => {
    if (asking && !allowedTools.has(event.toolName)) {
      return { block: true, reason: `/ask permits only ${[...allowedTools].join(", ")}; no shell commands or writes.` };
    }
  });

  // agent_end can precede retries and queued continuations; keep those read-only too.
  pi.on("agent_settled", async (_event, ctx) => finishAsk(ctx));
  pi.on("session_shutdown", async (_event, ctx) => finishAsk(ctx));
}
