# Pi `/ask`

`/ask <question>` answers using file inspection rather than implementing changes.
By default its execution guard permits only `read`, `grep`, `find`, and `ls`.
The guard is cleared when the run settles or is aborted. Retries and queued
continuations within the run remain restricted.

To preserve the existing prompt-cache prefix, `/ask` leaves the system prompt
and tool declarations unchanged. It prefixes the question with read-only
instructions in the message sent to the model. The model still sees disallowed
tools, but attempts to execute them are blocked.

## Additional tools

Add exact tool names to Pi's standard `~/.pi/agent/settings.json`:

```json
{
  "ask": {
    "additionalTools": ["web_search", "mcp__docs__search"]
  }
}
```

The names above are examples: the corresponding tools must already be loaded
and available through the session's existing tool configuration. Unknown names
produce a warning; this setting does not activate tools, install extensions, or
connect MCP servers. Even the default inspection tools are not activated by
`/ask` if they were previously inactive. Wildcards are not expanded.

Trusted project `.pi/settings.json` can override this setting. Pi merges nested
objects and replaces arrays, so a project's `additionalTools` list replaces the
user list. Run `/reload` after editing settings; each `/ask` reads Pi's current
effective settings and freezes its allowlist for the run.

In this repository, user settings are Home Manager-managed. Configure the same
setting through `programs.ai-agents.pi.settings.ask.additionalTools` instead of
editing the generated file.

`bash`, `powershell`, `edit`, and `write` cannot be enabled through this setting.
Malformed configuration prevents the answer from starting. Other tools are
explicitly trusted by name: the extension cannot verify that a custom/MCP tool
is actually read-only. Choose inspection tools, not mutating operations. Nested
tool calls that go through Pi's tool dispatcher are checked against the same
allowlist, but an extension can perform side effects directly; this is not an
OS sandbox.

## Try without activation

From the repository root:

```sh
pi --extension ./aspect/ai-agents/pi/ask.ts
```

After the operator activates the Home Manager configuration, Pi discovers the
extension under `~/.pi/agent/extensions/ask.ts`; `/reload` loads it in an existing
session.
