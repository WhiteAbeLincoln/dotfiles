# `programs.ai-agents` home-manager module — design

Date: 2026-06-02

## Goal

A single home-manager module (`modules/hm/ai-agents/module.nix`) that installs and
configures a set of coding harnesses (claude-code, codex, pi-coding-agent), and
manages a shared `~/.agents/` tree (context docs + skills) that every harness reads.
It wraps the upstream `programs.claude-code` and `programs.codex` home-manager modules
where they exist, and hand-rolls pi-coding-agent (which has no upstream module).

## Why a wrapper instead of using the upstream modules directly

The upstream `programs.claude-code` / `programs.codex` modules already write the agent
context file and link skills, but each writes into its *own* config dir. We want one
canonical source of truth at `~/.agents/` (the location codex and pi read natively), a
single `AGENTS.md` authored once with `@ctx/...` references, and per-agent copies derived
from it. The wrapper centralizes that, while still deferring to the upstream options for
everything they already model (settings schema, package wiring, skill linking).

## Option surface (`programs.ai-agents`)

```nix
programs.ai-agents = {
  # Master switch. Lets the whole block live in a shared config and be turned
  # off per-machine (e.g. `programs.ai-agents.enable = lib.mkForce false;`).
  enable = true;                               # mkEnableOption; default false

  # Directory containing AGENTS.md plus the context docs it references.
  contextDir = ./agents;                       # type: nullOr path; default null

  # One skill per entry, deployed to ~/.agents/skills/<name>.
  # Value is a local path, a fetcher's output (derivation), or a store-path string.
  skills = {                                   # type: attrsOf (oneOf [package path str]); default {}
    foo = ./skills/foo;
    bar = pkgs.fetchgit { url = "https://…"; rev = "…"; hash = "sha256-…"; };
    sub = "${pkgs.fetchgit { url = "…"; rev = "…"; hash = "sha256-…"; }}/skills/sub";
  };

  claude-code = {
    enable   = true;                           # mkEnableOption
    package  = pkgs.unstable.claude-code;      # mkPackageOption pkgs "claude-code" { nullable = true; }
    settings = { … };                          # free-form passthrough to programs.claude-code
  };

  codex = {
    enable   = true;
    package  = pkgs.codex;                     # mkPackageOption pkgs "codex" { nullable = true; }
    settings = { … };                          # free-form passthrough to programs.codex
  };

  pi = {
    enable   = true;
    package  = pkgs.pi-coding-agent;           # mkPackageOption pkgs "pi-coding-agent" { nullable = true; }
    settings = { … };                          # raw JSON -> ~/.pi/agent/settings.json
  };
};
```

### `settings` semantics

- **claude-code / codex**: `settings` is a *free-form passthrough* (`attrsOf anything`)
  merged verbatim into the upstream `programs.claude-code` / `programs.codex` option set,
  with `enable` and `package` stripped (those come from the dedicated `enable`/`package`
  options). So any upstream option is reachable, e.g.
  `settings = { settings = { theme = "dark"; }; enableMcpIntegration = true; mcpServers = {…}; }`.
- **pi**: no upstream module exists, so `settings` is raw JSON
  (`pkgs.formats.json {}` type) written to `~/.pi/agent/settings.json`.

### `package`

Each agent's `package` defaults to `pkgs.<name>` via `mkPackageOption` and is overridable
with `pkgs.unstable.<name>` (the unstable overlay is available in HM configs). Installation
flows through the upstream module's package option for claude/codex, and through
`home.packages` for pi.

## Behavior

The entire `config` is gated by `lib.mkIf cfg.enable`, so a single top-level
`enable = false` (or `lib.mkForce false` on a machine that overrides a shared
`enable = true`) makes the whole module a no-op regardless of the per-agent
enables. Everything below additionally applies only when `cfg.enable` is true.

### Canonical `~/.agents/` tree (built whenever `contextDir`/`skills` are set)

- `~/.agents/context/AGENTS.md` — the rewritten AGENTS.md (see below).
- `~/.agents/context/<rest>` — every entry of `contextDir` *except* `AGENTS.md`,
  symlinked individually (files and subdirectories alike) so the dir can hold unmanaged
  files too.
- `~/.agents/skills/<name>` — one symlink per `skills` entry, pointing at the resolved
  store path (or subpath).

### AGENTS.md reference rewrite

`AGENTS.md` is authored with `@ctx/<rel>` references. Before writing, every occurrence of
the literal `@ctx/` is replaced with `@<home>/.agents/context/` where `<home>` is the
absolute `config.home.homeDirectory` (e.g. `@/Users/abe/.agents/context/`). Absolute form
chosen over `~` so it resolves regardless of whether a given agent expands `~` in
`@`-references; it remains correct per-machine because `homeDirectory` is evaluated per build.

Mechanism: `builtins.replaceStrings ["@ctx/"] ["@${home}/.agents/context/"] (readFile …)`,
producing one rewritten string reused for all outputs.

### Per-agent wiring (store-copy approach)

Each agent gets its own home-manager-managed copy of the rewritten AGENTS.md (byte-identical
across agents). Context/skills are set with `lib.mkDefault` so values supplied through
`settings` can override them.

- **claude** (`mkIf claude-code.enable`):
  `programs.claude-code = mkMerge [ (removeAttrs settings ["enable" "package"]) { enable = true; package; context = mkDefault <rewritten>; } ]`.
  Upstream `context` writes `~/.claude/CLAUDE.md`. Skills are handled separately (see
  below), **not** via `programs.claude-code.skills`. Claude is the only agent that needs
  the skills relinked, because it reads `~/.claude/skills`, not `~/.agents/skills`.
- **codex** (`mkIf codex.enable`): same merge. Upstream `context` writes
  `~/.codex/AGENTS.md`. No skills link — codex reads `~/.agents/skills` natively.
- **pi** (`mkIf pi.enable`): `home.packages += package` (when non-null);
  `home.file."~/.pi/agent/AGENTS.md".source = <rewritten file>`;
  `home.file."~/.pi/agent/settings.json".source = jsonFormat.generate … settings` (when settings non-empty).
  No skills link — pi reads `~/.agents/skills` natively.

### Skills linking

Each skill is realised once in the store and exposed as a *single* (non-recursive)
`home.file` symlink — under `~/.agents/skills/<name>` always, and additionally under
`<claude configDir>/skills/<name>` when claude is enabled. Both links point at the same
store path, so the two locations are byte-identical and home-manager treats them as the
same content. Derivation values are coerced to their store-path string first
(`norm = v: if lib.isDerivation v then "${v}" else v`); local paths and store-path strings
pass through.

We deliberately do **not** use `programs.claude-code.skills`: it links a directory skill
with `recursive = true`, turning every file inside the skill (e.g. a skill that ships its
own `AGENTS.md`/`CLAUDE.md`) into an individually-managed path. Those per-file paths
collide with the single `~/.agents/skills/<name>` directory symlink during activation,
producing "Existing file … is in the way … will be skipped since they are the same"
warnings. A single directory symlink avoids the per-file management entirely.

## Files touched

1. **Create** `modules/hm/ai-agents/module.nix` — the module (options + implementation).
2. **Edit** `modules/hm/default.nix` — add `./ai-agents/module.nix` to `imports`.

No `program/ai-agents/` scaffolding (consumer config left to the user, per request).

## Decisions log

- `settings` = full passthrough to the upstream module (not just the config-file sub-option).
- AGENTS.md/CLAUDE.md per agent = store copies (reuse upstream `context`), not out-of-store
  symlinks to `~/.agents/context/AGENTS.md`.
- Skills linked into the agent dir only for claude; codex and pi read `~/.agents/skills`
  directly. Linking is done with our own single (non-recursive) `home.file` symlinks
  rather than `programs.claude-code.skills`, to avoid recursive per-file management that
  collides with the `~/.agents/skills` directory symlink.
- Git skills: caller passes an explicit fetcher output (`pkgs.fetchgit { url; rev; hash; }`
  etc.) rather than the module sniffing an attrset. Subdir-of-repo skills use store-path
  interpolation (`"${repo}/skills/foo"`).
- Reference rewrite target: absolute `${homeDirectory}/.agents/context/`.

## Edge cases / notes

- Pointing a skill at a subdir of a fetched repo keeps the whole repo in the store; only the
  subdir is exposed. Acceptable; sparse fetch is a later optimization if needed.
- `removeAttrs settings ["enable" "package"]` prevents merge conflicts if a user accidentally
  puts `enable`/`package` inside `settings`.
- Managing claude's `context` will replace any hand-written `~/.claude/CLAUDE.md` with the
  generated one — intended, since AGENTS.md becomes the single source.
- Top-level `enable` gates the whole module; within it, per-agent enables gate each agent,
  and the `~/.agents/` tree is written whenever `contextDir`/`skills` are configured.
```
