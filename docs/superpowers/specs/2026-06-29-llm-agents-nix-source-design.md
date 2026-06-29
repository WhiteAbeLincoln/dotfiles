# Source coding agents from `llm-agents.nix` — design

**Status:** design, awaiting review
**Date:** 2026-06-29

## Goal

Stop sourcing the coding harnesses (claude-code, codex, pi) from nixpkgs and
source them from [`numtide/llm-agents.nix`](https://github.com/numtide/llm-agents.nix)
instead. nixpkgs lags badly for these fast-moving tools; llm-agents.nix
repackages them and is updated daily, with prebuilt binaries on
`cache.numtide.com`.

This is a package-source swap only. The `programs.ai-agents` module already
takes a `package` per agent, so no module code changes.

## Scope

`program/ai-agents` is imported by **`machine/nighthawk/home.nix` only** —
neither globalhawk nor campbell pull it in. The change is therefore effectively
nighthawk-scoped:

- The overlay is added in `modules/common/overlays.nix` (shared by globalhawk +
  nighthawk system configs). It is lazy — globalhawk gains the `pkgs.llm-agents`
  attribute but never forces it, so it costs nothing there.
- The binary cache is configured on nighthawk only, since nighthawk is the only
  host that builds these packages.
- **campbell is untouched.** It does not import `program/ai-agents` and never
  references `pkgs.llm-agents`, so its inline overlay list in `flake.nix` does
  not need the llm-agents overlay. (This is the one place the otherwise-required
  "keep `modules/common/overlays.nix` and the campbell inline overlays in sync"
  rule does not apply — campbell does not consume this overlay.)

## What llm-agents.nix provides

- Per-system `packages.${system}.<name>` and two overlays:
  - `overlays.default` — injects `pkgs.llm-agents.<name>`, built against
    llm-agents' **own pinned unstable nixpkgs**. Store paths match what
    `cache.numtide.com` serves, so the cache hits.
  - `overlays.shared-nixpkgs` — builds against the consumer's `final` nixpkgs.
    Shares dependencies but misses the cache (revisions won't match), so these
    node/bytecode tools build from source.
- All three agents we use are present: `claude-code`, `codex`, `pi`.
- Supports `x86_64-linux`, `aarch64-linux`, `x86_64-darwin`, `aarch64-darwin`.
- Binary cache: `https://cache.numtide.com`, public key
  `niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g=`.

We use `overlays.default`: these are prebuilt npm/bytecode wrappers, and the
whole point of the switch is freshness via prebuilt binaries.

## Changes

### 1. `flake.nix` — add the input

```nix
llm-agents.url = "github:numtide/llm-agents.nix";
```

Deliberately **no** `inputs.nixpkgs.follows`. `overlays.default` ships packages
prebuilt against llm-agents' own pinned nixpkgs; making it follow our nixpkgs
would rebuild everything from source and miss `cache.numtide.com`. The cost is
one extra nixpkgs entry in `flake.lock` — the intended price for cache hits.

Add `llm-agents` to the `outputs = inputs @ { ... }` destructure if other inputs
are pulled out there, or just reach it via `inputs.llm-agents` (it is already in
`specialArgs`/`extraSpecialArgs` as part of `inputs`).

### 2. `modules/common/overlays.nix` — apply the overlay

Append `inputs.llm-agents.overlays.default` to the `nixpkgs.overlays` list,
alongside the existing `unstable` overlay and `mdadf`. The module already
receives `inputs` in its arguments.

### 3. `program/ai-agents/default.nix` — swap package sources

```nix
claude-code.package = pkgs.llm-agents.claude-code;   # was pkgs.unstable.claude-code
codex.package       = pkgs.llm-agents.codex;          # was pkgs.codex
pi.package          = pkgs.llm-agents.pi;             # was pkgs.pi-coding-agent
```

### 4. `modules/darwin/default.nix` — trust the binary cache (nighthawk)

Add to the existing `determinateNix.customSettings` block (which already sets
`extra-trusted-users`). Match the list syntax used by the commented-out
substituter examples already in that file:

```nix
extra-substituters = ["https://cache.numtide.com"];
extra-trusted-public-keys = [
  "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
];
```

(If `determinateNix.customSettings` rejects lists, fall back to a single
space-joined string — confirm against the option type during implementation.)

No cache config on globalhawk: it does not build these packages.

## Non-goals

- No changes to `programs.ai-agents` module code (`modules/hm/ai-agents/`).
- No campbell changes (machine not in use; does not import ai-agents).
- Not removing the `unstable` overlay — it is used elsewhere; we just stop
  referencing `pkgs.unstable.claude-code`.

## Notes / caveats

- **Updating cadence:** freshness now comes from `nix flake update llm-agents`
  (their repo auto-updates daily), replacing the "wait for nixpkgs" path.
- **First nighthawk switch:** the new system closure is built with the *current*
  nix.conf, before activation rewrites it — so the switch that introduces the
  cache may build the agents from source once. Subsequent builds hit the cache.
  Avoidable by passing `--option extra-substituters https://cache.numtide.com
  --option extra-trusted-public-keys niks3.numtide.com-1:...` on that one build.
- **If campbell ever adopts ai-agents:** add `inputs.llm-agents.overlays.default`
  to its inline `overlays = [...]` list in `flake.nix`, and arrange cache access
  on that machine separately (standalone HM cannot manage system nix.conf).

## Verification

```sh
nix flake check                      # eval all outputs
nix run .#darwin-rebuild -- build    # validate nighthawk closure, no activation
```

Confirm `pkgs.llm-agents.{claude-code,codex,pi}` resolve to llm-agents store
paths (not nixpkgs) via `nix eval`.
