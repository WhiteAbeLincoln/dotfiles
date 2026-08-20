# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This is a personal Nix dotfiles repository: a `flake-parts` flake that builds NixOS and nix-darwin configurations from explicitly selected, cross-environment aspects. The shared constructors also support standalone Home Manager inventory entries, although none are currently active. There is no application code to compile or test — the unit of work is a Nix evaluation that either succeeds or fails.

## Hosts (flake outputs)

Each active host is declared in `modules/flake/inventory.nix`, which selects its
literal aspect and machine-module paths:

| Output | Platform | Deployment module |
|---|---|---|
| `nixosConfigurations.globalhawk` | x86_64-linux (NixOS) | `machine/globalhawk/` |
| `nixosConfigurations.valkyrie` | x86_64-linux (NixOS) | `machine/valkyrie/` |
| `darwinConfigurations.nighthawk` | aarch64-darwin (nix-darwin) | `machine/nighthawk/` |

## Commands

Run from the repo root so `--flake .` resolves.

```sh
# Darwin (nighthawk) — preferred; the flake's darwin-rebuild wrapper auto-adds --flake .
nix run .#darwin-rebuild -- switch
# NixOS (globalhawk)
sudo nixos-rebuild switch --flake .#globalhawk
```

Swap `switch` for `build` to validate without activating. `./installer.sh
switch|build` also works (it auto-detects the host), but the commands above are
preferred for activation.

When validating a change, prefer `build` over `switch` — it catches evaluation errors without mutating the live system. Activating (`switch`) is a hard-to-reverse, outward-facing action; do not run it unless asked.

Build every active configuration without activation with:

```sh
nix build --no-link .#darwinConfigurations.nighthawk.system
nix build --no-link .#nixosConfigurations.valkyrie.config.system.build.toplevel
nix build --no-link .#nixosConfigurations.globalhawk.config.system.build.toplevel
```

Validation and formatting:

```sh
nix flake check          # evaluate all outputs
nix fmt -- .             # format all .nix with alejandra (the flake formatter)
```

### Testing configuration changes

This repository has no CI or habitual test runner, so do not add standalone
tests merely because they are possible. A check that nobody reliably runs is a
maintenance obligation, not dependable protection.

Use the smallest validation layer that exercises meaningful behaviour:

1. Treat a full configuration build as the required baseline. For globalhawk,
   use `nixos-rebuild build --flake .#globalhawk`. This evaluates the complete
   host and builds referenced nixidy manifests, charts, and packages. A later
   `switch` necessarily builds the configuration, so this validation cannot be
   silently skipped.
2. Add Nix module assertions only for genuine safety invariants or invalid
   option combinations that the type system cannot express. Good candidates
   include missing runtime-secret declarations, unsafe network exposure, and
   internally inconsistent retention/storage settings.
3. For Kubernetes workloads, prefer native runtime contracts—startup,
   readiness, and liveness probes plus monitoring of target health—over scripts
   that inspect rendered YAML structure.
4. Put a short, one-time live acceptance checklist in an implementation plan
   when deployment behaviour must be verified. Run it against the real host or
   cluster after activation; do not automatically turn it into a permanent Bash
   harness.
5. Reserve flake checks for reusable module logic with observable behaviour,
   such as module composition or validation rules. Do not add a flake check that
   merely restates the expected shape of one host's rendered configuration.

Do not introduce NixOS VM tests, disposable Kubernetes clusters, YAML-shape
Bash scripts, fake `kubectl` tests, or similar infrastructure unless their
specific risk reduction clearly justifies their execution and maintenance
cost. These are especially poor fits when they model globalhawk's k3s, ZFS, or
hardware environment less faithfully than a normal system build plus a live
acceptance check.

## Architecture

Configs are assembled from the central machine inventory. The inventory selects
shared and host-specific aspects, and the constructors project each resolved
aspect into its NixOS, nix-darwin, and Home Manager module classes.

- **`aspect/`** — explicitly selected, opinionated concerns. A leaf aspect may
  contribute `nixos`, `darwin`, and/or `homeManager` projections; directory
  aspects keep their outer projection in `default.nix` and concern-specific
  implementation or option modules alongside it. Composition-only profiles
  such as `common-cli.nix` and `development.nix` combine leaves through literal
  imports. Adding a file alone never activates it.
- **`machine/<host>/`** — hardware and genuinely host-specific deployment
  configuration. These modules do not own hostname, primary user, platform, or
  state versions; `modules/flake/inventory.nix` owns those facts.
- **`modules/`** — reusable infrastructure and composition plumbing:
  - `flake/` declares the inventory and aspect schemas, selects literal aspect
    and machine-module paths, resolves projections, constructs all outputs, and
    owns constructor checks.
  - `common/` supplies target-system metadata and overlays shared by NixOS and
    nix-darwin.
  - `common-hm/` supplies Home Manager metadata and defaults for embedded,
    standalone, and special-user Home Manager evaluations.
  - `nixos/` contains reusable option-bearing NixOS infrastructure, including
    the AI-agent sandbox and k3s workload/secret modules.
  - `darwin/` contains reusable nix-darwin infrastructure for system defaults.
- **`packages/`** — custom package definitions and per-platform package overlays. `packages/xmonad` is a git submodule (`abes-xmonad`).
- **`themes/`** — theming (gruvbox).
- **`secrets/`** — host secrets (emails, keys) imported by configs (e.g.
  `aspect/git/home.nix` and host-specific machine modules).

### Key conventions established by the flake-parts refactor

- **Inventory facts are authoritative.** `modules/flake/inventory.nix` owns each
  host's platform, hostname, primary user, and state versions. Constructors
  translate them into normal target options such as `meta.user` and
  `networking.hostName`; machine and aspect modules read those options or
  `pkgs.stdenv.hostPlatform`.
- **Only `inputs` passes through general `specialArgs`.** Host facts are ordinary
  module options, and each target uses its module system's native `lib`. Scoped
  APIs such as a workload's `extraSpecialArgs.nixosConfig` remain separate.
- **Unstable packages** are `pkgs.unstable.<name>`. The overlay list is defined
  once in `modules/common/overlay-list.nix`; system constructors install it via
  `modules/common/overlays.nix`, and the standalone Home Manager constructor
  applies the same list while constructing `pkgs`.
- The design intent behind these patterns is documented in `docs/superpowers/specs/`. Read the relevant spec before reworking the flake entry point or the ai-agents module.

### The `programs.ai-agents` module

`aspect/ai-agents/module.nix` defines the Home Manager options used by the
AI-agent aspect to configure coding harnesses (claude-code, codex, pi) around a
shared `~/.agents/` tree. The source of truth lives in
`aspect/ai-agents/agents/`: a single `AGENTS.md` plus context docs. `@ctx/<rel>`
references in `AGENTS.md` are rewritten to the absolute deployed path under
`~/.agents/context/`, and the result is fed to each enabled agent. Skills
(`programs.ai-agents.skills`) are realised once in the store and symlinked into
both `~/.agents/skills/` (read by codex/pi) and `~/.claude/skills/` (read by
claude). It defers to upstream `programs.claude-code` / `programs.codex` where
they exist; pi is hand-rolled. The Globalhawk sandbox imports the raw
`aspect/ai-agents/home.nix` projection explicitly for its operator and special
user rather than inheriting primary-user aspects automatically.

## Secrets (git-crypt)

Some files (proprietary t2 MacBook firmware, `secrets/`) are encrypted with git-crypt. To unlock a fresh checkout:

```sh
nix run .#decrypt-secrets      # gpg --decrypt local.key.asc | git-crypt unlock -
```

**This repo is public; only `secrets/` and the t2 firmware are encrypted.** Never write a value that lives in `secrets/*.nix` (bucket names, S3 endpoints/regions, API key IDs, passwords, keys, emails) into any unencrypted committed file — including `docs/`, specs, comments, and `.nix` files outside `secrets/`. Reference the Nix attribute *path* (e.g. `secrets.restic.b2.repo`) or a generic description instead of the literal value.

## Gotchas

- On macOS/zsh, `darwin-rebuild` can fail due to `NIX_PATH` being clobbered by `/etc/zshrc`. The README documents the fix.
- This environment uses **GNU sed** (via nix). Use `sed -i`, not BSD's `sed -i ''`. Prefer the Edit tool over sed regardless.
