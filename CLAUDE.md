# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This is a personal Nix dotfiles repository: a `flake-parts` flake that builds NixOS, nix-darwin, and standalone home-manager configurations from a shared, layered set of modules. There is no application code to compile or test — the unit of work is a Nix evaluation that either succeeds or fails.

## Hosts (flake outputs)

Each host is one entry point in `flake.nix`:

| Output | Platform | Entry point |
|---|---|---|
| `nixosConfigurations.globalhawk` | x86_64-linux (NixOS) | `machine/globalhawk/` |
| `darwinConfigurations.nighthawk` | aarch64-darwin (nix-darwin) | `machine/nighthawk/` |
| `homeConfigurations."awhite@4ZTHR73"` | x86_64-linux WSL (standalone HM) | `machine/campbell/home.nix` |

## Commands

Run from the repo root so `--flake .` resolves.

```sh
# Darwin (nighthawk) — preferred; the flake's darwin-rebuild wrapper auto-adds --flake .
nix run .#darwin-rebuild -- switch
# NixOS (globalhawk)
sudo nixos-rebuild switch --flake .#globalhawk
# Standalone home-manager (WSL / campbell)
home-manager switch --flake .#"awhite@4ZTHR73"
```

Swap `switch` for `build` to validate without activating. `./installer.sh switch|build` also works (it auto-detects the host), but the commands above are preferred.

When validating a change, prefer `build` over `switch` — it catches evaluation errors without mutating the live system. Activating (`switch`) is a hard-to-reverse, outward-facing action; do not run it unless asked.

Validation and formatting:

```sh
nix flake check          # evaluate all outputs
nix fmt                  # format all .nix with alejandra (the flake formatter)
```

## Architecture

Configs are assembled by composition, layered from general to specific. A host imports roles, roles import programs, and module plumbing supplies options and overlays underneath.

- **`machine/<host>/`** — per-host entry points. `default.nix` is the system (NixOS/darwin) layer; `home.nix` is the home-manager layer. These mostly just import roles and set host facts (hostname, `meta.user`, `stateVersion`).
- **`role/`** — reusable bundles imported by machines: `darwin.nix`, `nixos.nix` (system roles), `general.nix`, `dev.nix`, `devgui.nix` (HM roles). A role wires together a coherent set of programs + packages.
- **`program/<name>/`** — one home-manager module per tool (git, fish, vim, ai-agents, …). Convention: `default.nix` holds the concrete config (`programs.foo.enable = true` plus settings); when a program defines **custom options**, those live in a sibling `module.nix` that `default.nix` imports (see `program/git`, `program/ai-agents`). Static config payloads sit in a `files/` subdir.
- **`modules/`** — the module-system plumbing, split by evaluation layer:
  - `common/` — system layer shared by NixOS + darwin. `meta.nix` defines `meta.user` / `meta.isWSL`; `overlays.nix` adds the `pkgs.unstable` overlay and the `mdadf` package.
  - `common-hm/` — home-manager analogue of `meta.nix`.
  - `hm/` — home-manager modules (`defaults.nix`, `docker-rootless`, `ai-agents`).
  - `darwin/`, `windows/` — platform-specific system / WSL modules.
- **`lib/`** — `lib.mine`, a custom `lib` extension (types, darwin helpers, option builders). It is the **one** thing passed via `specialArgs` rather than a module option, because the module system can't rebind `lib` before evaluation. HM contexts get `lib.mine` plus `home-manager.lib` merged in.
- **`packages/`** — custom package definitions and per-platform package overlays. `packages/xmonad` is a git submodule (`abes-xmonad`).
- **`themes/`** — theming (gruvbox).
- **`secrets/`** — host secrets (emails, keys) imported by configs (e.g. `program/git`, `role/general` read `secrets/common.nix`).

### Key conventions established by the flake-parts refactor

- **No `specialArgs` threading for host facts.** Old `myUserName` / `isWSL` flags are now the `meta.user` / `meta.isWSL` module options (`modules/common/meta.nix`). `isDarwin` / `isNixOS` come from `pkgs.stdenv.hostPlatform`. Only `inputs` and the extended `lib` go through `specialArgs`.
- **Unstable packages** are `pkgs.unstable.<name>`, supplied by the overlay in `modules/common/overlays.nix`. Standalone HM can't set `nixpkgs.overlays` as a module option, so `flake.nix` applies the same overlay inline at the `pkgs`-construction site for the WSL config — keep those two in sync.
- The design intent behind these patterns is documented in `docs/superpowers/specs/`. Read the relevant spec before reworking the flake entry point or the ai-agents module.

### The `programs.ai-agents` module

`modules/hm/ai-agents/module.nix` is a custom wrapper that configures coding harnesses (claude-code, codex, pi) around a shared `~/.agents/` tree. The source of truth lives in `program/ai-agents/agents/`: a single `AGENTS.md` plus context docs. `@ctx/<rel>` references in `AGENTS.md` are rewritten to the absolute deployed path under `~/.agents/context/`, and the result is fed to each enabled agent. Skills (`programs.ai-agents.skills`) are realised once in the store and symlinked into both `~/.agents/skills/` (read by codex/pi) and `~/.claude/skills/` (read by claude). It defers to upstream `programs.claude-code` / `programs.codex` where they exist; pi is hand-rolled.

## Secrets (git-crypt)

Some files (proprietary t2 MacBook firmware, `secrets/`) are encrypted with git-crypt. To unlock a fresh checkout:

```sh
nix run .#decrypt-secrets      # gpg --decrypt local.key.asc | git-crypt unlock -
```

## Gotchas

- On macOS/zsh, `darwin-rebuild` can fail due to `NIX_PATH` being clobbered by `/etc/zshrc`. The README documents the fix.
- This environment uses **GNU sed** (via nix). Use `sed -i`, not BSD's `sed -i ''`. Prefer the Edit tool over sed regardless.
