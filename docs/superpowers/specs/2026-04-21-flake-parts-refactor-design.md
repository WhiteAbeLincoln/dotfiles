# Flake-Parts Refactor: Replace `make-cfg.nix` With Modules

**Status:** design, awaiting review
**Date:** 2026-04-21

## Goal

Delete the hand-rolled `make-cfg.nix` factory. Replace it with `flake-parts`
for outputs wiring and use the NixOS module system (not `specialArgs` threading)
to inject per-host context. Existing layout (`machine/`, `program/`, `role/`,
`modules/`, `packages/`, `themes/`, `lib/`) stays as-is — this refactor is
scoped to the flake entry point and the context-injection mechanism.

## Motivation

`make-cfg.nix` currently does two jobs:

1. Wire up flake outputs (`nixosConfigurations`, `darwinConfigurations`,
   `homeConfigurations`, `packages`, `formatter`) via a custom function and
   `flake-utils.lib.eachDefaultSystem`.
2. Build a `specialArgs` bundle (`pkgs-unstable`, `myUserName`, `isHM`,
   `isWSL`, `isDarwin`, `isNixOS`, extended `lib`) that gets threaded into
   every module.

Both of those have idiomatic replacements:

- **flake-parts** replaces job 1. `perSystem` gives us per-system outputs
  without `flake-utils`; `flake = { ... }` hosts the non-per-system outputs.
- **Module options + overlays** replace most of job 2. `pkgs-unstable`
  becomes `pkgs.unstable` via an overlay. `myUserName`, `isWSL` become
  `config.meta.user` / `config.meta.isWSL` options. `isDarwin` / `isNixOS`
  are already available via `pkgs.stdenv.hostPlatform` and don't need
  custom flags. `isHM` is always true in its current callers — it's dead
  code.

The one thing the module system can't do is extend `lib` before modules
evaluate, so `lib.mine` stays as a single `specialArgs` carve-out.

## Non-Goals

- No change to `program/<name>/`, `role/`, `modules/<name>/`, `machine/<name>/`
  directory layouts.
- No conversion of `program/<name>` into toggleable option modules.
- No adoption of sops-nix, easy-hosts, or other community flake-parts
  modules.
- No change to the secrets story (`git-crypt` + `local.key.asc` remains).
- No change to the set of hosts or their module contents (only the call-site).

## Design

### New `flake.nix`

```nix
{
  description = "Nix Dotfiles Flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.2511.*";
    nixpkgs-unstable.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";
    home-manager = {
      url = "https://flakehub.com/f/nix-community/home-manager/0.2511.*";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "https://flakehub.com/f/nix-darwin/nix-darwin/0.2511.*";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3.8.6";
    git-different = {
      url = "github:WhiteAbeLincoln/git-different";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    codex.url = "github:openai/codex/rust-v0.118.0";
    # flake-utils removed
  };

  outputs = inputs @ { self, flake-parts, nixpkgs, home-manager, darwin, determinate, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { pkgs, system, ... }: {
        formatter = pkgs.alejandra;
        packages = {
          decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
            ${pkgs.gnupg}/bin/gpg --decrypt ${./local.key.asc} \
              | ${pkgs.git-crypt}/bin/git-crypt unlock -
          '';
        } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
          darwin-rebuild = pkgs.writeShellScriptBin "darwin-rebuild" ''
            exec sudo ${inputs.darwin.packages.${system}.darwin-rebuild}/bin/darwin-rebuild --flake . "$@"
          '';
        };
      };

      flake = let
        mkLib = nixpkgs.lib.extend (self: super: {
          mine = import ./lib { lib = self; };
        });
        hmLib = nixpkgs.lib.extend (self: super:
          { mine = import ./lib { lib = self; }; } // home-manager.lib);
      in {
        nixosConfigurations.globalhawk = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; lib = mkLib; };
          modules = [
            ./modules/common
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; lib = hmLib; };
              home-manager.users.abe = {
                imports = [ ./modules/hm ./machine/globalhawk/home.nix ];
              };
              meta.user = "abe";
            }
            ./machine/globalhawk
          ];
        };

        darwinConfigurations.nighthawk = darwin.lib.darwinSystem {
          specialArgs = { inherit inputs; lib = mkLib; };
          modules = [
            ./modules/common
            ./modules/darwin
            determinate.darwinModules.default
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; lib = hmLib; };
              home-manager.users.abe = {
                imports = [ ./modules/hm ./machine/nighthawk/home.nix ];
              };
              meta.user = "abe";
            }
          ];
        };

        homeConfigurations."awhite@4ZTHR73" =
          home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              overlays = [
                (final: prev: {
                  unstable = import inputs.nixpkgs-unstable {
                    inherit (prev.stdenv.hostPlatform) system;
                    config.allowUnfree = true;
                  };
                })
              ];
            };
            extraSpecialArgs = { inherit inputs; lib = hmLib; };
            modules = [
              ./modules/common-hm
              ./modules/hm
              ./modules/windows
              ./machine/campbell/home.nix
              { meta.user = "awhite"; meta.isWSL = true; }
            ];
          };
      };
    };
}
```

### New modules

**`modules/common/default.nix`** — imported by every NixOS and Darwin config.
Applies the `unstable` overlay and declares `meta` options.

```nix
{ ... }: {
  imports = [
    ./overlays.nix
    ./meta.nix
  ];
}
```

**`modules/common/overlays.nix`** — exposes `pkgs.unstable`.

```nix
{ inputs, ... }: {
  nixpkgs.overlays = [
    (final: prev: {
      unstable = import inputs.nixpkgs-unstable {
        inherit (prev.stdenv.hostPlatform) system;
        config.allowUnfree = true;
      };
    })
  ];
  nixpkgs.config.allowUnfree = true;
}
```

**`modules/common/meta.nix`** — host-level metadata options.

```nix
{ lib, ... }: {
  options.meta = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "Primary user of this host.";
    };
    isWSL = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether this host runs inside WSL.";
    };
  };
}
```

**`modules/common-hm/default.nix`** — the standalone-home-manager analogue.
Declares only the `meta` options. The `unstable` overlay cannot be set via
a module here because standalone home-manager (`homeManagerConfiguration`)
doesn't accept `nixpkgs.overlays` — overlays must be applied when
constructing `pkgs`. The home-config's call site in `flake.nix` does that
directly (see below).

```nix
{ lib, ... }: {
  options.meta = {
    user  = lib.mkOption { type = lib.types.str; };
    isWSL = lib.mkOption { type = lib.types.bool; default = false; };
  };
}
```

In the home config block of `flake.nix`, replace the plain
`pkgs = import nixpkgs { ... }` with:

```nix
pkgs = import nixpkgs {
  system = "x86_64-linux";
  config.allowUnfree = true;
  overlays = [
    (final: prev: {
      unstable = import inputs.nixpkgs-unstable {
        inherit (prev.stdenv.hostPlatform) system;
        config.allowUnfree = true;
      };
    })
  ];
};
```

**`modules/hm/defaults.nix`** — HM-layer defaults that `baseHmModule`
currently sets in `make-cfg.nix`. Imported by `modules/hm/default.nix` so
every HM user picks it up automatically.

```nix
{ config, lib, pkgs, ... }: {
  programs.home-manager.enable = true;
  home.username = lib.mkDefault config.meta.user;
  home.homeDirectory = lib.mkDefault (
    if pkgs.stdenv.hostPlatform.isDarwin
    then "/Users/${config.meta.user}"
    else "/home/${config.meta.user}"
  );
}
```

Note: `home.stateVersion` is already set by each `machine/*/home.nix` and
stays there — it varies per machine (`24.05` on nighthawk, `23.11` on
globalhawk and campbell).

### Downstream migrations

Each of these is a find-and-replace in one or two lines per file:

| File | Change |
| --- | --- |
| `program/fish/default.nix` | Drop `pkgs-unstable`, `isHM`, `myUserName` from signature. Replace `pkgs-unstable.fish` with `pkgs.unstable.fish`. Delete the `lib.optionalAttrs isHM { ... }` guard, keep its body. |
| `program/direnv/default.nix` | Drop `pkgs-unstable`, `isHM` from signature. Replace `pkgs-unstable.direnv` with `pkgs.unstable.direnv`. Delete the `lib.optionalAttrs isHM { ... }` guard, keep its body. |
| `program/plex/default.nix` | Drop `pkgs-unstable` from signature. Replace `pkgs-unstable.plex` with `pkgs.unstable.plex`. |
| `machine/campbell/home.nix` | Drop `pkgs-unstable` from signature. Replace `pkgs-unstable.rbw` with `pkgs.unstable.rbw`. |
| `modules/hm/docker-rootless/module.nix` | Drop `isHM` from signature. Change `lib.mkIf (isHM && cfg.enable)` to `lib.mkIf cfg.enable`. |
| `modules/darwin/default.nix` | Replace `{ myUserName, ... }` with `{ config, ... }` and reference `config.meta.user` where `myUserName` was used. |
| `machine/nighthawk/default.nix` | Same: switch to `config.meta.user`. |
| `machine/globalhawk/default.nix` | Same. |
| `role/darwin.nix` | Same. |
| `modules/windows/winfiles/default.nix` | Replace `isWSL` function arg with `config.meta.isWSL` read inside. |

### Deletions

- `make-cfg.nix`
- `inputs.flake-utils` entry in `flake.nix` (and `flake.lock` entry on `nix flake lock`)

### Kept unchanged

- `program/`, `role/`, `modules/hm`, `modules/darwin`, `modules/windows`,
  `machine/`, `packages/`, `themes/`, `de/`, `user/`, `lib/`
- Every current call to `lib.mine.types.file` (via the `specialArgs`
  carve-out that still extends `lib`)
- `simple-darwin.nix`, `installer.sh`, `misc.sh`, `terminfo.src`,
  `tmux-256color.info`, `local.key.asc`, `secrets/`
- Every `machine/<name>/{default,home}.nix` module body

## Verification

The refactor is complete when all three of the following produce the same
output store paths as before (modulo `flake.lock` churn):

```bash
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
nix build .#darwinConfigurations.nighthawk.system
nix build .#homeConfigurations."awhite@4ZTHR73".activationPackage
```

Additionally:

- `nix flake check` passes.
- `nix run .#decrypt-secrets -- --help` resolves (on any system).
- `nix run .#darwin-rebuild -- --help` resolves (on Darwin).

## Risks and Open Questions

1. **`_module.args.lib` collision.** Passing `lib = mkLib` via
   `specialArgs` shadows NixOS's own `lib`. Current code already does this
   in `make-cfg.nix`; the new code preserves the pattern. No new risk.
2. **`nixpkgs.overlays` inside standalone home-manager.** Home-manager
   configs built via `homeManagerConfiguration` don't have a
   `nixpkgs.overlays` option — overlays must be applied when constructing
   `pkgs`. Resolved above: `modules/common-hm` declares only `meta`
   options; the home-config's `pkgs = import nixpkgs { ... }` applies the
   overlay inline.
3. **`isHM` is not entirely dead.** The `modules/hm/docker-rootless/module.nix`
   guard is dead because the module only loads in HM context. The
   `program/fish`/`program/direnv` guards gate blocks that call
   `home.sessionVariables` / HM-only options; those modules also only
   run in HM context today, so the guards are redundant. Confirmed by
   inspection of `make-cfg.nix:72-78` (`hmSystemModules` is imported via
   `baseHmModule`, which only runs at the HM layer). No behavior change
   expected from removing the guards.
4. **`specialArgs.lib` threading to HM.** `home-manager.extraSpecialArgs`
   is set per system config to `hmLib` (nixpkgs lib + `mine` + home-manager
   lib merged). `baseHmModule`'s current body (`home.stateVersion`,
   `home.username`, `home.homeDirectory`, `programs.home-manager.enable`)
   is preserved via the new `modules/hm/defaults.nix` described above:
   every `machine/*/home.nix` already sets `home.stateVersion`, and
   `defaults.nix` sets the other three from `config.meta.user` +
   `pkgs.stdenv.hostPlatform.isDarwin`.

## Migration Order

Refactor is done in one branch, landed as a single PR (the hosts must
migrate together because they share `flake.nix`). Suggested commit
sequence:

1. Add `modules/common/`, `modules/common-hm/`, and `modules/hm/defaults.nix`
   as new files; wire `defaults.nix` into `modules/hm/default.nix`.
2. Rewrite `flake.nix` to use `flake-parts`; delete `make-cfg.nix`,
   `flake-utils` input.
3. Find-replace downstream modules per the table above.
4. Run the three `nix build` verifications; fix drift.

---

Implementation plan follows once this spec is approved.
