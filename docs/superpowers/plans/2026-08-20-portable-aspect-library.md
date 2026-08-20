# Portable Aspect Library Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn `modules/aspect` into a portable flake-parts library that owns host construction while machines and repository policy compose exclusively through aspects.

**Architecture:** The library declares inventory, resolves four aspect projections, injects a normalized read-only host context, and generates mergeable NixOS, nix-darwin, and Home Manager outputs. Repository package policy, Home Manager preferences, Darwin behavior, and machine-native modules move into explicitly selected aspects; `modules/flake` retains only inventory, local outputs, and maintainer checks.

**Tech Stack:** Nix, flake-parts, NixOS modules, nix-darwin modules, Home Manager modules, Alejandra, pytest-based dead-Nix detector

**Spec:** `docs/superpowers/specs/2026-08-20-portable-aspect-library-design.md`

## Global Constraints

- Treat the already-staged `modules/flake` to `modules/aspect` moves as the starting state; do not discard, recreate, or silently unstage them.
- `modules/aspect` must not refer to repository paths outside its own directory after the migration.
- Aspect activation and composition edges remain literal Nix path expressions; do not add string lookup, directory discovery, or synthesized imports.
- Both `dotfiles.sharedAspects` and `dotfiles.hosts.<name>.aspects` remain plural lists.
- Remove `extraSystems`; derive flake-parts `systems` only from declared host platforms.
- Remove inventory `stateVersion`, `modules`, and `homeModules`; native projections own state versions and native imports.
- Rename inventory `primaryUser` to `user`.
- Target modules receive read-only `config.dotfiles.host = { class; system; hostName; user; }`; do not expose selected `aspects` there.
- Remove `meta.user`, `meta.hostName`, and unused `meta.isWSL` rather than retaining compatibility aliases.
- Only `inputs` remains a general target-module special argument. Host facts stay in ordinary module configuration.
- Preserve existing human-written comments, updating moved path references and option names in place.
- Preserve Globalhawk's service, Kubernetes, observability, storage, hardware, and sandbox trees as native leaves behind its machine aspect.
- Do not expose secret values in unencrypted files. Continue referencing secret attribute paths or generic descriptions.
- Run non-activating checks and builds only. Never run `switch` or another activation command.
- Commit messages explain why the change exists rather than listing changed files.

---

### Task 1: Finish the public-library relocation without exporting maintainer checks

**Files:**
- Modify: `flake.nix`
- Modify: `modules/aspect/default.nix`
- Move: `modules/aspect/checks.nix` -> `modules/flake/checks.nix`
- Move: `modules/aspect/tests/home-manager-aspect.nix` -> `modules/flake/tests/home-manager-aspect.nix`
- Modify: `modules/flake/default.nix`
- Modify: `modules/flake/outputs.nix`
- Preserve staged moves: `modules/aspect/aspect-options.nix`, `modules/aspect/lib.nix`, `modules/aspect/schema.nix`

**Interfaces:**
- Consumes: the user's staged relocation of aspect schema and constructor files into `modules/aspect`.
- Produces: public `modules/aspect/default.nix` without repository checks; local `modules/flake/checks.nix` owns `checks.home-manager-host-class`.

- [ ] **Step 1: Reproduce the broken staged import**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
```

Expected: FAIL because `modules/flake/outputs.nix` still imports nonexistent `modules/flake/lib.nix`.

- [ ] **Step 2: Move maintainer checks behind the repository module**

Run:

```sh
mkdir -p modules/flake/tests
git mv modules/aspect/checks.nix modules/flake/checks.nix
git mv modules/aspect/tests/home-manager-aspect.nix modules/flake/tests/home-manager-aspect.nix
```

Set `modules/aspect/default.nix` to:

```nix
{...}: {
  imports = [./schema.nix];
}
```

Set `modules/flake/default.nix` to:

```nix
{...}: {
  imports = [
    ./inventory.nix
    ./outputs.nix
    ./checks.nix
  ];
}
```

Update `modules/flake/checks.nix` to import `../aspect/lib.nix`; keep its fixture at `./tests/home-manager-aspect.nix`.

- [ ] **Step 3: Repair the temporary repository constructor import**

In `modules/flake/outputs.nix`, use:

```nix
constructors = import ../aspect/lib.nix {inherit inputs lib self;};
```

This is an intermediate boundary removed in Task 5.

- [ ] **Step 4: Verify and commit the relocation**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
nix flake check
git diff --check
git add flake.nix modules/aspect modules/flake
git commit -m "Keep library consumers free of maintainer checks"
```

Expected: the focused constructor check and flake check pass; no missing-library error remains.

---

### Task 2: Make package and Darwin policy explicitly selectable

**Files:**
- Modify: `modules/aspect/aspect-options.nix`
- Modify: `modules/aspect/lib.nix`
- Modify: `modules/flake/checks.nix`
- Modify: `modules/flake/tests/home-manager-aspect.nix`
- Create: `aspect/shared.nix`
- Create: `aspect/nixpkgs/default.nix`
- Create: `aspect/home-manager.nix`
- Create: `aspect/darwin-system.nix`
- Move: `aspect/darwin-desktop.nix` -> `aspect/darwin-desktop/default.nix`
- Move: `aspect/darwin-desktop-module.nix` -> `aspect/darwin-desktop/module.nix`
- Move: `modules/darwin/system-defaults/defaults-writer.nix` -> `aspect/darwin-desktop/defaults-writer.nix`
- Modify: `modules/common/default.nix`
- Delete: `modules/common/overlay-list.nix`
- Delete: `modules/common/overlays.nix`
- Delete: `modules/darwin/default.nix`
- Modify: `modules/flake/inventory.nix`

**Interfaces:**
- Consumes: `resolveAspects :: [DeferredModule] -> ResolvedAspect`.
- Produces: `ResolvedAspect.nixpkgs = { overlays :: [Raw]; config :: AttrSet Raw; }` beside deferred `nixos`, `darwin`, and `homeManager` projections.

- [ ] **Step 1: Write a failing package-projection fixture**

Add this to `modules/flake/tests/home-manager-aspect.nix` before `homeManager`:

```nix
nixpkgs.overlays = [
  (_final: prev: {
    aspectConstructorMarker = prev.writeText "aspect-constructor-marker" "overlay-applied";
  })
];
```

Add `pkgs` to the fixture's Home Manager arguments and add:

```nix
home.file.".aspect-nixpkgs-test".text = builtins.readFile pkgs.aspectConstructorMarker;
```

Add to the focused check:

```nix
assert testHome.config.home.file.".aspect-nixpkgs-test".text == "overlay-applied";
```

- [ ] **Step 2: Verify the undeclared projection fails**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
```

Expected: FAIL because the nested aspect system has no `nixpkgs` option.

- [ ] **Step 3: Declare and apply the package-set projection**

Add to `modules/aspect/aspect-options.nix`:

```nix
nixpkgs = {
  overlays = mkOption {
    type = types.listOf types.raw;
    default = [];
    description = "Nixpkgs overlays contributed by selected aspects.";
  };
  config = mkOption {
    type = types.lazyAttrsOf types.raw;
    default = {};
    description = "Nixpkgs configuration contributed by selected aspects.";
  };
};
```

Add to `modules/aspect/lib.nix`:

```nix
packageModule = resolved: {
  nixpkgs.overlays = resolved.nixpkgs.overlays;
  nixpkgs.config = resolved.nixpkgs.config;
};
```

Include `(packageModule resolved)` in both system constructors. Construct standalone Home Manager packages with:

```nix
pkgs = import inputs.nixpkgs {
  system = host.system;
  overlays = resolved.nixpkgs.overlays;
  config = resolved.nixpkgs.config;
};
```

Remove the constructor's direct import of `modules/common/overlay-list.nix`.

- [ ] **Step 4: Create explicit shared policy aspects**

Create `aspect/nixpkgs/default.nix`:

```nix
{inputs, ...}: {
  nixpkgs = {
    overlays = [
      (final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit (prev.stdenv.hostPlatform) system;
          config.allowUnfree = true;
        };
      })
      (import ../../packages/mdadf/overlay.nix)
      (final: _prev: {
        llm-agents = inputs.llm-agents.packages.${final.stdenv.hostPlatform.system};
      })
    ];
    config.allowUnfree = true;
  };
}
```

Move each explanatory overlay comment from `modules/common/overlay-list.nix` onto its matching definition.

Create `aspect/home-manager.nix`:

```nix
{lib, ...}: {
  homeManager = {
    programs.home-manager.enable = lib.mkDefault true;
  };
}
```

Create `aspect/shared.nix`:

```nix
{...}: {
  imports = [
    ./nixpkgs
    ./home-manager.nix
    ./common-cli.nix
  ];
}
```

Change `modules/common/default.nix` to import only `./meta.nix`, delete the two obsolete common overlay files, and select `[../../aspect/shared.nix]` as `sharedAspects` in inventory.

- [ ] **Step 5: Move Darwin behavior into explicit aspects**

Run:

```sh
mkdir -p aspect/darwin-desktop
git mv aspect/darwin-desktop.nix aspect/darwin-desktop/default.nix
git mv aspect/darwin-desktop-module.nix aspect/darwin-desktop/module.nix
git mv modules/darwin/system-defaults/defaults-writer.nix aspect/darwin-desktop/defaults-writer.nix
```

Set `aspect/darwin-desktop/default.nix` to:

```nix
{...}: {
  darwin = {
    imports = [
      ./defaults-writer.nix
      ./module.nix
    ];
  };
}
```

Create `aspect/darwin-system.nix` by wrapping the complete existing `modules/darwin/default.nix` configuration in a `darwin` projection. Preserve every existing comment and keep its temporary `config.meta.user` read until Task 4.

Keep `system.primaryUser = config.meta.user` temporarily in `aspect/darwin-desktop/module.nix`; Task 4 transfers that fact mapping to the library default aspect. Remove `../darwin` from `mkDarwin`, delete `modules/darwin`, and add `../../aspect/darwin-system.nix` to Nighthawk's current inventory aspects.

- [ ] **Step 6: Verify policy behavior and commit**

Run:

```sh
nix fmt -- --check modules/aspect aspect modules/common modules/flake
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
nix eval --json .#darwinConfigurations.nighthawk.config.nixpkgs.config.allowUnfree
nix eval --json .#nixosConfigurations.valkyrie.config.nixpkgs.config.allowUnfree
nix build --no-link .#darwinConfigurations.nighthawk.system
nix flake check
git add -A aspect modules/aspect modules/common modules/darwin modules/flake
git commit -m "Make deployment policy an explicit machine choice"
```

Expected: the fixture reads `overlay-applied`; both `allowUnfree` values are `true`; Nighthawk and flake checks pass.

---

### Task 3: Turn machines into aspect composition roots

**Files:**
- Modify: `modules/aspect/schema.nix`
- Modify: `modules/aspect/lib.nix`
- Modify: `modules/flake/inventory.nix`
- Modify: `modules/flake/checks.nix`
- Modify: `modules/flake/tests/home-manager-aspect.nix`
- Move: `machine/globalhawk/default.nix` -> `machine/globalhawk/nixos.nix`
- Create: `machine/globalhawk/default.nix`
- Move: `machine/valkyrie/default.nix` -> `machine/valkyrie/nixos.nix`
- Create: `machine/valkyrie/default.nix`
- Move: `machine/nighthawk/default.nix` -> `machine/nighthawk/darwin.nix`
- Create: `machine/nighthawk/default.nix`

**Interfaces:**
- Consumes: four-projection aspect modules and literal aspect imports.
- Produces: hosts with identity facts plus plural `aspects`; machine roots provide native state versions and imports.

- [ ] **Step 1: Add a failing inventory-shape check**

Add `config` to the top-level arguments of `modules/flake/checks.nix`, then bind:

```nix
hosts = config.dotfiles.hosts;
machineRootsOnly = lib.all (
  host:
    !(host ? stateVersion)
    && !(host ? modules)
    && !(host ? homeModules)
) (lib.attrValues hosts);
```

Add this Linux check:

```nix
checks.machine-aspect-roots = assert machineRootsOnly;
  pkgs.runCommand "machine-aspect-roots" {} "touch $out";
```

- [ ] **Step 2: Verify the old schema fails the new check**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.machine-aspect-roots
```

Expected: FAIL because evaluated hosts still contain `stateVersion`, `modules`, and `homeModules`.

- [ ] **Step 3: Create the machine composition roots**

Run:

```sh
git mv machine/globalhawk/default.nix machine/globalhawk/nixos.nix
git mv machine/valkyrie/default.nix machine/valkyrie/nixos.nix
git mv machine/nighthawk/default.nix machine/nighthawk/darwin.nix
```

Create `machine/globalhawk/default.nix`:

```nix
{...}: {
  imports = [
    ../../aspect/shell-utilities.nix
    ../../aspect/tmux
    ../../aspect/zellij
  ];

  nixos = {
    imports = [./nixos.nix];
    system.stateVersion = "23.11";
  };

  homeManager = {
    home.stateVersion = "23.11";
  };
}
```

Create `machine/valkyrie/default.nix`:

```nix
{inputs, ...}: {
  imports = [
    ../../aspect/ai-agents
    ../../aspect/plasma-desktop.nix
  ];

  nixos = {
    imports = [
      inputs.determinate.nixosModules.default
      ./nixos.nix
    ];
    system.stateVersion = "26.05";
  };

  homeManager = {
    imports = [./home.nix];
    home.stateVersion = "26.05";
  };
}
```

Create `machine/nighthawk/default.nix`:

```nix
{inputs, ...}: {
  imports = [
    ../../aspect/darwin-system.nix
    ../../aspect/darwin-desktop
    ../../aspect/shell-utilities.nix
    ../../aspect/development.nix
    ../../aspect/userscripts
  ];

  darwin = {
    imports = [
      inputs.determinate.darwinModules.default
      ./darwin.nix
    ];
    system.stateVersion = 5;
  };

  homeManager = {
    imports = [./home.nix];
    home.stateVersion = "24.05";
  };
}
```

- [ ] **Step 4: Remove native escape hatches from schema and constructors**

Delete `stateVersion`, `modules`, and `homeModules` from the host submodule in `modules/aspect/schema.nix`.

In `modules/aspect/lib.nix`, remove state-version checks and fact assignments, remove `host.modules` and `host.homeModules`, and set the primary Home Manager imports to:

```nix
imports = [../common-hm resolved.homeManager];
```

Put `home.stateVersion = "26.05"` in the fixture's `homeManager` projection. Replace the check host's old `homeModules` override with a second aspect:

```nix
aspects = [
  ./tests/home-manager-aspect.nix
  {
    homeManager = {
      programs.aspect-constructor.enable = false;
    };
  }
];
```

- [ ] **Step 5: Reduce inventory hosts to facts and machine roots**

Keep each current `class`, `system`, and `primaryUser`. Replace all other host selection fields with:

```nix
globalhawk.aspects = [../../machine/globalhawk];
valkyrie.aspects = [../../machine/valkyrie];
nighthawk.aspects = [../../machine/nighthawk];
```

Leave `extraSystems` until Task 5 transfers `systems` ownership.

- [ ] **Step 6: Verify native state and commit machine composition**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.machine-aspect-roots
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
nix eval --raw .#nixosConfigurations.globalhawk.config.system.stateVersion
nix eval --raw .#nixosConfigurations.valkyrie.config.system.stateVersion
nix eval --json .#darwinConfigurations.nighthawk.config.system.stateVersion
nix eval --raw .#darwinConfigurations.nighthawk.config.home-manager.users.abe.home.stateVersion
nix build --no-link .#darwinConfigurations.nighthawk.system
nix flake check
git add machine modules/aspect modules/flake
git commit -m "Keep machine history with the deployment that owns it"
```

Expected: focused checks pass; state versions are `23.11`, `26.05`, `5`, and `24.05`; Nighthawk builds.

---

### Task 4: Replace repository metadata with a portable host context

**Files:**
- Modify: `modules/aspect/schema.nix`
- Rename and modify: `modules/aspect/lib.nix` -> `modules/aspect/constructors.nix`
- Create: `modules/aspect/target/host-context.nix`
- Create: `modules/aspect/default-aspects/host-facts.nix`
- Modify: `modules/flake/outputs.nix`
- Modify: `modules/flake/checks.nix`
- Modify: `modules/flake/inventory.nix`
- Modify: `modules/flake/tests/home-manager-aspect.nix`
- Modify: `aspect/fish/default.nix`
- Modify: `aspect/darwin-system.nix`
- Modify: `aspect/darwin-desktop/module.nix`
- Modify: `machine/globalhawk/nixos.nix`
- Modify: `machine/globalhawk/k3s/default.nix`
- Modify: `machine/valkyrie/nixos.nix`
- Modify: `machine/nighthawk/darwin.nix`
- Modify: `modules/nixos/ai-agent-sandbox.nix`
- Delete: `modules/common/default.nix`
- Delete: `modules/common/meta.nix`
- Delete: `modules/common-hm/default.nix`
- Delete: `modules/common-hm/defaults.nix`

**Interfaces:**
- Consumes: inventory host facts and resolved projections.
- Produces: `config.dotfiles.host :: { class :: Enum; system :: String; hostName :: String; user :: String; }`, provider options, and `dotfiles.defaultAspects.enable :: Bool`.

- [ ] **Step 1: Make the fixture require normalized host context**

Add `config` to the fixture's Home Manager arguments and add:

```nix
home.file.".aspect-host-context".text = builtins.toJSON config.dotfiles.host;
```

Add to the focused check:

```nix
assert testHome.config.dotfiles.host == {
  class = "homeManager";
  system = "x86_64-linux";
  hostName = "aspect-test";
  user = "tester";
};
```

- [ ] **Step 2: Verify target context is absent**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
```

Expected: FAIL because the target Home Manager system has no `dotfiles.host` option.

- [ ] **Step 3: Rename the inventory user and add provider/default controls**

In `modules/aspect/schema.nix`, accept `inputs` as a module argument, rename `primaryUser` to `user`, add `dotfiles.defaultAspects.enable` as a Boolean defaulting to `true`, and add:

```nix
providers = {
  nixpkgs = mkOption {
    type = types.nullOr types.raw;
    default = inputs.nixpkgs or null;
    description = "Nixpkgs flake used by configuration constructors.";
  };
  homeManager = mkOption {
    type = types.nullOr types.raw;
    default = inputs.home-manager or null;
    description = "Home Manager flake used by configuration constructors.";
  };
  darwin = mkOption {
    type = types.nullOr types.raw;
    default = inputs.darwin or null;
    description = "Optional nix-darwin flake used by Darwin constructors.";
  };
};
```

Rename `primaryUser` to `user` in inventory and synthetic hosts.

- [ ] **Step 4: Declare target context and the default host-facts aspect**

Create `modules/aspect/target/host-context.nix`:

```nix
{lib, ...}: {
  options.dotfiles.host = lib.mkOption {
    readOnly = true;
    type = lib.types.submodule {
      options = {
        class = lib.mkOption {type = lib.types.enum ["nixos" "darwin" "homeManager"];};
        system = lib.mkOption {type = lib.types.str;};
        hostName = lib.mkOption {type = lib.types.str;};
        user = lib.mkOption {type = lib.types.str;};
      };
    };
    description = "Normalized inventory facts for the configuration being evaluated.";
  };
}
```

Create `modules/aspect/default-aspects/host-facts.nix`:

```nix
{self, ...}: {
  nixos = {config, lib, ...}: {
    nixpkgs.hostPlatform = config.dotfiles.host.system;
    networking.hostName = config.dotfiles.host.hostName;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
  };

  darwin = {config, lib, ...}: {
    nixpkgs.hostPlatform = config.dotfiles.host.system;
    networking.hostName = config.dotfiles.host.hostName;
    system.primaryUser = config.dotfiles.host.user;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
  };

  homeManager = {config, lib, pkgs, ...}: {
    home.username = config.dotfiles.host.user;
    home.homeDirectory = lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin
      then "/Users/${config.dotfiles.host.user}"
      else "/home/${config.dotfiles.host.user}"
    );
  };
}
```

- [ ] **Step 5: Make constructors depend only on their library directory**

Rename `lib.nix` to `constructors.nix`. Use arguments:

```nix
{
  defaultAspectsEnabled,
  inputs,
  lib,
  providers,
  self,
}:
```

Define:

```nix
hostFacts = host: {inherit (host) class system hostName user;};
hostContext = host: {
  imports = [./target/host-context.nix];
  dotfiles.host = hostFacts host;
};

resolveAspects = aspects:
  (lib.evalModules {
    modules =
      [./aspect-options.nix]
      ++ lib.optional defaultAspectsEnabled ./default-aspects/host-facts.nix
      ++ aspects;
    specialArgs = {inherit inputs self;};
  }).config;
```

Use configured providers for all constructors. Before construction, throw `dotfiles.hosts.<name>: nixpkgs provider is required`, `dotfiles.hosts.<name>: homeManager provider is required`, or `dotfiles.hosts.<name>: darwin provider is required for class darwin` for the corresponding null provider. Include `(hostContext host)` in every system and primary-user Home Manager module list. Remove all imports outside `modules/aspect`; continue passing only `{inherit inputs;}` as target special arguments.

- [ ] **Step 6: Prove default aspects can be disabled while context remains**

Import a second constructor set in `modules/flake/checks.nix` with `defaultAspectsEnabled = false`. Construct `testHomeWithoutDefaults` with an aspect containing:

```nix
homeManager = {config, ...}: {
  home.username = "custom-user";
  home.homeDirectory = "/tmp/custom-home";
  home.stateVersion = "26.05";
  home.file.".aspect-no-defaults".text = config.dotfiles.host.user;
};
```

Assert:

```nix
assert testHomeWithoutDefaults.config.dotfiles.host.user == "tester";
assert testHomeWithoutDefaults.config.home.username == "custom-user";
assert testHomeWithoutDefaults.config.home.file.".aspect-no-defaults".text == "tester";
```

Successful evaluation proves the authoritative default username mapping was not loaded.

- [ ] **Step 7: Migrate every metadata consumer and special Home Manager user**

Replace active `config.meta.user` with `config.dotfiles.host.user` in every file listed for this task, including comments, descriptions, and `defaultText`.

Remove `system.primaryUser` from `aspect/darwin-desktop/module.nix`; the bundled host-facts aspect now supplies it authoritatively.

Replace the sandbox user's deleted common-HM import with:

```nix
home-manager.users.${cfg.user} = {
  imports = cfg.sharedModules;
  home.username = cfg.user;
  home.homeDirectory = lib.mkDefault "/home/${cfg.user}";
  home.stateVersion = mkDefault config.system.stateVersion;
  programs.home-manager.enable = mkDefault true;
};
```

Delete `modules/common` and `modules/common-hm` after no consumers remain.

- [ ] **Step 8: Point temporary repository wiring at pure constructors**

In `modules/flake/outputs.nix`, import:

```nix
constructors = import ../aspect/constructors.nix {
  inherit inputs lib self;
  inherit (config.dotfiles) providers;
  defaultAspectsEnabled = config.dotfiles.defaultAspects.enable;
};
```

Change all construction and output naming from `primaryUser` to `user`.

- [ ] **Step 9: Verify host context and commit**

Run:

```sh
nix fmt -- --check modules/aspect modules/flake aspect machine modules/nixos
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
nix eval --json .#darwinConfigurations.nighthawk.config.dotfiles.host
nix eval --json .#nixosConfigurations.valkyrie.config.dotfiles.host
nix eval --raw .#darwinConfigurations.nighthawk.config.home-manager.users.abe.home.username
nix eval --raw .#nixosConfigurations.globalhawk.config.services.aiAgentSandbox.operator
rg -n 'meta\.(user|hostName|isWSL)|modules/(common-hm|common|darwin)' --glob '*.nix' .
nix build --no-link .#darwinConfigurations.nighthawk.system
nix flake check
git add aspect machine modules
git commit -m "Let target modules inspect host identity without repository coupling"
```

Expected: checks pass; host records contain only class/system/hostName/user; obsolete-reference search is empty; Nighthawk builds.

---

### Task 5: Let the portable library generate flake outputs

**Files:**
- Modify: `modules/aspect/default.nix`
- Create: `modules/aspect/outputs.nix`
- Modify: `modules/aspect/schema.nix`
- Modify: `modules/flake/outputs.nix`
- Modify: `modules/flake/checks.nix`
- Modify: `modules/flake/inventory.nix`
- Modify: `modules/flake/default.nix`

**Interfaces:**
- Consumes: `constructors.mkConfiguration :: String -> Host -> NativeConfiguration` and `config.dotfiles.hosts`.
- Produces: mergeable `flake.nixosConfigurations`, `flake.darwinConfigurations`, and `flake.homeConfigurations`; flake-parts `systems` derived only from hosts.

- [ ] **Step 1: Add a failing nested-consumer output check**

In `modules/flake/checks.nix`, construct:

```nix
portableFlake = inputs.flake-parts.lib.mkFlake {inherit inputs;} {
  imports = [../aspect];

  dotfiles = {
    sharedAspects = [];
    hosts.portable-test = {
      class = "homeManager";
      system = "x86_64-linux";
      hostName = "portable-test";
      user = "tester";
      aspects = [./tests/home-manager-aspect.nix];
    };
  };

  flake.homeConfigurations.unrelated = "sentinel";
};
```

Add:

```nix
checks.portable-aspect-library =
  assert portableFlake.homeConfigurations.unrelated == "sentinel";
  assert portableFlake.homeConfigurations."tester@portable-test".config.dotfiles.host.hostName == "portable-test";
    pkgs.runCommand "portable-aspect-library" {} "touch $out";
```

- [ ] **Step 2: Verify public output wiring is absent**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.portable-aspect-library
```

Expected: FAIL because importing `modules/aspect` does not generate `homeConfigurations` yet.

- [ ] **Step 3: Declare mergeable outputs and generate configurations**

Create `modules/aspect/outputs.nix`:

```nix
{
  config,
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ./constructors.nix {
    inherit inputs lib self;
    inherit (config.dotfiles) providers;
    defaultAspectsEnabled = config.dotfiles.defaultAspects.enable;
  };
  hosts = lib.mapAttrs (_: host:
    host
    // {aspects = config.dotfiles.sharedAspects ++ host.aspects;})
  config.dotfiles.hosts;
  byClass = class: lib.filterAttrs (_: host: host.class == class) hosts;
in {
  options.flake = {
    nixosConfigurations = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = {};
    };
    darwinConfigurations = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = {};
    };
    homeConfigurations = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = {};
    };
  };

  config = {
    systems = lib.unique (map (host: host.system) (lib.attrValues hosts));
    flake = {
      nixosConfigurations = lib.mapAttrs constructors.mkConfiguration (byClass "nixos");
      darwinConfigurations = lib.mapAttrs constructors.mkConfiguration (byClass "darwin");
      homeConfigurations = lib.mapAttrs' (
        name: host:
          lib.nameValuePair
          "${host.user}@${host.hostName}"
          (constructors.mkConfiguration name host)
      ) (byClass "homeManager");
    };
  };
}
```

Set `modules/aspect/default.nix` to:

```nix
{...}: {
  imports = [
    ./schema.nix
    ./outputs.nix
  ];
}
```

- [ ] **Step 4: Remove repository-owned wiring and `extraSystems`**

Delete `extraSystems` from schema and inventory. Remove constructor imports, host merging, class filtering, `systems`, and the entire `flake` block from `modules/flake/outputs.nix`. Retain its formatter, local packages, and Kubernetes checks unchanged.

Keep `modules/flake/default.nix` importing local inventory, outputs, and checks. Keep root `flake.nix` importing both `./modules/aspect` and `./modules/flake`.

- [ ] **Step 5: Verify drop-in construction and commit**

Run:

```sh
nix build --no-link .#checks.x86_64-linux.portable-aspect-library
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
nix flake show --json | jq '{nixos: (.nixosConfigurations | keys), darwin: (.darwinConfigurations | keys), home: ((.homeConfigurations // {}) | keys), packages: (.packages | keys)}'
nix build --no-link .#darwinConfigurations.nighthawk.system
nix flake check
rg -n 'extraSystems|constructors\.mkConfiguration|byClass' modules/flake modules/aspect
git add modules/aspect modules/flake flake.nix
git commit -m "Make inventory sufficient to construct deployment outputs"
```

Expected: reusable checks pass; current output names remain; package systems contain only inventory platforms; constructor/class routing occurs only under `modules/aspect`; `extraSystems` has no live references.

---

### Task 6: Align contributor guidance and perform full non-activating verification

**Files:**
- Modify: `AGENTS.md`
- Modify: `README.md`

**Interfaces:**
- Consumes: final portable library, policy aspects, machine roots, and local checks.
- Produces: current contributor guidance and evidence for integration handoff.

- [ ] **Step 1: Update architecture documentation**

Update `AGENTS.md` to state all of the following explicitly:

- `modules/aspect` is the portable flake-parts construction library;
- `modules/flake` contains inventory, local outputs, and maintainer checks;
- `aspect` contains explicitly selected repository policy;
- each `machine/<host>/default.nix` is an aspect composition root;
- target modules read `config.dotfiles.host`;
- native state versions live in machine projections;
- overlays/configuration come from the `nixpkgs` aspect projection;
- `modules/common`, `modules/common-hm`, and `modules/darwin` no longer exist.

Update `README.md` dead-Nix notes to describe literal machine aspect roots and their imported aspects, not separate inventory module lists. Keep the computed-path limitation unchanged.

- [ ] **Step 2: Scan for stale live references and library coupling**

Run:

```sh
rg -n 'meta\.(user|hostName|isWSL)|primaryUser|extraSystems|homeModules|modules/(common-hm|common|darwin|flake/lib)' --glob '*.nix' --glob '*.md' .
rg -n '\.\./(common|common-hm|darwin|flake)|\.\./\.\./(aspect|machine|packages)' modules/aspect --glob '*.nix'
```

Expected: the first search has no active-code or current-guidance matches; the superseded August 19 spec may retain historical descriptions. The second search is empty because the portable library refers only to its own files and configured providers.

- [ ] **Step 3: Run formatting, detector, and reusable checks**

Run:

```sh
nix fmt -- --check .
git diff --check
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run --with pytest pytest misc/test_find_dead_nix.py -v
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run misc/find_dead_nix.py
nix build --no-link .#checks.x86_64-linux.home-manager-host-class
nix build --no-link .#checks.x86_64-linux.machine-aspect-roots
nix build --no-link .#checks.x86_64-linux.portable-aspect-library
nix flake check
```

Expected: formatting and whitespace pass; 8 detector tests pass; detector reports 0 dead Nix files; all focused checks and `nix flake check` pass.

- [ ] **Step 4: Verify facts, native state, and shared package behavior**

Run:

```sh
nix eval --json .#nixosConfigurations.globalhawk.config.dotfiles.host
nix eval --json .#nixosConfigurations.valkyrie.config.dotfiles.host
nix eval --json .#darwinConfigurations.nighthawk.config.dotfiles.host
nix eval --raw .#nixosConfigurations.globalhawk.config.system.stateVersion
nix eval --raw .#nixosConfigurations.valkyrie.config.system.stateVersion
nix eval --json .#darwinConfigurations.nighthawk.config.system.stateVersion
nix eval --raw .#darwinConfigurations.nighthawk.config.home-manager.users.abe.home.stateVersion
nix eval --json .#nixosConfigurations.globalhawk.config.home-manager.users.abe.programs.bat.enable
nix eval --json .#nixosConfigurations.valkyrie.config.home-manager.users.abe.programs.bat.enable
nix eval --json .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.bat.enable
```

Expected: host records contain correct class/system/hostName/user and no aspect values; state versions remain `23.11`, `26.05`, `5`, and `24.05`; Bat remains enabled for all primary users.

- [ ] **Step 5: Build every active host without activation**

Run:

```sh
nix build --no-link .#darwinConfigurations.nighthawk.system
nix build --no-link .#nixosConfigurations.valkyrie.config.system.build.toplevel
nix build --no-link .#nixosConfigurations.globalhawk.config.system.build.toplevel
```

Expected: Nighthawk builds. The pre-refactor failing initrd derivations were `/nix/store/s2nr6kbmda17bph1g9zipiri6zw5ra5g-initrd-linux-7.0.10.drv` for Valkyrie and `/nix/store/igfmrk6282f3hh5ph5inngbkfagxlgvc-initrd-linux-6.12.91.drv` for Globalhawk. If a Linux build encounters the same missing-ncurses store failure at its exact baseline derivation, record it without repairing the store or activating. Any new evaluation, option, assertion, or different derivation failure is a migration regression.

- [ ] **Step 6: Review and commit guidance**

Run:

```sh
git diff --stat 8c3f627
git diff --check 8c3f627
git status --short --branch
git add AGENTS.md README.md
git commit -m "Keep future configuration changes inside portable boundaries"
```

Expected: only intended changes are present and the branch is clean after the commit. Do not stage secret-bearing files or ignored execution reports.
