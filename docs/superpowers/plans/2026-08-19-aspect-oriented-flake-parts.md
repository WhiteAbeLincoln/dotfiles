# Aspect-oriented flake-parts Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace repeated host/program/role wiring with a typed, path-selected aspect inventory that constructs NixOS, nix-darwin, and standalone Home Manager outputs.

**Architecture:** A flake-parts module declares a unified `dotfiles.hosts` inventory and evaluates selected aspect paths through a nested module system with `nixos`, `darwin`, and `homeManager` projections. One constructor turns inventory facts into standard target options, applies both the system and primary-user projections by default, and preserves legacy machine modules while reusable configuration moves into `aspect/`.

**Tech Stack:** Nix module system, flake-parts, NixOS, nix-darwin, Home Manager, Python 3.11, and pytest.

**Spec:** `docs/superpowers/specs/2026-08-19-aspect-oriented-flake-parts-design.md`

## Global Constraints

- Do not add Den, import-tree, filesystem discovery, or string-to-path aspect lookup.
- Every local aspect, profile, machine module, and dynamic Nix root remains reachable through a literal path expression.
- Selecting an aspect applies its system projection and its Home Manager projection to the primary user; unsupported projections are empty.
- Aspect-provided conventional enable options use `lib.mkDefault`; inventory facts use normal priority.
- The final general `specialArgs` contains only `inputs`; do not thread user, hostname, system, class, or a custom `lib` through it.
- Preserve globalhawk's service, Kubernetes, observability, storage, hardware, and secrets organization.
- Preserve existing human-written comments, moving and updating them with their configuration.
- Never copy values from `secrets/*.nix` into an unencrypted file.
- Preserve `nixosConfigurations.globalhawk`, `nixosConfigurations.valkyrie`, and `darwinConfigurations.nighthawk`.
- Support future `class = "homeManager"` entries even though no current host uses that class.
- Do not run `switch`; all validation is non-activating.
- Preserve unrelated working-tree changes and stage only the current task's files.

---

### Task 1: Make the dead-Nix detector understand literal dynamic directories

**Files:**
- Modify: `misc/find_dead_nix.py`
- Modify: `misc/test_find_dead_nix.py`

**Interfaces:**
- Consumes: Parsed absolute path nodes returned by `parse_output(file)`.
- Produces: `nix_targets(file: Path, root: Path) -> set[Path]` that follows normal module directories and recursive dynamic Nix roots.

- [ ] **Step 1: Capture pre-migration evaluation and build baselines**

```sh
mkdir -p /tmp/dotfiles-aspect-baseline
nix eval --raw .#darwinConfigurations.nighthawk.config.system.build.toplevel.drvPath > /tmp/dotfiles-aspect-baseline/nighthawk.drv
nix eval --raw .#nixosConfigurations.valkyrie.config.system.build.toplevel.drvPath > /tmp/dotfiles-aspect-baseline/valkyrie.drv
nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath > /tmp/dotfiles-aspect-baseline/globalhawk.drv
nix build --no-link .#darwinConfigurations.nighthawk.system
nix build --no-link .#nixosConfigurations.valkyrie.config.system.build.toplevel
nix build --no-link .#nixosConfigurations.globalhawk.config.system.build.toplevel
```

Expected: all drv paths are recorded and every available target builder succeeds. Record a missing target builder explicitly; never substitute `switch`.

- [ ] **Step 2: Add failing tests for both new reachability forms**

Add to `misc/test_find_dead_nix.py`:

```python
def test_nix_targets_follows_dynamic_nix_directory_without_root_default(tmp_path):
    root = tmp_path
    _write(root / "root.nix", "{ nixidy.chartsDir = ./charts; }")
    _write(root / "charts" / "alloy" / "default.nix", "{ chart = \"alloy\"; }")
    _write(root / "charts" / "loki" / "default.nix", "{ chart = \"loki\"; }")
    (root / "charts" / "README.md").write_text("not nix")

    targets = fdn.nix_targets(root / "root.nix", root)

    assert targets == {
        (root / "charts" / "alloy" / "default.nix").resolve(),
        (root / "charts" / "loki" / "default.nix").resolve(),
    }


def test_nix_targets_follows_nix_path_stored_in_arbitrary_option(tmp_path):
    root = tmp_path
    _write(
        root / "root.nix",
        "{ dotfiles.hosts.test.aspects = [ ./aspect/fish.nix ]; }",
    )
    _write(root / "aspect" / "fish.nix", "{ homeManager = {}; }")

    targets = fdn.nix_targets(root / "root.nix", root)

    assert targets == {(root / "aspect" / "fish.nix").resolve()}
```

- [ ] **Step 3: Run the focused tests and confirm the dynamic-directory case fails**

```sh
cd misc
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run --with pytest pytest \
  test_find_dead_nix.py::test_nix_targets_follows_dynamic_nix_directory_without_root_default \
  test_find_dead_nix.py::test_nix_targets_follows_nix_path_stored_in_arbitrary_option -v
```

Expected: the dynamic-directory test fails because `charts/default.nix` does not exist; the arbitrary-option test already passes and locks in the parser contract.

- [ ] **Step 4: Implement conservative dynamic-directory traversal**

Replace directory handling inside `nix_targets` with:

```python
        p = Path(match)
        if p.is_dir():
            default_module = p / "default.nix"
            if default_module.is_file():
                targets.add(default_module.resolve())
            else:
                targets.update(
                    child.resolve()
                    for child in p.rglob("*.nix")
                    if child.is_file()
                )
        elif p.suffix == ".nix" and p.is_file():
            targets.add(p.resolve())
```

Update the docstring to state that a referenced directory without a root `default.nix` is a dynamic Nix root whose `.nix` descendants are followed.

- [ ] **Step 5: Run the complete detector suite and real repository scan**

```sh
cd misc
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run --with pytest pytest test_find_dead_nix.py -v
cd ..
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run misc/find_dead_nix.py --verbose
```

Expected: pytest passes and the scan reports zero dead files, including no `charts/*/default.nix` false positives.

- [ ] **Step 6: Commit the detector correction**

```sh
git add misc/find_dead_nix.py misc/test_find_dead_nix.py
git commit -m "Keep dynamically loaded Nix trees visible to cleanup"
```

---

### Task 2: Introduce the typed aspect and host-constructor core

**Files:**
- Create: `modules/flake/aspect-options.nix`
- Create: `modules/flake/schema.nix`
- Create: `modules/flake/lib.nix`
- Create: `modules/flake/checks.nix`
- Create: `modules/flake/tests/home-manager-aspect.nix`
- Create: `modules/common/overlay-list.nix`
- Modify: `modules/common/overlays.nix`
- Modify: `modules/common/meta.nix`
- Modify: `modules/common-hm/default.nix`
- Modify: `flake.nix`

**Interfaces:**
- Consumes: `inputs`, `self`, inventory-shaped hosts, literal deferred modules, and the shared overlay list.
- Produces: `resolveAspects`, `checkHost`, and `mkConfiguration` from `modules/flake/lib.nix`.

- [ ] **Step 1: Create the nested aspect option schema**

Create `modules/flake/aspect-options.nix`:

```nix
{lib, ...}: let
  inherit (lib) mkOption types;
in {
  options = {
    nixos = mkOption {
      type = types.deferredModule;
      default = {};
      description = "NixOS module contributed by selected aspects.";
    };
    darwin = mkOption {
      type = types.deferredModule;
      default = {};
      description = "nix-darwin module contributed by selected aspects.";
    };
    homeManager = mkOption {
      type = types.deferredModule;
      default = {};
      description = "Home Manager module contributed by selected aspects.";
    };
  };
}
```

- [ ] **Step 2: Declare the flake-parts inventory options**

Create `modules/flake/schema.nix` with this fixed public shape and add useful descriptions to every option:

```nix
{lib, ...}: let
  inherit (lib) mkOption types;
  hostType = types.submodule ({name, ...}: {
    options = {
      class = mkOption {type = types.enum ["nixos" "darwin" "homeManager"];};
      system = mkOption {type = types.str;};
      hostName = mkOption {type = types.str; default = name;};
      primaryUser = mkOption {type = types.str;};
      stateVersion = {
        system = mkOption {
          type = types.nullOr (types.either types.str types.int);
          default = null;
        };
        home = mkOption {type = types.str;};
      };
      aspects = mkOption {type = types.listOf types.deferredModule; default = [];};
      modules = mkOption {type = types.listOf types.deferredModule; default = [];};
      homeModules = mkOption {type = types.listOf types.deferredModule; default = [];};
    };
  });
in {
  options.dotfiles = {
    sharedAspects = mkOption {type = types.listOf types.deferredModule; default = [];};
    hosts = mkOption {type = types.attrsOf hostType; default = {};};
    extraSystems = mkOption {type = types.listOf types.str; default = [];};
  };
}
```

- [ ] **Step 3: Extract the overlay list without changing system behavior**

Create `modules/common/overlay-list.nix` by moving the current three overlays and their comments into:

```nix
{inputs}: [
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
]
```

Change `modules/common/overlays.nix` to:

```nix
{inputs, ...}: {
  nixpkgs.overlays = import ./overlay-list.nix {inherit inputs;};
  nixpkgs.config.allowUnfree = true;
}
```

- [ ] **Step 4: Extend target metadata for inventory hostnames**

Add beside `meta.user` in both `modules/common/meta.nix` and `modules/common-hm/default.nix`:

```nix
hostName = lib.mkOption {
  type = lib.types.str;
  description = "Inventory hostname of this configuration.";
};
```

Keep `meta.isWSL`.

- [ ] **Step 5: Write the synthetic Home Manager aspect fixture**

Create `modules/flake/tests/home-manager-aspect.nix`:

```nix
{...}: {
  homeManager = {
    config,
    lib,
    ...
  }: {
    options.programs.aspect-constructor.enable =
      lib.mkEnableOption "the aspect constructor fixture";
    config = {
      programs.aspect-constructor.enable = lib.mkDefault true;
      home.file.".aspect-constructor-test".text =
        if config.programs.aspect-constructor.enable
        then "enabled"
        else "overridden";
    };
  };
}
```

- [ ] **Step 6: Implement the internal constructor library**

Create `modules/flake/lib.nix`. Export exactly `checkHost`, `mkConfiguration`, and `resolveAspects`; use this structure:

```nix
{
  inputs,
  lib,
  self,
}: let
  overlays = import ../common/overlay-list.nix {inherit inputs;};
  mkLib = inputs.nixpkgs.lib.extend (final: _prev: {
    mine = import ../../lib {lib = final;};
  });
  hmLib = inputs.nixpkgs.lib.extend (final: _prev:
    {mine = import ../../lib {lib = final;};}
    // inputs.home-manager.lib);
  sysArgs = {inherit inputs; lib = mkLib;};
  hmArgs = {inherit inputs; lib = hmLib;};

  resolveAspects = aspects:
    (lib.evalModules {
      modules = [./aspect-options.nix] ++ aspects;
      specialArgs = {inherit inputs;};
    }).config;

  checkHost = name: host:
    if host.hostName == ""
    then throw "dotfiles.hosts.${name}: hostName must not be empty"
    else if host.primaryUser == ""
    then throw "dotfiles.hosts.${name}: primaryUser must not be empty"
    else if host.class == "darwin" && !lib.hasSuffix "-darwin" host.system
    then throw "dotfiles.hosts.${name}: darwin requires a Darwin system"
    else if host.class == "nixos" && !lib.hasSuffix "-linux" host.system
    then throw "dotfiles.hosts.${name}: nixos requires a Linux system"
    else if host.class == "darwin" && !builtins.isInt host.stateVersion.system
    then throw "dotfiles.hosts.${name}: Darwin system stateVersion must be an integer"
    else if host.class == "nixos" && !builtins.isString host.stateVersion.system
    then throw "dotfiles.hosts.${name}: NixOS system stateVersion must be a string"
    else if host.class == "homeManager" && host.stateVersion.system != null
    then throw "dotfiles.hosts.${name}: Home Manager hosts must not set a system stateVersion"
    else host;

  homeModule = host: resolved: {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.extraSpecialArgs = hmArgs;
    home-manager.users.${host.primaryUser} = {
      imports = [../common-hm ../hm resolved.homeManager] ++ host.homeModules;
      meta.user = host.primaryUser;
      meta.hostName = host.hostName;
      home.stateVersion = host.stateVersion.home;
    };
  };

  systemFacts = host: {
    nixpkgs.hostPlatform = host.system;
    networking.hostName = host.hostName;
    meta.user = host.primaryUser;
    meta.hostName = host.hostName;
    system.stateVersion = host.stateVersion.system;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
  };

  mkNixos = host: resolved: inputs.nixpkgs.lib.nixosSystem {
    specialArgs = sysArgs;
    modules = [
      ../common
      inputs.home-manager.nixosModules.home-manager
      resolved.nixos
    ] ++ host.modules ++ [(systemFacts host) (homeModule host resolved)];
  };

  mkDarwin = host: resolved: inputs.darwin.lib.darwinSystem {
    specialArgs = sysArgs;
    modules = [
      ../common
      ../darwin
      inputs.home-manager.darwinModules.home-manager
      resolved.darwin
    ] ++ host.modules ++ [(systemFacts host) (homeModule host resolved)];
  };

  mkHomeManager = host: resolved:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {
        system = host.system;
        inherit overlays;
        config.allowUnfree = true;
      };
      extraSpecialArgs = hmArgs;
      modules = [../common-hm ../hm resolved.homeManager] ++ host.homeModules ++ [{
        meta.user = host.primaryUser;
        meta.hostName = host.hostName;
        home.stateVersion = host.stateVersion.home;
      }];
    };

  mkConfiguration = name: uncheckedHost: let
    host = checkHost name uncheckedHost;
    resolved = resolveAspects uncheckedHost.aspects;
  in
    if host.class == "nixos"
    then mkNixos host resolved
    else if host.class == "darwin"
    then mkDarwin host resolved
    else mkHomeManager host resolved;
in {
  inherit checkHost mkConfiguration resolveAspects;
}
```

This task intentionally retains the custom libraries as compatibility plumbing. Task 6 removes them after cross-class environment detection is gone.

- [ ] **Step 7: Add a flake check for the Home-Manager-only route**

Create `modules/flake/checks.nix`:

```nix
{
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ./lib.nix {inherit inputs lib self;};
  testHome = constructors.mkConfiguration "aspect-test" {
    class = "homeManager";
    system = "x86_64-linux";
    hostName = "aspect-test";
    primaryUser = "tester";
    stateVersion = {system = null; home = "26.05";};
    aspects = [./tests/home-manager-aspect.nix];
    modules = [];
    homeModules = [{programs.aspect-constructor.enable = false;}];
  };
in {
  perSystem = {pkgs, system, ...}:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks.home-manager-host-class =
        assert testHome.config.programs.aspect-constructor.enable == false;
        assert testHome.config.home.file.".aspect-constructor-test".text == "overridden";
          pkgs.runCommand "home-manager-host-class" {} "touch $out";
    };
}
```

Temporarily add to the current `mkFlake` body:

```nix
imports = [
  ./modules/flake/schema.nix
  ./modules/flake/checks.nix
];
```

- [ ] **Step 8: Run the constructor check and existing checks**

```sh
nix build .#checks.x86_64-linux.home-manager-host-class
nix flake check
```

Expected: the synthetic aspect is routed through the real standalone Home Manager constructor and existing checks remain green.

- [ ] **Step 9: Commit the constructor core**

```sh
git add flake.nix modules/flake modules/common modules/common-hm
git commit -m "Give every deployment class one composition contract"
```

---

### Task 3: Cut current outputs over to the central inventory

**Files:**
- Create: `modules/flake/default.nix`
- Create: `modules/flake/inventory.nix`
- Create: `modules/flake/outputs.nix`
- Modify: `flake.nix`
- Modify: `machine/globalhawk/default.nix`
- Modify: `machine/globalhawk/home.nix`
- Modify: `machine/nighthawk/default.nix`
- Modify: `machine/nighthawk/home.nix`
- Modify: `machine/valkyrie/default.nix`
- Modify: `machine/valkyrie/home.nix`

**Interfaces:**
- Consumes: Task 2's `dotfiles` schema and `mkConfiguration` function.
- Produces: All existing flake outputs from `dotfiles.hosts`, with inventory-owned identity and state versions.

- [ ] **Step 1: Declare the current inventory with no aspects selected yet**

Create `modules/flake/inventory.nix`:

```nix
{inputs, ...}: {
  dotfiles = {
    sharedAspects = [];
    extraSystems = ["x86_64-darwin"];
    hosts = {
      globalhawk = {
        class = "nixos";
        system = "x86_64-linux";
        primaryUser = "abe";
        stateVersion = {system = "23.11"; home = "23.11";};
        modules = [../../machine/globalhawk];
        homeModules = [../../machine/globalhawk/home.nix];
      };
      valkyrie = {
        class = "nixos";
        system = "x86_64-linux";
        primaryUser = "abe";
        stateVersion = {system = "26.05"; home = "26.05";};
        modules = [
          inputs.determinate.nixosModules.default
          ../../machine/valkyrie
        ];
        homeModules = [../../machine/valkyrie/home.nix];
      };
      nighthawk = {
        class = "darwin";
        system = "aarch64-darwin";
        primaryUser = "abe";
        stateVersion = {system = 5; home = "24.05";};
        modules = [
          inputs.determinate.darwinModules.default
          ../../machine/nighthawk
        ];
        homeModules = [../../machine/nighthawk/home.nix];
      };
    };
  };
}
```

- [ ] **Step 2: Map inventory entries to outputs**

Create `modules/flake/outputs.nix` with this mapping:

```nix
{
  config,
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ./lib.nix {inherit inputs lib self;};
  hosts = lib.mapAttrs (_: host:
    host
    // {aspects = config.dotfiles.sharedAspects ++ host.aspects;})
  config.dotfiles.hosts;
  byClass = class: lib.filterAttrs (_: host: host.class == class) hosts;
in {
  systems = lib.unique (
    config.dotfiles.extraSystems
    ++ map (host: host.system) (lib.attrValues hosts)
  );

  flake = {
    nixosConfigurations = lib.mapAttrs constructors.mkConfiguration (byClass "nixos");
    darwinConfigurations = lib.mapAttrs constructors.mkConfiguration (byClass "darwin");
    homeConfigurations = lib.mapAttrs' (
      name: host:
        lib.nameValuePair
        "${host.primaryUser}@${host.hostName}"
        (constructors.mkConfiguration name host)
    ) (byClass "homeManager");
  };

  perSystem = {pkgs, system, ...}: {
    formatter = pkgs.alejandra;
    checks = pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
      k3s-workloads-module = import ../../k8s/tests/workloads-module.nix {
        inherit inputs pkgs;
      };
      k3s-runtime-secrets-module = import ../../k8s/tests/runtime-secrets-module.nix {
        inherit inputs pkgs;
      };
    };
    packages = {
      decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
        ${pkgs.gnupg}/bin/gpg --decrypt ${../../local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
      '';
      audit-agent-access = pkgs.callPackage ../../packages/audit-agent-access.nix {};
      k3s-drift = pkgs.callPackage ../../packages/k3s-drift.nix {};
      populate-sops = pkgs.callPackage ../../packages/populate-sops.nix {};
      libation-reconcile = pkgs.callPackage ../../packages/libation-reconcile.nix {};
      libation-auth = pkgs.callPackage ../../packages/libation-auth.nix {};
    } // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
      darwin-rebuild = pkgs.writeShellScriptBin "darwin-rebuild" ''
        exec sudo ${inputs.darwin.packages.${system}.darwin-rebuild}/bin/darwin-rebuild --flake . "$@"
      '';
    };
  };
}
```

Move the explanatory comments from the existing `perSystem` block beside the corresponding definitions above. The Linux check attributes merge with `home-manager-host-class` from `checks.nix`.

- [ ] **Step 3: Create the entry module and minimize `flake.nix`**

Create `modules/flake/default.nix`:

```nix
{...}: {
  imports = [
    ./schema.nix
    ./checks.nix
    ./inventory.nix
    ./outputs.nix
  ];
}
```

Replace the current `outputs` function in `flake.nix` with:

```nix
outputs = inputs:
  inputs.flake-parts.lib.mkFlake {inherit inputs;} {
    imports = [./modules/flake];
  };
```

Keep every input declaration and its comments unchanged.

- [ ] **Step 4: Remove duplicated host facts from machine modules**

Delete only these definitions:

```text
machine/globalhawk/default.nix: networking.hostName, system.stateVersion
machine/globalhawk/home.nix:    home.stateVersion
machine/nighthawk/default.nix:  networking.hostName, system.stateVersion
machine/nighthawk/home.nix:     home.stateVersion
machine/valkyrie/default.nix:   networking.hostName, system.stateVersion
machine/valkyrie/home.nix:      home.stateVersion
```

Keep `networking.localHostName` and `networking.computerName` on nighthawk. Keep surrounding human comments when they still explain live configuration.

- [ ] **Step 5: Verify facts and behavior-preserving constructor output**

```sh
nix eval --raw .#nixosConfigurations.globalhawk.config.networking.hostName
nix eval --raw .#nixosConfigurations.valkyrie.config.networking.hostName
nix eval --raw .#darwinConfigurations.nighthawk.config.networking.hostName
nix eval --raw .#darwinConfigurations.nighthawk.config.system.build.toplevel.drvPath > /tmp/dotfiles-aspect-baseline/nighthawk-after-inventory.drv
nix eval --raw .#nixosConfigurations.valkyrie.config.system.build.toplevel.drvPath > /tmp/dotfiles-aspect-baseline/valkyrie-after-inventory.drv
nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath > /tmp/dotfiles-aspect-baseline/globalhawk-after-inventory.drv
diff -u /tmp/dotfiles-aspect-baseline/nighthawk.drv /tmp/dotfiles-aspect-baseline/nighthawk-after-inventory.drv
diff -u /tmp/dotfiles-aspect-baseline/valkyrie.drv /tmp/dotfiles-aspect-baseline/valkyrie-after-inventory.drv
diff -u /tmp/dotfiles-aspect-baseline/globalhawk.drv /tmp/dotfiles-aspect-baseline/globalhawk-after-inventory.drv
nix flake check
```

Expected: hostnames match their inventory keys and all drv comparisons are empty because this checkpoint changes composition only.

- [ ] **Step 6: Commit the inventory cutover**

```sh
git add flake.nix modules/flake machine/globalhawk/default.nix machine/globalhawk/home.nix machine/nighthawk/default.nix machine/nighthawk/home.nix machine/valkyrie/default.nix machine/valkyrie/home.nix
git commit -m "Let one inventory own machine identity and output wiring"
```

---

### Task 4: Create the shared cross-class CLI aspects

**Files:**
- Create: `aspect/common-cli.nix`
- Create: `aspect/modern-cli.nix`
- Move: `program/fish/` -> `aspect/fish/`
- Move: `program/git/` -> `aspect/git/`
- Move: `program/vim/` -> `aspect/vim/`
- Move: `program/direnv/` -> `aspect/direnv/`
- Move: `program/starship/` -> `aspect/starship/`
- Modify: `modules/flake/inventory.nix`
- Modify: `role/dev.nix`
- Modify: `machine/globalhawk/default.nix`
- Modify: `machine/globalhawk/home.nix`
- Modify: `machine/nighthawk/default.nix`
- Modify: `machine/nighthawk/home.nix`
- Modify: `machine/valkyrie/default.nix`
- Modify: `machine/valkyrie/home.nix`

**Interfaces:**
- Consumes: Aspect projection options and `dotfiles.sharedAspects`.
- Produces: `aspect/common-cli.nix`, selected once for every host, and class-specific leaf aspects.

- [ ] **Step 1: Record the failing shared-tool expectation**

```sh
nix eval .#nixosConfigurations.valkyrie.config.home-manager.users.abe.programs.bat.enable
```

Expected before migration: `false`. It becomes `true` after the shared profile is selected.

- [ ] **Step 2: Move leaf directories without losing comments or payloads**

```sh
mkdir -p aspect
git mv program/fish aspect/fish
git mv program/git aspect/git
git mv program/vim aspect/vim
git mv program/direnv aspect/direnv
git mv program/starship aspect/starship
git mv aspect/fish/default.nix aspect/fish/home.nix
git mv aspect/git/default.nix aspect/git/home.nix
git mv aspect/vim/default.nix aspect/vim/home.nix
git mv aspect/starship/default.nix aspect/starship/home.nix
```

- [ ] **Step 3: Wrap Fish in all target classes**

Create `aspect/fish/default.nix`:

```nix
{lib, ...}: let
  systemModule = {
    config,
    pkgs,
    ...
  }: {
    programs.fish.enable = lib.mkDefault true;
    users.users.${config.meta.user}.shell = lib.mkDefault pkgs.fish;
  };
in {
  nixos = systemModule;
  darwin = systemModule;
  homeManager = ./home.nix;
}
```

In `aspect/fish/home.nix`, preserve the implementation and comments, keep `imports = [./module.nix]`, and change `programs.fish.enable` to `lib.mkDefault true`. Update `aspect/fish/module.nix` comments from `program/zellij` to `aspect/zellij`.

- [ ] **Step 4: Wrap Git, Vim, and Starship**

Create each `default.nix` using:

```nix
{...}: {
  homeManager = ./home.nix;
}
```

In their `home.nix` files, add `lib` to arguments where needed and change only conventional enable values:

```nix
programs.git.enable = lib.mkDefault true;
programs.delta.enable = lib.mkDefault true;
programs.neovim.enable = lib.mkDefault true;
programs.starship.enable = lib.mkDefault true;
```

- [ ] **Step 5: Split Direnv by class instead of probing `lib ? hm`**

Replace `aspect/direnv/default.nix` with:

```nix
{lib, ...}: let
  settings = {pkgs, ...}: {
    programs.direnv = {
      enable = lib.mkDefault true;
      package = pkgs.unstable.direnv;
      nix-direnv.enable = lib.mkDefault true;
    };
  };
in {
  darwin = settings;
  homeManager = {pkgs, ...}: {
    imports = [settings];
    home.sessionVariables.DIRENV_INSTDIR = "${pkgs.unstable.direnv}";
  };
}
```

Do not add a NixOS system projection; Linux hosts receive Direnv through Home Manager.

- [ ] **Step 6: Add modern CLI defaults and the shared profile**

Create `aspect/modern-cli.nix`:

```nix
{lib, ...}: {
  homeManager = {pkgs, ...}: {
    programs.bat.enable = lib.mkDefault true;
    programs.eza.enable = lib.mkDefault true;
    home.packages = with pkgs; [fd ripgrep bottom xh lazygit];
    programs.fish.shellAliases = {
      cat = "bat --paging=never";
      ll = "eza --classify --long --all --header --git --hyperlink";
      tree = "eza --classify --long --git --hyperlink --tree --level=2";
      ls = "eza --classify --hyperlink";
    };
  };
}
```

Move the explanatory alias comments from `machine/nighthawk/home.nix` beside these aliases.

Create `aspect/common-cli.nix`:

```nix
{...}: {
  imports = [./fish ./git ./vim ./direnv ./starship ./modern-cli.nix];
}
```

- [ ] **Step 7: Select the profile and remove overlapping imports**

Set:

```nix
dotfiles.sharedAspects = [../../aspect/common-cli.nix];
```

Make these compatibility edits:

- `role/dev.nix`: remove the moved Git, Vim, Fish, Direnv, and Starship imports because `sharedAspects` now supplies them; keep Tmux, ShellCheck, jq, SSH, and keychain in the Home Manager role until Task 5. Do not import an aspect wrapper directly as a Home Manager module.
- `machine/globalhawk/home.nix`: keep the role and Zellij imports unchanged.
- `machine/nighthawk/home.nix`: remove Git, Vim, Fish, and Starship imports; retain AI agents and userscripts. Remove Bat, fd, ripgrep, eza, bottom, xh, and lazygit packages plus the moved aliases; retain ShellCheck, jq, SSH, and nix-index until Task 5.
- `machine/valkyrie/home.nix`: remove Git, Vim, Fish, Starship, and Direnv imports; retain AI agents and Zed.
- `machine/nighthawk/default.nix`: remove the Direnv import and direct `programs.fish.enable`.
- All NixOS machine modules: remove direct `programs.fish.enable` and primary-user `shell = pkgs.fish`.

- [ ] **Step 8: Verify shared activation**

```sh
nix eval .#nixosConfigurations.globalhawk.config.home-manager.users.abe.programs.bat.enable
nix eval .#nixosConfigurations.valkyrie.config.home-manager.users.abe.programs.bat.enable
nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.bat.enable
nix eval --raw .#nixosConfigurations.valkyrie.config.users.users.abe.shell.name
nix flake check
```

Expected: Bat is `true` in all primary homes, Valkyrie's shell resolves to Fish, and the flake checks pass.

- [ ] **Step 9: Commit the shared CLI migration**

```sh
git add aspect program role/dev.nix machine modules/flake/inventory.nix
git commit -m "Make the familiar CLI environment one explicit choice"
```

---

### Task 5: Migrate development, multiplexer, userscript, and AI-agent aspects

**Files:**
- Create: `aspect/development.nix`
- Create: `aspect/shell-utilities.nix`
- Move: `program/tmux/` -> `aspect/tmux/`
- Move: `program/zellij/` -> `aspect/zellij/`
- Move: `program/userscripts/` -> `aspect/userscripts/`
- Move: `program/ai-agents/` -> `aspect/ai-agents/`
- Move: `modules/hm/ai-agents/module.nix` -> `aspect/ai-agents/module.nix`
- Modify: `modules/nixos/ai-agent-sandbox.nix`
- Modify: `modules/hm/default.nix`
- Modify: `modules/flake/inventory.nix`
- Modify: `machine/globalhawk/home.nix`
- Modify: `machine/nighthawk/home.nix`
- Modify: `machine/valkyrie/home.nix`
- Delete: `role/dev.nix`

**Interfaces:**
- Consumes: Home Manager aspect projection and per-host literal aspect lists.
- Produces: `aspect/development.nix`, explicit AI/userscript/multiplexer aspects, and no remaining `program/` consumers.

- [ ] **Step 1: Move the remaining concern trees**

```sh
git mv program/tmux aspect/tmux
git mv program/zellij aspect/zellij
git mv program/userscripts aspect/userscripts
git mv program/ai-agents aspect/ai-agents
git mv modules/hm/ai-agents/module.nix aspect/ai-agents/module.nix
git mv aspect/tmux/default.nix aspect/tmux/home.nix
git mv aspect/zellij/default.nix aspect/zellij/home.nix
git mv aspect/userscripts/default.nix aspect/userscripts/home.nix
git mv aspect/ai-agents/default.nix aspect/ai-agents/home.nix
```

- [ ] **Step 2: Add wrappers and default enablement**

Create `default.nix` in each moved directory:

```nix
{...}: {
  homeManager = ./home.nix;
}
```

In each `home.nix`, preserve settings and imports, add `lib` to arguments, and change:

```nix
programs.tmux-custom.enable = lib.mkDefault true;
programs.zellij.enable = lib.mkDefault true;
programs.userscripts.enable = lib.mkDefault true;
programs.ai-agents.enable = lib.mkDefault true;
```

Because the AI-agent option module previously came from the global
`modules/hm/default.nix` import, also add `./module.nix` to
`aspect/ai-agents/home.nix`'s imports before removing that global registration.

Update moved comments from `program/fish` and `program/zellij` to their new aspect paths.

- [ ] **Step 3: Create the development profile**

Create `aspect/shell-utilities.nix` by moving the shared behavior currently
duplicated between `role/dev.nix` and `machine/nighthawk/home.nix`:

```nix
{lib, ...}: {
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.haskellPackages.ShellCheck];
    programs.jq.enable = lib.mkDefault true;
    programs.keychain.enable = lib.mkDefault pkgs.stdenv.isLinux;
    programs.ssh = {
      enable = lib.mkDefault true;
      settings."*" = {
        IgnoreUnknown = lib.mkDefault "AddKeysToAgent,UseKeychain";
        AddKeysToAgent = lib.mkDefault "yes";
        UseKeychain = lib.mkDefault "yes";
      };
    };
  };
}
```

Move the explanatory SSH comments from `machine/nighthawk/home.nix` beside
the settings. This aspect is selected only on Globalhawk and Nighthawk, where
the equivalent behavior already exists; it is deliberately not shared with
Valkyrie. Retain `programs.ssh.enableDefaultConfig = false` in Nighthawk's
machine Home Manager module so that host's existing choice overrides the
shared aspect without changing Globalhawk's defaults.

Create `aspect/development.nix`:

```nix
{inputs, ...}: {
  imports = [./ai-agents];
  homeManager = {pkgs, ...}: {
    home.packages = [
      pkgs.nil
      pkgs.diff2html-cli
      pkgs.difftastic
      inputs.git-different.packages.${pkgs.system}.default
      pkgs.imagemagick
    ];
  };
}
```

Keep work-only packages, local container/LLM tools, and credential configuration in `machine/nighthawk/home.nix`.

- [ ] **Step 4: Select development and host-specific aspects**

Set inventory lists to:

```nix
globalhawk.aspects = [
  ../../aspect/shell-utilities.nix
  ../../aspect/tmux
  ../../aspect/zellij
];
valkyrie.aspects = [../../aspect/ai-agents];
nighthawk.aspects = [
  ../../aspect/shell-utilities.nix
  ../../aspect/development.nix
  ../../aspect/userscripts
];
```

Remove migrated packages and imports from each `machine/*/home.nix`. Keep
Nighthawk's nix-index, work-only packages, local container/LLM tools, and
credential configuration. Keep Valkyrie's Zed package. Delete
`machine/globalhawk/home.nix` and remove its path from
`globalhawk.homeModules`; the selected shell-utilities, Tmux, and Zellij
aspects leave it with no live configuration.

- [ ] **Step 5: Point the sandbox at the aspect and remove global HM registration**

In `modules/nixos/ai-agent-sandbox.nix`, change:

```nix
default = [../../aspect/ai-agents/home.nix];
defaultText = lib.literalExpression "[ ../../aspect/ai-agents/home.nix ]";
```

The sandbox option consumes Home Manager modules directly, so it selects the
aspect's Home Manager projection implementation rather than the outer aspect
wrapper.

Remove `./ai-agents/module.nix` from `modules/hm/default.nix`. The option module is now imported only when the aspect is selected, including the sandbox's explicit special-user selection.

- [ ] **Step 6: Prove primary and special users still receive agent configuration**

```sh
nix eval .#nixosConfigurations.globalhawk.config.home-manager.users.agent.programs.ai-agents.enable
nix eval .#nixosConfigurations.globalhawk.config.home-manager.users.abe.programs.ai-agents.enable
nix eval .#nixosConfigurations.valkyrie.config.home-manager.users.abe.programs.ai-agents.enable
nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.userscripts.enable
nix flake check
```

Expected: all values are `true` and checks pass.

- [ ] **Step 7: Remove the obsolete development role and verify imports**

```sh
git rm role/dev.nix
rg -n "program/|\.\./program|\.\./\.\./program" --glob '*.nix' .
```

Expected: no live Nix references. Git does not track the now-empty `program/` directory.

- [ ] **Step 8: Commit the remaining aspect migration**

```sh
git add aspect machine modules role
git commit -m "Keep development concerns reusable across deployment classes"
```

---

### Task 6: Move platform opinions into aspects and remove obsolete plumbing

**Files:**
- Create: `aspect/darwin-desktop.nix`
- Move: `role/darwin.nix` -> `aspect/darwin-desktop-module.nix`
- Create: `aspect/plasma-desktop.nix`
- Modify: `modules/flake/inventory.nix`
- Modify: `machine/nighthawk/default.nix`
- Modify: `machine/valkyrie/default.nix`
- Move: `modules/hm/defaults.nix` -> `modules/common-hm/defaults.nix`
- Modify: `modules/common-hm/default.nix`
- Modify: `modules/nixos/ai-agent-sandbox.nix`
- Modify: `modules/flake/lib.nix`
- Delete: `modules/hm/default.nix`
- Delete: `modules/hm/docker-rootless/module.nix`
- Delete: `lib/default.nix`
- Delete: `lib/types.nix`
- Delete: `lib/darwin.nix`

**Interfaces:**
- Consumes: Literal host aspect lists and class-specific Fish/Direnv modules.
- Produces: Final directory boundaries, normal target libraries, and `specialArgs = { inherit inputs; }` only.

- [ ] **Step 1: Convert the Darwin role into a Darwin projection**

Move the complete existing module, preserving its comments:

```sh
git mv role/darwin.nix aspect/darwin-desktop-module.nix
```

Create `aspect/darwin-desktop.nix`:

```nix
{...}: {
  darwin = ./darwin-desktop-module.nix;
}
```

Remove the old role import from `machine/nighthawk/default.nix` and add `../../aspect/darwin-desktop.nix` to `nighthawk.aspects`.

- [ ] **Step 2: Extract the reusable Plasma projection**

Create `aspect/plasma-desktop.nix`:

```nix
{lib, ...}: {
  nixos = {
    services.desktopManager.plasma6.enable = lib.mkDefault true;
    services.displayManager = {
      sddm.enable = lib.mkDefault true;
      sddm.wayland.enable = lib.mkDefault true;
      defaultSession = lib.mkDefault "plasma";
    };
  };
}
```

Remove exactly those settings from `machine/valkyrie/default.nix`. Leave NetworkManager, Avahi, Samba discovery, SSH, boot, and hardware configuration machine-owned. Add `../../aspect/plasma-desktop.nix` to `valkyrie.aspects`.

- [ ] **Step 3: Consolidate common Home Manager infrastructure**

```sh
git mv modules/hm/defaults.nix modules/common-hm/defaults.nix
```

Add this import to `modules/common-hm/default.nix`:

```nix
imports = [./defaults.nix];
```

Update the moved comment to say the inventory owns `home.stateVersion`. In `modules/nixos/ai-agent-sandbox.nix`, change:

```nix
imports = [../common-hm ../hm] ++ cfg.sharedModules;
```

to:

```nix
imports = [../common-hm] ++ cfg.sharedModules;
```

Delete `modules/hm/default.nix` and the unused `modules/hm/docker-rootless/module.nix`.

- [ ] **Step 4: Remove the custom library compatibility layer**

In `modules/flake/lib.nix`, delete `mkLib` and `hmLib` and replace the argument sets with:

```nix
sysArgs = {inherit inputs;};
hmArgs = {inherit inputs;};
```

Remove `../hm` from embedded and standalone Home Manager import lists because `../common-hm` now imports defaults.

Delete only the unused root library:

```sh
git rm lib/default.nix lib/types.nix lib/darwin.nix
```

Do not remove or rename `machine/globalhawk/k3s/lib.nix`.

- [ ] **Step 5: Verify final dependency and directory boundaries**

```sh
rg -n "lib\.mine|modules/hm|role/|program/" --glob '*.nix' .
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run misc/find_dead_nix.py --verbose
nix flake check
```

Expected: no obsolete Nix references or `lib.mine` consumers, zero dead tracked Nix files, and passing checks. Historical Markdown references are not rewritten merely to silence searches.

- [ ] **Step 6: Commit the final architecture boundary**

```sh
git add aspect machine modules
git commit -m "Keep opinions separate from reusable module plumbing"
```

---

### Task 7: Update repository guidance and complete verification

**Files:**
- Modify: `AGENTS.md`
- Modify: `README.md`
- Modify: any `.nix` files changed by `nix fmt`

**Interfaces:**
- Consumes: Final inventory, aspect paths, detector behavior, and all active outputs.
- Produces: Accurate contributor guidance and evidence that the migration builds without activation.

- [ ] **Step 1: Update the repository architecture guide**

In `AGENTS.md` make these exact content changes:

- Add `nixosConfigurations.valkyrie` and remove the retired Campbell/WSL output.
- State that `modules/flake/inventory.nix` owns host facts and selects literal aspect paths.
- Replace `role/` and `program/` descriptions with `aspect/` leaf/profile semantics.
- Describe `machine/` as hardware and genuinely host-specific deployment configuration, not the owner of hostname, user, or state version.
- Describe `modules/flake`, `modules/common-hm`, `modules/nixos`, and `modules/darwin` using their final boundaries.
- State that only `inputs` is passed through general `specialArgs`.
- Update AI-agent module paths to `aspect/ai-agents/`.
- Add non-activating build commands for valkyrie while retaining the globalhawk full-build requirement.

Do not change the secrets policy or weaken globalhawk testing guidance.

- [ ] **Step 2: Update dead-file documentation for dynamic roots and inventory paths**

In `README.md`, replace the claim that the walker follows only import syntax with:

```text
The detector follows every literal repository `.nix` path in a reachable file,
including paths stored in inventory options. A referenced directory with no
root `default.nix` is treated conservatively as a dynamic Nix root; this keeps
nixidy chart definitions loaded through `nixidy.chartsDir` reachable.
```

Add Valkyrie's drv-path command to the deletion-safety example. Keep the stdout/stderr contract and computed-import limitation unchanged.

- [ ] **Step 3: Format and inspect the complete diff**

```sh
nix fmt
git diff --check
git status --short
git diff --stat
```

Expected: formatting succeeds, there are no whitespace errors, and no unrelated or secret-bearing changes appear.

- [ ] **Step 4: Run focused and repository-wide checks**

```sh
cd misc
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run --with pytest pytest test_find_dead_nix.py -v
cd ..
UV_CACHE_DIR=/tmp/dotfiles-aspect-uv-cache uv run misc/find_dead_nix.py
nix flake check
```

Expected: pytest passes, the detector exits zero, and all flake checks pass, including `home-manager-host-class`.

- [ ] **Step 5: Inspect authoritative facts and shared behavior**

```sh
nix eval --raw .#nixosConfigurations.globalhawk.config.networking.hostName
nix eval --raw .#nixosConfigurations.valkyrie.config.networking.hostName
nix eval --raw .#darwinConfigurations.nighthawk.config.networking.hostName
nix eval --raw .#nixosConfigurations.valkyrie.config.nixpkgs.hostPlatform.system
nix eval --raw .#darwinConfigurations.nighthawk.config.nixpkgs.hostPlatform.system
nix eval --raw .#nixosConfigurations.valkyrie.config.meta.user
nix eval --raw .#nixosConfigurations.valkyrie.config.home-manager.users.abe.home.username
nix eval --raw .#nixosConfigurations.valkyrie.config.home-manager.users.abe.home.homeDirectory
nix eval .#nixosConfigurations.globalhawk.config.system.stateVersion
nix eval .#nixosConfigurations.valkyrie.config.system.stateVersion
nix eval .#darwinConfigurations.nighthawk.config.system.stateVersion
nix eval .#nixosConfigurations.globalhawk.config.home-manager.users.abe.home.stateVersion
nix eval .#nixosConfigurations.valkyrie.config.home-manager.users.abe.home.stateVersion
nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.home.stateVersion
nix eval .#nixosConfigurations.globalhawk.config.home-manager.users.abe.programs.bat.enable
nix eval .#nixosConfigurations.valkyrie.config.home-manager.users.abe.programs.bat.enable
nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.bat.enable
nix eval .#nixosConfigurations.globalhawk.config.home-manager.users.agent.programs.ai-agents.enable
```

Expected: hostnames, platforms, user, home directory, and state versions match
the inventory; every boolean is `true`. The standalone constructor check also
proves that a host module can override an aspect's `mkDefault` enablement.

- [ ] **Step 6: Build every active configuration without activation**

```sh
nix build --no-link .#darwinConfigurations.nighthawk.system
nix build --no-link .#nixosConfigurations.valkyrie.config.system.build.toplevel
nix build --no-link .#nixosConfigurations.globalhawk.config.system.build.toplevel
```

Expected: all builds succeed. The globalhawk build exercises nixidy workloads and vendored charts. If this machine has no builder for a target, run that command on the appropriate host or configured remote builder before declaring completion.

- [ ] **Step 7: Commit documentation and formatter-only adjustments**

```sh
git add AGENTS.md README.md
git add -u -- '*.nix'
git commit -m "Keep future changes aligned with the aspect inventory"
```

- [ ] **Step 8: Confirm the handoff is clean**

```sh
git status --short
git log --oneline -7
```

Expected: the worktree is clean and commit messages document why each boundary changed.
