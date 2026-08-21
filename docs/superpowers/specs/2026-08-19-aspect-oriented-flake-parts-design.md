# Aspect-oriented flake-parts configuration

**Status:** approved
**Date:** 2026-08-19

## Goal

Reorganize the repository around explicitly selected, cross-environment
aspects while retaining flake-parts and the native Nix module systems.

An aspect may contribute NixOS, nix-darwin, and Home Manager modules in one
place. A central inventory selects aspect files for each machine, and a shared
constructor produces the corresponding flake output without repeating user,
platform, hostname, Home Manager, or special-argument wiring.

The migration must support three deployment classes:

- NixOS with Home Manager for the primary user.
- nix-darwin with Home Manager for the primary user.
- Standalone, user-mode Home Manager without system management.

The existing globalhawk system and Kubernetes service organization remains a
valid leaf of the new composition model. Nighthawk and valkyrie migrate fully;
globalhawk's reusable Home Manager configuration migrates, but its service,
Kubernetes, observability, storage, and hardware trees do not.

## Motivation

The current repository has valid Nix modules, but composition is spread across
several unrelated mechanisms:

- `flake.nix` repeats nearly identical NixOS and Darwin host construction.
- `machine/*/home.nix` manually imports programs for each host.
- `role/` groups some Home Manager programs, but hosts still mix roles and
  direct program imports.
- A cross-environment concern such as Fish is loaded separately into system and
  Home Manager evaluations and contains environment detection to compensate.
- Host facts such as platform and primary user are manually copied into each
  output, while hostname remains in the machine module.

The desired unit of selection is a concern, not an evaluation class. Selecting
Fish once should apply its NixOS or Darwin integration and its primary user's
Home Manager configuration. Selecting a Home-Manager-only tool should simply
contribute nothing to the system class.

Den demonstrates this aspect-oriented model, but its entity, policy, context,
and resolution machinery is more abstraction than this repository needs. A
small local module system provides the required behavior while retaining
flake-parts and familiar Nix modules.

## Design principles

1. **Explicit activation.** Adding a file has no effect. A reachable inventory
   or profile must include the aspect through a literal Nix path.
2. **One concern, multiple classes.** The NixOS, Darwin, and Home Manager parts
   of a concern live in one aspect file or directory.
3. **Native overrides.** Selected aspects enable conventional options with
   `lib.mkDefault`. A host disables or changes behavior through the native
   option, such as `programs.fish.enable = false`.
4. **Inventory facts are authoritative.** Platform, hostname, primary user, and
   state versions are declared once and translated into target-module options.
5. **No host-fact `specialArgs`.** Target modules read ordinary configuration
   options. Only `inputs` remains a general special argument.
6. **Incremental migration.** Existing machine modules remain valid entries in
   the inventory and need not become aspects during this project.
7. **Static reachability.** Aspect and profile edges use literal paths so
   `misc/find_dead_nix.py` can follow them.

## Flake entry point and module layout

`flake.nix` retains the input declarations and becomes a small flake-parts
entry point whose configuration imports `./modules/flake`.

The flake modules are split by responsibility:

```text
modules/flake/
  default.nix       Imports the schema, inventory, and output constructor
  schema.nix        Declares dotfiles inventory and aspect module options
  inventory.nix     Declares machines and literal aspect/module paths
  outputs.nix       Resolves aspects and constructs flake outputs
```

All four files are flake-parts modules or modules in the nested aspect module
system. Helper functions may be local implementation details of these modules;
they do not become another public factory API.

## Unified machine inventory

The inventory is the sole source of machine identity:

```nix
dotfiles = {
  sharedAspects = [
    ../../aspect/common-cli.nix
  ];

  hosts.nighthawk = {
    class = "darwin";
    system = "aarch64-darwin";
    primaryUser = "abe";
    stateVersion = {
      system = 5;
      home = "24.05";
    };
    aspects = [
      ../../aspect/development.nix
      ../../aspect/darwin-desktop.nix
    ];
    modules = [../../machine/nighthawk];
  };

  hosts.valkyrie = {
    class = "nixos";
    system = "x86_64-linux";
    primaryUser = "abe";
    stateVersion = {
      system = "26.05";
      home = "26.05";
    };
    aspects = [
      ../../aspect/ai-agents
      ../../aspect/plasma-desktop.nix
    ];
    modules = [../../machine/valkyrie];
  };
};
```

Each `hosts` entry has these fields:

| Field | Meaning |
| --- | --- |
| `class` | One of `nixos`, `darwin`, or `homeManager`. |
| `system` | Nix platform used for evaluation and package selection. |
| `hostName` | Network/inventory hostname; defaults to the entry's attribute name. |
| `primaryUser` | User receiving the default Home Manager aspect projection. |
| `stateVersion.system` | Required for NixOS and Darwin; absent for Home-Manager-only hosts. |
| `stateVersion.home` | Required Home Manager state version. |
| `aspects` | Explicit host-specific aspect modules, normally literal paths. |
| `modules` | Existing system modules and external integration modules. |
| `homeModules` | Existing primary-user HM modules; a migration and escape hatch. |

`dotfiles.sharedAspects` is prepended to every host's aspect list. This is
where the familiar cross-machine CLI environment is selected once.

The flake-parts `systems` value is derived from inventory systems. An optional
`dotfiles.extraSystems` list defaults to empty and exists only for package or
check outputs that require a platform with no declared host.

### Home-Manager-only machines

A user-mode machine uses the same inventory rather than a separate namespace:

```nix
dotfiles.hosts.work-laptop = {
  class = "homeManager";
  system = "x86_64-linux";
  primaryUser = "abe";
  stateVersion.home = "26.05";
  aspects = [../../aspect/development.nix];
};
```

It produces `homeConfigurations."abe@work-laptop"`, applies shared and
host-specific Home Manager aspects, and does not construct a system evaluation.
The output name may gain an explicit override only if a real deployment needs
one; it is not part of the initial schema.

## Aspect module system

An aspect is a module evaluated by a small nested module system. Its schema has
three mergeable deferred-module options:

- `nixos`
- `darwin`
- `homeManager`

All default to empty modules. Multiple selected aspects merge their
contributions using normal module semantics.

A simple cross-environment aspect looks like:

```nix
{lib, ...}: {
  nixos = {
    config,
    pkgs,
    ...
  }: {
    programs.fish.enable = lib.mkDefault true;
    users.users.${config.meta.user}.shell = lib.mkDefault pkgs.fish;
  };

  darwin = {
    config,
    pkgs,
    ...
  }: {
    programs.fish.enable = lib.mkDefault true;
    users.users.${config.meta.user}.shell = lib.mkDefault pkgs.fish;
  };

  homeManager = {
    imports = [./module.nix];
    programs.fish.enable = lib.mkDefault true;
  };
}
```

An aspect may omit any unsupported class. For example, modern interactive CLI
tools normally contribute only Home Manager configuration. The selection of
the aspect is its activation mechanism; a new aspect-specific `enable` option
is added only when the concern has genuine parameterization beyond selection.

Profiles are composition-only aspect modules with literal imports:

```nix
{
  imports = [
    ./fish
    ./modern-cli.nix
    ./starship.nix
  ];
}
```

This supports both leaf selection and higher-level profiles without strings,
filesystem discovery, or a global name registry.

## Aspect resolution and output construction

For each inventory entry, the constructor evaluates:

```text
aspect schema + shared aspects + host aspects
```

It then routes the result according to the host class:

| Host class | System projection | User projection | Output |
| --- | --- | --- | --- |
| `nixos` | `resolved.nixos` | `resolved.homeManager` | `nixosConfigurations.<host>` |
| `darwin` | `resolved.darwin` | `resolved.homeManager` | `darwinConfigurations.<host>` |
| `homeManager` | none | `resolved.homeManager` | `homeConfigurations."<user>@<host>"` |

NixOS and Darwin constructors add Home Manager for the primary user and
centralize:

- `home-manager.useGlobalPkgs`
- `home-manager.useUserPackages`
- Home Manager special arguments
- common system and Home Manager infrastructure modules
- system configuration revision

Special or service-owned users do not inherit primary-user aspects
automatically. Their Home Manager modules remain explicit. The globalhawk AI
agent sandbox continues to opt into the AI-agent aspect for its sandbox user
and operator, with its literal module path updated during migration.

## Host facts

The constructor translates inventory facts into ordinary target options.

For NixOS and Darwin it defines:

- the host platform from `system`
- `networking.hostName` from `hostName`
- `meta.user` from `primaryUser`
- the class-appropriate system state version
- `system.configurationRevision` when the flake has a revision

For the primary Home Manager evaluation it defines:

- `meta.user`
- `meta.hostName`
- `home.stateVersion`
- the existing default username and platform-derived home directory

Machine and aspect modules read `config.meta.user`,
`config.meta.hostName`, `config.networking.hostName`, or platform data from
`pkgs.stdenv.hostPlatform`. They do not receive a second host record through
`specialArgs`.

Inventory facts use normal option priority because the inventory is
authoritative. Aspect-provided enablement uses `lib.mkDefault` so explicit
machine or Home Manager configuration can override it conventionally.

Darwin presentation settings such as `networking.computerName` remain in a
Darwin aspect. They are not promoted to inventory facts without a demonstrated
cross-host need.

## Package sets, overlays, and `lib`

The shared unstable, local-package, and LLM-agent overlays remain defined once
and are installed by constructor-owned module plumbing. The standalone Home
Manager constructor uses the same overlay definition instead of copying it into
another output block.

This migration does not force all hosts to consume one preconstructed `pkgs`
value. Globalhawk still has a host-specific `packageOverrides` definition, so
centralizing `nixpkgs.pkgs` would either change its behavior or expand this
project into a separate package-set migration.

The root `lib/` has no active consumers beyond the extension created in
`flake.nix`. It is removed together with `mkLib`, `hmLib`, and the custom `lib`
special argument. Each target receives the normal library for its module class.
The unrelated local helper `machine/globalhawk/k3s/lib.nix` remains in place.

## Target repository boundaries

```text
flake.nix                    Inputs and the flake-parts entry point

aspect/                      Explicitly selectable, opinionated concerns
  common-cli.nix
  development.nix
  shell-utilities.nix
  fish/
  git/
  ai-agents/
  darwin-desktop.nix
  plasma-desktop.nix

machine/                     Hardware and genuinely host-specific deployment config
  globalhawk/
  nighthawk/
  valkyrie/

modules/
  flake/                     Inventory, aspect schema, and constructors
  common/                    Shared target-module plumbing
  common-hm/
  nixos/                     Reusable option-bearing NixOS infrastructure
  darwin/                    Reusable nix-darwin infrastructure

packages/                    Locally built software and scripts
```

The ownership rule is:

- `aspect/`: selected behavior for machines or users.
- `machine/`: configuration that exists because of a specific deployment or
  physical machine.
- `modules/`: reusable infrastructure that declares an option API or supports
  composition.
- `packages/`: buildable software.

The current `program/` and `role/` directories are migration sources. Their
content moves into `aspect/`, including option modules that belong to a
specific concern, and the empty directories are removed. Hostname-named aspects
may replace `machine/` in a later project, but are explicitly out of scope here.

## Dead-Nix reachability

The aspect design is intentionally compatible with the static detector:

- Inventory aspect and module selections are literal path expressions.
- Profile composition uses literal `imports` paths.
- No aspect path is synthesized from a string.
- No `readDir` or import-tree behavior activates aspects.

The current detector nevertheless reports four live chart definitions as dead.
`machine/globalhawk/k3s/default.nix` passes the literal `../../../charts`
directory to `nixidy.chartsDir`, and nixidy recursively discovers chart
`default.nix` files internally. The parser sees the directory path, but the
detector currently resolves a directory only to a root `default.nix`; the chart
directory has none.

Before using the detector as the migration cleanup gate, change its directory
handling as follows:

1. A referenced directory with a root `default.nix` retains the existing module
   import behavior and follows that file.
2. A referenced directory without a root `default.nix` is treated as a dynamic
   Nix root, and its `.nix` descendants are reachable.
3. Non-Nix descendants remain ignored.

This conservative rule prevents live dynamically loaded Nix definitions from
being deleted while retaining precise module-directory traversal for normal
imports. Extend `misc/test_find_dead_nix.py` with a `chartsDir`-style literal
directory reference containing nested chart definitions.

Also retain a test proving that a literal `.nix` path stored in an arbitrary
aspect option is reachable even when it does not appear under an `imports`
attribute. The parser operates on path nodes rather than option semantics, so
this is the contract on which inventory activation relies.

## Validation and errors

The inventory schema and constructor reject invalid declarations during
evaluation:

- `class` must be `nixos`, `darwin`, or `homeManager`.
- Darwin hosts require a Darwin platform.
- NixOS hosts require a Linux platform.
- Hostname and primary user must be non-empty.
- Home Manager state version is required and must be a string.
- Darwin system state version is required and must be an integer.
- NixOS system state version is required and must be a string.
- Home-Manager-only hosts must not define a system state version.

An aspect that omits a target class is valid and contributes an empty module.
Errors inside a selected aspect retain the aspect file's source location through
normal module evaluation.

## Migration sequence

1. Capture baseline evaluation results and non-activating builds from the
   current working tree. Preserve the existing uncommitted valkyrie changes.
2. Fix and test dynamic-directory handling in `misc/find_dead_nix.py`.
3. Add the flake inventory, aspect schema, and output constructors while
   retaining the existing machine modules.
4. Declare globalhawk, nighthawk, and valkyrie in the inventory with unchanged
   output names, host facts, and state versions.
5. Migrate reusable `program/` and `role/` content into leaf and profile aspects.
6. Move nighthawk and valkyrie completely onto selected aspects.
7. Move globalhawk's reusable Home Manager configuration onto shared aspects,
   but leave its system service trees unchanged.
8. Update the AI-agent sandbox's explicit shared module path.
9. Remove obsolete host-fact definitions, repeated Home Manager wiring,
   `program/`, `role/`, the unused root `lib/`, and dead compatibility modules.
10. Reduce `flake.nix` to inputs and the flake-parts entry-module import.
11. Format, evaluate, build, and run the dead-file detector.

The agreed intentional behavior change is that the shared modern CLI profile
becomes available on every current machine. Other settings should remain
equivalent.

## Verification

No configuration is activated during this project. Validation uses `build`, not
`switch`.

Run:

```sh
nix fmt
nix flake check
uv run misc/find_dead_nix.py
cd misc && uv run --with pytest pytest test_find_dead_nix.py
```

Build all active configurations on appropriate builders or hosts:

```sh
nix build .#darwinConfigurations.nighthawk.system
nix build .#nixosConfigurations.valkyrie.config.system.build.toplevel
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
```

The full globalhawk build remains the required baseline because it exercises
the nixidy workloads and dynamically discovered charts. If the current machine
cannot build a target platform and no remote builder is available, report that
limitation explicitly and do not represent the configuration as fully built.

Add one focused flake check for the reusable constructor's currently inactive
`homeManager` host-class path. It must evaluate a selected Home Manager aspect
through the real constructor and assert its observable option result. The live
NixOS and Darwin outputs exercise the other two routing paths; no structural
file-shape tests or additional test harnesses are added.

Finally inspect evaluated values for:

- output names
- platform and hostname
- primary user and home directory
- system and Home Manager state versions
- shared CLI activation
- class-specific native overrides

`misc/find_dead_nix.py` must report no dead tracked `.nix` files after cleanup,
including no false positives under `charts/`.

## Non-goals

- Adopting Den, import-tree, or another configuration framework.
- Automatically activating files based on directory contents.
- Converting globalhawk services, Kubernetes workloads, observability, storage,
  or hardware configuration into aspects.
- Replacing `machine/` with hostname-named aspects in this migration.
- Activating any generated configuration.
- Redesigning secrets or copying encrypted secret values into unencrypted files.
- Centralizing all hosts onto one preconstructed Nixpkgs package set.

## Appendix: transparent target-module imports

**Status:** feasible, deferred; this appendix is not part of the approved
implementation scope.

A later extension could allow an aspect to be imported directly from a NixOS,
nix-darwin, or Home Manager module and automatically apply the projection for
that evaluator. This is technically possible through the Nix module system's
built-in `_class` module argument. The constructors pinned by this repository
evaluate their modules with `_class` set to `"nixos"`, `"darwin"`, and
`"homeManager"`, respectively. Module arguments of this kind are available
while resolving `imports`, so choosing a projection from `_class` does not
require a configuration-dependent import.

The existing aspect attribute set cannot provide this behavior by itself. If a
native evaluator imports an unmodified aspect, it interprets `nixos`, `darwin`,
and `homeManager` as native configuration options and reports that they do not
exist. A future adapter such as `mkAspect` would instead return a module
function that dispatches according to `_class`.

The aspect evaluator should then use an explicit `class = "aspect"` rather than
the current unclassified `lib.evalModules` call. In the aspect class, the
adapter would expose the complete aspect definition. In a target class, it
would resolve the aspect and its imported profiles through the aspect module
system, then import `resolved.${_class}` into the target evaluator. Conceptually:

```nix
mkAspect ({lib, ...}: {
  imports = [./another-aspect.nix];

  nixos = {/* NixOS module */};
  darwin = {/* nix-darwin module */};
  homeManager = {/* Home Manager module */};
})
```

A throwaway feasibility probe evaluated this form through the repository's
pinned NixOS, nix-darwin, and Home Manager constructors and observed the three
expected class names. A second probe compared a profile importing a leaf aspect
through both routes:

```text
resolve aspect, then apply projection
directly import transparent aspect
```

The resulting projection values were equal in all three target classes. The
probe also found that merely forwarding the aspect's `imports` and selected
projection as sibling native imports can change order-sensitive merges. A
production adapter should preserve the nested aspect evaluation rather than
perform that simpler rewrite.

Transparency would be limited to the current evaluator. Importing an aspect
from a NixOS module would select its NixOS projection, but would not inherently
insert its Home Manager projection into Home Manager's separate nested module
evaluation. The inventory constructor would still be needed to route the
primary user's projection, unless the importing module explicitly adds the
aspect to `home-manager.users.<user>` or `home-manager.sharedModules`. Such an
automatic bridge would require a policy decision about which users inherit the
aspect and should not be implied by projection dispatch alone.

Aspect-level `nixpkgs` overlays and configuration also require explicit
routing. NixOS and nix-darwin can apply them to their native package set, and a
standalone Home Manager evaluation can construct a private package set from
them. Home Manager embedded with `home-manager.useGlobalPkgs` cannot modify the
already constructed system package set. The inventory constructor therefore
remains the natural owner of complete cross-environment selection and package
policy even if direct target-module imports are added.

If revisited, the preferred direction is an opt-in adapter that gives aspects
native-module behavior within one evaluator while retaining the inventory
constructor as the authoritative cross-environment composition path. In this
meaning, "transparent" means selecting the projection for the current module
class; it does not mean automatically crossing into related module evaluations.
