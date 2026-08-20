# Portable aspect library and machine composition roots

**Status:** approved
**Date:** 2026-08-20

## Relationship to the existing design

This design refines the aspect-oriented flake-parts architecture documented in
`2026-08-19-aspect-oriented-flake-parts-design.md`. It supersedes that design's
ownership boundaries for `modules/flake`, `modules/common`,
`modules/common-hm`, `modules/darwin`, machine entry points, host state
versions, and native module escape hatches. The existing principles of explicit
literal-path activation, native overrides, cross-environment aspects, and
static reachability remain in force.

## Goal

Make `modules/aspect` a portable flake-parts module that can be imported into
another Nix flake without depending on this repository's machines, packages,
overlays, or policy modules.

Importing the library and defining `dotfiles` inventory must be sufficient to
derive NixOS, nix-darwin, and standalone Home Manager outputs. This
repository's opinions are explicitly selected aspects rather than constructor
plumbing.

Machines become aspect composition roots. The central inventory retains host
identity and routing facts, but host-specific aspect selection and all native
module composition live under `machine/<name>`.

## Design principles

1. **The library owns construction.** The portable module resolves selected
   aspects, constructs each deployment class, derives `systems`, and defines
   mergeable flake outputs.
2. **Policy remains explicit.** The library's only enabled-by-default behavior
   maps normalized host facts to native target options. Package policy, Home
   Manager preferences, Darwin policy, and machine behavior remain selected
   aspects.
3. **Host context is ordinary configuration.** Target modules read a
   read-only `config.dotfiles.host` record. Host facts are not threaded through
   `specialArgs`.
4. **Native configuration remains native.** State versions and external native
   modules live in NixOS, Darwin, or Home Manager aspect projections rather
   than being translated from custom escape-hatch fields.
5. **Composition stays plural.** Both shared and per-host aspect selections are
   lists, even when this repository normally selects one composition root.
6. **Consumers do not inherit maintainer tests.** The public library module
   does not inject the library's own test suite into importing flakes.

## Public flake-parts module

`modules/aspect` is a complete flake-parts module:

```text
modules/aspect/
  default.nix
  schema.nix
  aspect-options.nix
  constructors.nix
  outputs.nix
  target/
    host-context.nix
  default-aspects/
    host-facts.nix
```

The public default module imports the schema and output construction. It does
not import repository checks or refer to paths outside `modules/aspect`.

The library declares `flake.nixosConfigurations`,
`flake.darwinConfigurations`, and `flake.homeConfigurations` as lazy attribute
sets of raw configuration values. This allows its generated outputs to coexist
with unrelated configurations defined by other flake-parts modules instead of
claiming each whole output attribute through flake-parts' unique raw fallback.

For each host, the library evaluates the aspect schema together with
`dotfiles.sharedAspects` and that host's `aspects`, then routes the resolved
projections to the appropriate constructor.

## Inventory API

The portable inventory is:

```nix
dotfiles = {
  defaultAspects.enable = true;

  providers = {
    nixpkgs = inputs.nixpkgs;
    homeManager = inputs.home-manager;
    darwin = inputs.darwin;
  };

  sharedAspects = [../../aspect/shared.nix];
  extraSystems = ["x86_64-darwin"];

  hosts.nighthawk = {
    class = "darwin";
    system = "aarch64-darwin";
    hostName = "nighthawk";
    user = "abe";
    aspects = [../../machine/nighthawk];
  };
};
```

Host fields are:

| Field | Meaning |
| --- | --- |
| `class` | One of `nixos`, `darwin`, or `homeManager`. |
| `system` | Nix platform used to construct the configuration. |
| `hostName` | Host identity; defaults to the inventory attribute name. |
| `user` | Primary user receiving the default Home Manager projection. |
| `aspects` | Explicit host-specific aspect modules. |

`primaryUser` is renamed to `user`. The old `stateVersion`, `modules`, and
`homeModules` inventory fields are removed.

`sharedAspects` and each host's `aspects` remain lists. This avoids an
artificial top-level restriction and permits normal module composition without
requiring wrapper files, while allowing composition roots as the usual style.

Provider values are raw flake input values and default to the conventional
input names. Consumers may override them when their inputs use different
names. Nixpkgs and Home Manager are required because every deployment class
constructs a Home Manager configuration. The Darwin provider defaults to
`inputs.darwin or null` and is required only when constructing a Darwin host.
The library passes the consuming flake's complete `inputs` set to outer
aspects and target modules; it does not pass host records through
`specialArgs`.

## Target host context

Every constructed NixOS, Darwin, and Home Manager evaluation includes an
internal module declaring a read-only host context:

```nix
config.dotfiles.host = {
  class = "darwin";
  system = "aarch64-darwin";
  hostName = "nighthawk";
  user = "abe";
};
```

This record contains normalized facts only. It deliberately excludes the
`aspects` selection because deferred module values are construction edges, not
host facts. Exposing them would leak module functions into target
configuration and risk self-reference while those modules are being resolved.

The old `meta.user`, `meta.hostName`, and unused `meta.isWSL` options are
removed. Downstream modules use `config.dotfiles.host.user`,
`config.dotfiles.host.hostName`, `config.dotfiles.host.system`, and
`config.dotfiles.host.class`.

The host context declaration and injection are unconditional library plumbing.
They remain available when default aspects are disabled.

## Default host-facts aspect

The library prepends one bundled aspect by default. It maps the normalized host
context to native target options:

- NixOS sets `nixpkgs.hostPlatform` and `networking.hostName`.
- Darwin sets `nixpkgs.hostPlatform`, `networking.hostName`, and
  `system.primaryUser`.
- Home Manager sets `home.username` and a default home directory derived from
  the target platform and user.
- System projections set `system.configurationRevision` when the source flake
  exposes a revision.

Inventory-derived identity mappings use normal module priority because the
host record is authoritative. A consumer changes them by changing inventory or
disabling the default aspects. The platform-derived Home Manager home directory
uses `lib.mkDefault` so a machine can select a nonstandard directory without
replacing the other fact mappings.

The library uses `dotfiles.defaultAspects.enable`, defaulting to `true`, to
control this behavior. When disabled, it still provides
`config.dotfiles.host`, but the consumer is responsible for mapping facts to
native options.

The constructor necessarily uses `host.system` to construct standalone Home
Manager's package set and `host.user` as the embedded Home Manager user key.
Those are structural inputs, not duplicated native fact mappings.

## Aspect schema and package-set projection

An aspect has four mergeable projections:

```nix
{
  nixpkgs = {
    overlays = [];
    config = {};
  };
  nixos = {};
  darwin = {};
  homeManager = {};
}
```

The `nixos`, `darwin`, and `homeManager` values are deferred native modules.
The `nixpkgs` projection carries mergeable overlays and package configuration.

NixOS and Darwin receive the resolved package policy through their native
`nixpkgs` options. Standalone Home Manager imports its package set using the
same resolved overlays and configuration before evaluating Home Manager
modules. Embedded Home Manager uses the system's global package set. This
ensures one selected package-policy aspect behaves consistently across all
three deployment classes.

The NixOS and Darwin constructors always install the corresponding Home
Manager integration module from the configured provider. This is deployment
plumbing needed to route the `homeManager` projection, not repository policy.

## Machine composition roots

Each machine directory becomes an aspect. For example:

```nix
# machine/nighthawk/default.nix
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

The current native `default.nix` becomes `darwin.nix` or `nixos.nix`. Home
Manager leaves remain `home.nix`. Globalhawk's service, Kubernetes,
observability, storage, hardware, and sandbox trees stay unchanged behind its
`nixos.nix` projection.

External modules such as Determinate Nix are imported inside the appropriate
native projection. This makes the inventory's `modules` and `homeModules`
escape hatches unnecessary.

State versions belong to machine history and are set through native target
options in each machine composition root. They are not reusable aspects and
are not inventory facts. Reusable aspects should explicitly set behavior they
depend on rather than assuming a state-version-dependent default. An aspect may
branch on a native state version or assert a minimum only when compatibility
requires it.

State versions select compatibility defaults in the current target modules;
they do not select an older option schema. Available and renamed options are
determined by the pinned NixOS, Home Manager, and nix-darwin inputs.

## Repository policy aspects

Repository-specific behavior moves to explicitly selected aspects:

```text
aspect/
  shared.nix
  nixpkgs/
  home-manager.nix
  darwin-system.nix
  darwin-desktop/
    default.nix
    module.nix
    defaults-writer.nix
```

`aspect/shared.nix` imports the Nixpkgs policy, Home Manager preference, and
common CLI profile. It is normally the sole entry in `sharedAspects`.

The Nixpkgs aspect owns the unstable and local overlays plus `allowUnfree`.
The Home Manager aspect enables `programs.home-manager` with `lib.mkDefault`.
The Darwin system aspect owns Determinate Nix management and cache trust
policy. It is selected by Nighthawk rather than installed by the constructor.

The custom defaults-writer module moves beside `darwin-desktop`, its only
consumer. The desktop aspect imports that option-bearing support module itself.
Desktop preferences remain independent from Nix installation and cache policy.

After migration, `modules/common`, `modules/common-hm`, and `modules/darwin`
are removed.

## Repository flake module

`modules/flake` contains only this repository's configuration:

```text
modules/flake/
  default.nix
  inventory.nix
  outputs.nix
  checks.nix
```

`inventory.nix` declares providers, shared aspect roots, host facts, and
machine aspect roots. `outputs.nix` defines the formatter, local packages, and
repository-specific per-system checks. It performs no aspect resolution or
host construction.

`checks.nix` exercises the portable library in this repository but is not
imported by `modules/aspect/default.nix`. Another flake importing the public
aspect module does not receive maintainer tests in its outputs.

## Validation and errors

The library validates nonempty host identity fields, deployment class and
platform compatibility, and provider availability before construction. Error
messages identify the failing `dotfiles.hosts.<name>` path. Native module
systems validate their own state-version values and target options.

Focused flake checks cover reusable behavior:

- A synthetic standalone Home Manager host verifies output generation, host
  context, native overrides, and the Nixpkgs projection.
- A synthetic host with default aspects disabled verifies that host context
  remains available while native fact mappings are absent unless supplied by
  the consumer.
- Generated flake configuration options compose with unrelated definitions.
- Current hosts expose the expected class, platform, hostname, user, native
  state versions, and selected behavior.

Repository validation also runs the dead-Nix detector, `nix flake check`, and
non-activating host builds. Literal inventory and aspect-import paths keep all
machine and aspect roots statically reachable. No validation step activates a
configuration.

## Resulting ownership

- `modules/aspect`: portable construction library, host context, and bundled
  default fact mapping.
- `aspect`: explicitly selected repository policy and reusable behavior.
- `machine`: host-specific aspect composition roots and native leaves.
- `modules/flake`: this repository's inventory, outputs, and library tests.
- `packages`: locally built software and scripts.

The resulting flake entry point imports the portable aspect module and the
repository flake module. Defining inventory is sufficient to generate all
deployment outputs; no consumer-owned output-construction block is required.
