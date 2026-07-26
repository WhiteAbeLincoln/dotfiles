# NixOS-integrated k3s workload authoring

**Status:** Approved design
**Date:** 2026-07-25

## Goal

Integrate the repository's Kubernetes workload authoring with the Globalhawk
NixOS module evaluation so host and cluster concerns can be colocated, share
typed configuration, and use normal Nix module merging.

The resulting public interface must hide nixidy as an implementation detail.
Nixidy remains the initial renderer, but switching renderers later must not
require renaming the surrounding NixOS options.

## Current problems

Globalhawk currently has two independent module evaluations:

- `nixosConfigurations.globalhawk` evaluates the host's NixOS modules.
- `nixidyEnvs.x86_64-linux.globalhawk` separately evaluates `k8s/**`.

The evaluations are joined manually in `flake.nix`. It imports
`machine/globalhawk/facts.nix` and `secrets/globalhawk.nix`, then copies an
ever-growing list of values into nixidy through `_module.args`.

This causes several problems:

- `flake.nix` grows whenever another value must cross the boundary.
- NixOS and nixidy modules cannot naturally inspect each other's evaluated
  configuration.
- `facts.nix` acts as an untyped substitute for shared module options.
- A native service and its Kubernetes bridge live in separate files even when
  the bridge exists only to expose that service.
- Kubernetes Secret manifests repeat activation-time sops templating boilerplate.

Nixidy itself already uses the Nix module system, including typed Kubernetes
resource options and standard override semantics. The missing piece is an
intentional bridge between its module evaluation and NixOS.

## Chosen architecture

NixOS owns a nested workload-renderer module evaluation:

```text
NixOS module evaluation (`config`)
  |
  | merges `services.k3s.workloads.module`
  | passes the outer configuration as `nixosConfig`
  v
Nested workload evaluation (`evaluatedConfig`)
  |
  | renders manifests
  v
`services.k3s.manifests.nixidy.source`
```

The two evaluations retain separate namespaces. Workload modules use their
normal `config` argument for renderer configuration and receive the complete
host configuration as `nixosConfig`. NixOS modules can inspect the nested result
through a read-only option.

This is preferable to importing nixidy's modules directly into the NixOS option
tree. Direct import would couple the repository to nixidy's internal module
layout and extended library. A nested evaluation uses nixidy's supported
`mkEnv` interface and preserves a clean boundary.

## Public NixOS interface

The integration extends the existing k3s namespace:

```nix
services.k3s.workloads = {
  enable = true;

  # Mergeable deferred module evaluated by the current workload renderer.
  module = {};

  # Read-only results.
  evaluatedConfig = {};
  renderedPackage = null;
};
```

The intended option types are:

- `enable`: boolean.
- `module`: `lib.types.deferredModule`.
- `evaluatedConfig`: read-only raw evaluated renderer configuration.
- `renderedPackage`: read-only package containing rendered manifests.

The option namespace deliberately does not mention nixidy. Only the integration
module calls `inputs.nixidy.lib.mkEnv`. A future renderer migration will still
require translating renderer-specific module bodies, but it will not require
renaming the host-facing interface.

`workloads` is preferred over `resources`: the nested definitions may include
applications, Helm rendering, raw YAML, transforms, namespaces, and Kubernetes
objects rather than only resource objects.

## Colocated service modules

An ordinary NixOS module may define a native service and contribute its
Kubernetes bridge from the same file:

```nix
{
  config,
  ...
}: {
  services.adguardhome = {
    enable = true;
    port = 3000;
  };

  services.k3s.workloads.module = {
    applications.adguard = {
      namespace = "network";
      resources.services.adguard.spec.ports.http = {
        port = 80;
        targetPort = config.services.adguardhome.port;
      };
    };
  };
}
```

The deferred module is created inside the outer module, so it may close over the
outer `config`. The integration also supplies `nixosConfig` explicitly so
reusable or cluster-focused modules can make the boundary clear:

```nix
services.k3s.workloads.module = {nixosConfig, ...}: {
  applications.adguard.resources.services.adguard.spec.ports.http.targetPort =
    nixosConfig.services.adguardhome.port;
};
```

Multiple NixOS modules may define `services.k3s.workloads.module`.
`types.deferredModule` combines those definitions into the nested evaluation.
Inside that evaluation, nixidy continues to provide typed resource options and
normal `mkDefault`, `mkMerge`, `mkForce`, and `mkIf` behavior.

Existing `k8s/*.nix` modules may be imported through the deferred module during
an incremental migration. Colocation is the desired end state where host and
cluster concerns form one service, not a requirement to combine unrelated
cluster infrastructure into host modules.

## Integration module responsibilities

The integration module:

1. Evaluates `services.k3s.workloads.module` using the current renderer.
2. Uses the NixOS host's `pkgs`.
3. Passes the outer NixOS configuration as `nixosConfig`.
4. Supplies renderer base configuration such as charts, target placeholders,
   and environment name.
5. Exposes the nested `config` as `workloads.evaluatedConfig`.
6. Exposes the rendered environment as `workloads.renderedPackage`.
7. Combines rendered YAML into the existing single multi-document manifest.
8. Assigns that manifest to the existing
   `services.k3s.manifests.nixidy.source`.

The internal manifest key remains `nixidy` during this behavior-preserving
migration. Its name is load-bearing for k3s Addon ownership and pruning, but it
is not part of the new authoring interface.

The combined manifest must continue to:

- exclude nixidy's `apps/` directory containing Argo CD Application resources;
- follow symlinked rendered application directories;
- use deterministic file ordering;
- remain one always-present file so content changes prune removed resources.

## Ownership and dependency rules

The component that creates or manages a value owns its option.

Host-owned values flow from NixOS into workloads:

- native listener ports;
- service UIDs and GIDs;
- host paths and directory ownership;
- network addresses and interfaces;
- sops declarations;
- the system timezone.

Workload-owned values remain in the nested module:

- namespaces;
- container ports;
- images;
- Deployments, Services, Ingresses, policies, and other cluster resources.

Shared policy with no natural service owner belongs in a small typed NixOS
namespace such as `homelab.*`, not in an untyped facts file.

The preferred dependency direction is:

```text
host-owned configuration
  -> workload configuration
  -> optional derived host behavior
```

NixOS modules may inspect `workloads.evaluatedConfig` for additive derived
behavior such as assertions, DNS records, or monitoring. Foundational host
values must not be derived from cluster definitions that consume those same
values. That rule prevents logical evaluation cycles.

Nix laziness permits useful references in both directions, but it cannot make an
actual circular definition meaningful. Arbitrary recursion errors cannot always
be replaced with targeted assertions, so the acyclic ownership rule is part of
the module contract.

## Replacing `facts.nix`

`machine/globalhawk/facts.nix` is removed gradually as values gain natural
owners:

- media, Immich, and Authelia UIDs move to the modules declaring those users;
- service state and storage paths move to typed service or storage options;
- native ports remain with their service modules;
- k3s pod and service CIDRs move to cluster-specific options;
- workload timezone references `config.time.timeZone`;
- ingress suffix and stable LAN addressing remain typed shared homelab options
  because no individual service owns them.

This is an incremental cleanup, not a prerequisite for introducing the bridge.

## Runtime Kubernetes Secrets

Runtime secrets remain NixOS-owned. NixOS controls sops decryption and
activation, while workload definitions reference Kubernetes Secret names and
keys without seeing plaintext.

Add a renderer-independent companion interface:

```nix
services.k3s.runtimeSecrets.immich-db = {
  namespace = "immich";
  stringData.password.sopsSecret = "immich_db_password";
};
```

For values stored base64-encoded in sops:

```nix
services.k3s.runtimeSecrets.authelia-key = {
  namespace = "auth";
  data."issuer.pem".sopsSecret = "authelia_oidc_issuer_key";
};
```

The distinction is explicit:

- `stringData` consumes a normal scalar sops value.
- `data` consumes a sops value that is already base64 encoded.

For each declaration, the NixOS integration:

1. Confirms the referenced `sops.secrets` option exists.
2. Generates a Kubernetes Secret manifest containing sops placeholders.
3. Registers it as a sops-nix template.
4. Writes the substituted, root-only manifest into k3s's auto-deploy directory
   during activation.
5. Lets k3s apply it outside the store-rendered workload Addon.

Only placeholders enter evaluated template content. Secret plaintext is
substituted at activation and must never enter nixidy output or the Nix store.

Runtime Secret files remain separate from the combined workload manifest
because they contain activation-time material and have a different ownership
and lifecycle.

The interface validates:

- referenced sops secrets are declared;
- Kubernetes names, namespaces, and keys are valid;
- a key is not present in both `data` and `stringData`;
- generated output paths are unique and constrained to the intended manifest
  directory.

## Assertions and failure behavior

Evaluation should fail with a targeted message when:

- workload integration is enabled while `services.k3s.enable` is false;
- a runtime Secret references an undeclared sops secret;
- a runtime Secret key appears in both encodings;
- a generated name, namespace, key, or output path is invalid;
- generated runtime-secret files collide.

Renderer assertions and warnings continue to be enforced when build outputs are
accessed, as they are in nixidy today.

## Migration

The migration is behavior-preserving and incremental:

1. Add the workload integration module while retaining the flake-level nixidy
   environment.
2. Evaluate both paths and establish normalized manifest equivalence.
3. Move shared renderer base configuration and the YAML-combining bridge behind
   `services.k3s.workloads`.
4. Switch the existing `services.k3s.manifests.nixidy.source` to the new
   `renderedPackage`.
5. Colocate AdGuard first, proving a native service can own a port consumed by
   its Kubernetes bridge.
6. Migrate the remaining workload modules incrementally.
7. Replace hand-written sops Kubernetes Secret templates with
   `services.k3s.runtimeSecrets`.
8. Remove `nixidyEnvs.x86_64-linux.globalhawk` from `flake.nix`.
9. Dismantle `facts.nix` as each value gains a typed owner.

The live Addon name, single-file delivery shape, and prune behavior do not
change during the migration.

## Verification

Tests and checks assert observable behavior:

- Compare normalized Kubernetes objects from the old and new evaluations,
  ignoring only known ordering or generated-path differences.
- Build the complete Globalhawk NixOS configuration.
- Confirm runtime-secret templates contain placeholders and no plaintext.
- Evaluate fixtures where separate NixOS modules contribute workload
  definitions and the resulting Kubernetes objects contain both contributions.
- Verify `mkDefault`, `mkMerge`, and `mkForce` semantics inside the workload
  evaluation.
- Verify a colocated fixture consumes a NixOS-owned port, UID, path, and
  timezone.
- Verify invalid runtime-secret declarations produce focused evaluation errors.
- Run the existing live drift checker only as an operator verification step;
  automated design and module tests must not mutate the cluster.

After normalized manifest equivalence and a complete host build pass, the old
flake-level environment can be removed without changing live resources.

## Non-goals

- Providing one shared `config` namespace for NixOS and Kubernetes.
- Abstracting the renderer-specific Kubernetes resource schema.
- Replacing nixidy during this work.
- Changing k3s delivery, Addon ownership, or pruning behavior.
- Moving host-level services into Kubernetes merely to simplify integration.
- Putting secret plaintext into evaluation-time workload manifests.
