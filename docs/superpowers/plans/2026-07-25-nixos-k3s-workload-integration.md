# NixOS-integrated k3s Workloads Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Evaluate Kubernetes workloads from the Globalhawk NixOS configuration, support colocated host and cluster concerns, generate runtime Kubernetes Secrets from typed sops-backed declarations, and retire the manually shared `facts.nix`.

**Architecture:** A NixOS module owns a nested workload-renderer evaluation exposed as `services.k3s.workloads`; nixidy is the initial private backend. NixOS configuration flows into workload modules as `nixosConfig`, while the evaluated workload configuration and rendered package flow back through read-only options. A second NixOS module renders typed `services.k3s.runtimeSecrets` declarations through sops-nix at activation.

**Tech Stack:** NixOS module system, `lib.types.deferredModule`, nixidy `mkEnv`, k3s auto-deploy manifests, sops-nix templates, Bash integration tests, Python/PyYAML drift checker.

## Global Constraints

- Keep `services.k3s.manifests.nixidy` as one always-present Addon; its name and single-file pruning behavior are load-bearing.
- Keep nixidy private to the integration implementation; public options use `services.k3s.workloads.{module,evaluatedConfig,renderedPackage}`.
- Preserve nixidy's typed resource options and `mkDefault`, `mkMerge`, `mkForce`, and `mkIf` behavior.
- Secret plaintext must never enter nixidy output or the Nix store; only sops placeholders may appear during evaluation.
- The creator of a port, UID/GID, path, network value, or secret owns its option.
- Do not reconcile the new Libation/Audible and Audiobookshelf image versions or hashes; only update their tests to use the new rendered-package path.
- Do not remove or rewrite existing human-authored comments unless their referenced paths or behavior change.
- Tests must assert evaluated or rendered behavior rather than the textual shape of Nix source.

---

### Task 1: Add the nested workload integration module

**Files:**
- Create: `modules/nixos/k3s-workloads.nix`
- Create: `k8s/tests/workloads-module.nix`
- Modify: `flake.nix`

**Interfaces:**
- Consumes: `inputs.nixidy.lib.mkEnv`, the host `pkgs`, and `services.k3s.enable`.
- Produces: `services.k3s.workloads.enable :: bool`, `module :: deferredModule`, `evaluatedConfig :: raw`, and `renderedPackage :: package`.
- Supplies: `nixosConfig` to every nested workload module.

- [ ] **Step 1: Add a failing module-evaluation check**

Create `k8s/tests/workloads-module.nix`. Evaluate the real integration module with a minimal stub for the upstream k3s options. Contribute one Deployment from two separate outer modules: one sets `replicas = lib.mkDefault 1`, the other sets `replicas = lib.mkForce 2`. Read a host-owned port through `nixosConfig` into the rendered Service.

```nix
{
  inputs,
  pkgs,
}: let
  lib = pkgs.lib;
  evaluated = lib.evalModules {
    specialArgs = {inherit inputs pkgs;};
    modules = [
      ../../modules/nixos/k3s-workloads.nix
      {
        options = {
          services.k3s.enable = lib.mkOption {type = lib.types.bool;};
          services.k3s.manifests = lib.mkOption {
            type = lib.types.attrsOf lib.types.anything;
            default = {};
          };
          services.fixture.port = lib.mkOption {type = lib.types.port;};
        };
        config = {
          services.k3s.enable = true;
          services.fixture.port = 4321;
          services.k3s.workloads = {
            enable = true;
            module = {lib, ...}: {
              nixidy.target.repository = "file:///dev/null";
              nixidy.target.branch = "main";
              applications.fixture = {
                namespace = "default";
                resources = {
                  deployments.fixture.spec = {
                    replicas = lib.mkDefault 1;
                    selector.matchLabels.app = "fixture";
                    template = {
                      metadata.labels.app = "fixture";
                      spec.containers.fixture.image = "registry.invalid/fixture:test";
                    };
                  };
                  services.fixture.spec = {
                    selector.app = "fixture";
                    ports.http = {
                      port = 80;
                      targetPort = lib.mkDefault 80;
                    };
                  };
                };
              };
            };
          };
        };
      }
      ({lib, ...}: {
        services.k3s.workloads.module = {
          applications.fixture.resources.deployments.fixture.spec.replicas =
            lib.mkForce 2;
        };
      })
      ({config, ...}: {
        services.k3s.workloads.module = {nixosConfig, ...}: {
          applications.fixture.resources.services.fixture.spec.ports.http.targetPort =
            nixosConfig.services.fixture.port;
        };
      })
    ];
  };
  workload = evaluated.config.services.k3s.workloads;
in
  assert workload.evaluatedConfig.applications.fixture.resources.deployments.fixture.spec.replicas == 2;
  assert workload.evaluatedConfig.applications.fixture.resources.services.fixture.spec.ports.http.targetPort == 4321;
    pkgs.runCommand "k3s-workloads-module-test" {} ''
      test -e ${workload.renderedPackage}
      touch "$out"
    ''
```

Add the Linux-only check in `flake.nix`:

```nix
checks = pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  k3s-workloads-module = import ./k8s/tests/workloads-module.nix {
    inherit inputs pkgs;
  };
};
```

- [ ] **Step 2: Run the check and confirm the missing module failure**

Run:

```bash
nix build .#checks.x86_64-linux.k3s-workloads-module
```

Expected: failure because `modules/nixos/k3s-workloads.nix` does not exist.

- [ ] **Step 3: Implement the workload options and nested evaluation**

Create `modules/nixos/k3s-workloads.nix` with:

```nix
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.k3s.workloads;
  rendered = inputs.nixidy.lib.mkEnv {
    inherit pkgs;
    extraSpecialArgs.nixosConfig = config;
    modules = [
      {
        nixidy.env = "globalhawk";
        nixidy.target.rootPath = "./manifests/globalhawk";
      }
      cfg.module
    ];
  };
  combinedManifest = pkgs.runCommand "k3s-workloads-globalhawk.yaml" {} ''
    : > "$out"
    for file in $(${pkgs.findutils}/bin/find -L ${rendered.environmentPackage} \
      -name '*.yaml' -not -path '*/apps/*' | sort); do
      cat "$file" >> "$out"
      printf '\n---\n' >> "$out"
    done
  '';
in {
  options.services.k3s.workloads = {
    enable = lib.mkEnableOption "Nix-authored Kubernetes workloads rendered into the k3s auto-deploy lane";

    module = lib.mkOption {
      type = lib.types.deferredModule;
      default = {};
      description = "Workload-renderer module merged from NixOS service modules.";
    };

    evaluatedConfig = lib.mkOption {
      type = lib.types.raw;
      readOnly = true;
      description = "Evaluated workload-renderer configuration.";
    };

    renderedPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = "Directory containing the rendered Kubernetes manifests.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.k3s.enable;
        message = "services.k3s.workloads requires services.k3s.enable";
      }
    ];

    services.k3s.workloads = {
      evaluatedConfig = rendered.config;
      renderedPackage = rendered.environmentPackage;
    };

    # Keep this exact Addon key: changing it changes k3s object ownership.
    services.k3s.manifests.nixidy.source = combinedManifest;
  };
}
```

If the nixidy revision requires `nixidy.env` to be set with `lib.mkDefault`, use
`lib.mkDefault "globalhawk"` so a test environment can override it. Do not move
the combined manifest into `renderedPackage`; the latter remains the inspectable
per-application render tree used by tests and drift tooling.

- [ ] **Step 4: Run the focused check**

Run:

```bash
nix build .#checks.x86_64-linux.k3s-workloads-module
```

Expected: PASS, proving cross-evaluation reads and nested override semantics.

- [ ] **Step 5: Verify the disabled-k3s assertion**

Temporarily set `services.k3s.enable = false` in the fixture, then run the same
build. Expected: failure containing:

```text
services.k3s.workloads requires services.k3s.enable
```

Restore `true` and rerun the passing check.

- [ ] **Step 6: Commit**

```bash
git add flake.nix modules/nixos/k3s-workloads.nix k8s/tests/workloads-module.nix
git commit -m "feat(k3s): let host configuration own workload evaluation"
```

---

### Task 2: Move Globalhawk onto the nested evaluation

**Files:**
- Modify: `machine/globalhawk/default.nix`
- Modify: `machine/globalhawk/k3s.nix`
- Modify: `flake.nix`
- Modify: `k8s/tests/libation-jitter.sh`
- Modify: `k8s/tests/libation-runtime-contract.sh`
- Modify: `packages/k3s-drift.py`

**Interfaces:**
- Consumes: `services.k3s.workloads` from Task 1 and the unchanged `k8s/default.nix` module tree.
- Produces: `.#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage` as the canonical rendered-tree path.
- Preserves: `services.k3s.manifests.nixidy.source` and all current `_module.args`.

- [ ] **Step 1: Build and retain the legacy rendered path**

Run before changing the flake output:

```bash
legacy_out="$(nix build --no-link --print-out-paths \
  .#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage)"
printf '%s\n' "$legacy_out" >/tmp/globalhawk-legacy-workloads
```

Expected: one nix store path saved in `/tmp/globalhawk-legacy-workloads`.

- [ ] **Step 2: Import and configure the integration module**

Add `../../modules/nixos/k3s-workloads.nix` to
`machine/globalhawk/default.nix`'s imports.

In `machine/globalhawk/k3s.nix`, remove `inputs.self.nixidyEnvs`, the local YAML
combiner, and the explicit `manifests.nixidy.source`. Add:

```nix
  services.k3s.workloads = {
    enable = true;
    module = let
      secrets = import ../../secrets/globalhawk.nix;
      facts = import ./facts.nix;
    in {
      imports = [../../k8s];
      _module.args = {
        wireguardAddresses = secrets.wireguard_addresses;
        vpnServerCities = secrets.vpn_server_cities;
        acmeEmail = secrets.acme_email;
        smtpSender = secrets.mail.fromAddress;
        smtpUser = secrets.mail.smtpUser;
        inherit
          (facts)
          ingressSuffix
          podCidr
          serviceCidr
          hostGatewayIp
          mediaRoot
          mediaUid
          timezone
          immichUid
          autheliaUid
          smtp
          ;
      };
    };
  };
```

This compatibility block intentionally preserves all current inputs for the
first cutover. Later tasks replace them with owned NixOS options.

- [ ] **Step 3: Remove the flake-level environment and update consumers**

Delete `nixidyEnvs.x86_64-linux` from `flake.nix`.

Replace the old build target in both Libation test scripts and
`packages/k3s-drift.py` with:

```text
.#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage
```

Update adjacent comments to describe the NixOS-owned workload output. Do not
change any Libation or Audiobookshelf image tags, digests, packages, CronJob
commands, or runtime assertions.

- [ ] **Step 4: Build the new rendered tree and compare normalized objects**

Run:

```bash
new_out="$(nix build --no-link --print-out-paths \
  .#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage)"
legacy_out="$(cat /tmp/globalhawk-legacy-workloads)"
tmp_compare="$(mktemp -d)"

find -L "$legacy_out" -type f -name '*.yaml' -not -path '*/apps/*' -print0 |
  sort -z |
  xargs -0 nix run nixpkgs#yq-go -- -o=json '.' |
  jq -S -s 'sort_by(.apiVersion, .kind, .metadata.namespace // "", .metadata.name)' \
  >"$tmp_compare/legacy.json"

find -L "$new_out" -type f -name '*.yaml' -not -path '*/apps/*' -print0 |
  sort -z |
  xargs -0 nix run nixpkgs#yq-go -- -o=json '.' |
  jq -S -s 'sort_by(.apiVersion, .kind, .metadata.namespace // "", .metadata.name)' \
  >"$tmp_compare/new.json"

diff -u "$tmp_compare/legacy.json" "$tmp_compare/new.json"
```

Expected: no diff. If `xargs yq` does not preserve multi-document input, replace
the two pipelines with a short loop that appends each `yq -o=json '.'` result;
the normalized JSON values, not filenames or ordering, are the comparison.

- [ ] **Step 5: Run the affected tests and full evaluation**

Run:

```bash
bash k8s/tests/libation-jitter.sh
bash k8s/tests/libation-runtime-contract.sh
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
nix flake check
```

Expected: all pass. The runtime-contract test may download the pinned Libation
image but must not contact the live cluster.

- [ ] **Step 6: Commit**

```bash
git add flake.nix machine/globalhawk/default.nix machine/globalhawk/k3s.nix \
  k8s/tests/libation-jitter.sh k8s/tests/libation-runtime-contract.sh \
  packages/k3s-drift.py
git commit -m "refactor(k3s): make the host evaluation the deployment source"
```

---

### Task 3: Colocate AdGuard and its Kubernetes bridge

**Files:**
- Modify: `machine/globalhawk/adguard.nix`
- Modify: `k8s/default.nix`
- Delete: `k8s/apps/adguard.nix`
- Modify: `k8s/tests/workloads-module.nix`

**Interfaces:**
- Consumes: outer NixOS `config.services.adguardhome.port`.
- Produces: `applications.adguard` from the same file that configures the native AdGuard service.
- Preserves: namespace `adguard`, Service/Ingress/EndpointSlice names, host, TLS, and host-gateway routing.

- [ ] **Step 1: Extend the module test with a rendered host-port assertion**

In the fixture, ensure the Service's `targetPort` comes from
`nixosConfig.services.fixture.port`, then assert both the evaluated value and
the rendered object:

```nix
  service =
    evaluated.config.services.k3s.workloads.evaluatedConfig
      .applications.fixture.resources.services.fixture;
in
  assert service.spec.ports.http.targetPort == 4321;
```

Run:

```bash
nix build .#checks.x86_64-linux.k3s-workloads-module
```

Expected: PASS before the real AdGuard move; this test guards the mechanism.

- [ ] **Step 2: Move the bridge definition into `adguard.nix`**

Change `machine/globalhawk/adguard.nix` to accept `config` and `lib`. Keep the
native configuration intact, then add:

```nix
  services.k3s.workloads.module = {nixosConfig, ...}: let
    host = "adguard${facts.ingressSuffix}";
    port = nixosConfig.services.adguardhome.port;
  in {
    applications.adguard = {
      namespace = "adguard";
      createNamespace = true;
      resources = {
        services.adguard.spec.ports.web = {
          inherit port;
          targetPort = port;
        };
        ingresses.adguard.spec = {
          ingressClassName = "traefik";
          tls = [{hosts = [host];}];
          rules = [{
            inherit host;
            http.paths = [{
              path = "/";
              pathType = "Prefix";
              backend.service = {
                name = "adguard";
                port.number = port;
              };
            }];
          }];
        };
      };
      yamls = [
        (builtins.toJSON {
          apiVersion = "discovery.k8s.io/v1";
          kind = "EndpointSlice";
          metadata = {
            name = "adguard";
            namespace = "adguard";
            labels."kubernetes.io/service-name" = "adguard";
          };
          addressType = "IPv4";
          endpoints = [{addresses = [facts.hostGatewayIp];}];
          ports = [{
            name = "web";
            inherit port;
            protocol = "TCP";
          }];
        })
      ];
    };
  };
```

Copy the existing comments with the moved bridge and update their file
references. Remove `./apps/adguard.nix` from `k8s/default.nix`, then delete that
file.

- [ ] **Step 3: Verify rendered AdGuard behavior**

Run:

```bash
out="$(nix build --no-link --print-out-paths \
  .#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage)"
service="$(find -L "$out/adguard" -name 'Service-adguard.yaml' -print -quit)"
endpoint="$(find -L "$out/adguard" -name 'EndpointSlice-adguard.yaml' -print -quit)"
native_port="$(nix eval \
  .#nixosConfigurations.globalhawk.config.services.adguardhome.port)"

test "$(nix run nixpkgs#yq-go -- '.spec.ports[0].targetPort' "$service")" = "$native_port"
test "$(nix run nixpkgs#yq-go -- '.ports[0].port' "$endpoint")" = "$native_port"
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
```

Expected: both rendered ports equal `3000`; the host builds.

- [ ] **Step 4: Commit**

```bash
git add machine/globalhawk/adguard.nix k8s/default.nix \
  k8s/tests/workloads-module.nix
git add -u k8s/apps/adguard.nix
git commit -m "refactor(adguard): keep its cluster bridge with its host service"
```

---

### Task 4: Add typed activation-time Kubernetes Secrets

**Files:**
- Create: `modules/nixos/k3s-runtime-secrets.nix`
- Create: `k8s/tests/runtime-secrets-module.nix`
- Modify: `flake.nix`
- Modify: `machine/globalhawk/default.nix`

**Interfaces:**
- Consumes: `config.sops.secrets.<name>` and `config.sops.placeholder.<name>`.
- Produces: `services.k3s.runtimeSecrets.<kubernetes-name>.{namespace,stringData,data}` and root-only `sops.templates`.
- Keeps: plaintext substitution at activation, outside the workload renderer and Nix store.

- [ ] **Step 1: Add a failing runtime-secret module check**

Create `k8s/tests/runtime-secrets-module.nix` with a minimal module evaluation
that stubs sops declarations and evaluates one scalar and one base64 key:

```nix
{
  inputs,
  pkgs,
}: let
  lib = pkgs.lib;
  evaluated = lib.evalModules {
    specialArgs = {inherit inputs pkgs;};
    modules = [
      ../../modules/nixos/k3s-runtime-secrets.nix
      {
        options = {
          sops.secrets = lib.mkOption {type = lib.types.attrsOf lib.types.anything;};
          sops.placeholder = lib.mkOption {type = lib.types.attrsOf lib.types.str;};
          sops.templates = lib.mkOption {
            type = lib.types.attrsOf (lib.types.submodule {
              options = {
                path = lib.mkOption {type = lib.types.str;};
                mode = lib.mkOption {type = lib.types.str;};
                owner = lib.mkOption {type = lib.types.str;};
                content = lib.mkOption {type = lib.types.lines;};
              };
            });
            default = {};
          };
        };
        config = {
          sops.secrets = {
            scalar = {};
            encoded = {};
          };
          sops.placeholder = {
            scalar = "<scalar-placeholder>";
            encoded = "<encoded-placeholder>";
          };
          services.k3s.runtimeSecrets.fixture = {
            namespace = "testing";
            stringData.password.sopsSecret = "scalar";
            data."payload.bin".sopsSecret = "encoded";
          };
        };
      }
    ];
  };
  template = evaluated.config.sops.templates."k3s-fixture.yaml";
  manifest = builtins.fromJSON template.content;
in
  assert template.path == "/var/lib/rancher/k3s/server/manifests/sops-fixture.yaml";
  assert template.mode == "0400";
  assert manifest.metadata.name == "fixture";
  assert manifest.metadata.namespace == "testing";
  assert manifest.stringData.password == "<scalar-placeholder>";
  assert manifest.data."payload.bin" == "<encoded-placeholder>";
    pkgs.runCommand "k3s-runtime-secrets-module-test" {} ''
      touch "$out"
    ''
```

Add it beside the workload check in `flake.nix`:

```nix
k3s-runtime-secrets-module = import ./k8s/tests/runtime-secrets-module.nix {
  inherit inputs pkgs;
};
```

- [ ] **Step 2: Run the check and confirm the missing module failure**

Run:

```bash
nix build .#checks.x86_64-linux.k3s-runtime-secrets-module
```

Expected: failure because `modules/nixos/k3s-runtime-secrets.nix` is absent.

- [ ] **Step 3: Implement declaration types, assertions, and templates**

Implement:

```nix
{
  config,
  lib,
  ...
}: let
  cfg = config.services.k3s.runtimeSecrets;
  valueType = lib.types.submodule {
    options.sopsSecret = lib.mkOption {
      type = lib.types.str;
      description = "Declared sops.secrets key substituted at activation.";
    };
  };
  secretType = lib.types.submodule ({name, ...}: {
    options = {
      namespace = lib.mkOption {type = lib.types.str;};
      stringData = lib.mkOption {
        type = lib.types.attrsOf valueType;
        default = {};
      };
      data = lib.mkOption {
        type = lib.types.attrsOf valueType;
        default = {};
      };
    };
  });
  dnsLabel = value:
    lib.stringLength value <= 63
    && builtins.match "[a-z0-9]([-a-z0-9]*[a-z0-9])?" value != null;
  keyValid = value:
    lib.stringLength value <= 253
    && builtins.match "[-._a-zA-Z0-9]+" value != null;
  refs = values: lib.mapAttrs (_: value: config.sops.placeholder.${value.sopsSecret}) values;
  templateFor = name: secret: {
    name = "k3s-${name}.yaml";
    value = {
      path = "/var/lib/rancher/k3s/server/manifests/sops-${name}.yaml";
      mode = "0400";
      owner = "root";
      content = builtins.toJSON {
        apiVersion = "v1";
        kind = "Secret";
        metadata = {
          inherit name;
          inherit (secret) namespace;
        };
        type = "Opaque";
        stringData = refs secret.stringData;
        data = refs secret.data;
      };
    };
  };
in {
  options.services.k3s.runtimeSecrets = lib.mkOption {
    type = lib.types.attrsOf secretType;
    default = {};
  };

  config = {
    assertions = lib.flatten (lib.mapAttrsToList (name: secret: [
      {
        assertion = dnsLabel name;
        message = "services.k3s.runtimeSecrets.${name}: invalid Kubernetes Secret name";
      }
      {
        assertion = dnsLabel secret.namespace;
        message = "services.k3s.runtimeSecrets.${name}: invalid namespace";
      }
      {
        assertion =
          lib.intersectLists (lib.attrNames secret.stringData) (lib.attrNames secret.data) == [];
        message = "services.k3s.runtimeSecrets.${name}: a key cannot appear in both stringData and data";
      }
      {
        assertion = lib.all keyValid (lib.attrNames (secret.stringData // secret.data));
        message = "services.k3s.runtimeSecrets.${name}: invalid Kubernetes Secret key";
      }
      {
        assertion = lib.all
          (value: builtins.hasAttr value.sopsSecret config.sops.secrets)
          (lib.attrValues (secret.stringData // secret.data));
        message = "services.k3s.runtimeSecrets.${name}: references an undeclared sops secret";
      }
    ]) cfg);

    sops.templates = builtins.listToAttrs (lib.mapAttrsToList templateFor cfg);
  };
}
```

The generated content is JSON with a `.yaml` filename; JSON is valid YAML and
avoids indentation bugs for placeholders.

- [ ] **Step 4: Import the module and run positive and negative checks**

Import `../../modules/nixos/k3s-runtime-secrets.nix` from
`machine/globalhawk/default.nix`.

Run the positive check:

```bash
nix build .#checks.x86_64-linux.k3s-runtime-secrets-module
```

Then temporarily add an overlapping `data.password` declaration to the fixture.
Expected failure must contain:

```text
a key cannot appear in both stringData and data
```

Restore the valid fixture and rerun the passing check.

- [ ] **Step 5: Commit**

```bash
git add flake.nix machine/globalhawk/default.nix \
  modules/nixos/k3s-runtime-secrets.nix k8s/tests/runtime-secrets-module.nix
git commit -m "feat(k3s): keep runtime secret material out of workload renders"
```

---

### Task 5: Replace hand-written Kubernetes Secret templates

**Files:**
- Modify: `machine/globalhawk/sops.nix`
- Modify: `k8s/tests/runtime-secrets-module.nix`

**Interfaces:**
- Consumes: `services.k3s.runtimeSecrets` from Task 4.
- Produces: the same Kubernetes Secret names, namespaces, keys, and values as the current hand-written templates.
- Preserves: non-Kubernetes `restic-env` and `wireless.env` templates verbatim.

- [ ] **Step 1: Declare every existing Kubernetes Secret through the typed interface**

Add these declarations to `machine/globalhawk/sops.nix`:

```nix
services.k3s.runtimeSecrets = {
  cloudflare-api-token = {
    namespace = "cert-manager";
    stringData.api-token.sopsSecret = "cf_api_token";
  };
  mullvad-wg = {
    namespace = "media";
    stringData.WIREGUARD_PRIVATE_KEY.sopsSecret = "mullvad_wg_key";
  };
  immich-db = {
    namespace = "immich";
    stringData.password.sopsSecret = "immich_db_password";
  };
  authelia-secrets = {
    namespace = "auth";
    stringData = {
      jwt.sopsSecret = "authelia_jwt";
      session.sopsSecret = "authelia_session";
      storage-encryption.sopsSecret = "authelia_storage_encryption";
      oidc-hmac.sopsSecret = "authelia_oidc_hmac";
      smtp-password.sopsSecret = "smtp_password";
    };
  };
  authelia-oidc-key = {
    namespace = "auth";
    data."issuer.pem".sopsSecret = "authelia_oidc_issuer_key";
  };
  authelia-users = {
    namespace = "auth";
    data."users_database.yml".sopsSecret = "authelia_users";
  };
  authelia-oidc-client-hashes = {
    namespace = "auth";
    stringData = {
      immich.sopsSecret = "immich_oidc_client_secret_hash";
      audiobookshelf.sopsSecret = "abs_oidc_client_secret_hash";
      calibre-web.sopsSecret = "cwa_oidc_client_secret_hash";
    };
  };
  immich-oidc = {
    namespace = "immich";
    stringData = {
      client-secret.sopsSecret = "immich_oidc_client_secret";
      admin-api-key.sopsSecret = "immich_admin_api_key";
    };
  };
  abs-oidc = {
    namespace = "library";
    stringData = {
      client-secret.sopsSecret = "abs_oidc_client_secret";
      admin-token.sopsSecret = "abs_admin_token";
    };
  };
  cwa-oidc = {
    namespace = "library";
    stringData.client-secret.sopsSecret = "cwa_oidc_client_secret";
  };
};
```

- [ ] **Step 2: Remove only the replaced template bodies**

Delete the ten `sops-*.yaml` entries from `sops.templates`. Keep
`restic-env` and `wireless.env` unchanged. Update comments to point to the typed
runtime-secret declarations.

- [ ] **Step 3: Verify evaluated manifests and placeholder safety**

Run:

```bash
nix eval --json \
  .#nixosConfigurations.globalhawk.config.services.k3s.runtimeSecrets \
  | jq -e '
      length == 10
      and .immich-db.namespace == "immich"
      and .authelia-users.data["users_database.yml"].sopsSecret == "authelia_users"
    '

nix eval --raw \
  '.#nixosConfigurations.globalhawk.config.sops.templates."k3s-immich-db.yaml".content' \
  | jq -e '
      .metadata.name == "immich-db"
      and .metadata.namespace == "immich"
      and .stringData.password
    '

nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
nix flake check
```

Expected: ten declarations, valid JSON Secret content containing a nonempty
placeholder, and a successful host build. Do not print or search decrypted
secret files.

- [ ] **Step 4: Commit**

```bash
git add machine/globalhawk/sops.nix k8s/tests/runtime-secrets-module.nix
git commit -m "refactor(secrets): describe cluster secrets instead of templating YAML"
```

---

### Task 6: Introduce typed shared homelab and cluster values

**Files:**
- Create: `machine/globalhawk/options.nix`
- Modify: `machine/globalhawk/default.nix`
- Modify: `machine/globalhawk/k3s.nix`
- Modify: `machine/globalhawk/adguard.nix`
- Modify: `machine/globalhawk/disks.nix`
- Modify: `machine/globalhawk/backup.nix`
- Modify: `machine/globalhawk/immich-storage.nix`

**Interfaces:**
- Produces: `homelab.media.root`, `homelab.ingressSuffix`, `homelab.network.*`, and `services.k3s.clusterNetwork.*`.
- Consumes: these typed options from host modules and the workload compatibility arguments.
- Preserves: every value currently in `facts.nix`.

- [ ] **Step 1: Add typed declarations and Globalhawk values**

Create `machine/globalhawk/options.nix`:

```nix
{lib, ...}: {
  options = {
    homelab = {
      media.root = lib.mkOption {
        type = lib.types.str;
        description = "Root of the host media dataset shared with workloads.";
      };
      ingressSuffix = lib.mkOption {
        type = lib.types.str;
        description = "Suffix appended to homelab ingress application names.";
      };
      network = {
        lanInterface = lib.mkOption {type = lib.types.str;};
        lanIp = lib.mkOption {type = lib.types.str;};
        lanGateway = lib.mkOption {type = lib.types.str;};
        lanSubnet = lib.mkOption {type = lib.types.str;};
      };
    };
    services.k3s.clusterNetwork = {
      podCidr = lib.mkOption {type = lib.types.str;};
      serviceCidr = lib.mkOption {type = lib.types.str;};
      hostGatewayIp = lib.mkOption {type = lib.types.str;};
    };
  };

  config = {
    homelab = {
      media.root = "/data/Media";
      ingressSuffix = ".h.abrahamwhite.com";
      network = {
        lanInterface = "enp1s0";
        lanIp = "192.168.1.50";
        lanGateway = "192.168.1.1";
        lanSubnet = "192.168.1.0/24";
      };
    };
    services.k3s.clusterNetwork = {
      podCidr = "10.42.0.0/16";
      serviceCidr = "10.43.0.0/16";
      hostGatewayIp = "10.42.0.1";
    };
  };
}
```

Import `./options.nix` before modules that consume the values.

- [ ] **Step 2: Migrate host network and storage consumers**

Replace imports of `facts.nix` in the listed files:

```nix
mediaRoot = config.homelab.media.root;
lan = config.homelab.network;
clusterNetwork = config.services.k3s.clusterNetwork;
```

Use `config.time.timeZone` instead of `facts.timezone`. Keep all paths, tmpfiles
rules, ACLs, firewall behavior, and comments semantically unchanged.

- [ ] **Step 3: Feed workload compatibility arguments from typed options**

In `machine/globalhawk/k3s.nix`, replace the corresponding `facts` inheritance:

```nix
_module.args = {
  ingressSuffix = config.homelab.ingressSuffix;
  inherit (config.services.k3s.clusterNetwork) podCidr serviceCidr hostGatewayIp;
  mediaRoot = config.homelab.media.root;
  timezone = config.time.timeZone;
  # Secret-derived and UID/mail arguments remain until Task 7.
};
```

AdGuard's colocated bridge reads `config.homelab.ingressSuffix` and
`config.services.k3s.clusterNetwork.hostGatewayIp`.

- [ ] **Step 4: Verify values and rendered equivalence**

Run:

```bash
nix eval --raw .#nixosConfigurations.globalhawk.config.homelab.media.root
nix eval --raw .#nixosConfigurations.globalhawk.config.homelab.ingressSuffix
nix eval --raw .#nixosConfigurations.globalhawk.config.services.k3s.clusterNetwork.hostGatewayIp
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
nix flake check
```

Expected values:

```text
/data/Media
.h.abrahamwhite.com
10.42.0.1
```

- [ ] **Step 5: Commit**

```bash
git add machine/globalhawk/options.nix machine/globalhawk/default.nix \
  machine/globalhawk/k3s.nix machine/globalhawk/adguard.nix \
  machine/globalhawk/disks.nix machine/globalhawk/backup.nix \
  machine/globalhawk/immich-storage.nix
git commit -m "refactor(globalhawk): give shared infrastructure typed owners"
```

---

### Task 7: Move identity and mail values to their owners and remove `facts.nix`

**Files:**
- Modify: `machine/globalhawk/default.nix`
- Modify: `machine/globalhawk/immich-storage.nix`
- Modify: `machine/globalhawk/authelia-storage.nix`
- Modify: `machine/globalhawk/mail.nix`
- Modify: `machine/globalhawk/k3s.nix`
- Delete: `machine/globalhawk/facts.nix`

**Interfaces:**
- Consumes: `config.users.users.*`, `config.programs.msmtp.accounts.default`, `config.time.timeZone`, and typed options from Task 6.
- Produces: the same compatibility arguments for remaining cluster-only modules without a shared facts file.
- Preserves: `_media = 994`, Immich `988`, Authelia `989`, SMTP host `smtp.mail.me.com`, and SMTP port `587`.

- [ ] **Step 1: Put literal IDs in the modules that create the users**

Set the IDs at their owners:

```nix
# machine/globalhawk/default.nix
users.groups._media.gid = 994;
users.users._media.uid = 994;

# machine/globalhawk/immich-storage.nix
users.groups.immich.gid = 988;
users.users.immich.uid = 988;

# machine/globalhawk/authelia-storage.nix
users.groups.authelia.gid = 989;
users.users.authelia.uid = 989;
```

Within each file, derive repeated uses from `config.users.users.<name>.uid` rather
than repeating the literal.

- [ ] **Step 2: Put the SMTP endpoint in the module that creates the transport**

In `machine/globalhawk/mail.nix`, set:

```nix
programs.msmtp.accounts.default = {
  host = "smtp.mail.me.com";
  port = 587;
  # Preserve the existing auth, TLS, user, passwordeval, and from settings.
};
```

- [ ] **Step 3: Source all remaining workload arguments from NixOS config**

In `machine/globalhawk/k3s.nix`, use:

```nix
_module.args = {
  mediaUid = config.users.users._media.uid;
  immichUid = config.users.users.immich.uid;
  autheliaUid = config.users.users.authelia.uid;
  smtp = {
    inherit (config.programs.msmtp.accounts.default) host port;
  };
  # Keep the already migrated typed/shared and secret-derived arguments.
};
```

This is the final compatibility injection block. It remains useful while
cluster-only files accept concise arguments, but every value now originates in
an owning NixOS option rather than `facts.nix`.

- [ ] **Step 4: Prove there are no facts consumers and delete the file**

Run:

```bash
rg -n 'facts\.nix|facts\.' flake.nix machine/globalhawk k8s
```

Expected: no matches except historical documentation. Delete
`machine/globalhawk/facts.nix`.

- [ ] **Step 5: Verify the owned values and complete system**

Run:

```bash
test "$(nix eval .#nixosConfigurations.globalhawk.config.users.users._media.uid)" = 994
test "$(nix eval .#nixosConfigurations.globalhawk.config.users.users.immich.uid)" = 988
test "$(nix eval .#nixosConfigurations.globalhawk.config.users.users.authelia.uid)" = 989
test "$(nix eval .#nixosConfigurations.globalhawk.config.programs.msmtp.accounts.default.port)" = 587

bash k8s/tests/libation-jitter.sh
bash k8s/tests/libation-runtime-contract.sh
nix build .#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
nix flake check
```

Expected: all assertions and builds pass.

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/default.nix machine/globalhawk/immich-storage.nix \
  machine/globalhawk/authelia-storage.nix machine/globalhawk/mail.nix \
  machine/globalhawk/k3s.nix
git add -u machine/globalhawk/facts.nix
git commit -m "refactor(globalhawk): let service modules own their shared values"
```

---

### Task 8: Final documentation and verification

**Files:**
- Modify: `README.md`
- Modify: comments in `flake.nix`, `machine/globalhawk/k3s.nix`, `packages/k3s-drift.nix`, and `packages/k3s-drift.py` if any still describe a standalone nixidy flake output.

**Interfaces:**
- Documents: the canonical rendered output, colocation pattern, ownership rule, runtime-secret interface, and live verification command.
- Produces: no runtime behavior changes.

- [ ] **Step 1: Document the operator and authoring interfaces**

Add concise README examples:

```nix
services.k3s.workloads.module = {nixosConfig, ...}: {
  applications.example = {
    # Kubernetes resources may read host-owned configuration here.
  };
};

services.k3s.runtimeSecrets.example = {
  namespace = "example";
  stringData.password.sopsSecret = "example_password";
};
```

Document the canonical inspect/build command:

```bash
nix build .#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage
```

State that `nix run .#k3s-drift` is read-only and requires a live kubeconfig.

- [ ] **Step 2: Scan for stale names and forbidden scope expansion**

Run:

```bash
rg -n 'nixidyEnvs\.x86_64-linux\.globalhawk|import .*facts\.nix|facts\.' \
  --glob '!docs/superpowers/**'

git diff master...HEAD -- packages/libation-reconcile.nix k8s/apps/libation.nix \
  k8s/apps/audiobookshelf.nix
```

Expected:

- No live-code references to the old flake output or `facts.nix`.
- No changes to Libation/Audible or Audiobookshelf package/image pins and hashes
  beyond the test target updates explicitly made in Task 2.

- [ ] **Step 3: Run final verification**

Run:

```bash
nix fmt -- --check .
nix flake check
nix build .#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage
nix build .#nixosConfigurations.globalhawk.config.system.build.toplevel
bash k8s/tests/libation-jitter.sh
bash k8s/tests/libation-runtime-contract.sh
```

Expected: every command passes. Do not run `nixos-rebuild switch` or mutate the
live cluster as part of automated verification.

- [ ] **Step 4: Commit**

```bash
git add README.md flake.nix machine/globalhawk/k3s.nix \
  packages/k3s-drift.nix packages/k3s-drift.py
git commit -m "docs(k3s): make the host-owned workload flow discoverable"
```

- [ ] **Step 5: Perform the operator-only post-merge check**

After review and deployment by the operator:

```bash
nix run .#k3s-drift
```

Expected: no orphaned, missing, or untracked resources. This step is deliberately
outside agent automation because it reads the live cluster.
