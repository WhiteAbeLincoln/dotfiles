# globalhawk k3s Migration Implementation Plan (Phases 0–2)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move globalhawk's torrent/arr docker workloads onto a single-node k3s cluster whose manifests are authored in Nix and delivered by `nixos-rebuild switch`, standing up the ingress + TLS + sealed-secrets foundation along the way — without a big-bang cutover.

**Architecture:** k3s runs as a NixOS service (`services.k3s`, single server node, bundled Traefik kept). Two delivery lanes feed k3s's auto-deploy directory (`/var/lib/rancher/k3s/server/manifests`): (1) **third-party controllers** (cert-manager, sealed-secrets) as pinned upstream release YAML via `services.k3s.manifests.<x>.source` — plain YAML, no Helm; (2) **our workloads and custom resources** authored as **nixidy** modules under `k8s/`, rendered to a `result/` tree, then concatenated (excluding nixidy's ArgoCD `apps/` dir) into a single multi-doc manifest fed to `services.k3s.manifests.nixidy.source`. `nixos-rebuild switch` links these files and k3s applies them. No ArgoCD, no `kubectl` in the activation path.

**Tech Stack:** NixOS (`services.k3s`), nixidy (`github:arnarg/nixidy/latest`), Traefik (k3s-bundled ingress), cert-manager (self-signed internal CA `ClusterIssuer`), sealed-secrets (`kubeseal`), gluetun + qbittorrent (shared-pod-netns VPN), linuxserver.io arr images.

## Global Constraints

- **Everything stays Nix; Helm is unacceptable.** Third-party controllers are delivered as pinned upstream *plain YAML* (via `pkgs.fetchurl` → `services.k3s.manifests.<x>.source`), never via `services.k3s.autoDeployCharts` / HelmChart CRs. Our workloads are nixidy modules rendered to plain YAML.
- **No secret literals in unencrypted committed files.** The repo is public; only `secrets/` and t2 firmware are git-crypt'd. The Mullvad WireGuard key, the immich DB password, and the home domain must never appear verbatim in `k8s/`, `machine/`, `docs/`, or any `.nix` outside `secrets/`. Reference them via `secrets.*` attribute paths or, for in-cluster material, via a `SealedSecret`.
- **`services.k3s.manifests` does not prune.** Deleting a manifest leaves resources running. Removals are deliberate manual `kubectl delete`. Do not rely on file deletion to remove a workload.
- **`switch` is the deploy step and is operator-driven on globalhawk.** Per repo convention, prefer `sudo nixos-rebuild build --flake .#globalhawk` to validate evaluation without activating. Applying (`switch`) is how each task actually deploys; run it on globalhawk. **Known gotcha:** `nixos-rebuild switch` on globalhawk can exit 101 at a dbus reload while activation still applied — verify success via `readlink /run/current-system` (or that the workload changed), not the exit code.
- **Migration runs docker and k3s side-by-side.** Do **not** re-enable `networking.firewall` until the Phase 2 decommission task; k3s Traefik binds host 80/443 while the docker high-ports (7878/8989/9091/9696/6881, immich 2283) are still live. Traefik's 80/443 does not collide with those.
- **Storage is in-place.** Every migrated app `hostPath`-mounts its *existing* `/data/Media/docker-services/torrent-config/<app>` config dir and (where needed) `/data/Media`. No data is copied. `runAsUser`/`runAsGroup`/`fsGroup = 994` (the `_media` uid/gid) preserve today's `PUID`/`PGID`.
- **Format & check after every Nix change:** `nix fmt` then `sudo nixos-rebuild build --flake .#globalhawk` (and `nix flake check` where noted). `_media` uid/gid is `994` (see `machine/globalhawk/default.nix`).
- **Deferred (NOT in this plan):** immich migration (stays on `oci-containers`), calibre-web migration (stays native), DNS-01/Let's-Encrypt certs, the IdP/SSO/forward-auth wiring, Tailscale ingress exposure, and convenient wildcard DNS for `*.home.<domain>` (validation here uses `curl --resolve`/`-H Host:` instead).

---

## File Structure

**New — nixidy authoring lane (flake-output level, rendered to plain YAML):**
- `k8s/default.nix` — the `globalhawk` environment root; sets `nixidy.target.*`, imports every app/infra module, receives `ingressSuffix` as a module arg.
- `k8s/lib.nix` — shared nixidy helpers (`mkArrApp`, label conventions).
- `k8s/infra/cert-manager.nix` — the self-signed `ClusterIssuer` + internal-CA `Certificate` + CA `ClusterIssuer` (the controller itself installs via the NixOS lane).
- `k8s/infra/network.nix` — the `media` namespace + default-deny `NetworkPolicy` boundary.
- `k8s/apps/whoami.nix` — the trivial end-to-end proof app (Phase 0).
- `k8s/apps/plex.nix` — `ExternalName` Service + Ingress fronting the native Plex.
- `k8s/apps/arr.nix` — prowlarr/radarr/sonarr Deployments + Services + Ingresses.
- `k8s/apps/torrent.nix` — the `torrent-vpn` Pod (gluetun+qbittorrent) + qbittorrent Service.

**New — NixOS delivery lane (host wiring):**
- `machine/globalhawk/k3s.nix` — enables `services.k3s`; installs cert-manager & sealed-secrets upstream YAML via `services.k3s.manifests`; wires the nixidy→k3s concatenation bridge; owns the firewall change at decommission.

**Modified:**
- `flake.nix` — add the `nixidy` input and the `nixidyEnvs.x86_64-linux.globalhawk` output.
- `machine/globalhawk/default.nix` — import `./k3s.nix`; (Phase 2) delete the torrent/arr `oci-containers`, the `torrent` docker network, and `init-torrent-network`.
- `secrets/globalhawk.nix` — add `ingressSuffix` (git-crypt'd).

---

## Phase 0 — Foundation

### Task 0.1: Add nixidy and the empty environment scaffold

**Files:**
- Modify: `flake.nix`
- Create: `k8s/default.nix`
- Create: `k8s/lib.nix`
- Modify: `secrets/globalhawk.nix`

**Interfaces:**
- Produces: flake output `nixidyEnvs.x86_64-linux.globalhawk` (a nixidy environment; its rendered-YAML tree is at `.environmentPackage`). `secrets.ingressSuffix : str`. `k8s/lib.nix` exports `{ mkArrApp, appLabels }` (bodies added in Task 1.2 — start with `appLabels` only).

- [ ] **Step 1: Add the nixidy input**

In `flake.nix`, inside `inputs = { ... }`, add (nixidy tracks nixpkgs-unstable internally; do not `follows` ours — its generators pin their own):

```nix
    nixidy.url = "github:arnarg/nixidy/latest";
```

- [ ] **Step 2: Add `ingressSuffix` to the git-crypt'd secrets**

Confirm `secrets/globalhawk.nix` is encrypted first: `git-crypt status secrets/globalhawk.nix` must show `encrypted`. Then add the attribute. It is a **suffix appended to each app name** to form the ingress host (`host = "<app>" + ingressSuffix`). The interim value gives single-label mDNS names (dash, not a dotted subdomain — macOS mDNS resolves `radarr-globalhawk.local` but not `radarr.globalhawk.local`). The real-domain follow-up swaps this to a dotted suffix (e.g. `.home.abewhite.dev`). Encrypted, so the literal is safe here and ONLY here:

```nix
  ingressSuffix = "-globalhawk.local";
```

- [ ] **Step 3: Create the nixidy helper stub `k8s/lib.nix`**

```nix
# Shared helpers for the globalhawk nixidy modules.
{lib}: rec {
  # Common labels applied to every workload we author, so NetworkPolicy and
  # kubectl selectors have a stable handle.
  appLabels = name: {
    "app.kubernetes.io/name" = name;
    "app.kubernetes.io/managed-by" = "nixidy";
  };
  # mkArrApp is defined in Task 1.2 (Phase 1). Placeholder kept intentionally
  # minimal until then.
}
```

- [ ] **Step 4: Create the environment root `k8s/default.nix` (empty apps for now)**

```nix
# The `globalhawk` nixidy environment: the single source of truth for every
# workload we author in Nix. Rendered to plain YAML and delivered to k3s by
# machine/globalhawk/k3s.nix — ArgoCD is NOT used, so nixidy.target is only a
# formality required by the module system.
{
  ingressSuffix,
  pkgs,
  lib,
  ...
}: {
  # nixidy requires a target repo/branch even when we consume the rendered
  # YAML directly. These values are never pushed anywhere.
  nixidy.target.repository = "file:///dev/null";
  nixidy.target.branch = "main";

  imports = [
    # apps and infra modules are added task-by-task:
    # ./infra/network.nix
    # ./infra/cert-manager.nix
    # ./apps/whoami.nix
    # ./apps/plex.nix
    # ./apps/arr.nix
    # ./apps/torrent.nix
  ];
}
```

- [ ] **Step 5: Add the `nixidyEnvs` flake output**

In `flake.nix`, inside the top-level `flake = let ... in { ... }` block (where `self` and `inputs` are in scope), add this output alongside `nixosConfigurations`:

```nix
        nixidyEnvs.x86_64-linux = inputs.nixidy.lib.mkEnvs {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          envs.globalhawk.modules = [
            ./k8s
            {
              # Inject the home domain from the git-crypt'd secrets so no ingress
              # hostname literal lands in a committed unencrypted file.
              _module.args.ingressSuffix =
                (import ./secrets/globalhawk.nix).ingressSuffix;
            }
          ];
        };
```

- [ ] **Step 6: Verify the environment evaluates and locate the manifests attribute**

Run: `nix eval .#nixidyEnvs.x86_64-linux.globalhawk --apply builtins.attrNames`
**RESOLVED (this nixidy rev, deb28dc):** the env exposes `[ activationPackage bootstrapPackage config declarativePackage environmentPackage meta resources ]` — there is **no `build` attribute**. The rendered-YAML tree is **`.environmentPackage`** (a derivation building to a dir of `apps/` + `<namespace>/` YAML). Task 0.2's bridge uses `.environmentPackage`. (Note: flakes only see git-tracked files, so `k8s/` must be `git add`-ed before any `nix build` of the env — otherwise it errors "Path 'k8s' … is not tracked by Git".)

- [ ] **Step 7: Build the (empty) rendered manifests**

Run: `git add k8s && nix build .#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage -o /tmp/nixidy-result && find /tmp/nixidy-result/`
Expected: builds successfully; tree contains only an empty `apps/` dir (no workloads yet).

- [ ] **Step 8: Format and commit**

```bash
nix fmt
git add flake.nix flake.lock secrets/globalhawk.nix k8s/default.nix k8s/lib.nix
git commit -m "feat(globalhawk): scaffold nixidy authoring lane for k3s migration

Establishes the Nix-authored workload lane before any workload moves, so the
render-to-YAML pipeline is proven independently of the first migration."
```

---

### Task 0.2: Enable k3s and prove the nixidy→k3s bridge with a trivial app

This is the de-risking spike the spec calls for ("prove one trivial app end-to-end"). It locks the delivery mechanism before any real workload depends on it.

**Files:**
- Create: `machine/globalhawk/k3s.nix`
- Create: `k8s/apps/whoami.nix`
- Modify: `machine/globalhawk/default.nix` (add import)
- Modify: `k8s/default.nix` (uncomment whoami import)

**Interfaces:**
- Consumes: `self.nixidyEnvs.x86_64-linux.globalhawk.build.manifests` (Task 0.1), `appLabels` (Task 0.1).
- Produces: `services.k3s` enabled; `services.k3s.manifests.nixidy.source` fed from the concatenation bridge. Cluster reachable via `sudo k3s kubectl` / `KUBECONFIG=/etc/rancher/k3s/k3s.yaml`.

- [ ] **Step 1: Write the trivial app manifest `k8s/apps/whoami.nix`**

```nix
# Phase-0 end-to-end proof: a stateless echo server. Exists only to validate the
# author→render→deliver→apply→reach chain; removed once real apps are migrated.
{lib, ...}: let
  labels = (import ../lib.nix {inherit lib;}).appLabels "whoami";
in {
  applications.whoami = {
    namespace = "whoami";
    createNamespace = true;
    resources = {
      deployments.whoami.spec = {
        replicas = 1;
        selector.matchLabels = labels;
        template = {
          metadata.labels = labels;
          spec.containers.whoami = {
            image = "traefik/whoami:v1.10.2";
            ports.http.containerPort = 80;
          };
        };
      };
      services.whoami.spec = {
        selector = labels;
        ports.http = {
          port = 80;
          targetPort = 80;
        };
      };
    };
  };
}
```

- [ ] **Step 2: Wire whoami into the environment**

In `k8s/default.nix`, uncomment `./apps/whoami.nix` in `imports`.

- [ ] **Step 3: Write the k3s host module `machine/globalhawk/k3s.nix`**

```nix
# Single-node k3s + the delivery lanes described in
# docs/superpowers/plans/2026-07-22-globalhawk-k3s-migration.md.
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  # The nixidy-rendered YAML tree (Task 0.1 Step 6 confirmed this attribute for
  # nixidy rev deb28dc — the env has no `build` attr; environmentPackage is the
  # derivation whose output is the apps/ + <namespace>/ YAML tree).
  nixidyManifests = inputs.self.nixidyEnvs.x86_64-linux.globalhawk.environmentPackage;

  # Bridge: concatenate every rendered document into one multi-doc manifest,
  # EXCLUDING nixidy's apps/ dir (those are ArgoCD Application CRs whose kind
  # k3s does not know — we deliver the workloads directly, not via ArgoCD).
  nixidyCombined =
    pkgs.runCommand "nixidy-globalhawk.yaml" {} ''
      shopt -s nullglob
      : > "$out"
      for f in $(find ${nixidyManifests} -name '*.yaml' -not -path '*/apps/*' | sort); do
        cat "$f" >> "$out"
        printf '\n---\n' >> "$out"
      done
    '';
in {
  services.k3s = {
    enable = true;
    role = "server";
    # Traefik (bundled) and servicelb (klipper) are kept — do NOT disable them.
    # Graceful shutdown so pods drain on reboot.
    gracefulNodeShutdown.enable = true;
    manifests = {
      # Our nixidy-authored workloads, delivered as one multi-doc file.
      nixidy.source = nixidyCombined;
    };
  };

  # kubectl for the operator without sudo gymnastics; k3s writes this file.
  environment.systemPackages = [pkgs.kubectl pkgs.kubernetes-helm pkgs.kubeseal];
  environment.variables.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
}
```

- [ ] **Step 4: Import the k3s module**

In `machine/globalhawk/default.nix`, add `./k3s.nix` to the `imports` list (next to `./backup.nix`).

- [ ] **Step 5: Validate evaluation (no activation)**

Run: `nix fmt && sudo nixos-rebuild build --flake .#globalhawk`
Expected: builds with no evaluation errors.

- [ ] **Step 6: Deploy (operator-run on globalhawk)**

Run: `sudo nixos-rebuild switch --flake .#globalhawk`
Expected: activates. If it exits 101 at a dbus reload, verify anyway (next step) — activation still applied.

- [ ] **Step 7: Verify the node and the trivial app end-to-end**

```bash
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl get nodes            # expect: globalhawk  Ready
kubectl -n whoami get pods   # expect: whoami-... Running
kubectl -n whoami port-forward svc/whoami 8088:80 &
curl -s localhost:8088 | grep -qi 'Hostname' && echo OK-BRIDGE
kill %1
```
Expected: node `Ready`, whoami pod `Running`, `OK-BRIDGE` printed. This proves author→render→deliver→apply→reach.

- [ ] **Step 8: Commit**

```bash
git add machine/globalhawk/k3s.nix machine/globalhawk/default.nix k8s/apps/whoami.nix k8s/default.nix
git commit -m "feat(globalhawk): stand up k3s and prove the nixidy manifest bridge

Locks the author-in-Nix -> render YAML -> services.k3s.manifests delivery path
against a trivial app before any real workload depends on it."
```

---

### Task 0.2b: Read-only kubectl for the sandbox agent user (added during execution)

**Rationale:** the automation runs as the unprivileged `agent` sandbox user (no
sudo, root-only kubeconfig), so it cannot verify k3s state itself. This mirrors
the existing read-only `docker-socket-proxy` access: give the agent read-only
kubectl so it self-serves most verification gates, without cluster-admin.

**Files:**
- Modify: `modules/nixos/ai-agent-sandbox.nix` (add `services.aiAgentSandbox.k3s`)
- Modify: `machine/globalhawk/default.nix` (`k3s.enable = true`)
- Modify: `machine/globalhawk/k3s.nix` (drop the global root-only `KUBECONFIG` env)

**What it does:**
- In-cluster RBAC via `services.k3s.manifests.agent-readonly-rbac.content`: a
  `ServiceAccount agent-readonly` (kube-system) bound cluster-wide to the built-in
  read-only `view` ClusterRole (excludes Secrets, and all write/exec/port-forward
  verbs), a long-lived `service-account-token` Secret, and a small supplementary
  ClusterRole for `nodes`/`namespaces`/`persistentvolumes` (read-only).
- A root systemd oneshot (`agent-readonly-kubeconfig`, `After=k3s.service`) reads
  the SA token+CA and writes an **agent-owned 0400** kubeconfig
  (`/etc/rancher/k3s/agent-readonly.kubeconfig`, also `~agent/.kube/config`).
- The agent HM home ships a `kubectl` wrapper pinning that kubeconfig.

**Boundary preserved:** the agent can `get`/`list`/`logs`/`describe`/`events`
cluster-wide but **cannot** read Secret/SealedSecret contents or `exec`/
`port-forward`. So leak-tests, port-forwards, and unseal checks stay operator-run.
The kubeconfig is 0400 (not world-readable), unlike the docker proxy port.

**Verify (agent, after the operator switches):** `kubectl get nodes`,
`kubectl -n whoami get pods`, and confirm `kubectl -n kube-system get secret X`
is **denied** (proves the no-secrets boundary holds).

---

### Task 0.3: Install cert-manager and the internal-CA issuer

**Files:**
- Modify: `machine/globalhawk/k3s.nix` (add cert-manager upstream manifest)
- Create: `k8s/infra/cert-manager.nix`
- Modify: `k8s/default.nix` (uncomment cert-manager import)

**Interfaces:**
- Consumes: k3s manifests lane (Task 0.2).
- Produces: `ClusterIssuer/globalhawk-ca` — the internal CA every app Ingress references via the `cert-manager.io/cluster-issuer: globalhawk-ca` annotation.

- [ ] **Step 1: Pin and install the cert-manager controller (upstream YAML, no Helm)**

In `machine/globalhawk/k3s.nix`, add to the `let` block:

```nix
  certManagerVersion = "v1.16.2";
  certManagerYaml = pkgs.fetchurl {
    url = "https://github.com/cert-manager/cert-manager/releases/download/${certManagerVersion}/cert-manager.yaml";
    # Obtain with:
    #   nix store prefetch-file --hash-type sha256 <url>
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };
```

and inside `services.k3s.manifests = { ... }` add:

```nix
      cert-manager.source = certManagerYaml;
```

- [ ] **Step 2: Fill the real hash**

Run: `nix store prefetch-file --hash-type sha256 https://github.com/cert-manager/cert-manager/releases/download/v1.16.2/cert-manager.yaml`
Copy the printed `sha256-...` into the `hash` field, replacing the placeholder.

- [ ] **Step 3: Author the self-signed → internal-CA issuer chain `k8s/infra/cert-manager.nix`**

cert-manager CRDs (`ClusterIssuer`, `Certificate`) are not core kinds, so author them as raw objects via `resources.raw` (nixidy passes raw attrsets straight through to YAML):

```nix
# Internal CA for LAN TLS. No inbound 80/443 exists (double-NAT), so ACME
# HTTP-01 is impossible; DNS-01/Let's Encrypt is deferred to the SSO/ingress
# follow-on. Here: a self-signed issuer bootstraps a root CA, which becomes the
# ClusterIssuer every app Ingress uses. Clients trust the root CA once imported.
{...}: {
  applications.cert-manager-config = {
    namespace = "cert-manager";
    createNamespace = false; # created by the controller manifest
    resources.raw = [
      {
        apiVersion = "cert-manager.io/v1";
        kind = "ClusterIssuer";
        metadata.name = "selfsigned-bootstrap";
        spec.selfSigned = {};
      }
      {
        apiVersion = "cert-manager.io/v1";
        kind = "Certificate";
        metadata = {
          name = "globalhawk-ca";
          namespace = "cert-manager";
        };
        spec = {
          isCA = true;
          commonName = "globalhawk-internal-ca";
          secretName = "globalhawk-ca-key-pair";
          duration = "87600h"; # 10y
          privateKey = {
            algorithm = "ECDSA";
            size = 256;
          };
          issuerRef = {
            name = "selfsigned-bootstrap";
            kind = "ClusterIssuer";
            group = "cert-manager.io";
          };
        };
      }
      {
        apiVersion = "cert-manager.io/v1";
        kind = "ClusterIssuer";
        metadata.name = "globalhawk-ca";
        spec.ca.secretName = "globalhawk-ca-key-pair";
      }
    ];
  };
}
```

- [ ] **Step 4: Wire it into the environment**

In `k8s/default.nix`, uncomment `./infra/cert-manager.nix`.

- [ ] **Step 5: Build, deploy, verify**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n cert-manager rollout status deploy/cert-manager --timeout=180s
kubectl get clusterissuer globalhawk-ca -o jsonpath='{.status.conditions[0].type}={.status.conditions[0].status}'
```
Expected: cert-manager rolls out; `globalhawk-ca` prints `Ready=True`.
(cert-manager may take a minute to reconcile the CRDs on first apply; re-run the last two lines if the issuer is not yet Ready.)

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/k3s.nix k8s/infra/cert-manager.nix k8s/default.nix
git commit -m "feat(globalhawk): add cert-manager with an internal-CA ClusterIssuer

LAN-only box behind double-NAT can't do ACME HTTP-01; a self-signed root CA
gives every service a cert now, with DNS-01 left as an additive swap later."
```

---

### Task 0.4: Give the trivial app a Traefik ingress with internal-CA TLS

**Files:**
- Modify: `k8s/apps/whoami.nix` (add Ingress)

**Interfaces:**
- Consumes: `globalhawk-ca` ClusterIssuer (Task 0.3), `ingressSuffix` (Task 0.1), Traefik (k3s-bundled).
- Produces: the Ingress annotation/host pattern (`<app>${ingressSuffix}`, `ingressClassName = "traefik"`, `cert-manager.io/cluster-issuer = "globalhawk-ca"`) reused by every later app.

- [ ] **Step 1: Add the Ingress to `k8s/apps/whoami.nix`**

Change the module signature to take `ingressSuffix`, and add an Ingress resource:

```nix
{
  lib,
  ingressSuffix,
  ...
}: let
  labels = (import ../lib.nix {inherit lib;}).appLabels "whoami";
  host = "whoami${ingressSuffix}";
in {
  applications.whoami = {
    namespace = "whoami";
    createNamespace = true;
    resources = {
      # ... deployments.whoami and services.whoami unchanged ...
      ingresses.whoami = {
        metadata.annotations."cert-manager.io/cluster-issuer" = "globalhawk-ca";
        spec = {
          ingressClassName = "traefik";
          tls = [
            {
              hosts = [host];
              secretName = "whoami-tls";
            }
          ];
          rules = [
            {
              inherit host;
              http.paths = [
                {
                  path = "/";
                  pathType = "Prefix";
                  backend.service = {
                    name = "whoami";
                    port.number = 80;
                  };
                }
              ];
            }
          ];
        };
      };
    };
  };
}
```

(Keep the `deployments.whoami` and `services.whoami` blocks from Task 0.2 — repeated here only by reference to avoid a merge mistake; do not delete them.)

- [ ] **Step 2: Build, deploy, verify TLS end-to-end (no DNS needed)**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
# cert issued?
kubectl -n whoami get secret whoami-tls -o name   # expect: secret/whoami-tls
# reachable through Traefik on 443 via Host header (DNS is a later spec):
HOST="whoami.$(sudo grep -oP 'ingressSuffix\s*=\s*"\K[^"]+' /dev/stdin <<<"$(nix eval --raw .#nixidyEnvs.x86_64-linux.globalhawk --apply 'e: \"\"' 2>/dev/null || true)")"
curl -sk --resolve "whoami.$(kubectl get ingress -n whoami whoami -o jsonpath='{.spec.rules[0].host}' | cut -d. -f2-):443:127.0.0.1" \
  "https://$(kubectl get ingress -n whoami whoami -o jsonpath='{.spec.rules[0].host}')" | grep -qi Hostname && echo OK-INGRESS-TLS
```
Expected: `whoami-tls` secret exists (cert-manager issued it from `globalhawk-ca`); `OK-INGRESS-TLS` printed (Traefik terminated TLS and routed to whoami). Simpler manual check: `curl -sk -H "Host: whoami-globalhawk.local" https://127.0.0.1 | grep Hostname`.

- [ ] **Step 3: Commit**

```bash
git add k8s/apps/whoami.nix
git commit -m "feat(globalhawk): route whoami through Traefik with internal-CA TLS

Proves the ingress + cert-manager annotation pattern that every migrated app
will reuse, so name-based HTTPS access is validated before the arr migration."
```

---

### Task 0.5: Install the sealed-secrets controller

**Files:**
- Modify: `machine/globalhawk/k3s.nix` (add sealed-secrets upstream manifest)

**Interfaces:**
- Produces: the sealed-secrets controller in `kube-system`; `kubeseal` (already installed in Task 0.2) can now encrypt secrets against the cluster key. Consumed by Phase 2's Mullvad key.

- [ ] **Step 1: Pin and install the controller (upstream YAML, no Helm)**

In `machine/globalhawk/k3s.nix` `let` block:

```nix
  sealedSecretsVersion = "0.27.3";
  sealedSecretsYaml = pkgs.fetchurl {
    url = "https://github.com/bitnami-labs/sealed-secrets/releases/download/v${sealedSecretsVersion}/controller.yaml";
    hash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";
  };
```

and inside `services.k3s.manifests`:

```nix
      sealed-secrets.source = sealedSecretsYaml;
```

- [ ] **Step 2: Fill the real hash**

Run: `nix store prefetch-file --hash-type sha256 https://github.com/bitnami-labs/sealed-secrets/releases/download/v0.27.3/controller.yaml`
Copy the `sha256-...` into the `hash`.

- [ ] **Step 3: Build, deploy, verify a round-trip**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n kube-system rollout status deploy/sealed-secrets-controller --timeout=180s
# round-trip: seal a throwaway secret and confirm the controller unseals it
echo -n 's3cr3t' | kubectl create secret generic probe --dry-run=client --from-file=x=/dev/stdin -o yaml \
  | kubeseal --controller-name sealed-secrets-controller --controller-namespace kube-system -o yaml \
  | kubectl apply -f -
sleep 5
kubectl get secret probe -o jsonpath='{.data.x}' | base64 -d   # expect: s3cr3t
kubectl delete sealedsecret probe; kubectl delete secret probe
```
Expected: controller `Ready`; the unsealed `probe` secret decodes to `s3cr3t`.

- [ ] **Step 4: Commit**

```bash
git add machine/globalhawk/k3s.nix
git commit -m "feat(globalhawk): add sealed-secrets controller

Prerequisite for moving the Mullvad key off plaintext-in-/nix/store: sealed
secrets are safe to keep in the world-readable store and in git."
```

---

### Task 0.6: Surface native Plex through ingress via ExternalName

**Files:**
- Create: `k8s/apps/plex.nix`
- Modify: `k8s/default.nix` (uncomment plex import)

**Interfaces:**
- Consumes: `globalhawk-ca`, `ingressSuffix`, Traefik.
- Produces: `https://plex-globalhawk.local` routing to the native Plex on the host. No SSO/forward-auth (Plex is an SSO exception by design).

- [ ] **Step 1: Confirm the native Plex listen port**

Run: `sudo ss -ltnp | grep -i plex` (Plex Media Server default web port is `32400`).
Expected: note the port; used below (assume `32400`).

- [ ] **Step 2: Author the ExternalName Service + Ingress `k8s/apps/plex.nix`**

An `ExternalName` Service can't be a normal Ingress backend directly with a port, so pair it with an explicit `Endpoints`/port Service. Simplest robust form: a headless Service + manual `EndpointSlice` pointing at the host IP. Here the host is reachable in-cluster at the node IP; use the k3s node's primary LAN IP (read at author time from secrets or hardcode the stable static IP if the box has one — prefer a `hostAlias`). Concretely, use a Service with manual Endpoints:

```nix
# Plex stays NATIVE (GPU /dev/dri, plex.tv account, native clients that can't do
# forward-auth). We only give it a hostname + TLS + a row in the same routing
# table as everything else, per the design's host<->cluster boundary.
{
  lib,
  ingressSuffix,
  ...
}: let
  host = "plex${ingressSuffix}";
  # The host's in-cluster-reachable address. k3s pods can reach the node via its
  # LAN IP; set this to globalhawk's stable LAN IP.
  nodeIp = "127.0.0.1"; # replaced in Step 3 with the real node IP
  plexPort = 32400;
in {
  applications.plex = {
    namespace = "plex";
    createNamespace = true;
    resources = {
      services.plex.spec = {
        ports.web = {
          port = plexPort;
          targetPort = plexPort;
        };
      };
      raw = [
        {
          apiVersion = "v1";
          kind = "Endpoints";
          metadata = {
            name = "plex";
            namespace = "plex";
          };
          subsets = [
            {
              addresses = [{ip = nodeIp;}];
              ports = [
                {
                  name = "web";
                  port = plexPort;
                }
              ];
            }
          ];
        }
      ];
      ingresses.plex = {
        metadata.annotations."cert-manager.io/cluster-issuer" = "globalhawk-ca";
        spec = {
          ingressClassName = "traefik";
          tls = [
            {
              hosts = [host];
              secretName = "plex-tls";
            }
          ];
          rules = [
            {
              inherit host;
              http.paths = [
                {
                  path = "/";
                  pathType = "Prefix";
                  backend.service = {
                    name = "plex";
                    port.number = plexPort;
                  };
                }
              ];
            }
          ];
        };
      };
    };
  };
}
```

- [ ] **Step 3: Set the real node IP**

Run: `ip -4 addr show | grep -oP '(?<=inet\s)192\.168\.\d+\.\d+'` (globalhawk's LAN IP).
Replace `nodeIp = "127.0.0.1"` with that address. (Using the node IP rather than `127.0.0.1` ensures the Endpoints target is routable from the Traefik pod.)

- [ ] **Step 4: Wire in, build, deploy, verify**

```bash
# uncomment ./apps/plex.nix in k8s/default.nix first
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
curl -sk -H "Host: plex-globalhawk.local" https://127.0.0.1/identity | grep -qi 'MediaContainer' && echo OK-PLEX-INGRESS
```
Expected: `OK-PLEX-INGRESS` (Traefik routed to native Plex's `/identity`, which returns a `MediaContainer` XML doc). Confirm native Plex playback/transcode is unaffected (it was never touched).

- [ ] **Step 5: Commit**

```bash
git add k8s/apps/plex.nix k8s/default.nix
git commit -m "feat(globalhawk): surface native Plex via ExternalName + ingress

Gives Plex a hostname and TLS in the shared routing table while it keeps its
direct /dev/dri transcoding — proving the off-cluster-backend pattern."
```

**Phase 0 gate:** node `Ready`; whoami reachable over TLS by name; cert-manager issues from `globalhawk-ca`; sealed-secrets round-trips; Plex reachable via ingress and still transcoding natively.

---

## Phase 1 — arr stack

### Task 1.1: Create the `media` namespace and NetworkPolicy boundary

**Files:**
- Create: `k8s/infra/network.nix`
- Modify: `k8s/default.nix` (uncomment network import)

**Interfaces:**
- Produces: namespace `media` (label `boundary=media`); a default-deny-ingress `NetworkPolicy` plus allow-rules for (a) intra-namespace traffic and (b) Traefik (kube-system) → media pods. Every Phase 1/2 workload lives in `media`.

- [ ] **Step 1: Author `k8s/infra/network.nix`**

```nix
# The torrent/arr stack lives in one namespace with a real boundary: nothing
# outside `media` may open connections into it EXCEPT the ingress controller.
# This is the isolation the flat docker `torrent` bridge never had.
{...}: {
  applications.media-network = {
    namespace = "media";
    createNamespace = true;
    resources.raw = [
      {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "default-deny-ingress";
          namespace = "media";
        };
        spec = {
          podSelector = {};
          policyTypes = ["Ingress"];
        };
      }
      {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "allow-intra-and-ingress";
          namespace = "media";
        };
        spec = {
          podSelector = {};
          policyTypes = ["Ingress"];
          ingress = [
            # intra-namespace (arr <-> qbittorrent)
            {from = [{podSelector = {};}];}
            # Traefik in kube-system may reach app pods (webUIs)
            {
              from = [
                {
                  namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                }
              ];
            }
          ];
        };
      }
    ];
  };
}
```

- [ ] **Step 2: Build, deploy, verify**

```bash
# uncomment ./infra/network.nix in k8s/default.nix
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl get ns media -o jsonpath='{.metadata.name}'          # expect: media
kubectl -n media get networkpolicy                            # expect: both policies listed
```
Expected: `media` namespace exists; both NetworkPolicies present.

- [ ] **Step 3: Commit**

```bash
git add k8s/infra/network.nix k8s/default.nix
git commit -m "feat(globalhawk): add media namespace with NetworkPolicy boundary

Draws the torrent-stack-vs-rest isolation the design calls for before any arr
app lands in the namespace."
```

---

### Task 1.2: Migrate prowlarr (and define the reusable `mkArrApp` helper)

**Files:**
- Modify: `k8s/lib.nix` (implement `mkArrApp`)
- Create: `k8s/apps/arr.nix`
- Modify: `k8s/default.nix` (uncomment arr import)

**Interfaces:**
- Consumes: `media` namespace (Task 1.1), `globalhawk-ca`, `ingressSuffix`, `_media` uid/gid `994`.
- Produces: `mkArrApp { name; image; port; extraVolumes ? []; }` → a nixidy resources fragment (Deployment + Service + Ingress). Config dir convention: `/data/Media/docker-services/torrent-config/<name>` → `/config`. Reused by radarr/sonarr in 1.3/1.4.

- [ ] **Step 1: Implement `mkArrApp` in `k8s/lib.nix`**

Replace the placeholder comment with:

```nix
  # mkArrApp builds the Deployment+Service+Ingress triple shared by every
  # linuxserver.io *arr app. Config dir is hostPath-mounted in place (no data
  # copy); PUID/PGID semantics preserved via runAsUser/Group/fsGroup = 994.
  mkArrApp = {
    name,
    image,
    port,
    ingressSuffix,
    extraVolumes ? [],
    extraMounts ? [],
  }: let
    labels = appLabels name;
    host = "${name}${ingressSuffix}";
    mediaUid = 994;
  in {
    "${name}" = {
      namespace = "media";
      createNamespace = false;
      resources = {
        deployments."${name}".spec = {
          replicas = 1;
          selector.matchLabels = labels;
          template = {
            metadata.labels = labels;
            spec = {
              securityContext = {
                runAsUser = mediaUid;
                runAsGroup = mediaUid;
                fsGroup = mediaUid;
              };
              containers."${name}" = {
                inherit image;
                env = [
                  {
                    name = "TZ";
                    value = "America/Denver";
                  }
                ];
                ports.http.containerPort = port;
                volumeMounts =
                  [
                    {
                      name = "config";
                      mountPath = "/config";
                    }
                  ]
                  ++ extraMounts;
              };
              volumes =
                [
                  {
                    name = "config";
                    hostPath = {
                      path = "/data/Media/docker-services/torrent-config/${name}";
                      type = "Directory";
                    };
                  }
                ]
                ++ extraVolumes;
            };
          };
        };
        services."${name}".spec = {
          selector = labels;
          ports.http = {
            port = port;
            targetPort = port;
          };
        };
        ingresses."${name}" = {
          metadata.annotations."cert-manager.io/cluster-issuer" = "globalhawk-ca";
          spec = {
            ingressClassName = "traefik";
            tls = [
              {
                hosts = [host];
                secretName = "${name}-tls";
              }
            ];
            rules = [
              {
                inherit host;
                http.paths = [
                  {
                    path = "/";
                    pathType = "Prefix";
                    backend.service = {
                      name = name;
                      port.number = port;
                    };
                  }
                ];
              }
            ];
          };
        };
      };
    };
  };
```

- [ ] **Step 2: Create `k8s/apps/arr.nix` with prowlarr only (radarr/sonarr added next tasks)**

The `/data/Media` mount (`extraVolumes`/`extraMounts`) is defined once here so radarr/sonarr reuse it:

```nix
{
  lib,
  ingressSuffix,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  # Shared /data/Media mount for the apps that manage the library.
  mediaVolume = {
    name = "media";
    hostPath = {
      path = "/data/Media";
      type = "Directory";
    };
  };
  mediaMount = {
    name = "media";
    mountPath = "/data";
  };
in {
  applications =
    l.mkArrApp {
      name = "prowlarr";
      image = "lscr.io/linuxserver/prowlarr:latest";
      port = 9696;
      inherit ingressSuffix;
    };
  # radarr and sonarr merged in via lib.mkMerge in Tasks 1.3/1.4.
}
```

- [ ] **Step 3: Wire in, build, deploy, verify prowlarr**

```bash
# uncomment ./apps/arr.nix in k8s/default.nix
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n media rollout status deploy/prowlarr --timeout=180s
curl -sk -H "Host: prowlarr-globalhawk.local" https://127.0.0.1/ | grep -qi 'prowlarr' && echo OK-PROWLARR
```
Expected: prowlarr rolls out reading its **existing** `/config` (indexers/settings intact); reachable via ingress. Confirm in the UI that existing indexers are present (proves the hostPath config carried over).

- [ ] **Step 4: Commit**

```bash
git add k8s/lib.nix k8s/apps/arr.nix k8s/default.nix
git commit -m "feat(globalhawk): migrate prowlarr to k3s with in-place config

First real workload off docker; establishes the reusable arr-app shape
(hostPath config, 994 uid/gid, Traefik ingress) for radarr/sonarr."
```

---

### Task 1.3: Migrate radarr

**Files:**
- Modify: `k8s/apps/arr.nix`

**Interfaces:**
- Consumes: `mkArrApp` (Task 1.2), `mediaVolume`/`mediaMount` (Task 1.2). radarr needs the `/data/Media` mount for library management.

- [ ] **Step 1: Add radarr to `k8s/apps/arr.nix`**

Wrap the return in `lib.mkMerge` so multiple apps compose. Change the `in { applications = ...; }` to:

```nix
in {
  applications = lib.mkMerge [
    (l.mkArrApp {
      name = "prowlarr";
      image = "lscr.io/linuxserver/prowlarr:latest";
      port = 9696;
      inherit ingressSuffix;
    })
    (l.mkArrApp {
      name = "radarr";
      image = "lscr.io/linuxserver/radarr:latest";
      port = 7878;
      inherit ingressSuffix;
      extraVolumes = [mediaVolume];
      extraMounts = [mediaMount];
    })
  ];
}
```

- [ ] **Step 2: Build, deploy, verify radarr**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n media rollout status deploy/radarr --timeout=180s
curl -sk -H "Host: radarr-globalhawk.local" https://127.0.0.1/ | grep -qi 'radarr' && echo OK-RADARR
```
Expected: radarr rolls out; existing movies/settings present in UI; `/data` library visible.

- [ ] **Step 3: Commit**

```bash
git add k8s/apps/arr.nix
git commit -m "feat(globalhawk): migrate radarr to k3s

Reuses mkArrApp with the /data/Media mount so radarr keeps its library and
config with zero data movement."
```

---

### Task 1.4: Migrate sonarr

**Files:**
- Modify: `k8s/apps/arr.nix`

**Interfaces:**
- Consumes: `mkArrApp`, `mediaVolume`/`mediaMount`.

- [ ] **Step 1: Add sonarr to the `lib.mkMerge` list in `k8s/apps/arr.nix`**

```nix
    (l.mkArrApp {
      name = "sonarr";
      image = "lscr.io/linuxserver/sonarr:latest";
      port = 8989;
      inherit ingressSuffix;
      extraVolumes = [mediaVolume];
      extraMounts = [mediaMount];
    })
```

- [ ] **Step 2: Build, deploy, verify sonarr**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n media rollout status deploy/sonarr --timeout=180s
curl -sk -H "Host: sonarr-globalhawk.local" https://127.0.0.1/ | grep -qi 'sonarr' && echo OK-SONARR
```
Expected: sonarr rolls out; existing series/settings present; `/data` visible.

- [ ] **Step 3: Commit**

```bash
git add k8s/apps/arr.nix
git commit -m "feat(globalhawk): migrate sonarr to k3s

Completes the arr trio on k3s; all three now read their in-place docker config
dirs and reach the shared /data/Media library."
```

**Phase 1 gate:** prowlarr/radarr/sonarr all reachable by name over TLS, each showing its pre-migration config and library. The docker copies of these three still run (not yet removed) — that's fine; Phase 2 decommissions them once qbit is proven.

---

## Phase 2 — torrent + VPN

### Task 2.1: Seal the Mullvad WireGuard key

**Files:**
- Create: `k8s/apps/torrent.nix` (SealedSecret only, in this task)
- Modify: `k8s/default.nix` (uncomment torrent import)

**Interfaces:**
- Consumes: sealed-secrets controller (Task 0.5), `secrets.wireguard_private_key` (already in `secrets/globalhawk.nix`).
- Produces: `SealedSecret/mullvad-wg` in `media` → unseals to `Secret/mullvad-wg` with key `WIREGUARD_PRIVATE_KEY`, consumed by gluetun in Task 2.2.

- [ ] **Step 1: Generate the SealedSecret out-of-band (operator step, on globalhawk)**

The private key is NOT typed into any file — it's piped from the decrypted secrets and sealed to the cluster. Run on globalhawk with the repo unlocked (`nix run .#decrypt-secrets`):

```bash
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
KEY=$(nix eval --raw .#nixosConfigurations.globalhawk.config.services.k3s.enable >/dev/null 2>&1; \
      nix eval --raw --impure --expr '(import ./secrets/globalhawk.nix).wireguard_private_key')
kubectl create secret generic mullvad-wg -n media \
  --from-literal=WIREGUARD_PRIVATE_KEY="$KEY" --dry-run=client -o yaml \
  | kubeseal --controller-name sealed-secrets-controller --controller-namespace kube-system \
      --format yaml > /tmp/mullvad-sealed.yaml
unset KEY
```
The resulting `/tmp/mullvad-sealed.yaml` is encrypted to the cluster and safe to commit.

- [ ] **Step 2: Embed the SealedSecret into `k8s/apps/torrent.nix`**

Copy the `spec.encryptedData.WIREGUARD_PRIVATE_KEY` value (a long base64 blob) and the `spec.template` from `/tmp/mullvad-sealed.yaml` into a raw resource. It's encrypted, so it may live in this committed file:

```nix
# torrent-vpn: gluetun (Mullvad WireGuard) + qbittorrent in ONE pod, sharing the
# pod network namespace — the k8s-native replacement for docker's
# --network=container:vpn. Only qbittorrent egresses via the VPN; the arr apps
# do not (they reach qbit by cluster DNS). The WG key is a SealedSecret, closing
# the old plaintext-in-/nix/store leak.
{lib, ...}: {
  applications.torrent = {
    namespace = "media";
    createNamespace = false;
    resources.raw = [
      {
        apiVersion = "bitnami.com/v1alpha1";
        kind = "SealedSecret";
        metadata = {
          name = "mullvad-wg";
          namespace = "media";
        };
        spec = {
          encryptedData.WIREGUARD_PRIVATE_KEY = "PASTE_ENCRYPTED_BLOB_HERE";
          template = {
            metadata = {
              name = "mullvad-wg";
              namespace = "media";
            };
            type = "Opaque";
          };
        };
      }
    ];
  };
}
```

- [ ] **Step 3: Wire in, build, deploy, verify the secret unseals**

```bash
# uncomment ./apps/torrent.nix in k8s/default.nix
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
sleep 5
kubectl -n media get secret mullvad-wg -o jsonpath='{.data.WIREGUARD_PRIVATE_KEY}' | base64 -d | head -c 8 && echo " ...OK-UNSEAL"
```
Expected: the controller produced `Secret/mullvad-wg`; the decoded value starts with the real key prefix. (Do not print the whole key.)

- [ ] **Step 4: Commit**

```bash
git add k8s/apps/torrent.nix k8s/default.nix
git commit -m "feat(globalhawk): seal the Mullvad WireGuard key for in-cluster use

Moves the VPN key from plaintext container env in /nix/store to a SealedSecret
decrypted only inside the cluster, fixing the pre-existing at-rest-on-host leak."
```

---

### Task 2.2: Deploy the torrent-vpn pod and qbittorrent Service

**Files:**
- Modify: `k8s/apps/torrent.nix`

**Interfaces:**
- Consumes: `Secret/mullvad-wg` (Task 2.1), `media` namespace, `_media` uid/gid `994`, `secrets.wireguard_addresses` + `secrets.server_cities` (add to `secrets/globalhawk.nix` if not present — the addresses/cities from today's `oci-containers.vpn.environment` are config, not keys, but keep them out of committed unencrypted files per the WG-config convention).
- Produces: `Service/qbittorrent` (`:9091`) in `media`, resolvable at `qbittorrent.media.svc.cluster.local`.

- [ ] **Step 1: Add the WG addresses/cities to secrets**

In `secrets/globalhawk.nix`, add (values copied from the current `oci-containers.vpn.environment`):

```nix
  wireguard_addresses = "10.69.148.156/32,fc00:bbbb:bbbb:bb01::6:949b/128";
  vpn_server_cities = "stockholm,amsterdam";
```

Thread them into the nixidy env: in `flake.nix`'s `nixidyEnvs` inline module, extend `_module.args` with `wireguardAddresses` and `vpnServerCities` from the secrets import.

- [ ] **Step 2: Add the Pod + Service to `k8s/apps/torrent.nix`**

Change the signature to `{ lib, wireguardAddresses, vpnServerCities, ... }` and append to `resources.raw` (Pod) plus add a `services` block. gluetun needs `NET_ADMIN` and `/dev/net/tun`; qbittorrent shares the pod netns automatically:

```nix
      {
        apiVersion = "v1";
        kind = "Pod";
        metadata = {
          name = "torrent-vpn";
          namespace = "media";
          labels = {"app.kubernetes.io/name" = "qbittorrent";};
        };
        spec = {
          securityContext.fsGroup = 994;
          volumes = [
            {
              name = "tun";
              hostPath = {
                path = "/dev/net/tun";
                type = "CharDevice";
              };
            }
            {
              name = "qbt-config";
              hostPath = {
                path = "/data/Media/docker-services/torrent-config/qbittorrent";
                type = "Directory";
              };
            }
            {
              name = "downloads";
              hostPath = {
                path = "/data/Media/torrents/downloads";
                type = "Directory";
              };
            }
          ];
          containers = [
            {
              name = "gluetun";
              image = "qmcgaw/gluetun";
              securityContext.capabilities.add = ["NET_ADMIN"];
              volumeMounts = [
                {
                  name = "tun";
                  mountPath = "/dev/net/tun";
                }
              ];
              env = [
                {name = "TZ"; value = "America/Denver";}
                {name = "VPN_TYPE"; value = "wireguard";}
                {name = "VPN_SERVICE_PROVIDER"; value = "mullvad";}
                {name = "WIREGUARD_ADDRESSES"; value = wireguardAddresses;}
                {name = "SERVER_CITIES"; value = vpnServerCities;}
                {
                  name = "WIREGUARD_PRIVATE_KEY";
                  valueFrom.secretKeyRef = {
                    name = "mullvad-wg";
                    key = "WIREGUARD_PRIVATE_KEY";
                  };
                }
              ];
            }
            {
              name = "qbittorrent";
              image = "lscr.io/linuxserver/qbittorrent:latest";
              securityContext = {
                runAsUser = 994;
                runAsGroup = 994;
              };
              env = [
                {name = "TZ"; value = "America/Denver";}
                {name = "PUID"; value = "994";}
                {name = "PGID"; value = "994";}
                {name = "WEBUI_PORT"; value = "9091";}
                {name = "TORRENTING_PORT"; value = "6881";}
              ];
              volumeMounts = [
                {name = "qbt-config"; mountPath = "/config";}
                {name = "downloads"; mountPath = "/data/torrents/downloads";}
              ];
            }
          ];
        };
      }
```

and add a Service (as a typed resource, alongside `raw`):

```nix
    services.qbittorrent.spec = {
      selector = {"app.kubernetes.io/name" = "qbittorrent";};
      ports.webui = {
        port = 9091;
        targetPort = 9091;
      };
    };
```

Also add a qbittorrent Ingress (reuse the whoami/arr ingress shape, host `qbittorrent${ingressSuffix}`, backend `qbittorrent:9091`) so the WebUI is reachable by name — thread `ingressSuffix` into the module args.

- [ ] **Step 3: Build, deploy, verify the pod runs**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n media get pod torrent-vpn -o jsonpath='{.status.phase}'   # expect: Running
kubectl -n media logs torrent-vpn -c gluetun | grep -i 'wireguard is up' && echo OK-VPN-UP
```
Expected: `Running`; gluetun reports the tunnel up.

- [ ] **Step 4: VPN LEAK TEST (Phase 2 hard gate)**

```bash
kubectl -n media exec torrent-vpn -c gluetun -- \
  wget -qO- https://am.i.mullvad.net/connected | grep -qi 'You are connected' && echo OK-VPN-CONNECTED
# and confirm qbittorrent egress goes through the tunnel (shares gluetun netns):
kubectl -n media exec torrent-vpn -c qbittorrent -- \
  sh -c 'wget -qO- https://am.i.mullvad.net/connected' | grep -qi 'connected'
```
Expected: `OK-VPN-CONNECTED`; qbittorrent's egress also reports connected (proving it shares the VPN netns). **If this fails, STOP** — do not proceed; qbittorrent must never egress outside the tunnel.

- [ ] **Step 5: Verify WebUI reachable**

```bash
curl -sk -H "Host: qbittorrent-globalhawk.local" https://127.0.0.1/ | grep -qi 'qbittorrent' && echo OK-QBIT-UI
```
Expected: `OK-QBIT-UI`; existing qbit config/torrents present in the UI.

- [ ] **Step 6: Commit**

```bash
git add k8s/apps/torrent.nix secrets/globalhawk.nix flake.nix
git commit -m "feat(globalhawk): move qbittorrent+gluetun to a shared-netns k3s pod

Replaces docker's --network=container:vpn with a single pod; only qbittorrent
egresses via Mullvad, preserving today's topology and kill-switch. Leak-tested."
```

---

### Task 2.3: Point the arr apps at the in-cluster qbittorrent

**Files:**
- (No Nix change if the arr configs already store the qbit host; this is an in-app configuration + verification task.)

**Interfaces:**
- Consumes: `Service/qbittorrent` DNS (`qbittorrent.media.svc.cluster.local:9091`).

- [ ] **Step 1: Update the download-client host in each arr app**

In prowlarr/radarr/sonarr (via their web UIs, or their `/config` if scripted), set the qBittorrent download-client host to `qbittorrent.media.svc.cluster.local` port `9091` (replacing the old gluetun-published `localhost:9091`/host-IP endpoint). Since arr and qbit now share the `media` namespace, short name `qbittorrent:9091` also resolves.

- [ ] **Step 2: Verify connectivity**

```bash
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
kubectl -n media exec deploy/radarr -- \
  sh -c 'wget -qO- --timeout=5 http://qbittorrent:9091/ >/dev/null && echo reachable'
```
Expected: `reachable`. In each arr UI, "Test" on the qBittorrent download client succeeds (green). Add/search a test item end-to-end if desired.

- [ ] **Step 3: Commit (if any scripted config changed)**

```bash
git commit --allow-empty -m "chore(globalhawk): repoint arr download clients at in-cluster qbittorrent

arr -> qbit now goes over cluster DNS within the media namespace; verified the
download-client connection test passes."
```

---

### Task 2.4: Partial decommission and firewall re-enable

**Files:**
- Modify: `machine/globalhawk/default.nix`

**Interfaces:**
- Removes: the `vpn`/`qbittorrent`/`prowlarr`/`radarr`/`sonarr` `oci-containers`, the `torrent` docker network, and `init-torrent-network`. Re-enables `networking.firewall` keeping immich's port (immich stays on docker until its follow-on plan).

- [ ] **Step 1: Remove the migrated docker workloads**

In `machine/globalhawk/default.nix`:
- Delete the `vpn`, `prowlarr`, `qbittorrent`, `radarr`, `sonarr` entries from `virtualisation.oci-containers.containers` (leave the commented `minecraft-tina` block as-is per the "don't remove comments" rule).
- Delete the `systemd.services.init-torrent-network` block.
- Remove the now-unused `vuetorrent` let-binding and the `../../program/plex`? — **No**: keep Plex. Only remove `vuetorrent` (it was a qbit-only volume). Keep the `_media` user/group (still used by Samba, calibre-web, immich).

- [ ] **Step 2: Re-enable the firewall with the locked-down rules**

Replace `networking.firewall.enable = false;` and the broad `allowedTCPPorts`/`allowedUDPPorts` with:

```nix
  # Re-enabled after the torrent/arr migration. Ingress (Traefik) fronts every
  # migrated app on 80/443; the old per-service high ports are gone. Immich's
  # port stays open until immich migrates (its follow-on plan drops it and adds
  # the final nmap gate). Samba/xrdp open their own ports via their modules.
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    80 # Traefik ingress (HTTP -> HTTPS redirect / ACME later)
    443 # Traefik ingress (HTTPS)
    config.services.immich-custom.port # temporary: immich still on docker
  ];
```

(ssh 22 is opened by `services.openssh`; Samba by `services.samba.openFirewall`; xrdp by `services.xrdp.openFirewall`; Tailscale manages its own interface. k3s API 6443 / kubelet 10250 / flannel 8472 are single-node and need no LAN exposure — leaving them unlisted keeps them off the LAN.)

- [ ] **Step 3: Remove the torrent docker network (manual, one-time)**

After switching (next step), the `torrent` network is orphaned. Remove it:

```bash
docker network rm torrent 2>/dev/null || true
```

- [ ] **Step 4: Build, deploy, verify**

```bash
nix fmt && sudo nixos-rebuild build --flake .#globalhawk
sudo nixos-rebuild switch --flake .#globalhawk
# migrated docker containers gone:
docker ps --format '{{.Names}}' | grep -Ev 'immich' | grep -E 'vpn|qbittorrent|prowlarr|radarr|sonarr' && echo "STILL PRESENT (bad)" || echo OK-DOCKER-CLEAN
# immich still up (deferred):
docker ps --format '{{.Names}}' | grep -q immich_server && echo OK-IMMICH-STILL-UP
# firewall on, only intended ports (run from ANOTHER LAN host):
#   nmap -Pn globalhawk   -> expect 22, 80, 443, immich port, samba, xrdp only
```
Expected: `OK-DOCKER-CLEAN`, `OK-IMMICH-STILL-UP`; from another LAN host `nmap` shows only ssh/80/443/immich/samba/xrdp — the arr/qbit high ports (7878/8989/9091/9696/6881) are gone.

- [ ] **Step 5: Verify the full stack post-decommission**

Re-run the Phase 1 and Phase 2 ingress checks (whoami, prowlarr, radarr, sonarr, qbittorrent, plex over `https://<app>-globalhawk.local`) and the VPN leak test once more, to confirm nothing regressed when docker's copies stopped.

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/default.nix
git commit -m "feat(globalhawk): decommission migrated docker torrent/arr stack, re-enable firewall

The arr/qbit workloads now live in k3s, so their oci-containers, the torrent
bridge, and init-torrent-network are removed and the firewall comes back up
locked down (immich's port kept until its own migration)."
```

**Phase 2 gate (and plan completion):** VPN leak test passes from the torrent pod; arr→qbit connectivity confirmed; migrated docker containers gone; firewall re-enabled and `nmap` shows only intended ports; immich untouched and still serving; Plex native + reachable via ingress.

---

## What this plan intentionally leaves for follow-on specs

- **Immich (Phase 3):** still on `oci-containers` (`services.immich-custom`). Its migration (Postgres/pgvecto-rs StatefulSet + redis + server + ml, DB state move) and the *final* firewall lockdown (dropping immich's port, the closing `nmap` gate) are a separate plan.
- **calibre-web:** stays native `services.calibre-web`; re-authoring as a k8s workload is deferred.
- **SSO/forward-auth, the IdP choice, and convenient `*.home.<domain>` DNS:** follow-on. Until DNS lands, reach apps with `curl -H "Host: …"` or a local `hosts` entry / `--resolve`.
- **DNS-01 / Let's Encrypt certs:** the `globalhawk-ca` ClusterIssuer is the swap point; moving to publicly-trusted certs is additive. **Confirmed viable despite the never-public posture** — DNS-01 proves domain control via a TXT record and needs no inbound exposure. Two provider paths for the domain (Porkbun-registered, Cloudflare-backed DNS): (a) **preferred** — delegate the zone to a free Cloudflare account and use cert-manager's *native* Cloudflare DNS-01 solver + a scoped API token (sealed); (b) keep DNS at Porkbun and deploy the community cert-manager Porkbun webhook solver + a Porkbun API key (sealed). The internal-CA choice for this plan is a scope decision (keep Phase 0 lean; avoid per-device root-CA trust being the only blocker), not a technical impossibility.

## Self-Review notes

- **Spec coverage:** substrate (k3s+nixidy, Task 0.1/0.2) ✓; no-prune accepted (Global Constraints) ✓; sealed-secrets fixing the WG leak (0.5, 2.1) ✓; host↔cluster boundary incl. Plex ExternalName (0.6) ✓; VPN sidecar as shared-netns pod (2.2) ✓; storage/hostPath + 994 (Global Constraints, 1.2, 2.2) ✓; GPU kept out (nothing added) ✓; security posture: firewall re-enable + name-based access + NetworkPolicy (2.4, 0.4, 1.1) ✓; phased migration 0→2 + partial decommission ✓; validation gates per phase ✓. Immich/calibre-web explicitly deferred per resolved scope ✓.
- **Bridge risk:** the one genuine unknown (exact nixidy attribute for the rendered YAML tree) is isolated in Task 0.1 Step 6 with a verification command and a documented fallback, before anything depends on it.
