# globalhawk service architecture — single-node k3s authored in Nix

**Status:** design, awaiting review
**Date:** 2026-07-22

## Goal

Replace globalhawk's two-substrate service story — some workloads as native NixOS
`services.*`, others as `virtualisation.oci-containers` — with **one consistent model**:
a single-node **k3s** cluster whose every workload is authored in Nix and delivered by
`nixos-rebuild switch`. The unit of consistency is the *application* layer; host-level
system services stay NixOS-native (see boundary below).

The refactor is motivated by, and must not preclude, three near-future capabilities that
are **out of scope to design here** but whose requirements shape the substrate choice:

1. A **reverse proxy / ingress** so services are reached by name, not by port number.
2. **Unified login (OIDC/SSO)** across the services that can accept it.
3. **Tailscale** WAN access (globalhawk sits behind a double-NAT with no router control).

Non-goals for this design: choosing or wiring the IdP; designing the ingress routing
table or the SSO policy; Tailscale exposure mechanics. Those are follow-on specs. This
document commits only to the orchestration substrate, the host/cluster boundary, the
migration path, and the security posture — chosen so the three items above land cleanly
on k8s primitives later.

## Why k3s, authored in Nix (and not the alternatives)

Two end-states were considered seriously:

- **All-in NixOS** (native `services.*` + one container idiom via oci-containers/quadlets,
  plus a NixOS reverse proxy and Authelia). Lightest on resources, best GPU story, most
  NixOS-idiomatic — but the proxy+OIDC wiring is bespoke per service, the gluetun
  `--network=container:vpn` hack persists, and it leans on systemd fluency the operator
  has *less* of than k8s.
- **Single-node k3s, authored in Nix** (chosen). The operator is more fluent in k8s
  incantations than in systemd, and the reverse-proxy + OIDC + cert-management future
  needs map directly onto first-class k8s primitives (ingress + forward-auth +
  cert-manager). It unifies deploy/ingress/secret/config into one model and gives
  liveness/self-healing for free.

The decisive constraint was **"everything must remain Nix, and Helm is unacceptable."**
That is satisfiable: see the authoring/delivery model below.

The end-state is k3s; it is reached via a **phased migration** rather than a big-bang
cutover (see Migration phases), so the box stays usable throughout and each hard case is
de-risked in isolation.

## Authoring and delivery model

The chain is **author in Nix → render plain YAML → deliver via k3s auto-deploy**, with no
ArgoCD and no `kubectl` in the activation path.

- **Cluster:** `services.k3s` on NixOS, single node, `role = server`.
- **Authoring — nixidy.** Workloads are defined as nixidy modules in this flake. nixidy
  implements the "rendered manifests pattern": its build output is a directory of plain
  YAML, and ArgoCD is only *one* of its delivery options — the authoring layer is fully
  separable from it. nixidy's `generators.fromCRDModule` produces typed Nix options from
  any third-party CRD or Helm chart (cert-manager, ingress, sealed-secrets, …), which is
  the ergonomic win over hand-writing or over kubenix's more manual CRD import.
- **Delivery — `services.k3s.manifests`.** The nixidy-rendered YAML is fed into the
  NixOS `services.k3s.manifests` option, which links files into
  `/var/lib/rancher/k3s/server/manifests/` before k3s starts; k3s applies them on boot
  and on change. Thus `nixos-rebuild switch` *is* the apply step. Prior art for this exact
  pure-Nix pattern: `rorosen/k3s-nix`.

### Decision: no pruning, manual deletes (confirmed)

`services.k3s.manifests` does **not** prune — deleting a manifest file leaves its
resources running. Removing a service is therefore a deliberate manual `kubectl delete`
(or a `.skip` file), not an automatic consequence of editing the flake. This is accepted:
on a single-node home box, service *removals* are rare, and the alternative
(`nixidy apply --prune` as an activation step) reintroduces `kubectl`-at-activation and a
live-cluster dependency during rebuild. Revisit if removals become frequent.

### Decision: secrets via sealed-secrets (confirmed)

Because manifests delivered through `services.k3s.manifests` live in the **world-readable
Nix store**, and a k8s `Secret` is only base64 (not encryption), plaintext secrets must
never be rendered into a delivered manifest. The **sealed-secrets** controller resolves
this: a `SealedSecret` is encrypted to the cluster's key and is safe to keep in the store
and in git; the in-cluster controller decrypts it into a real `Secret`.

This *fixes an existing leak* rather than introducing new risk: today the Mullvad
WireGuard private key is interpolated into a container's `environment` and lands verbatim
in `/nix/store` (world-readable). The current git-crypt posture protects the value at
rest in the repo but not on the running host. sealed-secrets closes that gap.
(sops-nix rendering a Secret at activation time is a viable alternative if sealed-secrets
proves annoying; the choice is reversible.)

## Host ↔ cluster boundary

"Consistency" targets the application layer. Host-level services that are entangled with
the kernel, the bootloader, hardware, or the operator's workstation experience stay
NixOS-native — orchestrating them would add friction for no benefit.

| Stays NixOS-native | Moves into k3s |
|---|---|
| ZFS, bootloader, sshd, Tailscale, Avahi | prowlarr, radarr, sonarr |
| Samba, desktop/xrdp, AI-agent-sandbox | qbittorrent + gluetun (one pod) |
| **Plex** (keeps direct `/dev/dri`) | immich (migrated last) |
| pipewire, smartd, restic, firewall | calibre-web, homebridge, future apps |

**Plex is deliberately native** — it is difficult to run under k3s (GPU, its own
networking assumptions) and gains nothing from orchestration. It is *surfaced* in-cluster
as an `ExternalName` Service plus an ingress route, so it still gets a hostname, TLS from
cert-manager, and a row in the same routing table as everything else. Leaving Plex native
does **not** compromise the future SSO design, for two reasons:

1. The reverse proxy is a front door independent of where the workload runs; every
   ingress controller can route to an off-cluster backend (`ExternalName` / manual
   `Endpoints`).
2. Plex is an **SSO exception regardless of substrate**: it has its own account system
   (plex.tv) and native streaming clients (Roku, Apple TV, mobile) that cannot perform
   the interactive forward-auth redirect. Standard practice is to bypass forward-auth for
   Plex entirely — so it was never going to be an SSO-integrated app, in k3s or on the
   host.

Conversely, Immich has **native OIDC** and the *arr apps tolerate forward-auth, so the
future IdP still earns its keep on the migrated stack even with Plex opting out.

## The three hard cases

### VPN sidecar (torrent stack)

Today: `gluetun` (Mullvad WireGuard) publishes qbittorrent's ports; `qbittorrent` shares
gluetun's netns via `--network=container:vpn`; prowlarr/radarr/sonarr sit on the
`torrent` bridge and reach qbit through gluetun's published port. Only qbittorrent
egresses through the VPN — the arr apps do not.

Target: a single **`torrent-vpn` pod** = `gluetun` (network provider) + `qbittorrent`
sharing the pod network namespace. This is the k8s-native replacement for
`--network=container:vpn`. A `qbittorrent` Service exposes the WebUI (:9091); prowlarr /
radarr / sonarr are ordinary Deployments that reach it via cluster DNS. **This preserves
today's topology exactly** — only qbittorrent's traffic transits the VPN. gluetun keeps
the kill-switch; the Mullvad port-forward for the 6881 listening port works as it does
now.

### Storage / state

All migrated apps keep their **existing on-disk config directories** via `hostPath`
volumes — `/data/Media/docker-services/torrent-config/{prowlarr,qbittorrent,radarr,sonarr}`
map straight in, so migration copies **no data**. `runAsUser` / `runAsGroup` / `fsGroup`
are set to `994` (the `_media` uid/gid), preserving the current `PUID`/`PGID` semantics.
The media library (`/data/Media`) is `hostPath`-mounted read-write into the apps that need
it, matching the current `/data` mapping the arr apps and qbittorrent already expect.

### GPU

GPU is **deliberately kept out of the cluster.** Plex — the heavy transcoder — stays
native with direct `/dev/dri`, and Immich ML runs on CPU as it does today. This avoids the
k8s device-plugin complexity entirely. If a future workload genuinely needs in-cluster
GPU, that is a separate decision.

## Security posture

This is a real upgrade over the status quo, in which `networking.firewall.enable = false`
leaves the box wide open and every service is reached by a memorized port number.

- **Re-enable the firewall.** Expose only: 22 (ssh), 80/443 (ingress), Samba on the LAN,
  and Tailscale's own interface. k3s's internal ports (6443 API, 10250 kubelet, flannel
  VXLAN 8472/udp) are bound off the LAN — on a single node the API server and kubelet do
  not need LAN exposure.
- **Name-based access.** Everything becomes `https://<app>.home.<domain>` via ingress;
  the "remember the port" model is retired.
- **Namespaced boundaries.** Per-namespace separation plus `NetworkPolicy` draws a real
  boundary between the torrent stack and the rest of the cluster — a boundary that does
  not exist today.

## Migration phases

The end-state (k3s) is reached incrementally; the box stays usable throughout, and each
hard case is validated before the next begins.

- **Phase 0 — foundation.** Stand up `services.k3s`, the nixidy scaffolding in the flake,
  the ingress controller, cert-manager, and sealed-secrets. Prove one trivial app
  end-to-end *and* Plex-via-`ExternalName` reachable behind ingress.
- **Phase 1 — arr stack.** Migrate prowlarr/radarr/sonarr. Their config dirs are already
  on `/data`, mapped as `hostPath`; no data copy. Validate each via its ingress hostname.
- **Phase 2 — torrent + VPN.** Migrate qbittorrent + gluetun as the shared-netns pod.
  **Validate VPN egress with a leak test** (`curl am.i.mullvad.net/connected` from inside
  the pod must report connected) and confirm arr → qbit connectivity.
- **Phase 3 — immich.** Migrate the stateful workload (Postgres/pgvecto-rs + redis +
  server + ml). Highest risk (DB state); done last, and may be deferred if Phases 0–2
  prove sufficient for now.
- **Phase 4 — decommission.** Remove the `virtualisation.oci-containers` block, the docker
  `torrent` network and the `init-torrent-network` systemd unit, and re-enable the
  firewall with the locked-down rule set above.

## Out of scope now, but enabled by this substrate

The IdP choice (Authentik / Authelia / Kanidm), forward-auth wiring, and Tailscale ingress
exposure are follow-on specs. **One constraint to bank now:** the double-NAT means there is
no inbound 80/443 from the WAN, so cert-manager must issue via **DNS-01 (Let's Encrypt) or
an internal CA** — HTTP-01 cannot work. This design does not preclude either.

## Validation

- `nix flake check` and `nixidy build` evaluate cleanly.
- k3s node reports `Ready`; every migrated app is reachable via its ingress hostname.
- VPN leak test passes from the torrent pod (Phase 2 gate).
- arr → qbittorrent connectivity confirmed.
- Plex transcoding intact (native, unaffected) and reachable via its `ExternalName` route.
- `nmap` from another LAN host shows only the intended open ports (Phase 4 gate).

## Open items to resolve during planning

- Ingress controller: keep k3s's bundled Traefik, or replace with ingress-nginx? (Traefik
  is already present and its middleware model suits forward-auth; leaning keep.)
- Immich packaging under nixidy: adapt the upstream Helm chart via `fromCRDModule`/chart
  generators, or hand-author the StatefulSet + services? Decided in Phase 3.
- Exact firewall rule set and which interface k3s internal ports bind to.
