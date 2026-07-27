# Globalhawk Observability Design

**Date:** 2026-07-26

## Purpose

Add a fully self-hosted observability stack for the single-node `globalhawk`
k3s cluster and its NixOS-native services. Grafana must provide one place to
inspect Kubernetes workloads, Plex, AdGuard Home, host hardware and operating
system health, systemd logs, and actionable alert state.

The first version deliberately has no external telemetry or notification
delivery. A complete `globalhawk` or k3s outage therefore remains invisible
until the operator checks the host.

## Goals

- Retain metrics and logs for 30 days on the root filesystem.
- Collect Kubernetes resource and health metrics for every workload, plus
  application-native metrics where a workload exposes them.
- Collect host-native metrics for NixOS, Plex, and AdGuard.
- Collect logs from every Kubernetes pod and Kubernetes events.
- Collect full logs for selected host services plus warning-and-higher host and
  kernel journal entries.
- Include CPU, memory, filesystems, networking, systemd, temperatures,
  SMART/NVMe, ZFS, and backup health.
- Authenticate Grafana through native OIDC with the existing Authelia provider.
- Evaluate useful alerts and display their state in Grafana without delivering
  notifications.
- Keep the stack declarative through NixOS, nixidy, pinned charts and images,
  SOPS runtime secrets, and provisioned Grafana resources.
- Prevent telemetry failures or resource spikes from disrupting production
  services.

## Non-goals

- External uptime checks or dead-man monitoring.
- Email or other alert notification delivery.
- High availability, replication, or multi-node scheduling.
- Distributed Loki, Mimir, Thanos, or external object storage.
- Tracing or application instrumentation changes.
- Backing up Prometheus or Loki history.
- Migrating existing workload storage to a new CSI provider.

Kubernetes storage modernization is tracked separately in
`docs/superpowers/specs/2026-07-26-kubernetes-storage-modernization-design.md`.
The observability PVCs use the existing k3s local-path provisioner.

## Chosen approach

Use a Kubernetes-native control plane with host-native collectors:

- `kube-prometheus-stack` supplies Prometheus Operator, Prometheus,
  Alertmanager, kube-state-metrics, Grafana, Kubernetes dashboards, and baseline
  rules.
- Loki runs as one monolithic replica with filesystem storage.
- Grafana Alloy runs in Kubernetes to collect all pod logs and Kubernetes
  events.
- NixOS runs Alloy for the targeted journal, node exporter for host metrics,
  and hardware-specific exporters or textfile collectors.
- Read-only adapters expose aggregate Plex and AdGuard API metrics.

This follows the repository's existing nixidy/chart delivery model and lets
workloads declare monitoring through `ServiceMonitor` and `PodMonitor`
resources when they expose native metrics. kube-state-metrics and cAdvisor
cover resource and lifecycle health for workloads that do not. A hand-built
Prometheus configuration would have fewer controllers but would centralize
every scrape target and require bespoke Kubernetes dashboards and rules. An
entirely host-native stack would survive a k3s failure but would make
Kubernetes discovery, credentials, workload ownership, and networking
substantially more awkward.

## Architecture

```text
k3s workloads ──metrics──► Prometheus ──► Grafana
      │                       ▲
      └──logs──► Alloy ─► Loki│
                              │
NixOS host ──node/SMART───────┤
      │                       │
      ├──Plex/AdGuard adapters
      │
      └──targeted journal──► host Alloy ─► Loki
```

### Kubernetes components

The `monitoring` namespace contains:

- Prometheus Operator.
- One Prometheus instance.
- One Alertmanager instance with a no-op receiver.
- kube-state-metrics.
- One Grafana instance.
- One monolithic Loki instance.
- Alloy as a single-node DaemonSet.
- Plex and AdGuard metric adapters unless packaging constraints make a
  host-native adapter substantially simpler.

Prometheus, Loki, Alertmanager, exporters, and Alloy have no public ingress.
Grafana is exposed as `grafana${config.homelab.ingressSuffix}` through Traefik.

### Host components

NixOS owns:

- Node exporter for kernel, CPU, memory, load, filesystems, inodes, network,
  systemd, and hardware sensor metrics.
- SMART/NVMe collection.
- ZFS pool, capacity, error, and scrub metrics.
- Restic result and age metrics.
- Alloy journal collection.

Exporter listeners bind only where the k3s bridge can reach them. The NixOS
firewall must not expose exporter ports to the LAN. Monitoring NetworkPolicies
allow only required scrape, ingestion, DNS, and Traefik paths.

## Storage and retention

The root filesystem currently has 256 GiB free out of 461 GiB. Reserve a
nominal 55 GiB:

| Component | PVC | Retention control |
|---|---:|---|
| Prometheus | 20 GiB | 30 days and a 17 GiB TSDB size ceiling |
| Loki | 30 GiB | 30 days with compaction and deletion |
| Grafana | 5 GiB | Configuration and UI state only |

All volumes are single-replica k3s local-path PVCs. Local-path requested sizes
are not filesystem quotas. Prometheus enforces its own size limit; Loki is
bounded through time retention and ingestion limits rather than a hard byte
quota. A dedicated loopback filesystem or filesystem quotas would add
operational complexity and is not part of this design.

Protection layers are:

- Loki global and per-stream ingestion-rate limits.
- A conservative maximum log-line size, initially 64 KiB.
- Rejection of excessively old entries.
- Low-cardinality Prometheus scrape and relabel rules.
- Root filesystem alerts at 70%, 80%, and 90%.
- Observability storage growth panels and alerts.
- An operator response at 80% that shortens Loki retention before considering
  any other cleanup.

No automatic process may delete unrelated root filesystem content. Review
actual growth after 7–14 days and adjust the split while preserving a large
root-disk reserve.

Grafana configuration and repository-owned dashboards are reproducible.
Prometheus and Loki data are disposable and are not backed up.

## Metrics collection contract

Initial scrape intervals are:

- Kubernetes control plane, kubelet, cAdvisor, kube-state-metrics, and node
  exporter: 30 seconds.
- Application `ServiceMonitor` targets: 30 seconds by default.
- Plex and AdGuard adapters: 60 seconds.
- SMART/NVMe metrics: 5 minutes.

Host metrics include:

- CPU, load, memory, swap, network, filesystems, inodes, and mount health.
- systemd unit state and failed-unit counts.
- Temperatures and available hardware sensors.
- SMART/NVMe health, wear, temperature, error counts, and self-test state.
- ZFS pool health, capacity, errors, scrub state, and last successful scrub age.
- Restic backup age and result through node-exporter textfile metrics.
- NixOS generation or build metadata only when operationally useful, without
  store paths as labels.

Plex metrics are aggregate: reachability, version/update state, active and
transcoding session counts, library counts, and request latency where the API
supports them. AdGuard metrics include reachability, total queries, blocked
percentage, response latency, query types, upstream health, and filter state.

The implementation must first evaluate available third-party adapters. An
adapter may be used only when its source and image can be pinned, it is
compatible with the deployed service API, and its emitted metric set passes the
label privacy audit below. If no candidate meets that contract, implement a
minimal in-repository read-only collector for only the listed API fields rather
than deploying an abandoned or over-broad exporter.

No service metric may label usernames, client addresses, DNS domains, media
titles, filenames, session identifiers, or raw URLs.

## Log collection contract

### Kubernetes

Kubernetes Alloy collects:

- Every container's stdout and stderr in every namespace.
- Normal and warning Kubernetes events.
- Previous-container logs when Alloy observes a restart and the runtime has not
  already rotated them.

Indexed labels are limited to cluster, namespace, workload, pod, container,
node, and stream. Pod UID, container ID, image digest, filenames, annotations,
and arbitrary parsed application fields are not indexed labels. Useful fields
may remain structured metadata where supported.

Known access-log or health-check noise is filtered only after measuring the
first week of traffic. Loki counts rejected or truncated lines and exposes
those counters to Prometheus.

### Host journal

Host Alloy reads the journal once and forwards:

- Full logs for `k3s.service`, `plex.service`, `adguardhome.service`,
  `smartd.service`, ZFS import/mount/ZED/scrub/trim services,
  `zfs-media-posixacl.service`, `restic-backups-media.service`, its failure
  notifier, `nixos-upgrade.service`, `nix-gc.service`,
  `nix-optimise.service`, and relevant wired and wireless networking units.
- Warning-and-higher entries from all other systemd units and the kernel.

Filtering occurs in the Alloy processing pipeline so one journal reader does
not duplicate an allowlisted unit's warning entries. Unit, priority, transport,
boot ID, and hostname are queryable labels. PID and other high-churn journal
fields are not.

Journal delivery is best-effort while Loki is unavailable. Alloy resumes from
its saved journal position; journald remains the short-term source until its own
retention rotates.

## Grafana authentication

Grafana uses native Generic OAuth/OIDC with Authelia. The ingress does not use
the `media-forward-auth` middleware, which remains the compatibility mechanism
for applications such as the *arr stack that lack native SSO.

Register a confidential Authelia client with:

- Client ID `grafana`.
- An admins-only authorization policy requiring two-factor authentication.
- Authorization-code flow with PKCE/S256.
- Scopes `openid profile email groups`.
- Redirect URI
  `https://grafana${config.homelab.ingressSuffix}/login/generic_oauth`.
- `client_secret_basic` token endpoint authentication.
- A Grafana-specific ID-token claims policy containing email, name, groups, and
  preferred username, following Authelia's Grafana compatibility guidance.

The hashed client secret is delivered to Authelia through its existing
SOPS-backed secret structure. The plaintext counterpart is delivered only to
Grafana in a separate SOPS-backed Kubernetes Secret. Neither value enters the
Nix store or a rendered repository artifact.

Grafana maps `group:admins` to the default organization's `Admin` role with
strict role mapping. It does not allow OIDC to assign Grafana server
administrator. The local break-glass account remains the only server admin.
OAuth auto-login remains disabled so the local login form is usable through
`kubectl port-forward` when Authelia or Traefik is unavailable.

## Dashboards

Data sources, folders, dashboards, and stable dashboard UIDs are provisioned
declaratively. Repository-provisioned dashboards are read-only in practice:
use the UI to experiment, then export useful edits back into the repository
before deployment overwrites them.

Dashboard organization is:

- **Home**
  - Homelab overview showing cluster, host, storage, services, and active alerts.
- **Kubernetes**
  - Cluster resources.
  - Namespaces and workloads.
  - Pods and containers.
  - Networking, CoreDNS, and Traefik.
  - Persistent storage.
- **Host**
  - CPU, memory, network, and filesystems.
  - Temperatures and SMART/NVMe.
  - ZFS pool and scrub health.
  - systemd, backups, NixOS maintenance, and host logs.
- **Services**
  - Plex.
  - AdGuard Home.
  - Application workload health.
- **Logs**
  - Kubernetes log explorer.
  - Host journal explorer.
  - Kubernetes events.
- **Observability**
  - Prometheus targets and ingestion.
  - Loki ingestion, rejected lines, retention, and queries.
  - Alloy and Grafana health.

Useful upstream dashboards are retained, but the default landing dashboard is
purpose-built for `globalhawk`.

## Alerts

Rules are declarative `PrometheusRule` resources. Grafana displays their state;
tuning remains reviewable in Nix. Alertmanager initially sends every alert to a
no-op receiver. A future email pipeline changes only Alertmanager routing.

Every rule has `warning` or `critical` severity plus a concise symptom, likely
impact, and first diagnostic link. Initial coverage includes:

- Node unavailable or exporter target down.
- Sustained CPU saturation, memory pressure, swap, or load.
- Root/ZFS filesystem capacity and inode thresholds.
- SMART failure, NVMe wear/errors, high temperature, or degraded ZFS.
- Stale or failed ZFS scrub and restic backup.
- Failed important systemd units.
- Kubernetes pods pending, crash-looping, restarting frequently, OOM-killed, or
  unavailable.
- Deployments or StatefulSets below desired replicas.
- PVC capacity pressure.
- CoreDNS and Traefik error or latency symptoms.
- Plex or AdGuard unavailable.
- AdGuard upstream failure or sustained abnormal DNS latency.
- Prometheus, Loki, Alloy, Alertmanager, or Grafana unhealthy.
- Loki rejection or unexpected ingestion-volume spikes.

Plex activity and AdGuard blocking percentages begin as dashboard signals, not
alerts. Thresholds require observed normal behavior.

## Failure behavior

- Exporters and adapters are read-only; their failure cannot stop Plex or
  AdGuard.
- Monitoring workloads have resource requests and limits so telemetry load does
  not starve production workloads.
- Prometheus or Loki corruption is recovered by recreating the affected
  disposable volume.
- Grafana configuration and dashboards recover from repository state; UI-only
  experiments and session state may be lost.
- Exporter failures appear as Prometheus target failures and overview signals.
- The stack cannot report a total host or k3s outage because all components are
  local. This is accepted for the first version.

## Deferred follow-up: automatic Plex authentication refresh

The first rollout uses an operator-supplied Plex token. Replace it in a
follow-up with Plex device authentication and automatically refreshed
seven-day JWTs:

- Use the existing token only to bootstrap a registered exporter device.
- Generate a stable device identifier and Ed25519 keypair, and persist the
  device credentials and refresh state on a small, access-restricted
  `local-path` PVC.
- Keep the image root filesystem read-only and retain
  `automountServiceAccountToken = false`; only the state mount is writable.
- Refresh before expiry and continue serving the last valid token while a
  retry remains possible.
- Expose aggregate readiness, expiry, last-refresh, and refresh-failure metrics
  and alert before authentication becomes unusable.
- Never log tokens, device private keys, household activity, or Plex response
  bodies.

Loss or revocation of the registered device may require a new one-time
bootstrap token. Ordinary token expiry must not require operator action.

## Rollout

1. **Monitoring foundation**
   - Vendor pinned charts.
   - Create the namespace, PVCs, NetworkPolicies, Prometheus, Alertmanager,
     Grafana, and Kubernetes metrics.
   - Measure resource use before adding logs.
2. **Kubernetes logs**
   - Add Loki and Kubernetes Alloy.
   - Verify all namespaces and Kubernetes events.
   - Measure daily volume and cardinality.
3. **Host integration**
   - Add host, SMART/NVMe, sensors, ZFS, systemd, backup, and journal collection.
   - Confirm only the intended host exporter ports are cluster-reachable.
4. **Plex and AdGuard**
   - Select or implement adapters using the contract above.
   - Audit metrics before Prometheus stores them.
   - Add service dashboards.
5. **OIDC and operational rules**
   - Register the Authelia client and verify local recovery login before relying
     on OIDC.
   - Provision the overview dashboards and initial rules.
   - Observe and tune without notifications.

Rollback removes the observability workload module and host collectors through
NixOS. It must not modify or delete Plex, AdGuard, existing workload data, or
unrelated Authelia clients.

## Testing and acceptance

Do not create an observability-specific test framework, NixOS VM, disposable
k3s cluster, fake-`kubectl` suite, or YAML-shape Bash scripts.

The required pre-activation validation is:

```sh
nixos-rebuild build --flake .#globalhawk
```

The full system build evaluates the host and builds the referenced nixidy
render, charts, and packages. Add Nix module assertions only for genuine safety
invariants that existing types cannot express, such as enabling observability
without k3s, missing required runtime-secret declarations, unsafe host exporter
exposure, or inconsistent retention settings.

Kubernetes probes and observability target health are the permanent behavioral
checks. After the first deployment, run this one-time live acceptance
checklist:

- An `admins` user can log in through Authelia OIDC; a non-admin user is denied.
- Local Grafana break-glass login works through `kubectl port-forward`.
- Expected Prometheus targets are healthy and every namespace is represented.
- A controlled test pod's stdout appears in Loki.
- A controlled allowlisted host-unit warning appears in Loki.
- Routine logs from a non-allowlisted unit do not appear, while a warning from
  that unit does.
- CPU, memory, disk, temperature, SMART/NVMe, ZFS, systemd, restic, Plex, and
  AdGuard panels show current data.
- A synthetic test-workload failure produces and clears its alert.
- Restarting Grafana, Prometheus, Loki, and Alloy retains the state each
  component promises to persist.
- After at least seven days, measured growth projects within the 30-day storage
  envelope.

The existing flake checks remain limited to reusable module composition and
secret validation logic. This feature does not add host-specific shape tests.
