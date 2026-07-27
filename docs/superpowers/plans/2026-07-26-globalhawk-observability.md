# Globalhawk Observability Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deploy a declarative, self-hosted Prometheus/Grafana/Loki/Alloy stack that observes globalhawk's k3s workloads, NixOS host, Plex, and AdGuard Home for 30 days.

**Architecture:** Prometheus, Alertmanager, Grafana, monolithic Loki, and Kubernetes Alloy run in a `monitoring` namespace rendered through the existing NixOS-to-nixidy integration. NixOS-native exporters and Alloy expose host metrics and targeted journal logs through the configured k3s host gateway, while Grafana authenticates directly to Authelia with OIDC.

**Tech Stack:** NixOS 26.05 modules, nixidy, pinned Helm charts, k3s, Prometheus Operator, Grafana, Loki monolithic mode, Grafana Alloy, SOPS, Authelia OIDC, Traefik.

## Global Constraints

- The design source of truth is `docs/superpowers/specs/2026-07-26-globalhawk-observability-design.md`.
- Retention is 30 days: Prometheus requests 20 GiB with a 17 GiB TSDB ceiling, Loki requests 30 GiB, and Grafana requests 5 GiB.
- Use the existing k3s local-path StorageClass; storage modernization is out of scope.
- The stack is single-replica and local-only; do not add external storage, remote write, tracing, or notification delivery.
- Pin every chart version/hash and every non-chart container image digest.
- Runtime secrets must use `machine/globalhawk/sops.nix`; plaintext must never enter the Nix store or an unencrypted file.
- Grafana uses native Authelia OIDC. Do not attach `media-forward-auth` to its ingress.
- Only Authelia `admins` may enter Grafana; map them to organization `Admin`, never `GrafanaAdmin`.
- Exporter ports bind to `config.services.k3s.clusterNetwork.hostGatewayIp`, not the LAN address, and must not open the firewall.
- Never index usernames, client addresses, DNS names, media titles, filenames, session identifiers, pod UIDs, container IDs, image digests, or arbitrary parsed fields as metric/log labels.
- Do not add NixOS VM tests, fake `kubectl`, YAML-shape Bash tests, or host-specific flake checks.
- The required authoring gate is `nixos-rebuild build --flake .#globalhawk`; activation and live acceptance are explicit operator actions.
- Do not run `nixos-rebuild switch`, edit encrypted SOPS values, or mutate Kubernetes unless the operator explicitly authorizes it.

## File map

- `charts/kube-prometheus-stack/default.nix` — pinned Prometheus Operator stack chart.
- `charts/loki/default.nix` — pinned monolithic Loki chart.
- `charts/alloy/default.nix` — pinned Kubernetes Alloy chart.
- `machine/globalhawk/observability/default.nix` — imports the focused observability units.
- `machine/globalhawk/observability/stack.nix` — namespace, chart releases, PVC configuration, data sources, services, ingress, and monitoring NetworkPolicies.
- `machine/globalhawk/observability/kubernetes-logs.nix` — Kubernetes Alloy configuration for pod logs and events.
- `machine/globalhawk/observability/host.nix` — NixOS node/SMART/ZFS exporters, host Alloy, and restic textfile metrics.
- `machine/globalhawk/observability/services.nix` — Plex and AdGuard metric adapters and their Prometheus monitors.
- `machine/globalhawk/observability/sso.nix` — Grafana OIDC workload settings; Authelia registration remains in the existing Authelia service file.
- `machine/globalhawk/observability/alerts.nix` — globalhawk-owned `PrometheusRule` groups.
- `machine/globalhawk/observability/dashboards.nix` — dashboard ConfigMaps and Grafana provisioning metadata.
- `machine/globalhawk/observability/dashboards/*.json` — only globalhawk-owned dashboards; upstream chart dashboards remain chart-owned.
- `machine/globalhawk/default.nix` — imports the observability directory.
- `machine/globalhawk/services/authelia.nix` — admins-only Grafana authorization policy, claims policy, and OIDC client.
- `machine/globalhawk/sops.nix` — declares Grafana, OIDC, Plex, and AdGuard runtime-secret mappings.
- `secrets/globalhawk.sops.yaml` — operator-populated encrypted secret values.

---

### Task 1: Pin charts and establish the monitoring module boundary

**Files:**
- Create: `charts/kube-prometheus-stack/default.nix`
- Create: `charts/loki/default.nix`
- Create: `charts/alloy/default.nix`
- Create: `machine/globalhawk/observability/default.nix`
- Modify: `machine/globalhawk/default.nix`

**Interfaces:**
- Consumes: `services.k3s.workloads.module`, `nixidy.chartsDir`, and the chart-attribute convention used by `charts/authelia/default.nix`.
- Produces: `charts."kube-prometheus-stack"`, `charts.loki`, and `charts.alloy`; one imported observability module boundary for later tasks.

- [ ] **Step 1: Resolve current stable chart releases and fixed-output hashes**

Use the stable releases resolved on 2026-07-26:

```bash
nix run nixpkgs#kubernetes-helm -- repo add prometheus-community https://prometheus-community.github.io/helm-charts
nix run nixpkgs#kubernetes-helm -- repo add grafana-community https://grafana-community.github.io/helm-charts
nix run nixpkgs#kubernetes-helm -- repo add grafana https://grafana.github.io/helm-charts
nix run nixpkgs#kubernetes-helm -- repo update
nix run nixpkgs#kubernetes-helm -- show chart prometheus-community/kube-prometheus-stack --version 87.19.2
nix run nixpkgs#kubernetes-helm -- show chart grafana-community/loki --version 18.5.4
nix run nixpkgs#kubernetes-helm -- show chart grafana/alloy --version 1.11.0
```

Create `charts/kube-prometheus-stack/default.nix`:

```nix
{
  repo = "https://prometheus-community.github.io/helm-charts";
  chart = "kube-prometheus-stack";
  version = "87.19.2"; # latest stable as of 2026-07-26
  chartHash = "sha256-JSev9W4hiuZ5CvsTXiaR/YsB5EDDKILCaSQkg8ELkfc=";
}
```

Create `charts/loki/default.nix`:

```nix
{
  repo = "https://grafana-community.github.io/helm-charts";
  chart = "loki";
  version = "18.5.4"; # latest stable as of 2026-07-26
  chartHash = "sha256-PRc5qsX3F1Cn1naqfUMmxyHauCkqIOfjlKbEISu78N0=";
}
```

Create `charts/alloy/default.nix`:

```nix
{
  repo = "https://grafana.github.io/helm-charts";
  chart = "alloy";
  version = "1.11.0"; # latest stable as of 2026-07-26
  chartHash = "sha256-kpCrs9HNmKXYcKOFGsv/mDItE/HyN40XvHjYcbloP90=";
}
```

These recursive hashes were computed through the locked nixidy
`downloadHelmChart` derivation, not from the compressed chart archives.

- [ ] **Step 2: Create the focused import boundary**

Create `machine/globalhawk/observability/default.nix`:

```nix
{...}: {
  imports = [
    ./stack.nix
    ./kubernetes-logs.nix
    ./host.nix
    ./services.nix
    ./sso.nix
    ./alerts.nix
    ./dashboards.nix
  ];
}
```

Create empty module files for those seven imports, each containing:

```nix
{...}: {}
```

Add `./observability` to the imports in `machine/globalhawk/default.nix`,
adjacent to `./k3s`.

- [ ] **Step 3: Format and build**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: the full globalhawk configuration builds and the generated system has
no runtime changes beyond importing empty modules and fetching pinned charts.

- [ ] **Step 4: Commit**

```bash
git add charts/kube-prometheus-stack charts/loki charts/alloy machine/globalhawk/default.nix machine/globalhawk/observability
git commit -m "Prepare pinned inputs so monitoring is reproducible"
```

### Task 2: Deploy the metrics, log-storage, and dashboard foundation

**Files:**
- Modify: `machine/globalhawk/observability/stack.nix`

**Interfaces:**
- Consumes: `charts.kube-prometheus-stack`, `charts.loki`, `config.homelab.ingressSuffix`, and `k8sLib.appLabels`.
- Produces: the `monitoring` namespace; Prometheus at `prometheus-operated.monitoring.svc.cluster.local:9090`; Alertmanager; Grafana at `grafana.monitoring.svc.cluster.local`; Loki's in-cluster write/query endpoint; and the Grafana ingress.

- [ ] **Step 1: Inspect the pinned chart interfaces**

Render the exact pinned defaults rather than relying on remembered values:

```bash
nix run nixpkgs#kubernetes-helm -- show values prometheus-community/kube-prometheus-stack --version 87.19.2 > /tmp/kube-prometheus-stack-values.yaml
nix run nixpkgs#kubernetes-helm -- show values grafana-community/loki --version 18.5.4 > /tmp/loki-values.yaml
rg -n 'retention|retentionSize|volumeClaimTemplate|serviceMonitorSelector|podMonitorSelector|persistence|deploymentMode|Monolithic|filesystem|schemaConfig|compactor|clusterIP' /tmp/kube-prometheus-stack-values.yaml /tmp/loki-values.yaml
```

Expected: identify the exact pinned keys for Prometheus persistence/retention,
Grafana persistence, Alertmanager configuration, monolithic Loki, filesystem
storage, schema v13, and retention compaction. Do not copy `/tmp` files into the
repository.

- [ ] **Step 2: Add the kube-prometheus-stack release**

In `stack.nix`, contribute an application named `monitoring-stack` with
namespace `monitoring`, `createNamespace = true`, and a
`helm.releases.kube-prometheus-stack` release using the pinned chart. Configure:

```nix
{
  prometheus = {
    prometheusSpec = {
      retention = "30d";
      retentionSize = "17GB";
      scrapeInterval = "30s";
      evaluationInterval = "30s";
      serviceMonitorSelectorNilUsesHelmValues = false;
      podMonitorSelectorNilUsesHelmValues = false;
      ruleSelectorNilUsesHelmValues = false;
      serviceMonitorNamespaceSelector = {};
      podMonitorNamespaceSelector = {};
      ruleNamespaceSelector = {};
      storageSpec.volumeClaimTemplate.spec = {
        storageClassName = "local-path";
        accessModes = ["ReadWriteOnce"];
        resources.requests.storage = "20Gi";
      };
      resources = {
        requests = {
          cpu = "250m";
          memory = "1Gi";
        };
        limits.memory = "3Gi";
      };
    };
  };
  alertmanager = {
    enabled = true;
    config = {
      route.receiver = "discard";
      receivers = [{name = "discard";}];
    };
  };
  nodeExporter.enabled = false;
  # k3s embeds these components in one server process instead of exposing the
  # kubeadm-style endpoints expected by the chart.
  kubeControllerManager.enabled = false;
  kubeScheduler.enabled = false;
  kubeProxy.enabled = false;
  kubeEtcd.enabled = false;
  grafana = {
    persistence = {
      enabled = true;
      storageClassName = "local-path";
      accessModes = ["ReadWriteOnce"];
      size = "5Gi";
    };
    resources = {
      requests = {
        cpu = "100m";
        memory = "256Mi";
      };
      limits.memory = "1Gi";
    };
  };
}
```

Adapt only key names confirmed from the pinned chart in Step 1. Disable
chart-owned ingress; author the ingress below so it follows repository
conventions.

Keep kube-apiserver, kubelet/cAdvisor, and CoreDNS monitoring enabled. The live
k3s `kube-dns` Service has the chart-expected `k8s-app: kube-dns` label and
`metrics` port 9153.

- [ ] **Step 3: Add monolithic Loki**

Add `helm.releases.loki` in the same application. Using the exact pinned-chart
keys from Step 1, configure:

```nix
{
  deploymentMode = "Monolithic";
  loki = {
    auth_enabled = false;
    commonConfig.replication_factor = 1;
    storage.type = "filesystem";
    schemaConfig.configs = [
      {
        from = "2024-04-01";
        store = "tsdb";
        object_store = "filesystem";
        schema = "v13";
        index = {
          prefix = "loki_index_";
          period = "24h";
        };
      }
    ];
    compactor = {
      retention_enabled = true;
      delete_request_store = "filesystem";
    };
    limits_config = {
      retention_period = "720h";
      reject_old_samples = true;
      reject_old_samples_max_age = "168h";
      ingestion_rate_mb = 4;
      ingestion_burst_size_mb = 8;
      max_line_size = 65536;
      max_line_size_truncate = true;
      allow_structured_metadata = true;
    };
  };
  singleBinary = {
    replicas = 1;
    persistence = {
      enabled = true;
      storageClass = "local-path";
      size = "30Gi";
    };
    resources = {
      requests = {
        cpu = "200m";
        memory = "512Mi";
      };
      limits.memory = "2Gi";
    };
  };
  gateway = {
    enabled = true;
    # Reserved for host Alloy; verified unused on 2026-07-26.
    service.clusterIP = "10.43.0.50";
  };
  minio.enabled = false;
  backend.replicas = 0;
  read.replicas = 0;
  write.replicas = 0;
}
```

If the pinned chart renamed `singleBinary` to `monolithic`, use the confirmed
new key while preserving one replica and the exact persistence/resource
contract above. Set every unused deployment mode's replica count to zero as
required by the pinned chart.

- [ ] **Step 4: Provision data sources and the Grafana ingress**

Configure chart-provisioned data sources with stable UIDs:

```nix
grafana.additionalDataSources = [
  {
    name = "Loki";
    uid = "loki";
    type = "loki";
    access = "proxy";
    url = "http://loki-gateway.monitoring.svc.cluster.local";
    isDefault = false;
  }
];
```

Keep the chart's Prometheus data source; chart 87.19.2 already assigns it UID
`prometheus`. Add a raw nixidy ingress:

```nix
resources.ingresses.grafana.spec = {
  ingressClassName = "traefik";
  tls = [{hosts = ["grafana${config.homelab.ingressSuffix}"];}];
  rules = [{
    host = "grafana${config.homelab.ingressSuffix}";
    http.paths = [{
      path = "/";
      pathType = "Prefix";
      backend.service = {
        name = "kube-prometheus-stack-grafana";
        port.number = 80;
      };
    }];
  }];
};
```

Do not add a forward-auth annotation.

- [ ] **Step 5: Add baseline NetworkPolicies**

Create monitoring policies that:

- Default-deny ingress for the namespace.
- Permit intra-namespace traffic.
- Permit Traefik pods from `kube-system` to Grafana port 3000/service port 80.
- Permit Prometheus scrapes to monitoring components.
- Permit DNS egress and required in-cluster/application scrape egress.

Do not add a default-deny egress policy in this first task unless every
Prometheus cross-namespace and host-exporter path is explicitly represented;
an incomplete egress policy would silently break discovery.

- [ ] **Step 6: Monitor bundled Traefik**

Add a `PodMonitor` in `monitoring` that selects Traefik pods in `kube-system`:

```nix
{
  apiVersion = "monitoring.coreos.com/v1";
  kind = "PodMonitor";
  metadata = {
    name = "traefik";
    namespace = "monitoring";
  };
  spec = {
    namespaceSelector.matchNames = ["kube-system"];
    selector.matchLabels."app.kubernetes.io/name" = "traefik";
    podMetricsEndpoints = [
      {
        port = "metrics";
        interval = "30s";
        path = "/metrics";
      }
    ];
  };
}
```

The live bundled Traefik pod exposes named port `metrics` on 9100 but its
LoadBalancer Service omits that port, so a PodMonitor is intentional.

- [ ] **Step 7: Build and inspect the rendered package**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
rendered="$(nix eval --raw .#nixosConfigurations.globalhawk.config.services.k3s.workloads.renderedPackage)"
find -L "$rendered" -path '*monitoring*' -type f | sort
```

Expected: the build succeeds; the monitoring render contains the charts,
Grafana ingress, PVC templates, and NetworkPolicies. Inspecting is diagnostic,
not a permanent shape test.

- [ ] **Step 8: Commit**

```bash
git add machine/globalhawk/observability/stack.nix
git commit -m "Establish a bounded local telemetry control plane"
```

### Task 3: Add NixOS host metrics and targeted journal delivery

**Files:**
- Modify: `machine/globalhawk/observability/host.nix`
- Modify: `machine/globalhawk/backup.nix`

**Interfaces:**
- Consumes: `config.services.k3s.clusterNetwork.hostGatewayIp`, the Loki gateway service selected in Task 2, and `/var/lib/prometheus-node-exporter-text-files`.
- Produces: node metrics on port 9100, SMART metrics on 9633, ZFS metrics on 9134, restic textfile metrics, and host journal entries in Loki.

- [ ] **Step 1: Enable host exporters on the k3s gateway**

Configure:

```nix
let
  hostGatewayIp = config.services.k3s.clusterNetwork.hostGatewayIp;
  textfileDir = "/var/lib/prometheus-node-exporter-text-files";
in {
  services.prometheus.exporters = {
    node = {
      enable = true;
      listenAddress = hostGatewayIp;
      openFirewall = false;
      enabledCollectors = ["systemd" "hwmon" "textfile"];
      extraFlags = ["--collector.textfile.directory=${textfileDir}"];
    };
    smartctl = {
      enable = true;
      listenAddress = hostGatewayIp;
      openFirewall = false;
      maxInterval = "5m";
    };
    zfs = {
      enable = true;
      listenAddress = hostGatewayIp;
      openFirewall = false;
      pools = ["pool"];
    };
  };
  systemd.tmpfiles.rules = [
    "d ${textfileDir} 0755 root root -"
  ];
}
```

Use the NixOS exporter modules already present in nixpkgs 26.05; do not package
duplicate exporters.

- [ ] **Step 2: Emit atomic restic textfile metrics**

In `backup.nix`, add a `backupCleanupCommand` that writes metrics to a temporary
file and atomically renames it:

```bash
metrics_dir=/var/lib/prometheus-node-exporter-text-files
metrics_tmp="$metrics_dir/restic-media.prom.$$"
finished="$(date +%s)"
{
  printf 'restic_backup_last_success_timestamp_seconds{backup="media"} %s\n' "$finished"
  printf 'restic_backup_last_status{backup="media"} 1\n'
} > "$metrics_tmp"
chmod 0644 "$metrics_tmp"
mv -f "$metrics_tmp" "$metrics_dir/restic-media.prom"
```

Add the textfile directory to the restic service's writable paths if service
hardening requires it. Update `restic-media-failure.service` to atomically
write:

```text
restic_backup_last_status{backup="media"} 0
```

while preserving the existing email body and comments. Do not remove or replace
the existing failure notification.

- [ ] **Step 3: Configure host Alloy**

Enable the NixOS Alloy module and write
`environment.etc."alloy/host-logs.alloy".text`. Configure one
`loki.source.journal` reader, relabel these journal fields:

```alloy
loki.relabel "host_journal" {
  forward_to = []

  rule {
    source_labels = ["__journal__systemd_unit"]
    target_label  = "unit"
  }
  rule {
    source_labels = ["__journal_priority_keyword"]
    target_label  = "priority"
  }
  rule {
    source_labels = ["__journal__transport"]
    target_label  = "transport"
  }
  rule {
    source_labels = ["__journal__boot_id"]
    target_label  = "boot_id"
  }
  rule {
    source_labels = ["__journal__hostname"]
    target_label  = "host"
  }
}
```

Feed it through one `loki.process` pipeline. Keep entries when `unit` matches:

```text
k3s|plex|adguardhome|smartd|zfs-(import.*|mount.*|zed|scrub.*|trim.*|media-posixacl)|restic-backups-media|restic-media-failure|nixos-upgrade|nix-gc|nix-optimise|network-addresses-enp1s0|wpa_supplicant
```

or when priority is `warning`, `err`, `crit`, `alert`, or `emerg`, or transport
is `kernel` with one of those priorities. Drop all other entries before
`loki.write`.

Point `loki.write` at `http://10.43.0.50/loki/api/v1/push`. In Task 2, assign
`10.43.0.50` to the Loki gateway Service; it was unused in the live cluster on
2026-07-26 and is inside the pinned `10.43.0.0/16` service range. Document that
reservation beside both the Kubernetes Service and host Alloy URL. Do not use a
NodePort: `tailscale0` is trusted and would unintentionally expose Loki to the
tailnet.

Enable:

```nix
services.alloy = {
  enable = true;
  extraFlags = [
    "--server.http.listen-addr=${hostGatewayIp}:12345"
    "--disable-reporting"
  ];
};
```

The NixOS module already grants `systemd-journal`; do not run Alloy as root.

- [ ] **Step 4: Add Prometheus monitors for host endpoints**

In the nixidy contribution inside `host.nix`, create selector-less Services and
EndpointSlices in `monitoring`, following the existing Plex/AdGuard pattern.
Use service names `globalhawk-node`, `globalhawk-smartctl`,
`globalhawk-zfs`, and `globalhawk-alloy`; all endpoints point to
`hostGatewayIp`.

Add `ServiceMonitor` raw resources with 30-second intervals for node/ZFS/Alloy
and 5 minutes for SMART. Give every target stable labels:

```yaml
cluster: globalhawk
node: globalhawk
source: nixos
```

- [ ] **Step 5: Build**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: the NixOS units, Alloy configuration, and monitoring resources build
without opening new firewall ports.

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/observability/host.nix machine/globalhawk/backup.nix
git commit -m "Expose host failures without widening the network boundary"
```

### Task 4: Collect all Kubernetes pod logs and events

**Files:**
- Modify: `machine/globalhawk/observability/kubernetes-logs.nix`

**Interfaces:**
- Consumes: `charts.alloy` and `http://loki-gateway.monitoring.svc.cluster.local/loki/api/v1/push`.
- Produces: one Alloy DaemonSet that writes all pod logs and Kubernetes events to Loki with the approved label set.

- [ ] **Step 1: Inspect the pinned Alloy chart**

Run:

```bash
nix run nixpkgs#kubernetes-helm -- show values grafana/alloy --version 1.11.0 > /tmp/alloy-values.yaml
rg -n 'controller|daemonset|configMap|rbac|serviceMonitor|resources|mounts' /tmp/alloy-values.yaml
```

Expected: confirm the pinned keys for a DaemonSet controller, inline Alloy
configuration, RBAC, ServiceMonitor, and resource settings.

- [ ] **Step 2: Configure pod discovery and labels**

Add an Alloy Helm release in namespace `monitoring`. Run it as a DaemonSet with
RBAC sufficient to watch pods, namespaces, nodes, and events. Use
`discovery.kubernetes "pod"` plus `discovery.relabel "pod_logs"` and keep only
pods whose node name equals the Alloy pod's node.

Map only:

```alloy
namespace = __meta_kubernetes_namespace
pod       = __meta_kubernetes_pod_name
container = __meta_kubernetes_pod_container_name
node      = __meta_kubernetes_pod_node_name
```

Derive `workload` from the owning controller name, stripping ReplicaSet hashes
where possible. Add static `cluster = "globalhawk"`. Do not promote UID,
container ID, image, annotations, or arbitrary parsed fields.

Feed discovered targets to `loki.source.kubernetes`, then a process stage that
adds `stream` from stdout/stderr and writes to:

```text
http://loki-gateway.monitoring.svc.cluster.local/loki/api/v1/push
```

- [ ] **Step 3: Add Kubernetes events**

Add one `loki.source.kubernetes_events` component with:

```alloy
loki.source.kubernetes_events "cluster" {
  job_name   = "integrations/kubernetes/eventhandler"
  log_format = "logfmt"
  forward_to = [loki.process.kubernetes_events.receiver]
}
```

Add only `cluster = "globalhawk"` and `source = "kubernetes-events"` as static
labels before writing to Loki. Because globalhawk has one node and one Alloy
DaemonSet pod, this produces one event stream without duplicates.

- [ ] **Step 4: Enable Alloy self-monitoring and limits**

Set resource requests to 100m CPU/128 MiB and a 512 MiB memory limit. Enable the
chart's ServiceMonitor. Configure the Alloy service to be cluster-internal.
Loki, not Alloy, owns the 64 KiB line-size and ingestion limits defined in Task
2.

- [ ] **Step 5: Build**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: the full configuration builds and the rendered DaemonSet has no
host-journal mount or privileged security context.

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/observability/kubernetes-logs.nix
git commit -m "Make cluster failures queryable across every namespace"
```

### Task 5: Add least-privilege Grafana and service runtime secrets

**Files:**
- Modify: `machine/globalhawk/sops.nix`
- Modify: `secrets/globalhawk.sops.yaml` (operator action)

**Interfaces:**
- Consumes: the typed `services.k3s.runtimeSecrets` module.
- Produces: `grafana-secrets`, `plex-exporter`, and `adguard-exporter` Kubernetes Secrets plus the Grafana hash in Authelia's client-hash Secret.

- [ ] **Step 1: Declare the SOPS keys**

Add these keys to `sops.secrets`:

```nix
grafana_admin_password = {};
grafana_oidc_client_secret = {};
grafana_oidc_client_secret_hash = {};
plex_api_token = {};
adguard_metrics_password = {};
```

Extend `authelia-oidc-client-hashes.stringData`:

```nix
grafana.sopsSecret = "grafana_oidc_client_secret_hash";
```

Add runtime Secrets:

```nix
grafana-secrets = {
  namespace = "monitoring";
  stringData = {
    admin-password.sopsSecret = "grafana_admin_password";
    oidc-client-secret.sopsSecret = "grafana_oidc_client_secret";
  };
};
plex-exporter = {
  namespace = "monitoring";
  stringData.token.sopsSecret = "plex_api_token";
};
adguard-exporter = {
  namespace = "monitoring";
  stringData.password.sopsSecret = "adguard_metrics_password";
};
```

Do not combine service credentials into Grafana's Secret.

- [ ] **Step 2: Have the operator populate encrypted values**

Generate a long random Grafana password and OIDC client secret. Generate the
Authelia-compatible digest with the same Authelia CLI/chart version used by the
existing clients:

```bash
openssl rand -base64 36
kubectl -n auth exec deploy/authelia -- authelia crypto hash generate argon2 --password 'OIDC_CLIENT_SECRET'
sops secrets/globalhawk.sops.yaml
```

The operator adds the five encrypted keys. `plex_api_token` is an existing Plex
administrator API token. `adguard_metrics_password` is the plaintext password
for the existing `admin` account; the declarative bcrypt hash cannot be used to
authenticate to the AdGuard API.

Do not paste any value into shell history in the final implementation. Prefer a
temporary protected environment variable or interactive prompt when executing
the hash command.

- [ ] **Step 3: Build without exposing plaintext**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: the build succeeds using SOPS placeholders; `git diff` contains only
encrypted SOPS values and mappings, never plaintext.

- [ ] **Step 4: Commit**

```bash
git add machine/globalhawk/sops.nix secrets/globalhawk.sops.yaml
git commit -m "Keep monitoring credentials scoped to their consumers"
```

### Task 6: Integrate Grafana with native Authelia OIDC

**Files:**
- Modify: `machine/globalhawk/services/authelia.nix`
- Modify: `machine/globalhawk/observability/sso.nix`

**Interfaces:**
- Consumes: `grafana-secrets/oidc-client-secret`, `authelia-oidc-client-hashes/grafana`, and the existing canonical Authelia issuer.
- Produces: admins-only Authelia client `grafana` and Grafana Generic OAuth settings.

- [ ] **Step 1: Add an admins-only Authelia authorization policy**

Beside the existing `family` policy, add:

```nix
admin = {
  default_policy = "deny";
  rules = [
    {
      policy = "two_factor";
      subject = ["group:admins"];
    }
  ];
};
```

Do not broaden the existing family policy.

- [ ] **Step 2: Add Grafana's claims policy and client**

Add:

```nix
claims_policies.grafana.id_token = [
  "email"
  "name"
  "groups"
  "preferred_username"
];
```

Append this client:

```nix
{
  client_id = "grafana";
  client_name = "Grafana";
  client_secret.path = "/secrets/authelia-oidc-client-hashes/grafana";
  authorization_policy = "admin";
  claims_policy = "grafana";
  public = false;
  require_pkce = true;
  pkce_challenge_method = "S256";
  redirect_uris = [
    "https://grafana${ingressSuffix}/login/generic_oauth"
  ];
  scopes = ["openid" "profile" "email" "groups"];
  response_types = ["code"];
  grant_types = ["authorization_code"];
  token_endpoint_auth_method = "client_secret_basic";
  access_token_signed_response_alg = "none";
  userinfo_signed_response_alg = "none";
}
```

- [ ] **Step 3: Configure Grafana Generic OAuth**

In `sso.nix`, extend
`applications.monitoring-stack.helm.releases.kube-prometheus-stack.values.grafana`
through `services.k3s.workloads.module`. Read the client secret from
`grafana-secrets` using chart 87.19.2's `envValueFrom` secret-key reference.
Configure:

```ini
[server]
root_url = https://grafana${config.homelab.ingressSuffix}

[auth]
disable_login_form = false
oauth_auto_login = false

[auth.generic_oauth]
enabled = true
name = Authelia
client_id = grafana
scopes = openid profile email groups
auth_url = https://auth${config.homelab.ingressSuffix}/api/oidc/authorization
token_url = https://auth${config.homelab.ingressSuffix}/api/oidc/token
api_url = https://auth${config.homelab.ingressSuffix}/api/oidc/userinfo
login_attribute_path = preferred_username
name_attribute_path = name
groups_attribute_path = groups
use_pkce = true
auth_style = InHeader
allow_sign_up = true
role_attribute_path = contains(groups[*], 'admins') && 'Admin' || 'None'
role_attribute_strict = true
allow_assign_grafana_admin = false
```

Pass the secret through `GF_AUTH_GENERIC_OAUTH_CLIENT_SECRET`. Pass
`grafana-secrets/admin-password` as `GF_SECURITY_ADMIN_PASSWORD`; set the admin
user to `admin`. Never embed either value in `grafana.ini`.

- [ ] **Step 4: Ensure in-cluster issuer resolution**

The existing exact CoreDNS rewrite for `auth${ingressSuffix}` already maps the
canonical issuer to Traefik. Reuse it. Do not add a second issuer URL or use the
cluster-local Authelia Service as the OIDC issuer.

- [ ] **Step 5: Build**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: Authelia and Grafana render with secret references; the Grafana
Ingress has no forward-auth middleware.

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/services/authelia.nix machine/globalhawk/observability/sso.nix
git commit -m "Let Grafana own its authenticated session and roles"
```

### Task 7: Add Plex and AdGuard aggregate metrics

**Files:**
- Modify: `machine/globalhawk/observability/services.nix`
- Create only if required by the decision gate: `packages/plex-exporter.nix`
- Create only if required by the decision gate: `packages/adguard-exporter.nix`
- Modify only if a local package is created: `packages/overlays/linux.nix`

**Interfaces:**
- Consumes: `plex-exporter/token`, `adguard-exporter/password`, Plex at `hostGatewayIp:32400`, and AdGuard at `hostGatewayIp:3000`.
- Produces: cluster-internal exporter Services and `ServiceMonitor` targets with aggregate, privacy-audited metrics.

- [ ] **Step 1: Apply the adapter decision gate**

Evaluate candidate source repositories and images. A candidate passes only if:

- Its source or release has maintenance activity within the last 12 months.
- The source revision and container digest/package source can be pinned.
- It works with the deployed Plex/AdGuard API versions.
- It exposes only aggregate metrics or supports dropping disallowed labels
  before ingestion.
- It does not log API credentials or query payloads.

For Plex, begin with `jsclayton/prometheus-plex-exporter` but do not accept it
without checking its latest commit and emitted metric labels. For AdGuard, do
not use an archived exporter merely because an old dashboard references it.

If a candidate fails, implement the minimal read-only collector described in
the design using a small Go package. Its `/metrics` handler must return errors
as HTTP 500 and log them; production code must not use `panic`, `log.Fatal`, or
unchecked type assertions. It may expose only:

```text
service_up
service_api_request_duration_seconds
plex_active_sessions
plex_transcoding_sessions
plex_library_items
adguard_queries_total
adguard_blocked_queries_total
adguard_query_duration_seconds
adguard_protection_enabled
adguard_filter_enabled
```

Do not add a collector merely to normalize an already suitable maintained
exporter.

- [ ] **Step 2: Deploy the Plex adapter**

Deploy one replica in `monitoring`. Read its token from
`plex-exporter/token`, target `http://${hostGatewayIp}:32400`, expose only a
cluster-internal Service, and add a 60-second `ServiceMonitor`.

Before allowing Prometheus to scrape it, inspect `/metrics` manually from a
temporary pod and list every label name. Add metric relabel drops for any
series carrying title, user, client, address, path, session, filename, or URL
labels. If dropping those labels would merge semantically distinct counters,
drop the entire metric instead.

- [ ] **Step 3: Deploy the AdGuard adapter**

Deploy one replica in `monitoring`. Use username `admin`, read the password from
`adguard-exporter/password`, target `http://${hostGatewayIp}:3000`, expose only
a cluster-internal Service, and add a 60-second `ServiceMonitor`.

Apply the same label audit. Drop client- and domain-bearing series entirely.

- [ ] **Step 4: Build**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: the adapters are pinned, credentials are Secret references, and
neither Service has an Ingress or NodePort.

- [ ] **Step 5: Commit**

```bash
git add machine/globalhawk/observability/services.nix packages
git commit -m "Expose service health without retaining household activity"
```

If no package files changed, stage only `services.nix`.

### Task 8: Provision dashboards and actionable rules

**Files:**
- Modify: `machine/globalhawk/observability/dashboards.nix`
- Modify: `machine/globalhawk/observability/alerts.nix`
- Create: `machine/globalhawk/observability/dashboards/globalhawk-overview.json`
- Create: `machine/globalhawk/observability/dashboards/host-health.json`
- Create: `machine/globalhawk/observability/dashboards/services.json`
- Create: `machine/globalhawk/observability/dashboards/logs.json`
- Create: `machine/globalhawk/observability/dashboards/observability.json`

**Interfaces:**
- Consumes: data-source UIDs `prometheus` and `loki`, metrics/log labels defined by Tasks 2–7, and kube-prometheus-stack's upstream dashboards/rules.
- Produces: stable dashboard UIDs and globalhawk-owned `PrometheusRule` groups.

- [ ] **Step 1: Provision dashboard folders and files**

Create one ConfigMap per dashboard or one size-safe ConfigMap for all five JSON
files. Label it with the kube-prometheus Grafana dashboard-sidecar label and
folder annotation confirmed from the pinned chart.

Use these stable UIDs and titles:

```text
globalhawk-overview  — Globalhawk Overview
globalhawk-host      — Host Health
globalhawk-services  — Plex and AdGuard
globalhawk-logs      — Logs and Events
globalhawk-telemetry — Observability Stack
```

Each dashboard uses a 30-second refresh and variables only for bounded labels
such as namespace, workload, unit, and severity.

- [ ] **Step 2: Build the overview and host dashboards**

The overview must show:

- Active warning/critical alerts.
- Node readiness, pod readiness, and restart count.
- CPU, memory, root filesystem, and ZFS utilization.
- SMART/ZFS/restic status.
- Plex and AdGuard reachability.
- Prometheus target health and Loki rejection rate.

The host dashboard must show:

- CPU by mode, load, memory, swap, and network rates.
- Root and ZFS bytes/inodes.
- hwmon temperatures.
- SMART/NVMe health, temperature, wear, and errors.
- ZFS state, errors, capacity, scrub age.
- failed systemd units and restic age/status.
- a linked Loki panel filtered to `{source="nixos"}`.

Use PromQL metric discovery against the live exporters during the operator
phase to confirm exact SMART/ZFS metric names before finalizing those panels;
do not guess names from a different exporter.

- [ ] **Step 3: Build service, log, and self-monitoring dashboards**

The service dashboard shows only aggregate Plex and AdGuard metrics approved by
the label audit.

The log dashboard provides:

- `{cluster="globalhawk"}` pod logs with namespace/workload/container filters.
- `{source="nixos"}` host logs with unit/priority filters.
- `{source="kubernetes-events"}` events.

The observability dashboard shows Prometheus samples/targets/storage,
Alertmanager health, Loki ingestion/rejections/queries, Alloy component health,
and Grafana process health.

- [ ] **Step 4: Add globalhawk-owned alert rules**

Create declarative `PrometheusRule` groups with 30-second evaluation. Reuse
upstream kube-prometheus rules rather than duplicating them. Add only
globalhawk-specific rules:

- Root filesystem: warning above 70% for 15m, critical above 90% for 5m.
- ZFS pool: critical whenever health is not ONLINE; warning on scrub age above
  the configured scrub cadence plus 48h.
- SMART/NVMe: critical health failure; warning temperature/wear thresholds
  supported by actual exporter metrics.
- Restic: warning when last success is older than 30h; critical on
  `restic_backup_last_status == 0` or age above 48h.
- Important systemd unit failed for 5m.
- Plex/AdGuard target down for 5m.
- Sustained AdGuard latency or upstream failure only after the metric contract
  is confirmed.
- Loki rejected lines above zero for 5m or ingestion exceeding the measured
  safe baseline.
- Root storage projection threatening the 30-day envelope.

Every rule includes:

```yaml
severity: warning|critical
cluster: globalhawk
```

and annotations `summary`, `description`, and `runbook_url` pointing to a stable
Grafana dashboard URL. Plex activity and AdGuard block percentage remain
dashboard-only.

- [ ] **Step 5: Build**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
```

Expected: dashboard JSON is valid because chart rendering consumes each
ConfigMap value, alert expressions render, and no UI-exported numeric dashboard
IDs are committed.

- [ ] **Step 6: Commit**

```bash
git add machine/globalhawk/observability/dashboards.nix machine/globalhawk/observability/dashboards machine/globalhawk/observability/alerts.nix
git commit -m "Give failures a durable operator-facing diagnosis path"
```

### Task 9: Activate in stages and perform live acceptance

**Files:**
- Modify if results require durable tuning: `machine/globalhawk/observability/*.nix`
- Modify if results require durable tuning: `machine/globalhawk/observability/dashboards/*.json`

**Interfaces:**
- Consumes: the complete built NixOS generation and the acceptance checklist in the design.
- Produces: a running, measured 30-day observability installation with documented initial tuning.

- [ ] **Step 1: Review the build and desired/live diff**

Run read-only checks:

```bash
nixos-rebuild build --flake .#globalhawk
nix run .#k3s-drift
git status --short
```

Expected: the build succeeds; drift shows only the intentional monitoring and
Authelia additions; the worktree is clean.

- [ ] **Step 2: Operator activates the generation**

With explicit operator authorization:

```bash
sudo nixos-rebuild switch --flake .#globalhawk
```

Watch:

```bash
kubectl -n monitoring get pods,pvc
kubectl -n monitoring rollout status deploy/kube-prometheus-stack-operator
kubectl -n monitoring rollout status deploy/kube-prometheus-stack-grafana
kubectl -n monitoring get prometheus,alertmanager
```

Use the actual rendered resource names if the pinned charts prefix them
differently.

- [ ] **Step 3: Validate authentication and recovery**

- Log into Grafana through Authelia as an `admins` user.
- Confirm a non-admin Authelia account is denied.
- Confirm the OIDC user is organization `Admin`, not server admin.
- Run:

```bash
kubectl -n monitoring port-forward svc/kube-prometheus-stack-grafana 3000:80
```

and confirm the local `admin` account can sign in at
`http://127.0.0.1:3000/login`.

- [ ] **Step 4: Validate metrics and privacy**

In Grafana/Prometheus:

- Confirm every expected target is healthy.
- Confirm each Kubernetes namespace has kube-state/cAdvisor data.
- Confirm CPU, memory, disk, inode, network, temperature, SMART/NVMe, ZFS,
  systemd, restic, Plex, and AdGuard panels have current samples.
- Inspect Plex and AdGuard series labels and confirm none of the forbidden
  household/activity labels exist.
- Confirm exporter ports are unreachable from the LAN and tailnet but reachable
  from a monitoring pod.

- [ ] **Step 5: Validate logs and events**

Create a temporary pod that writes a unique non-secret line, then remove it:

```bash
kubectl run observability-log-check --image=busybox:1.36 --restart=Never -- sh -c 'echo globalhawk-observability-log-check'
kubectl wait --for=condition=Ready pod/observability-log-check --timeout=60s || true
kubectl delete pod observability-log-check
```

Confirm the line appears in Loki. Trigger a harmless warning in a temporary
systemd unit or use an existing recent warning and confirm:

- Allowlisted unit info/warnings appear.
- Non-allowlisted routine info does not appear.
- Non-allowlisted warnings do appear.
- Kubernetes events appear once, not duplicated.

- [ ] **Step 6: Validate alert lifecycle**

Create a temporary Deployment with an impossible image, wait for the appropriate
pending/image-pull alert, then delete it:

```bash
kubectl create deployment observability-alert-check --image=registry.invalid/does-not-exist:test
kubectl delete deployment observability-alert-check
```

Confirm the alert fires after its configured `for` duration and resolves after
deletion. Do not alter a production workload to test alerting.

- [ ] **Step 7: Validate persistence**

Restart Grafana, Prometheus, Loki, and Alloy one at a time with
`kubectl rollout restart`, waiting for readiness between components. Confirm:

- Provisioned dashboards and OIDC still work.
- Existing Prometheus samples remain queryable.
- Existing Loki entries remain queryable.
- Alloy resumes without duplicating a large journal window.

- [ ] **Step 8: Tune after 7–14 days**

Record actual Prometheus and Loki disk growth, memory use, ingestion rates,
rejected lines, and noisy streams. Make only evidence-backed changes to:

- Loki rate limits and noise filters.
- Resource requests/limits.
- Alert thresholds and `for` durations.
- Dashboard queries.

Keep 30-day retention unless measured growth threatens the root reserve. If it
does, shorten Loki retention first as specified by the design.

- [ ] **Step 9: Final build, drift check, and commit tuning**

Run:

```bash
nix fmt
nixos-rebuild build --flake .#globalhawk
nix run .#k3s-drift
```

Expected: the build succeeds and drift reports no unexplained hand-created
resources.

If tuning changed tracked files:

```bash
git add machine/globalhawk/observability
git commit -m "Tune monitoring around measured globalhawk behavior"
```
