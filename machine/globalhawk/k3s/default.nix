# Single-node k3s + the delivery lanes described in
# docs/superpowers/plans/2026-07-22-globalhawk-k3s-migration.md.
{
  config,
  pkgs,
  lib,
  ...
}: let
  lan = config.homelab.network;
  clusterNetwork = config.services.k3s.clusterNetwork;
  # cert-manager controller, installed from pinned upstream release YAML (plain
  # YAML, no Helm). Includes its CRDs + namespace + deployments.
  certManagerVersion = "v1.16.2";
  certManagerYaml = pkgs.fetchurl {
    url = "https://github.com/cert-manager/cert-manager/releases/download/${certManagerVersion}/cert-manager.yaml";
    hash = "sha256-HVHN7NRC8fX4l4Pp4BabldNyck2iA8x13XpcTlChDOY=";
  };
in {
  services.k3s.workloads = {
    enable = true;
    module = let
      secrets = import ../../../secrets/globalhawk.nix;
    in {
      # nixidy requires a target repo/branch even when we consume the rendered
      # YAML directly. These values are never pushed anywhere.
      nixidy.target.repository = "file:///dev/null";
      nixidy.target.branch = "main";

      # Vendored Helm charts (FODs). mkChartAttrs walks this dir for default.nix
      # files and exposes them as the `charts` arg to every module.
      nixidy.chartsDir = ../../../charts;

      imports = [
        ./infra/cert-manager.nix
        ./infra/wildcard-tls.nix
      ];

      _module.args = {
        k8sLib = import ./lib.nix { inherit lib; };
        common = {
          acmeEmail = secrets.acme_email;
          ingressSuffix = config.homelab.ingressSuffix;
        };
      };
    };
  };

  services.k3s = {
    enable = true;
    role = "server";
    # Traefik (bundled) and servicelb (klipper) are kept — do NOT disable them.
    # Graceful shutdown so pods drain on reboot.
    gracefulNodeShutdown.enable = true;
    # Pin the cluster network so it's identical on a rebuild — this is what makes
    # cluster-net.nix's hostGatewayIp a guaranteed constant. These match k3s's
    # current defaults, so pinning them is a no-op on the running cluster.
    extraFlags = [
      "--cluster-cidr=${clusterNetwork.podCidr}"
      "--service-cidr=${clusterNetwork.serviceCidr}"
      # Pin the node IP + flannel interface to the static LAN address. Without
      # this k3s auto-detects from the default route, which broke when the node's
      # IP moved from its old DHCP lease to the typed static LAN IP: a running k3s
      # kept the stale IP for the Node InternalIP, flannel public-ip, and the
      # kubernetes apiserver endpoint, so pods hit "no route to host" on service
      # IPs. Pinning also prevents picking the wrong NIC (wlo1 is also up).
      "--node-ip=${lan.lanIp}"
      "--flannel-iface=${lan.lanInterface}"
    ];
    manifests = {
      # Our nixidy-authored workloads, delivered as ONE always-present multi-doc
      # file. This single-file shape is load-bearing for cleanup: k3s tracks it
      # as a single `nixidy` Addon and re-applies with wrangler's objectset apply
      # (WithOwner+WithGVK), which PRUNES by default. So removing a workload from
      # k8s/** is a content change to this file — which k3s prunes automatically
      # on the next `switch` (verified 2026-07-23) — NOT a file deletion. Pruning
      # only breaks if the Addon itself vanishes. `services.k3s.workloads.enable`
      # and the imported integration module own the always-present
      # `services.k3s.manifests.nixidy` Addon; don't disable or remove them to
      # "clean up" — that orphans every child. Use `nix run .#k3s-drift` to
      # verify live vs desired.
      # Third-party controllers: pinned upstream YAML, applied before our CRs
      # (k3s retries until the CRDs they define are established).
      cert-manager.source = certManagerYaml;
    };
  };

  # kubectl/helm/sops on PATH for the operator. The trusted user gets a
  # ready-to-use admin kubeconfig at ~/.kube/config (below); the sandbox agent
  # user gets its own read-only kubeconfig via services.aiAgentSandbox.k3s.
  # sops replaces kubeseal — secrets are managed via machine/globalhawk/sops.nix.
  environment.systemPackages = [pkgs.kubectl pkgs.kubernetes-helm pkgs.sops pkgs.k9s];

  # k3s applies the combined manifest through Wrangler objectset pruning.
  # EndpointSlices inherit their Service's objectset hash label, so a manifest
  # re-apply can mistake controller-owned slices for removed manifest objects
  # and prune them. Kubernetes does not recreate a deleted slice until the
  # corresponding Service or Pod changes, leaving Traefik and ClusterIP routing
  # without backends after some k3s restarts.
  #
  # Re-enqueue only selector-backed Services that are missing a controller-owned
  # slice. Repeat briefly because the Addon controller and EndpointSlice
  # controller start asynchronously with k3s. This preserves objectset pruning
  # for actual manifest resources instead of marking Services as non-prunable.
  systemd.services.k3s-endpointslice-reconcile = {
    description = "Restore EndpointSlices pruned during k3s manifest reconciliation";
    after = ["k3s.service"];
    requires = ["k3s.service"];
    wantedBy = ["multi-user.target"];
    path = [pkgs.kubectl pkgs.coreutils pkgs.jq];
    serviceConfig.Type = "oneshot";
    script = ''
      export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

      for _ in $(seq 1 30); do
        if kubectl get --raw=/readyz >/dev/null 2>&1; then
          while IFS=$'\t' read -r namespace name; do
            kubectl annotate service \
              --namespace "$namespace" \
              "$name" \
              endpointslice.kubernetes.io/reconcile-at="$(date +%s%N)" \
              --overwrite
          done < <(
            kubectl get services --all-namespaces -o json |
              jq --raw-output \
                --slurpfile slices <(kubectl get endpointslices --all-namespaces -o json) \
                '
                  .items[]
                  | select((.spec.selector // {} | length) > 0)
                  | . as $service
                  | select(
                      any(
                        $slices[0].items[];
                        .metadata.namespace == $service.metadata.namespace
                        and .metadata.labels["kubernetes.io/service-name"] == $service.metadata.name
                        and .metadata.labels["endpointslice.kubernetes.io/managed-by"] == "endpointslice-controller.k8s.io"
                      )
                      | not
                    )
                  | [.metadata.namespace, .metadata.name]
                  | @tsv
                '
          )
        fi
        sleep 2
      done
    '';
  };

  # Convenience: give the trusted operator (meta.user) a ready-to-use admin
  # kubeconfig at ~/.kube/config so kubectl/k9s work with no sudo and no
  # KUBECONFIG juggling. A root oneshot copies k3s's admin config into the
  # operator's home once k3s has written it.
  #
  # Copy (not a symlink, and NOT --write-kubeconfig-mode=0644): the admin config
  # is full cluster-admin, and the read-only sandbox agent shares the operator's
  # `users` group — making /etc/rancher/k3s/k3s.yaml group/world-readable would
  # hand the agent cluster-admin. An 0600 file in the operator's home is not.
  systemd.services.operator-kubeconfig = let
    user = config.meta.user;
    home = config.users.users.${user}.home;
  in {
    description = "Install the k3s admin kubeconfig for ${user}";
    after = ["k3s.service"];
    wants = ["k3s.service"];
    wantedBy = ["multi-user.target"];
    path = [pkgs.coreutils];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      # k3s writes its admin kubeconfig early in startup; wait briefly for it.
      for _ in $(seq 1 30); do [ -f /etc/rancher/k3s/k3s.yaml ] && break; sleep 1; done
      install -d -o ${user} -g users -m 0700 ${home}/.kube
      install -o ${user} -g users -m 0600 /etc/rancher/k3s/k3s.yaml ${home}/.kube/config
    '';
  };
}
