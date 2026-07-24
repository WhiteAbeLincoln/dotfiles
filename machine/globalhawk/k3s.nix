# Single-node k3s + the delivery lanes described in
# docs/superpowers/plans/2026-07-22-globalhawk-k3s-migration.md.
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  net = import ./facts.nix;
  # The nixidy-rendered YAML tree. nixidy rev deb28dc exposes this as
  # `environmentPackage` (there is no `build` attr) — its build output is the
  # apps/ + <namespace>/ YAML tree.
  nixidyManifests = inputs.self.nixidyEnvs.x86_64-linux.globalhawk.environmentPackage;

  # Bridge: concatenate every rendered document into one multi-doc manifest,
  # EXCLUDING nixidy's apps/ dir (those are ArgoCD Application CRs whose kind
  # k3s does not know — we deliver the workloads directly, not via ArgoCD).
  # -L: nixidy's environmentPackage is a tree of SYMLINKS (env/<ns> ->
  # /nix/store/nixidy-app-<ns>), so find must follow them or it descends into
  # nothing. The -path exclusion still matches the logical prefix, so apps/
  # (ArgoCD Application CRs) stays excluded.
  # cert-manager controller, installed from pinned upstream release YAML (plain
  # YAML, no Helm). Includes its CRDs + namespace + deployments.
  certManagerVersion = "v1.16.2";
  certManagerYaml = pkgs.fetchurl {
    url = "https://github.com/cert-manager/cert-manager/releases/download/${certManagerVersion}/cert-manager.yaml";
    hash = "sha256-HVHN7NRC8fX4l4Pp4BabldNyck2iA8x13XpcTlChDOY=";
  };

  nixidyCombined = pkgs.runCommand "nixidy-globalhawk.yaml" {} ''
    : > "$out"
    for f in $(${pkgs.findutils}/bin/find -L ${nixidyManifests} -name '*.yaml' -not -path '*/apps/*' | sort); do
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
    # Pin the cluster network so it's identical on a rebuild — this is what makes
    # cluster-net.nix's hostGatewayIp a guaranteed constant. These match k3s's
    # current defaults, so pinning them is a no-op on the running cluster.
    extraFlags = [
      "--cluster-cidr=${net.podCidr}"
      "--service-cidr=${net.serviceCidr}"
      # Pin the node IP + flannel interface to the static LAN address. Without
      # this k3s auto-detects from the default route, which broke when the node's
      # IP moved from its old DHCP lease to the static facts.lanIp: a running k3s
      # kept the stale IP for the Node InternalIP, flannel public-ip, and the
      # kubernetes apiserver endpoint, so pods hit "no route to host" on service
      # IPs. Pinning also prevents picking the wrong NIC (wlo1 is also up).
      "--node-ip=${net.lanIp}"
      "--flannel-iface=${net.lanInterface}"
    ];
    manifests = {
      # Our nixidy-authored workloads, delivered as ONE always-present multi-doc
      # file. This single-file shape is load-bearing for cleanup: k3s tracks it
      # as a single `nixidy` Addon and re-applies with wrangler's objectset apply
      # (WithOwner+WithGVK), which PRUNES by default. So removing a workload from
      # k8s/** is a content change to this file — which k3s prunes automatically
      # on the next `switch` (verified 2026-07-23) — NOT a file deletion. Pruning
      # only breaks if the Addon itself vanishes: don't remove this source entry
      # or set enable=false to "clean up" — that orphans every child. Use
      # `nix run .#k3s-drift` to verify live vs desired.
      nixidy.source = nixidyCombined;
      # Third-party controllers: pinned upstream YAML, applied before our CRs
      # (k3s retries until the CRDs they define are established).
      cert-manager.source = certManagerYaml;
    };
  };

  # kubectl/helm/sops on PATH for the operator. No global KUBECONFIG env: the
  # admin config at /etc/rancher/k3s/k3s.yaml is root-only, so operators use
  # `sudo k3s kubectl ...` (which finds it automatically), and the sandbox agent
  # user gets its own read-only kubeconfig via services.aiAgentSandbox.k3s.
  # sops replaces kubeseal — secrets are managed via machine/globalhawk/sops.nix.
  environment.systemPackages = [pkgs.kubectl pkgs.kubernetes-helm pkgs.sops];
}
