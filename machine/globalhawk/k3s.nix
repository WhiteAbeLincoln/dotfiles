# Single-node k3s + the delivery lanes described in
# docs/superpowers/plans/2026-07-22-globalhawk-k3s-migration.md.
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
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
    manifests = {
      # Our nixidy-authored workloads, delivered as one multi-doc file.
      nixidy.source = nixidyCombined;
      # Third-party controller: pinned upstream YAML, applied before our CRs
      # (k3s retries until the CRDs it defines are established).
      cert-manager.source = certManagerYaml;
    };
  };

  # kubectl/kubeseal/helm on PATH for the operator. No global KUBECONFIG env: the
  # admin config at /etc/rancher/k3s/k3s.yaml is root-only, so operators use
  # `sudo k3s kubectl ...` (which finds it automatically), and the sandbox agent
  # user gets its own read-only kubeconfig via services.aiAgentSandbox.k3s.
  environment.systemPackages = [pkgs.kubectl pkgs.kubernetes-helm pkgs.kubeseal];
}
