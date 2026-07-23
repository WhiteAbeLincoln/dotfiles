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
    ./infra/network.nix
    ./infra/cert-manager.nix
    ./infra/wildcard-tls.nix
    # ./apps/whoami.nix  # debugging canary; enable to test ingress/TLS/routing
    ./apps/plex.nix
    ./apps/arr.nix
    ./apps/torrent.nix
  ];
}
