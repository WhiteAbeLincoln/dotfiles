# Read-only reconciliation check between the nixidy desired state and live k3s.
# Never mutates; safe to run against any kubeconfig. See ./k3s-drift.py for the
# three drift buckets it reports. Deliberately NOT part of `nix flake check`
# (needs a live cluster) — exposed as `nix run .#k3s-drift`, run from the repo
# root so its `nix build` of the nixidy env resolves the working tree.
{
  writeShellApplication,
  python3,
  kubectl,
  nix,
  coreutils,
}: let
  py = python3.withPackages (ps: [ps.pyyaml]);
in
  writeShellApplication {
    name = "k3s-drift";
    runtimeInputs = [py kubectl nix coreutils];
    text = ''exec python3 ${./k3s-drift.py} "$@"'';
  }
