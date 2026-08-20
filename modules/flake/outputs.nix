{inputs, ...}: {
  perSystem = {
    pkgs,
    system,
    ...
  }: {
    formatter = pkgs.alejandra;
    checks = pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
      k3s-workloads-module = import ../../k8s/tests/workloads-module.nix {
        inherit inputs pkgs;
      };
      k3s-runtime-secrets-module = import ../../k8s/tests/runtime-secrets-module.nix {
        inherit inputs pkgs;
      };
    };
    packages =
      {
        decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
          ${pkgs.gnupg}/bin/gpg --decrypt ${../../local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
        '';
        # Read-only audit of the globalhawk AI-agent sandbox. Deliberately NOT
        # part of `nix flake check` — it must never block activation.
        audit-agent-access = pkgs.callPackage ../../packages/audit-agent-access.nix {};
        # Read-only drift check: diffs the host-owned workload render against
        # live k3s. `switch` already prunes removed workloads (single-combined-
        # file lane), so this is trust-but-verify, not a delete mechanism.
        k3s-drift = pkgs.callPackage ../../packages/k3s-drift.nix {};
        # Schema-driven generated, derived, and operator-managed sops fields.
        populate-sops = pkgs.callPackage ../../packages/populate-sops.nix {};
        libation-reconcile = pkgs.callPackage ../../packages/libation-reconcile.nix {};
        libation-auth = pkgs.callPackage ../../packages/libation-auth.nix {};
      }
      // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        # always include the --flake argument pointing to the current working
        # directory (since we usually won't be running this command from
        # anywhere else)
        darwin-rebuild = pkgs.writeShellScriptBin "darwin-rebuild" ''
          exec sudo ${inputs.darwin.packages.${system}.darwin-rebuild}/bin/darwin-rebuild --flake . "$@";
        '';
      };
  };
}
