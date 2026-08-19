{
  description = "Nix Dotfiles Flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    # current stable nixpkgs
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.2605.*";
    # unstable nixpkgs
    nixpkgs-unstable.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";
    home-manager = {
      url = "https://flakehub.com/f/nix-community/home-manager/0.2605.*";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "https://flakehub.com/f/nix-darwin/nix-darwin/0.2605.*";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # https://github.com/DeterminateSystems/determinate?tab=readme-ov-file#nix-darwin
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3.21.0";
    git-different = {
      url = "github:WhiteAbeLincoln/git-different";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # AI coding agents (claude-code, codex, pi), repackaged and updated daily.
    # Intentionally no `inputs.nixpkgs.follows`: its `packages` output is
    # prebuilt against llm-agents' own pinned nixpkgs, so following ours would
    # rebuild from source and miss cache.numtide.com.
    llm-agents.url = "github:numtide/llm-agents.nix";
    # nixidy: author k8s workloads as Nix modules, render to plain YAML for
    # delivery via services.k3s.manifests (no ArgoCD, no Helm). Tracks its own
    # nixpkgs deliberately — its CRD generators pin against it.
    nixidy.url = "github:arnarg/nixidy/latest";
    # sops-nix: activation-time secret decryption (age; key derived from the
    # host's SSH ed25519 key via ssh-to-age). Replaces sealed-secrets (k8s) and
    # the git-crypt->/nix/store leak for globalhawk's runtime secrets. See
    # docs/superpowers/specs/2026-07-23-globalhawk-secrets-sops-design.md.
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # virby.url = "github:quinneden/virby-nix-darwin";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    darwin,
    determinate,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        pkgs,
        system,
        ...
      }: {
        formatter = pkgs.alejandra;
        checks = pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          k3s-workloads-module = import ./k8s/tests/workloads-module.nix {
            inherit inputs pkgs;
          };
          k3s-runtime-secrets-module = import ./k8s/tests/runtime-secrets-module.nix {
            inherit inputs pkgs;
          };
        };
        packages =
          {
            decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
              ${pkgs.gnupg}/bin/gpg --decrypt ${./local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
            '';
            # Read-only audit of the globalhawk AI-agent sandbox. Deliberately NOT
            # part of `nix flake check` — it must never block activation.
            audit-agent-access = pkgs.callPackage ./packages/audit-agent-access.nix {};
            # Read-only drift check: diffs the host-owned workload render against
            # live k3s. `switch` already prunes removed workloads (single-combined-
            # file lane), so this is trust-but-verify, not a delete mechanism.
            k3s-drift = pkgs.callPackage ./packages/k3s-drift.nix {};
            # Schema-driven generated, derived, and operator-managed sops fields.
            populate-sops = pkgs.callPackage ./packages/populate-sops.nix {};
            libation-reconcile = pkgs.callPackage ./packages/libation-reconcile.nix {};
            libation-auth = pkgs.callPackage ./packages/libation-auth.nix {};
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

      flake = let
        # `lib.mine` extension. Has to go through specialArgs because the
        # module system can't rebind `lib` before evaluation starts.
        mkLib = nixpkgs.lib.extend (self: _super: {
          mine = import ./lib {lib = self;};
        });
        # HM contexts also need home-manager.lib merged in (for lib.hm.dag
        # and the `lib ? hm` probe direnv uses to detect HM).
        hmLib = nixpkgs.lib.extend (self: _super:
          {mine = import ./lib {lib = self;};}
          // home-manager.lib);

        sysArgs = {
          inherit inputs;
          lib = mkLib;
        };
        hmArgs = {
          inherit inputs;
          lib = hmLib;
        };

      in {
        nixosConfigurations.globalhawk = nixpkgs.lib.nixosSystem {
          specialArgs = sysArgs;
          modules = [
            ./modules/common
            home-manager.nixosModules.home-manager
            ./machine/globalhawk
            {
              nixpkgs.hostPlatform = "x86_64-linux";
              meta.user = "abe";
              system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = hmArgs;
              home-manager.users.abe = {
                imports = [
                  ./modules/common-hm
                  ./modules/hm
                  ./machine/globalhawk/home.nix
                ];
                meta.user = "abe";
              };
            }
          ];
        };

        darwinConfigurations.nighthawk = darwin.lib.darwinSystem {
          specialArgs = sysArgs;
          modules = [
            ./modules/common
            ./modules/darwin
            determinate.darwinModules.default
            home-manager.darwinModules.home-manager
            ./machine/nighthawk
            {
              nixpkgs.hostPlatform = "aarch64-darwin";
              meta.user = "abe";
              system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = hmArgs;
              home-manager.users.abe = {
                imports = [
                  ./modules/common-hm
                  ./modules/hm
                  ./machine/nighthawk/home.nix
                ];
                meta.user = "abe";
              };
            }
          ];
        };
      };
    };
}
