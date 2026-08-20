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

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [./modules/flake];
    };
}
