{
  description = "Nix Dotfiles Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  # unfortunately, outputs must be a function and not a thunk
  # otherwise this could be cleaner
  outputs = inputs:
    import ./make-cfg.nix inputs {
      flake = {
        pkgs,
        system,
        inputs,
        ...
      }: {
        formatter = pkgs.alejandra;
        defaultPackage = inputs.home-manager.defaultPackage.${system};
        packages = {
          decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
            ${pkgs.gnupg}/bin/gpg --decrypt ${./local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
          '';
        };
      };
      nixos = [
        {
          machine = "globalhawk";
          user = "abe";
        }
      ];
      darwin = [
        {
          machine = "nighthawk";
          user = "abe";
        }
      ];
      home = [
        {
          machine = "4ZTHR73";
          user = "awhite";
          dir = "campbell";
          isWSL = true;
        }
        {
          machine = "trace";
          user = "trace";
          modules = [
            (args: builtins.trace (builtins.attrNames args) {})
          ];
        }
      ];
    };
}
