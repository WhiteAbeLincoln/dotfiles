{
  description = "Nix Dotfiles Flake";

  inputs = {
    # current stable nixpkgs
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/*";
    # unstable nixpkgs
    nixpkgs-unstable.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";
    home-manager = {
      url = "https://flakehub.com/f/nix-community/home-manager/0.2505.4807";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "https://flakehub.com/f/nix-darwin/nix-darwin/0.2505.2185";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "https://flakehub.com/f/numtide/flake-utils/0.1.102";
    # https://github.com/DeterminateSystems/determinate?tab=readme-ov-file#nix-darwin
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/3.8.6";
    # virby.url = "github:quinneden/virby-nix-darwin";
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
        packages = let
          darwin-packages =
            if system == "x86_64-darwin" || system == "aarch64-darwin"
            then {
              # always include the --flake argument pointing to the current working directory
              # (since we usually won't be running this command from anywhere else)
              # TODO: maybe we can resolve the filesystem location of this flake so that the command could be run from anywhere
              # this would need to be installed as a systemPackage instead of a flake package then...
              darwin-rebuild = pkgs.writeShellScriptBin "darwin-rebuild" ''
                exec sudo ${inputs.darwin.packages.${system}.darwin-rebuild}/bin/darwin-rebuild --flake . "$@";
              '';
            }
            else {};
        in
          {
            decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
              ${pkgs.gnupg}/bin/gpg --decrypt ${./local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
            '';
          }
          // darwin-packages;
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
          # modules = [
          #   inputs.virby.darwinModules.default
          # ];
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
