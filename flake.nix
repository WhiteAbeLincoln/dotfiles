{
    description = "Nix Dotfiles Flake";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
        home-manager = {
            url = "github:nix-community/home-manager/release-23.11";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
        baseModule = { ... }: {
            system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
            nix.settings.experimental-features = ["nix-command" "flakes"];
            nix.registry.nixpkgs.flake = nixpkgs;
        };
        nixosCfg = ({ machine, user }: {
            ${machine} = nixpkgs.lib.nixosSystem {
                system = system;
                specialArgs = { myUserName = user; };
                modules = [
                    baseModule
                    ./machine/${machine}/default.nix
                    home-manager.nixosModules.home-manager {
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        home-manager.users.${user} = { ... }@params: {
                            imports = [ ./machine/${machine}/home.nix ];

                            programs.home-manager.enable = true;
                            home.homeDirectory = "/home/${user}";
                        };
                    }
                ];
            };
        });
    in
    {
        nixosConfigurations = nixosCfg { machine = "globalhawk"; user = "abe"; };
        homeConfigurations = {
            "awhite@4ZTHR73" = home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = { inherit inputs; };
                modules = [ ./machine/campbell/home.nix ];
            };
        };
    };
}
