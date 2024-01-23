{
    description = "Campbell NixOS Flake";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
        nixos-hardware.url = "github:NixOS/nixos-hardware/master";
        home-manager = {
            url = "github:nix-community/home-manager/release-23.11";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { self, nixpkgs, home-manager, nixos-hardware, ... }@inputs: {
        nixosConfigurations = {
            nixos = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                specialArgs = { inherit inputs; };
                modules = [
                    # this is a 5550, but close enough. Doesn't seem to be specific to the 5550
                    nixos-hardware.nixosModules.dell-precision-5530
                    ./configuration.nix
                    home-manager.nixosModules.home-manager {
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        home-manager.users.awhite = import ./home.nix;
                    }
                ];
            };
        };
    };
}