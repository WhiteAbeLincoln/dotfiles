{
    description = "Campbell Home-Manager Flake";

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
    in
    {
        homeConfigurations = {
            "awhite@4ZTHR73" = home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = { inherit inputs; };
                modules = [ ./machine/campbell/home.nix ];
            };
        };
    };
}
