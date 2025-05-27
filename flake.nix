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

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    flake-utils,
    darwin,
    ...
  } @ inputs: let
    # from https://github.com/bangedorrunt/nix/blob/f5a8a5d2f023f7d6558b0ce7051ff5e258860f55/flake.nix#L60
    # Extend nixpkgs.lib with custom lib and HM lib
    # Custom `./lib` will exposed as `lib.mine`
    # NOTE merge with `home-manager.lib` otherwise build will fail.
    # My guess is `lib` will override system lib, so some/all attributes of
    # system lib will be _undefined_, thus build error!
    mkLib = hm: nixpkgs:
      nixpkgs.lib.extend
      (self: super: {mine = import ./lib {lib = self;};} // hm);

    baseModule = {...}: {
      system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      nix.settings.experimental-features = ["nix-command" "flakes"];
      nix.registry.nixpkgs.flake = nixpkgs;
    };

    specialArgs = {
      user,
      isNixOS ? false,
      isWSL ? false,
      hm ? {},
      system ? "x86_64-linux",
      ...
    }: let
      lib = mkLib hm inputs.nixpkgs;
    in {
      myUserName = user;
      isHM = builtins.hasAttr "hm" lib;
      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = (_: true);
        };
      };
      inherit lib inputs isNixOS isWSL;
    };

    extraSpecialArgs = args: specialArgs (args // {hm = home-manager.lib;});

    hmCfg = {
      machine,
      user,
      dir ? null,
      system ? "x86_64-linux",
      modules ? null,
      ...
    } @ args: let
      machineDir =
        if dir != null
        then dir
        else machine;
      mods =
        if modules != null
        then modules
        else [./machine/${machineDir}/home.nix];
    in {
      "${user}@${machine}" = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowUnfreePredicate = (_: true);
          };
        };
        extraSpecialArgs = extraSpecialArgs args;
        modules =
          [
            {
              home.stateVersion = nixpkgs.lib.mkDefault "24.05";
              home.username = nixpkgs.lib.mkDefault user;
              home.homeDirectory = nixpkgs.lib.mkDefault /home/${user};
            }
          ]
          ++ mods;
      };
    };
    darwinCfg = {
      machine,
      user,
      system ? "aarch64-darwin",
      ...
    } @ args: {
      ${machine} = darwin.lib.darwinSystem {
        system = system;
        specialArgs = specialArgs args;
        modules = [
          baseModule
          {nixpkgs.hostPlatform = system;}
          ./modules/darwin/default.nix
          ./machine/${machine}/default.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${user} = {...} @ params: {
              imports = [./machine/${machine}/home.nix];

              programs.home-manager.enable = true;
              home.homeDirectory = "/Users/${user}";
            };
            home-manager.extraSpecialArgs = extraSpecialArgs args;
          }
        ];
      };
    };
    nixosCfg = {
      machine,
      user,
      system ? "x86_64-linux",
      ...
    } @ args: {
      ${machine} = nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = specialArgs args;
        modules = [
          baseModule
          ./machine/${machine}/default.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${user} = {...} @ params: {
              imports = [./machine/${machine}/home.nix];

              programs.home-manager.enable = true;
              home.homeDirectory = "/home/${user}";
            };
            home-manager.extraSpecialArgs = extraSpecialArgs (args // {isNixOS = true;});
          }
        ];
      };
    };

    mkCfgs = fn: cfgs: nixpkgs.lib.mergeAttrsList (map fn cfgs);
    nixosCfgs = mkCfgs nixosCfg;
    darwinCfgs = mkCfgs darwinCfg;
    hmCfgs = mkCfgs hmCfg;

    flakeCfg = fn:
      flake-utils.lib.eachDefaultSystem (system: (fn {
        inherit system;
        pkgs = nixpkgs.legacyPackages.${system};
      }));
  in
    (flakeCfg ({pkgs, system, ...}: {
      formatter = pkgs.alejandra;
      defaultPackage = home-manager.defaultPackage.${system};
    }))
    // {
      nixosConfigurations = nixosCfgs [
        {
          machine = "globalhawk";
          user = "abe";
        }
      ];
      darwinConfigurations = darwinCfgs [
        {
          machine = "nighthawk";
          user = "abe";
        }
      ];
      homeConfigurations = hmCfgs [
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
