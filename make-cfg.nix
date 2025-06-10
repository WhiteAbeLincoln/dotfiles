{
  libPath ? ./lib,
  flake ? ({...}: {}),
  nixos ? [],
  darwin ? [],
  home ? [],
}:
{
  self,
  nixpkgs,
  nixpkgs-unstable,
  home-manager,
  flake-utils,
  # darwin,
  ...
} @ inputs:
let
  # from https://github.com/bangedorrunt/nix/blob/f5a8a5d2f023f7d6558b0ce7051ff5e258860f55/flake.nix#L60
  # Extend nixpkgs.lib with custom lib and HM lib
  # Custom `./lib` will exposed as `lib.mine`
  # NOTE merge with `home-manager.lib` otherwise build will fail.
  # My guess is `lib` will override system lib, so some/all attributes of
  # system lib will be _undefined_, thus build error!
  mkLib = hm: nixpkgs:
    nixpkgs.lib.extend
    (self: super: {mine = import libPath {lib = self;};} // hm);

  flakeModule = {
    nix.settings.experimental-features = ["nix-command" "flakes"];
    nix.registry.nixpkgs.flake = nixpkgs;
  };

  mkPkgs = nixpkgs: system:
    import nixpkgs {
      inherit system;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = (_: true);
      };
    };

  specialArgs = {
    user,
    system,
    isNixOS ? false,
    isWSL ? false,
    isDarwin ? false,
    hm ? {},
    ...
  }: let
    lib = mkLib hm inputs.nixpkgs;
  in {
    myUserName = user;
    isHM = builtins.hasAttr "hm" lib;
    pkgs-unstable = mkPkgs inputs.nixpkgs-unstable system;
    inherit lib inputs isNixOS isWSL isDarwin;
  };

  extraSpecialArgs = args: specialArgs (args // {hm = home-manager.lib;});

  hmSystemModules = extraArgs:
    [./modules/hm] ++ (if extraArgs.isWSL then [./modules/windows] else []);

  baseHmModule = machineDir: { myUserName, lib, isDarwin, ... }: let
    machineModule = (if machineDir == "" then [] else [./machine/${machineDir}/home.nix]);
  in {
    imports = machineModule;
    programs.home-manager.enable = true;
    home.stateVersion = lib.mkDefault "25.05";
    home.username = lib.mkDefault myUserName;
    home.homeDirectory = lib.mkDefault (if isDarwin then "/Users/${myUserName}" else "/home/${myUserName}");
  };

  systemCfg = {
    mkSystem,
    hmModule,
    defSystem,
    systemModules,
  }: argsIn: let
    # the @ args capture doesn't seem to include the defaults
    # so we must merge manually
    defs = {
      system = defSystem;
      dir = null;
      modules = [];
    };
    args = defs // argsIn;
    inherit (args) user machine system dir modules;

    machineDir =
      if dir != null
      then dir
      else machine;
    machineModule = (if machineDir == "" then [] else [./machine/${machineDir}/default.nix]);
  in {
    ${machine} = mkSystem (let
      extraSpecialArgs = extraSpecialArgs args;
    in
    {
      inherit system;
      specialArgs = specialArgs args;
      modules = [
        flakeModule
        {
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          nixpkgs.hostPlatform = system;
        }
      ]
        ++ systemModules
        ++ machineModule
        ++ modules
        ++ (hmSystemModules extraSpecialArgs)
        ++ [
          hmModule
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = extraSpecialArgs;
            home-manager.users.${user} = baseHmModule machineDir;
          }
        ];
    });
  };

  homeCfg = argsIn: let
    # the @ args capture doesn't seem to include the defaults
    # so we must merge manually
    defs = {
      system = "x86_64-linux";
      dir = null;
      modules = [];
    };
    args = defs // argsIn;
    inherit (args) user machine system dir modules;

    extraArgs = extraSpecialArgs args;
    machineDir =
      if dir != null
      then dir
      else machine;
  in {
    "${user}@${machine}" = home-manager.lib.homeManagerConfiguration {
      pkgs = mkPkgs inputs.nixpkgs system;
      extraSpecialArgs = extraArgs;
      modules = [
        flakeModule
        ({ pkgs, ... }: { nix.package = pkgs.nix; })
      ]
        ++ modules
        ++ (hmSystemModules extraArgs)
        ++ [(baseHmModule machineDir)];
    };
  };

  nixosCfg = systemCfg {
    mkSystem = nixpkgs.lib.nixosSystem;
    hmModule = home-manager.nixosModules.home-manager;
    defSystem = "x86_64-linux";
    systemModules = [];
  };

  darwinCfg = systemCfg {
    mkSystem = inputs.darwin.lib.darwinSystem;
    hmModule = home-manager.darwinModules.home-manager;
    defSystem = "aarch64-darwin";
    systemModules = [
      ./modules/darwin
    ];
  };

  flakeCfg = fn:
    flake-utils.lib.eachDefaultSystem (system: (fn {
      inherit system inputs;
      pkgs = mkPkgs inputs.nixpkgs system;
      pkgs-unstable = mkPkgs inputs.nixpkgs-unstable system;
    }));
in (flakeCfg flake) // {
  nixosConfigurations = nixpkgs.lib.mergeAttrsList (map nixosCfg nixos);
  darwinConfigurations = nixpkgs.lib.mergeAttrsList (map darwinCfg darwin);
  homeConfigurations = nixpkgs.lib.mergeAttrsList (map homeCfg home);
}
