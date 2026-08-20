{
  inputs,
  lib,
  self,
}: let
  sysArgs = {inherit inputs;};
  hmArgs = {inherit inputs;};

  resolveAspects = aspects:
    (lib.evalModules {
      modules = [./aspect-options.nix] ++ aspects;
      specialArgs = {inherit inputs;};
    }).config;

  checkHost = name: host:
    if host.hostName == ""
    then throw "dotfiles.hosts.${name}: hostName must not be empty"
    else if host.primaryUser == ""
    then throw "dotfiles.hosts.${name}: primaryUser must not be empty"
    else if host.class == "darwin" && !lib.hasSuffix "-darwin" host.system
    then throw "dotfiles.hosts.${name}: darwin requires a Darwin system"
    else if host.class == "nixos" && !lib.hasSuffix "-linux" host.system
    then throw "dotfiles.hosts.${name}: nixos requires a Linux system"
    else if host.class == "darwin" && !builtins.isInt host.stateVersion.system
    then throw "dotfiles.hosts.${name}: Darwin system stateVersion must be an integer"
    else if host.class == "nixos" && !builtins.isString host.stateVersion.system
    then throw "dotfiles.hosts.${name}: NixOS system stateVersion must be a string"
    else if host.class == "homeManager" && host.stateVersion.system != null
    then throw "dotfiles.hosts.${name}: Home Manager hosts must not set a system stateVersion"
    else host;

  packageModule = resolved: {
    nixpkgs.overlays = resolved.nixpkgs.overlays;
    nixpkgs.config = resolved.nixpkgs.config;
  };

  homeModule = host: resolved: {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.extraSpecialArgs = hmArgs;
    home-manager.users.${host.primaryUser} = {
      imports = [../common-hm resolved.homeManager] ++ host.homeModules;
      meta.user = host.primaryUser;
      meta.hostName = host.hostName;
      home.stateVersion = host.stateVersion.home;
    };
  };

  systemFacts = host: {
    nixpkgs.hostPlatform = host.system;
    networking.hostName = host.hostName;
    meta.user = host.primaryUser;
    meta.hostName = host.hostName;
    system.stateVersion = host.stateVersion.system;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
  };

  mkNixos = host: resolved:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = sysArgs;
      modules =
        [
          ../common
          inputs.home-manager.nixosModules.home-manager
          resolved.nixos
          (packageModule resolved)
        ]
        ++ host.modules ++ [(systemFacts host) (homeModule host resolved)];
    };

  mkDarwin = host: resolved:
    inputs.darwin.lib.darwinSystem {
      specialArgs = sysArgs;
      modules =
        [
          ../common
          inputs.home-manager.darwinModules.home-manager
          resolved.darwin
          (packageModule resolved)
        ]
        ++ host.modules ++ [(systemFacts host) (homeModule host resolved)];
    };

  mkHomeManager = host: resolved:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {
        system = host.system;
        overlays = resolved.nixpkgs.overlays;
        config = resolved.nixpkgs.config;
      };
      extraSpecialArgs = hmArgs;
      modules =
        [../common-hm resolved.homeManager]
        ++ host.homeModules
        ++ [
          {
            meta.user = host.primaryUser;
            meta.hostName = host.hostName;
            home.stateVersion = host.stateVersion.home;
          }
        ];
    };

  mkConfiguration = name: uncheckedHost: let
    host = checkHost name uncheckedHost;
    resolved = resolveAspects uncheckedHost.aspects;
  in
    if host.class == "nixos"
    then mkNixos host resolved
    else if host.class == "darwin"
    then mkDarwin host resolved
    else mkHomeManager host resolved;
in {
  inherit checkHost mkConfiguration resolveAspects;
}
