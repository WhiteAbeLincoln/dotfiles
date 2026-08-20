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
      imports = [../common-hm resolved.homeManager];
      meta.user = host.primaryUser;
      meta.hostName = host.hostName;
    };
  };

  systemFacts = host: {
    nixpkgs.hostPlatform = host.system;
    networking.hostName = host.hostName;
    meta.user = host.primaryUser;
    meta.hostName = host.hostName;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
  };

  mkNixos = host: resolved:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = sysArgs;
      modules = [
        ../common
        inputs.home-manager.nixosModules.home-manager
        resolved.nixos
        (packageModule resolved)
        (systemFacts host)
        (homeModule host resolved)
      ];
    };

  mkDarwin = host: resolved:
    inputs.darwin.lib.darwinSystem {
      specialArgs = sysArgs;
      modules = [
        ../common
        inputs.home-manager.darwinModules.home-manager
        resolved.darwin
        (packageModule resolved)
        (systemFacts host)
        (homeModule host resolved)
      ];
    };

  mkHomeManager = host: resolved:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {
        system = host.system;
        overlays = resolved.nixpkgs.overlays;
        config = resolved.nixpkgs.config;
      };
      extraSpecialArgs = hmArgs;
      modules = [
        ../common-hm
        resolved.homeManager
        {
          meta.user = host.primaryUser;
          meta.hostName = host.hostName;
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
