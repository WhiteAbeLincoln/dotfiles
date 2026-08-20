{
  defaultAspectsEnabled,
  inputs,
  lib,
  providers,
  self,
}: let
  sysArgs = {inherit inputs;};
  hmArgs = {inherit inputs;};

  hostFacts = host: {inherit (host) class system hostName user;};
  hostContext = host: {
    imports = [./target/host-context.nix];
    dotfiles.host = hostFacts host;
  };

  resolveAspects = aspects:
    (lib.evalModules {
      modules =
        [./aspect-options.nix]
        ++ lib.optional defaultAspectsEnabled ./default-aspects/host-facts.nix
        ++ aspects;
      specialArgs = {inherit inputs self;};
    }).config;

  checkHost = name: host:
    if host.hostName == ""
    then throw "dotfiles.hosts.${name}: hostName must not be empty"
    else if host.user == ""
    then throw "dotfiles.hosts.${name}: user must not be empty"
    else if host.class == "darwin" && !lib.hasSuffix "-darwin" host.system
    then throw "dotfiles.hosts.${name}: darwin requires a Darwin system"
    else if host.class == "nixos" && !lib.hasSuffix "-linux" host.system
    then throw "dotfiles.hosts.${name}: nixos requires a Linux system"
    else if providers.nixpkgs == null
    then throw "dotfiles.hosts.${name}: nixpkgs provider is required"
    else if providers.homeManager == null
    then throw "dotfiles.hosts.${name}: homeManager provider is required"
    else if host.class == "darwin" && providers.darwin == null
    then throw "dotfiles.hosts.${name}: darwin provider is required for class darwin"
    else host;

  packageModule = resolved: {
    nixpkgs.overlays = resolved.nixpkgs.overlays;
    nixpkgs.config = resolved.nixpkgs.config;
  };

  homeModule = host: resolved: {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.extraSpecialArgs = hmArgs;
    home-manager.users.${host.user} = {
      imports = [(hostContext host) resolved.homeManager];
    };
  };

  mkNixos = host: resolved:
    providers.nixpkgs.lib.nixosSystem {
      specialArgs = sysArgs;
      modules = [
        providers.homeManager.nixosModules.home-manager
        (hostContext host)
        resolved.nixos
        (packageModule resolved)
        (homeModule host resolved)
      ];
    };

  mkDarwin = host: resolved:
    providers.darwin.lib.darwinSystem {
      specialArgs = sysArgs;
      modules = [
        providers.homeManager.darwinModules.home-manager
        (hostContext host)
        resolved.darwin
        (packageModule resolved)
        (homeModule host resolved)
      ];
    };

  mkHomeManager = host: resolved:
    providers.homeManager.lib.homeManagerConfiguration {
      pkgs = import providers.nixpkgs {
        system = host.system;
        overlays = resolved.nixpkgs.overlays;
        config = resolved.nixpkgs.config;
      };
      extraSpecialArgs = hmArgs;
      modules = [
        (hostContext host)
        resolved.homeManager
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
