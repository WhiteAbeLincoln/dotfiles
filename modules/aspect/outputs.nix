{
  config,
  inputs,
  lib,
  self,
  ...
}: let
  constructors = import ./constructors.nix {
    inherit inputs lib self;
    inherit (config.dotfiles) providers;
    defaultAspectsEnabled = config.dotfiles.defaultAspects.enable;
  };
  hosts = lib.mapAttrs (_: host:
    host
    // {aspects = config.dotfiles.sharedAspects ++ host.aspects;})
  config.dotfiles.hosts;
  byClass = class: lib.filterAttrs (_: host: host.class == class) hosts;
in {
  options.flake = {
    darwinConfigurations = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = {};
    };
    homeConfigurations = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = {};
    };
  };

  config = {
    systems = lib.unique (map (host: host.system) (lib.attrValues hosts));
    flake = {
      nixosConfigurations = lib.mapAttrs constructors.mkConfiguration (byClass "nixos");
      darwinConfigurations = lib.mapAttrs constructors.mkConfiguration (byClass "darwin");
      homeConfigurations = lib.mapAttrs' (
        name: host:
          lib.nameValuePair
          "${host.user}@${host.hostName}"
          (constructors.mkConfiguration name host)
      ) (byClass "homeManager");
    };
  };
}
