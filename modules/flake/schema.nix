{lib, ...}: let
  inherit (lib) mkOption types;
  hostType = types.submodule ({name, ...}: {
    options = {
      class = mkOption {
        type = types.enum ["nixos" "darwin" "homeManager"];
        description = "Deployment class used to construct this host.";
      };
      system = mkOption {
        type = types.str;
        description = "Nix platform used to evaluate this host.";
      };
      hostName = mkOption {
        type = types.str;
        default = name;
        description = "Inventory hostname of this configuration.";
      };
      primaryUser = mkOption {
        type = types.str;
        description = "Primary user receiving the Home Manager configuration.";
      };
      stateVersion = {
        system = mkOption {
          type = types.nullOr (types.either types.str types.int);
          default = null;
          description = "Class-appropriate system state version, or null for Home Manager hosts.";
        };
        home = mkOption {
          type = types.str;
          description = "Home Manager state version for the primary user.";
        };
      };
      aspects = mkOption {
        type = types.listOf types.deferredModule;
        default = [];
        description = "Aspect modules selected specifically for this host.";
      };
      modules = mkOption {
        type = types.listOf types.deferredModule;
        default = [];
        description = "Native system modules added to this host.";
      };
      homeModules = mkOption {
        type = types.listOf types.deferredModule;
        default = [];
        description = "Native Home Manager modules added for the primary user.";
      };
    };
  });
in {
  options.dotfiles = {
    sharedAspects = mkOption {
      type = types.listOf types.deferredModule;
      default = [];
      description = "Aspect modules selected for every inventory host.";
    };
    hosts = mkOption {
      type = types.attrsOf hostType;
      default = {};
      description = "Inventory of deployment targets keyed by host name.";
    };
    extraSystems = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional platforms needed only for per-system flake outputs.";
    };
  };
}
