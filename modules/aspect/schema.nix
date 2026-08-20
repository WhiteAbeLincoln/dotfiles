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
      aspects = mkOption {
        type = types.listOf types.deferredModule;
        default = [];
        description = "Aspect modules selected specifically for this host.";
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
