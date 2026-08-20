{
  inputs,
  lib,
  ...
}: let
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
      user = mkOption {
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
    defaultAspects.enable = mkOption {
      type = types.bool;
      default = true;
      description = "Whether the library's bundled default aspects are enabled.";
    };
    providers = {
      nixpkgs = mkOption {
        type = types.nullOr types.raw;
        default = inputs.nixpkgs or null;
        description = "Nixpkgs flake used by configuration constructors.";
      };
      homeManager = mkOption {
        type = types.nullOr types.raw;
        default = inputs.home-manager or null;
        description = "Home Manager flake used by configuration constructors.";
      };
      darwin = mkOption {
        type = types.nullOr types.raw;
        default = inputs.darwin or null;
        description = "Optional nix-darwin flake used by Darwin constructors.";
      };
    };
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
  };
}
