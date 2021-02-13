{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.tridactyl;
  firefoxCfg = config.programs.firefox;
in
  {
    options = {
      programs.tridactyl = {
        enable = mkEnableOption "tridactyl";
        package = mkOption {
          type = types.package;
          default = pkgs.nur.repos.rycee.firefox-addons.tridactyl;
          defaultText = literalExample "pkgs.nur.repos.rycee.firefox-addons.tridactyl";
          description = ''
            The package providing the Firefox extension to use.
          '';
        };
        enableNative = mkEnableOption "enableNative";
        tridactylrc = mkOption {
          type = types.nullOr ((import ../../lib/types.nix { inherit lib; }).file);
          default = null;
        };
      };
    };
    config = mkIf (firefoxCfg.enable && cfg.enable){
      programs.firefox = {
        extensions = [
          cfg.package
        ];
        extraPackageConfig = {
          enableTridactylNative = cfg.enableNative;
        };
      };
      xdg.configFile = mkIf (cfg.tridactylrc != null) {
        "tridactyl/tridactylrc" = cfg.tridactylrc;
      };
    };
  }
