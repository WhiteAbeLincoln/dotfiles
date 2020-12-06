{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.firefox;
in

{
  options = {
    programs.firefox = {
      extraPackageConfig = mkOption {
        default = {};
        description = ''
          Additional config options expected by the Firefox package
        '';
      };
    };
  };
  config = mkIf (cfg.extraPackageConfig != {}) {
    programs.firefox.package = pkgs.firefox.override {
      cfg = cfg.extraPackageConfig;
    };
  };
}
