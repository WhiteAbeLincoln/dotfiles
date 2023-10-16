{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.plex;
in
{
  options = {
    services.plex = {
      enable = mkEnableOption (lib.mdDoc "Plex Media Server");

      # most of these options are unused, but remain to be compatible with the NixOS options
      dataDir = mkOption {
        type = types.str;
        default = "/Users/${cfg.user}/Library/Application Support";
        description = lib.mdDoc ''
          The directory where Plex stores its data files.
        '';
      };

      openFirewall = mkOption {
        type = types.bool;
        default = false;
        description = lib.mdDoc ''
          Open ports in the firewall for the media server.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "plex";
        description = lib.mdDoc ''
          User account under which Plex runs.
        '';
      };

      group = mkOption {
        type = types.str;
        default = "plex";
        description = lib.mdDoc ''
          Group under which Plex runs.
        '';
      };

      extraPlugins = mkOption {
        type = types.listOf types.path;
        default = [];
        description = lib.mdDoc ''
          A list of paths to extra plugin bundles to install in Plex's plugin
          directory. Every time the systemd unit for Plex starts up, all of the
          symlinks in Plex's plugin directory will be cleared and this module
          will symlink all of the paths specified here to that directory.
        '';
        example = literalExpression ''
          [
            (builtins.path {
              name = "Audnexus.bundle";
              path = pkgs.fetchFromGitHub {
                owner = "djdembeck";
                repo = "Audnexus.bundle";
                rev = "v0.2.8";
                sha256 = "sha256-IWOSz3vYL7zhdHan468xNc6C/eQ2C2BukQlaJNLXh7E=";
              };
            })
          ]
        '';
      };

      extraScanners = mkOption {
        type = types.listOf types.path;
        default = [];
        description = lib.mdDoc ''
          A list of paths to extra scanners to install in Plex's scanners
          directory.

          Every time the systemd unit for Plex starts up, all of the symlinks
          in Plex's scanners directory will be cleared and this module will
          symlink all of the paths specified here to that directory.
        '';
        example = literalExpression ''
          [
            (fetchFromGitHub {
              owner = "ZeroQI";
              repo = "Absolute-Series-Scanner";
              rev = "773a39f502a1204b0b0255903cee4ed02c46fde0";
              sha256 = "4l+vpiDdC8L/EeJowUgYyB3JPNTZ1sauN8liFAcK+PY=";
            })
          ]
        '';
      };

      package = mkOption {
        type = types.package;
        default = pkgs.plex;
        defaultText = literalExpression "pkgs.plex";
        description = lib.mdDoc ''
          The Plex package to use. Plex subscribers may wish to use their own
          package here, pointing to subscriber-only server versions.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.plex = {
      serviceConfig = {
        ProgramArguments = [ "${cfg.package}/plex-run-script" ];
        KeepAlive = true;
        RunAtLoad = true;
        EnvironmentVariables = {
          PLEX_DATADIR = cfg.dataDir;
          PLEX_PLUGINS = concatMapStringsSep ":" builtins.toString cfg.extraPlugins;
          PLEX_SCANNERS = concatMapStringsSep ":" builtins.toString cfg.extraScanners;
        };
        ProcessType = "Interactive";
        StandardOutPath = "/tmp/plex.out.log";
        StandardErrorPath = "/tmp/plex.err.log";
        UserName = cfg.user;
        GroupName = cfg.group;
      };
    };
  };
}
