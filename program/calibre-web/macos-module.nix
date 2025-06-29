args @ {
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.calibre-web;
  myLib = ((import ../../lib) args).lib.awhite;

  inherit (lib) concatStringsSep mkEnableOption mkIf mkOption optional optionalString types;
in {
  options = {
    services.calibre-web = {
      enable = mkEnableOption (lib.mdDoc "Calibre-Web");

      package = lib.mkPackageOption pkgs "calibre-web" {};

      listen = {
        ip = mkOption {
          type = types.str;
          default = "::1";
          description = lib.mdDoc ''
            IP address that Calibre-Web should listen on.
          '';
        };

        port = mkOption {
          type = types.port;
          default = 8083;
          description = lib.mdDoc ''
            Listen port for Calibre-Web.
          '';
        };
      };

      dataDir = mkOption {
        type = types.str;
        default = "calibre-web";
        description = lib.mdDoc ''
          The directory below {file}`~/Library/Application Support` where Calibre-Web stores its data.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "calibre-web";
        description = lib.mdDoc "User account under which Calibre-Web runs.";
      };

      group = mkOption {
        type = types.str;
        default = "calibre-web";
        description = lib.mdDoc "Group account under which Calibre-Web runs.";
      };

      openFirewall = mkOption {
        type = types.bool;
        default = false;
        description = lib.mdDoc ''
          Open ports in the firewall for the server.
        '';
      };

      options = {
        calibreLibrary = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = lib.mdDoc ''
            Path to Calibre library.
          '';
        };

        enableBookConversion = mkOption {
          type = types.bool;
          default = false;
          description = lib.mdDoc ''
            Configure path to the Calibre's ebook-convert in the DB.
          '';
        };

        enableKepubify = mkEnableOption (lib.mdDoc "kebup conversion support");

        enableBookUploading = mkOption {
          type = types.bool;
          default = false;
          description = lib.mdDoc ''
            Allow books to be uploaded via Calibre-Web UI.
          '';
        };

        reverseProxyAuth = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = lib.mdDoc ''
              Enable authorization using auth proxy.
            '';
          };

          header = mkOption {
            type = types.str;
            default = "";
            description = lib.mdDoc ''
              Auth proxy header name.
            '';
          };
        };
      };
    };
  };

  config = mkIf cfg.enable (
    lib.mkMerge [
      {home.packages = [cfg.package];}
      (let
        dataRoot = "/Users/${cfg.user}/Library/Application Support";
        appDb = "${dataRoot}/${cfg.dataDir}/app.db";
        gdriveDb = "${dataRoot}/${cfg.dataDir}/gdrive.db";
        calibreWebCmd = ''${cfg.package}/bin/calibre-web -p "${appDb}" -g "${gdriveDb}"'';

        settings = concatStringsSep ", " (
          [
            "config_port = ${toString cfg.listen.port}"
            "config_uploading = ${
              if cfg.options.enableBookUploading
              then "1"
              else "0"
            }"
            "config_allow_reverse_proxy_header_login = ${
              if cfg.options.reverseProxyAuth.enable
              then "1"
              else "0"
            }"
            "config_reverse_proxy_login_header_name = '${cfg.options.reverseProxyAuth.header}'"
          ]
          ++ optional (cfg.options.calibreLibrary != null) "config_calibre_dir = '${cfg.options.calibreLibrary}'"
          ++ optional cfg.options.enableBookConversion "config_converterpath = '${pkgs.calibre}/bin/ebook-convert'"
          ++ optional cfg.options.enableKepubify "config_kepubifypath = '${pkgs.kepubify}/bin/kepubify'"
        );
        preScript =
          ''
            mkdir -p "${dataRoot}/${cfg.dataDir}"
            __RUN_MIGRATIONS_AND_EXIT=1 ${calibreWebCmd}

            ${pkgs.sqlite}/bin/sqlite3 "${appDb}" "update settings set ${settings}"
          ''
          + optionalString (cfg.options.calibreLibrary != null) ''
            test -f "${cfg.options.calibreLibrary}/metadata.db" || { echo "Invalid Calibre library"; exit 1; }
          '';
      in
        myLib.launchdAgent {
          name = "calibre-web";
          enable = true;
          script =
            preScript
            + ''
              ${calibreWebCmd} -i ${cfg.listen.ip}
            '';
          config = {
            KeepAlive = true;
            RunAtLoad = true;
            UserName = cfg.user;
            GroupName = cfg.group;
          };
        })
    ]
  );
}
