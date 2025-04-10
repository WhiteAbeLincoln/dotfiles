{ lib, config, options, pkgs, ... }:

with lib;
let
  secrets = import ../../secrets/globalhawk.nix;
  cfg = config.services.restic-b2;
  getMount = pool: ''
    getPoolMount() {
      output="$(mount)"
      output="''${output#*${pool} on }"
      output="''${output%% type zfs*}"
      echo "$output"
    }
  '';
  backupCfg = options.services.restic.backups.type.nestedTypes.elemType;
in
{
  options = {
    services.restic-b2 = {
      enable = mkEnableOption "restic-b2";
      user = mkOption {
        type = types.str;
        default = "_restic";
      };
      group = mkOption {
        type = types.str;
        default = "_restic";
      };
      additionalGroups = mkOption {
        type = types.listOf types.str;
        default = [];
      };
      zfsPool = mkOption {
        type = types.str;
      };
      password = mkOption {
        type = types.str;
      };
      env = mkOption {
        type = types.attrsOf types.string;
        default = {};
      };
      config = mkOption {
        type = backupCfg;
      };
    };
  };
}

  # users.groups._restic = {};
  # users.users._restic = {
  #   isSystemUser = true;
  #   group = "_restic";
  #   createHome = false;
  # };
  # environment.etc = {
  #   "restic/backblaze-password" = {
  #     text = secrets.restic.b2.pass;
  #     user = "_restic";
  #     group = "_restic";
  #     mode = "0600";
  #   };
  #   "restic/backblaze-env" = {
  #     text = ''
  #       AWS_ACCESS_KEY_ID="${secrets.restic.b2.key_id}"
  #       AWS_SECRET_ACCESS_KEY="${secrets.restic.b2.app_key}"
  #     '';
  #     user = "_restic";
  #     group = "_restic";
  #     mode = "0600";
  #   };
  # };

  # services.restic.backups = {
  #   backblaze = {
  #     initialize = true;
  #     repository = secrets.restic.b2.repo;
  #     passwordFile = "/etc/restic/backblaze-password";
  #     user = "_restic";
  #     dynamicFilesFrom = ''
  #       base="/data/Media/.zfs/snapshot/$(date +restic_%F)"
  #       # we don't want to back up everything, just
  #       # some select paths

  #       for folder in photos immich documents docker-services/homebridge; do
  #         if [ -d "$base/$folder" ]; then
  #           echo "$base/$folder"
  #         fi
  #       done
  #     '';
  #     # create a zfs snapshot
  #     backupPrepareCommand = ''
  #       # create a new snapshot
  #       snapshot="$(date +restic_%F)"
  #       zfs snap "pool/media@$snapshot"
  #     '';
  #     backupCleanupCommand = ''
  #       # do a rolling snapshot of 7 days
  #       limit="$(date --date '7 days ago' +restic_%F)"
  #       for f in /data/Media/.zfs/snapshot/restic_*; do
  #         name="$(basename $f)"
  #         if expr "$name" \< "$limit" > /dev/null; then
  #           zfs destroy "pool/media@$name"
  #         fi
  #       done
  #     '';
  #   };
  # };
# }
