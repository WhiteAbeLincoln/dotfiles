{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.immich;
  machineLearningTag = if cfg.mlAcceleration == "cpu" then cfg.immichVersion else "${cfg.immichVersion}-${cfg.mlAcceleration}";
  environment = {
    UPLOAD_LOCATION = cfg.uploadDir;
    IMMICH_VERSION = cfg.immichVersion;
    DB_PASSWORD = cfg.dbPassword;
    DB_HOSTNAME = "immich_postgres";
    DB_USERNAME = "postgres";
    DB_DATABASE_NAME = "immich";
    REDIS_HOSTNAME = "immich_redis";
  };
  transcodingCfg = {
    cpu = {};
    nvenc = {};
    rkmpp = {};
    quicksync = { extraOptions = ["--device=/dev/dri:/dev/dri"]; };
    vaapi = { extraOptions = ["--device=/dev/dri:/dev/dri"]; };
    vaapi-wsl = {
      volumes = [ "/usr/lib/wsl:/usr/lib/wsl" ];
      extraOptions = ["--device=/dev/dri:/dev/dri"];
      environment = {
        LD_LIBRARY_PATH = "/usr/lib/wsl/lib";
        LIBVA_DRIVER_NAME = "d3d12";
      };
    };
  };
  mlCfg = {
    cpu = {};
    cuda = {};
    armnn = {
      extraOptions = [
        "--device=/dev/mali0:/dev/mali0"
      ];
      volumes = [
        "/lib/firmware/mali_csffw.bin:/lib/firmware/mali_csffw.bin:ro" # Mali firmware for your chipset (not always required depending on the driver)
        "/usr/lib/libmali.so:/usr/lib/libmali.so:ro" # Mali driver for your chipset (always required)
      ];
    };
    openvino = {
      extraOptions = [
        "--device-cgroup-rule=c 189:* rmw"
        "--device=/dev/dri:/dev/dri"
      ];
      volumes = [
        "/dev/bus/usb:/dev/bus/usb"
      ];
    };
    openvino-wsl = {
      extraOptions = [
        "--device=/dev/dri:/dev/dri"
        "--device=/dev/dxg:/dev/dxg"
      ];
      volumes = [
        "/dev/bus/usb:/dev/bus/usb"
        "/usr/lib/wsl:/usr/lib/wsl"
      ];
    };
  };
  # https://stackoverflow.com/a/54505212
  recursiveMerge = (attrList:
    let f = attrPath:
      zipAttrsWith (n: values:
        if tail values == []
          then head values
        else if all isList values
          then unique (concatLists values)
        else if all isAttrs values
          then f (attrPath ++ [n]) values
        else last values
      );
    in f [] attrList);
  libraryVolumes = mapAttrsToList (name: path: "${path}:/mnt/media/${name}:ro") cfg.externalLibraries;
in
{
  options = {
    services.immich = {
      enable = mkEnableOption "immich";
      externalLibraries = mkOption {
        type = types.attrsOf types.str;
        description = "A map of external library paths";
        default = {};
      };
      uploadDir = mkOption {
        type = types.str;
        description = "The filesystem location of your photos";
      };
      immichVersion = mkOption {
        type = types.str;
        description = "The version of immich to run";
        default = "release";
      };
      transcoding = mkOption {
        type = types.enum [
          "cpu"
          # "nvenc"
          "quicksync"
          # "rkmpp"
          "vaapi"
          "vaapi-wsl"
        ];
        default = "cpu";
      };
      mlAcceleration = mkOption {
        type = types.enum [
          "cpu"
          "armnn"
          # "cuda"
          # "openvino"
          "openvino-wsl"
        ];
        default = "cpu";
      };
      dbPassword = mkOption {
        type = types.str;
      };
      backupDir = mkOption {
        type = types.str;
      };
      backupSchedule = mkOption {
        type = types.str;
        default = "@daily";
      };
      port = mkOption {
        type = types.ints.u16;
        default = 2283;
      };
    };
  };
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ cfg.port ];
    virtualisation.oci-containers.containers = {
      immich_server = {
        image = "ghcr.io/immich-app/immich-server:${cfg.immichVersion}";
        cmd = [ "start.sh" "immich" ];
        volumes = [
          "${cfg.uploadDir}:/usr/src/app/upload"
          "/etc/localtime:/etc/localtime:ro"
        ] ++ libraryVolumes;
        ports = [ "${toString cfg.port}:3001" ];
        autoStart = true;
        environment = environment;
        dependsOn = [
          "immich_redis"
          "immich_postgres"
        ];
        extraOptions = [ "--network=immich-bridge" ];
      };
      immich_microservices = recursiveMerge [
        {
          image = "ghcr.io/immich-app/immich-server:${cfg.immichVersion}";
          cmd = [ "start.sh" "microservices" ];
          volumes = [
            "${cfg.uploadDir}:/usr/src/app/upload"
            "/etc/localtime:/etc/localtime:ro"
          ] ++ libraryVolumes;
          autoStart = true;
          environment = environment;
          dependsOn = [
            "immich_redis"
            "immich_postgres"
          ];
          extraOptions = [ "--network=immich-bridge" ];
        }
        transcodingCfg.${cfg.transcoding}
      ];
      immich_db_dumper = {
        image = "prodrigestivill/postgres-backup-local";
        environment = environment // {
          POSTGRES_HOST = "immich_postgres";
          POSTGRES_DB = environment.DB_DATABASE_NAME;
          POSTGRES_USER = environment.DB_USERNAME;
          POSTGRES_PASSWORD = environment.DB_PASSWORD;
          SCHEDULE = cfg.backupSchedule;
          BACKUP_DIR = "/db_dumps";
        };
        volumes = [ "${cfg.backupDir}/db_dumps" ];
        dependsOn = [
          "immich_postgres"
        ];
        autoStart = true;
        extraOptions = [ "--network=immich-bridge" ];
      };
      immich_machine_learning = recursiveMerge [
        {
          hostname = "immich-machine-learning";
          image = "ghcr.io/immich-app/immich-machine-learning:${machineLearningTag}";
          autoStart = true;
          environment = environment;
          volumes = [
            "model-cache:/cache"
          ];
          extraOptions = [ "--network=immich-bridge" ];
        }
        mlCfg.${cfg.mlAcceleration}
      ];
      immich_redis = {
        image = "registry.hub.docker.com/library/redis:6.2-alpine@sha256:51d6c56749a4243096327e3fb964a48ed92254357108449cb6e23999c37773c5";
        autoStart = true;
        extraOptions = [ "--network=immich-bridge" ];
      };
      immich_postgres = {
        image = "registry.hub.docker.com/tensorchord/pgvecto-rs:pg14-v0.2.0@sha256:90724186f0a3517cf6914295b5ab410db9ce23190a2d9d0b9dd6463e3fa298f0";
        autoStart = true;
        environment = {
          POSTGRES_PASSWORD = cfg.dbPassword;
          POSTGRES_USER = environment.DB_USERNAME;
          POSTGRES_DB = environment.DB_DATABASE_NAME;
        };
        volumes = [
          "pgdata:/var/lib/postgresql/data"
        ];
        extraOptions = [ "--network=immich-bridge" ];
      };
    };
    systemd.services.init-immich-network = {
      description = "Create the network bridge for immich.";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      script = ''
        # Put a true at the end to prevent getting non-zero return code, which will
        # crash the whole service.
        check=$(${pkgs.docker}/bin/docker network ls | grep "immich-bridge" || true)
        if [ -z "$check" ];
          then ${pkgs.docker}/bin/docker network create immich-bridge
          else echo "immich-bridge already exists in docker"
        fi
      '';
    };
  };
}
