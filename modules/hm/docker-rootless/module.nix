# https://github.com/NixOS/nixpkgs/blob/nixos-25.05/nixos/modules/virtualisation/docker-rootless.nix
{
  config,
  lib,
  pkgs,
  isHM,
  ...
}: let
  cfg = config.virtualisation.docker.rootless;
  # proxy_env = config.networking.proxy.envVars;
  settingsFormat = pkgs.formats.json {};
  daemonSettingsFile = settingsFormat.generate "daemon.json" cfg.daemon.settings;
in {
  ###### interface

  # if we're nixos, we don't need this module since it's already included
  options = {
    virtualisation.docker.rootless = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          This option enables docker in a rootless mode, a daemon that manages
          linux containers. To interact with the daemon, one needs to set
          {command}`DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock`.
        '';
      };

      setSocketVariable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Point {command}`DOCKER_HOST` to rootless Docker instance for
          normal users by default.
        '';
      };

      daemon.settings = lib.mkOption {
        type = settingsFormat.type;
        default = {};
        example = {
          ipv6 = true;
          "fixed-cidr-v6" = "fd00::/80";
        };
        description = ''
          Configuration for docker daemon. The attributes are serialized to JSON used as daemon.conf.
          See https://docs.docker.com/engine/reference/commandline/dockerd/#daemon-configuration-file
        '';
      };

      package = lib.mkPackageOption pkgs "docker" {};
    };
  };

  ###### implementation

  config = lib.mkIf (isHM && cfg.enable) {
    home.packages = [cfg.package];
    # environment.systemPackages = [ cfg.package ];

    systemd.user.sessionVariables = lib.mkIf cfg.setSocketVariable {
      DOCKER_HOST = "unix://$XDG_RUNTIME_DIR/docker.sock";
    };

    # Taken from https://github.com/moby/moby/blob/master/contrib/dockerd-rootless-setuptool.sh
    systemd.user.services.docker = {
      Install.WantedBy = ["default.target"];
      # needs newuidmap from pkgs.shadow
      # path = [ "/run/wrappers" ];
      # environment = proxy_env;
      Unit = {
        Description = "Docker Application Container Engine (Rootless)";
        Documentation = "https://docs.docker.com/go/rootless/";
        # docker-rootless doesn't support running as root.
        ConditionUser = "!@system";
        StartLimitIntervalSec = 60;
        StartLimitBurst = 3;
      };
      Service = {
        ExecStart = "${cfg.package}/bin/dockerd-rootless --config-file=${daemonSettingsFile}";
        ExecReload = "${pkgs.procps}/bin/kill -s HUP $MAINPID";
        TimeoutSec = 0;
        RestartSec = 2;
        Restart = "always";
        LimitNOFILE = "infinity";
        LimitNPROC = "infinity";
        LimitCORE = "infinity";
        TasksMax = "infinity";
        Delegate = true;
        Type = "notify";
        NotifyAccess = "all";
        KillMode = "mixed";
      };
    };
  };
}
