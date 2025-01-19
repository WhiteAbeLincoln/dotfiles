{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.homebridge;
in
{
  options = {
    services.homebridge = {
      enable = mkEnableOption "homebridge";
      cfgDir = mkOption {
        type = types.str;
        description = "The location to store homebridge configs";
      };
      user = mkOption {
        type = types.nullOr types.str;
        description = "Override the username or UID (and optionally groupname or GID) used in the container.";
        default = null;
      };
      additionalPorts = mkOption {
        type = types.listOf types.ints.u16;
        description = "Additional ports to open";
        default = [];
      };
    };
  };
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      8581
      51082 
    ] ++ cfg.additionalPorts;
    virtualisation.oci-containers.containers = {
      homebridge = {
        image = "homebridge/homebridge";
        user = cfg.user;
        volumes = [
          "${cfg.cfgDir}:/homebridge"
          "/etc/localtime:/etc/localtime:ro"
        ];
        environment = { TZ = config.time.timeZone; };
        autoStart = true;
        extraOptions = [ "--network=host" ];
      };
    };
  };
}
