{lib, ...}: {
  options = {
    homelab = {
      media.root = lib.mkOption {
        type = lib.types.str;
        description = "Root of the host media dataset shared with workloads.";
      };
      ingressSuffix = lib.mkOption {
        type = lib.types.str;
        description = "Suffix appended to homelab ingress application names.";
      };
      network = {
        lanInterface = lib.mkOption {type = lib.types.str;};
        lanIp = lib.mkOption {type = lib.types.str;};
        lanGateway = lib.mkOption {type = lib.types.str;};
        lanSubnet = lib.mkOption {type = lib.types.str;};
      };
    };
    services.k3s.clusterNetwork = {
      podCidr = lib.mkOption {type = lib.types.str;};
      serviceCidr = lib.mkOption {type = lib.types.str;};
      hostGatewayIp = lib.mkOption {type = lib.types.str;};
    };
  };

  config = {
    homelab = {
      media.root = "/data/Media";
      ingressSuffix = ".h.abrahamwhite.com";
      network = {
        lanInterface = "enp1s0";
        lanIp = "192.168.1.50";
        lanGateway = "192.168.1.1";
        lanSubnet = "192.168.1.0/24";
      };
    };
    services.k3s.clusterNetwork = {
      podCidr = "10.42.0.0/16";
      serviceCidr = "10.43.0.0/16";
      hostGatewayIp = "10.42.0.1";
    };
  };
}
