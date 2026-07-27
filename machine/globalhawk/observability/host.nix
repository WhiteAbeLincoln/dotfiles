{config, ...}: let
  hostGatewayIp = config.services.k3s.clusterNetwork.hostGatewayIp;
  textfileDir = "/var/lib/prometheus-node-exporter-text-files";
in {
  services.prometheus.exporters = {
    node = {
      enable = true;
      listenAddress = hostGatewayIp;
      openFirewall = false;
      enabledCollectors = ["systemd" "hwmon" "textfile"];
      extraFlags = ["--collector.textfile.directory=${textfileDir}"];
    };
    smartctl = {
      enable = true;
      listenAddress = hostGatewayIp;
      openFirewall = false;
      maxInterval = "5m";
    };
    zfs = {
      enable = true;
      listenAddress = hostGatewayIp;
      openFirewall = false;
      pools = ["pool"];
    };
  };

  systemd.tmpfiles.rules = [
    "d ${textfileDir} 0755 root root -"
  ];

  services.alloy = {
    enable = true;
    extraFlags = [
      "--server.http.listen-addr=${hostGatewayIp}:12345"
      "--disable-reporting"
    ];
  };

  environment.etc."alloy/host-logs.alloy".text = ''
    loki.source.journal "host_journal" {
      forward_to = [loki.relabel.host_journal.receiver]
    }

    loki.relabel "host_journal" {
      forward_to = [loki.process.host_journal.receiver]

      rule {
        source_labels = ["__journal__systemd_unit"]
        target_label  = "unit"
      }
      rule {
        source_labels = ["__journal_priority_keyword"]
        target_label  = "priority"
      }
      rule {
        source_labels = ["__journal__transport"]
        target_label  = "transport"
      }
      rule {
        source_labels = ["__journal__boot_id"]
        target_label  = "boot_id"
      }
      rule {
        source_labels = ["__journal__hostname"]
        target_label  = "host"
      }
    }

    loki.process "host_journal" {
      forward_to = [loki.write.monitoring.receiver]

      stage.match {
        selector = "{unit!~\"(k3s|plex|adguardhome|smartd|zfs-(import.*|mount.*|zed|scrub.*|trim.*|media-posixacl)|restic-backups-media|restic-media-failure|nixos-upgrade|nix-gc|nix-optimise|network-addresses-enp1s0|wpa_supplicant)[.]service\", priority!~\"warning|err|crit|alert|emerg\"}"
        action   = "drop"
      }
    }

    loki.write "monitoring" {
      endpoint {
        // Reserved by the Loki gateway Service for host Alloy.
        url = "http://10.43.0.50/loki/api/v1/push"
      }
    }
  '';

  services.k3s.workloads.module = {k8sLib, ...}: let
    labels = k8sLib.appLabels "globalhawk-host";
    targetLabels = {
      cluster = "globalhawk";
      node = "globalhawk";
      source = "nixos";
    };
    mkManifest = value: builtins.toJSON value;
    mkService = name: port: {
      apiVersion = "v1";
      kind = "Service";
      metadata = {
        inherit name;
        namespace = "monitoring";
        labels =
          labels
          // targetLabels
          // {"observability.globalhawk/endpoint" = name;};
      };
      spec.ports = [
        {
          name = "metrics";
          inherit port;
          targetPort = port;
        }
      ];
    };
    mkEndpointSlice = name: port: {
      apiVersion = "discovery.k8s.io/v1";
      kind = "EndpointSlice";
      metadata = {
        inherit name;
        namespace = "monitoring";
        labels = labels // {"kubernetes.io/service-name" = name;};
      };
      addressType = "IPv4";
      endpoints = [{addresses = [hostGatewayIp];}];
      ports = [
        {
          name = "metrics";
          inherit port;
          protocol = "TCP";
        }
      ];
    };
    mkServiceMonitor = name: interval: {
      apiVersion = "monitoring.coreos.com/v1";
      kind = "ServiceMonitor";
      metadata = {
        inherit name labels;
        namespace = "monitoring";
      };
      spec = {
        selector.matchLabels."observability.globalhawk/endpoint" = name;
        targetLabels = builtins.attrNames targetLabels;
        endpoints = [
          {
            port = "metrics";
            inherit interval;
            path = "/metrics";
          }
        ];
      };
    };
    endpoints = [
      {
        name = "globalhawk-node";
        port = 9100;
        interval = "30s";
      }
      {
        name = "globalhawk-smartctl";
        port = 9633;
        interval = "5m";
      }
      {
        name = "globalhawk-zfs";
        port = 9134;
        interval = "30s";
      }
      {
        name = "globalhawk-alloy";
        port = 12345;
        interval = "30s";
      }
    ];
  in {
    applications.globalhawk-host = {
      namespace = "monitoring";
      createNamespace = false;
      yamls =
        builtins.concatMap (
          endpoint:
            map mkManifest [
              (mkService endpoint.name endpoint.port)
              (mkEndpointSlice endpoint.name endpoint.port)
              (mkServiceMonitor endpoint.name endpoint.interval)
            ]
        )
        endpoints;
    };
  };
}
