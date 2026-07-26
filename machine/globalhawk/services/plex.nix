{
  pkgs,
  config,
  ...
}: {
  # importing this conditionally causes recursion issues
  # instead, macos machines will be required to import the module statically
  # imports = [./macos-module.nix];

  services.plex = {
    enable = true;
    openFirewall = true;
    package = pkgs.unstable.plex;
    extraScanners = [
      (pkgs.fetchFromGitHub {
        owner = "ZeroQI";
        repo = "Absolute-Series-Scanner";
        rev = "b33b1935480cae76007a82f8887cb173200cfc53";
        sha256 = "YupSXgFi/qfODuay3LoIl/1178gtU+MwhiZJAvvGV2g=";
      })
    ];
    extraPlugins = [
      (builtins.path {
        name = "Hama.bundle";
        path = pkgs.fetchFromGitHub {
          owner = "ZeroQI";
          repo = "Hama.bundle";
          rev = "c6987a00e68b23883a263481c823bb7aa7684c21";
          sha256 = "pH7oO0dsTA2zXsquwCV6z8IdNoDwippP806KT9TX4RU=";
        };
      })
      # an audiobook library organizer
      # (builtins.path {
      #   name = "Audnexus.bundle";
      #   path = pkgs.fetchFromGitHub {
      #     owner = "djdembeck";
      #     repo = "Audnexus.bundle";
      #     rev = "v0.2.8";
      #     sha256 = "sha256-IWOSz3vYL7zhdHan468xNc6C/eQ2C2BukQlaJNLXh7E=";
      #   };
      # })
    ];
  };

  services.k3s.workloads.module = {nixosConfig, ...}: let
    host = "plex${config.homelab.ingressSuffix}";
    port = 32400;
    clusterNetwork = config.services.k3s.clusterNetwork;
  in {
    applications.plex = {
      namespace = "plex";
      createNamespace = true;
      resources = {
        # Selector-less Service: its Endpoints are managed by hand (below), not by
        # a pod selector, so it can front an off-cluster backend.
        services.plex.spec.ports.web = {
          port = port;
          targetPort = port;
        };
        ingresses.plex = {
          spec = {
            ingressClassName = "traefik";
            tls = [{hosts = [host];}];
            rules = [
              {
                inherit host;
                http.paths = [
                  {
                    path = "/";
                    pathType = "Prefix";
                    backend.service = {
                      name = "plex";
                      port.number = port;
                    };
                  }
                ];
              }
            ];
          };
        };
      };
      # EndpointSlice (the non-deprecated replacement for Endpoints) binds the
      # selector-less Service to the off-cluster backend. The
      # kubernetes.io/service-name label links it to the `plex` Service; the port
      # name "web" matches the Service port. Authored raw (JSON is valid YAML).
      yamls = [
        (builtins.toJSON {
          apiVersion = "discovery.k8s.io/v1";
          kind = "EndpointSlice";
          metadata = {
            name = "plex";
            namespace = "plex";
            labels."kubernetes.io/service-name" = "plex";
          };
          addressType = "IPv4";
          endpoints = [{addresses = [clusterNetwork.hostGatewayIp];}];
          ports = [
            {
              name = "web";
              port = port;
              protocol = "TCP";
            }
          ];
        })
      ];
    };
  };
}
