{
  config,
  pkgs,
  ...
}: let
  hostGatewayIp = config.services.k3s.clusterNetwork.hostGatewayIp;
  plexExporterImage = pkgs.callPackage ../../../packages/plex-exporter.nix {};
  adguardExporterImage = pkgs.callPackage ../../../packages/adguard-exporter.nix {};
in {
  # Local, content-addressed archives are imported into containerd before k3s
  # starts. The workload never depends on a mutable registry tag.
  services.k3s.images = [
    plexExporterImage
    adguardExporterImage
  ];

  services.k3s.workloads.module = {k8sLib, ...}: let
    mkManifest = value: builtins.toJSON value;
    mkAdapter = {
      name,
      image,
      secretName,
      secretKey,
      secretMount,
      environment,
      allowedMetrics,
    }: let
      labels =
        k8sLib.appLabels name
        // {
          "app.kubernetes.io/component" = "metrics-adapter";
          "observability.globalhawk/scrape" = "aggregate";
        };
    in [
      (mkManifest {
        apiVersion = "apps/v1";
        kind = "Deployment";
        metadata = {
          inherit name labels;
          namespace = "monitoring";
        };
        spec = {
          replicas = 1;
          selector.matchLabels = k8sLib.appLabels name;
          template = {
            metadata.labels = labels;
            spec = {
              automountServiceAccountToken = false;
              securityContext = {
                runAsNonRoot = true;
                runAsUser = 65532;
                runAsGroup = 65532;
                fsGroup = 65532;
                seccompProfile.type = "RuntimeDefault";
              };
              containers = [
                {
                  inherit name image;
                  imagePullPolicy = "Never";
                  env =
                    map (entry: {
                      inherit (entry) name value;
                    })
                    environment;
                  ports = [
                    {
                      name = "metrics";
                      containerPort = 9100;
                      protocol = "TCP";
                    }
                  ];
                  readinessProbe = {
                    httpGet = {
                      path = "/metrics";
                      port = "metrics";
                    };
                    initialDelaySeconds = 5;
                    periodSeconds = 30;
                    timeoutSeconds = 20;
                    failureThreshold = 3;
                  };
                  resources = {
                    requests = {
                      cpu = "10m";
                      memory = "16Mi";
                    };
                    limits = {
                      cpu = "100m";
                      memory = "64Mi";
                    };
                  };
                  securityContext = {
                    allowPrivilegeEscalation = false;
                    readOnlyRootFilesystem = true;
                    capabilities.drop = ["ALL"];
                  };
                  volumeMounts = [
                    {
                      name = "credentials";
                      mountPath = secretMount;
                      subPath = secretKey;
                      readOnly = true;
                    }
                  ];
                }
              ];
              volumes = [
                {
                  name = "credentials";
                  secret.secretName = secretName;
                  secret.defaultMode = 288;
                }
              ];
            };
          };
        };
      })
      (mkManifest {
        apiVersion = "v1";
        kind = "Service";
        metadata = {
          inherit name labels;
          namespace = "monitoring";
        };
        spec = {
          type = "ClusterIP";
          selector = k8sLib.appLabels name;
          ports = [
            {
              name = "metrics";
              port = 9100;
              targetPort = "metrics";
              protocol = "TCP";
            }
          ];
        };
      })
      (mkManifest {
        apiVersion = "monitoring.coreos.com/v1";
        kind = "ServiceMonitor";
        metadata = {
          inherit name labels;
          namespace = "monitoring";
        };
        spec = {
          selector.matchLabels =
            k8sLib.appLabels name
            // {"observability.globalhawk/scrape" = "aggregate";};
          endpoints = [
            {
              port = "metrics";
              path = "/metrics";
              interval = "60s";
              scrapeTimeout = "30s";
              metricRelabelings =
                [
                  {
                    action = "keep";
                    sourceLabels = ["__name__"];
                    regex = builtins.concatStringsSep "|" allowedMetrics;
                  }
                ]
                ++ map (label: {
                  action = "drop";
                  sourceLabels = [label];
                  regex = ".+";
                }) [
                  "title"
                  "user"
                  "client"
                  "address"
                  "path"
                  "session"
                  "filename"
                  "url"
                  "domain"
                ];
            }
          ];
        };
      })
    ];
  in {
    applications.service-metrics-adapters = {
      namespace = "monitoring";
      createNamespace = false;
      yamls =
        (mkAdapter {
          name = "plex-exporter";
          image = "localhost/plex-exporter:1.0.2";
          secretName = "plex-exporter";
          secretKey = "token";
          secretMount = "/run/credentials/plex-token";
          environment = [
            {
              name = "PLEX_URL";
              value = "http://${hostGatewayIp}:32400";
            }
            {
              name = "PLEX_TOKEN_FILE";
              value = "/run/credentials/plex-token";
            }
          ];
          allowedMetrics = [
            "service_up"
            "service_api_request_duration_seconds"
            "plex_active_sessions"
            "plex_transcoding_sessions"
            "plex_library_items"
          ];
        })
        ++ (mkAdapter {
          name = "adguard-exporter";
          image = "localhost/adguard-exporter:1.0.2";
          secretName = "adguard-exporter";
          secretKey = "password";
          secretMount = "/run/credentials/adguard-password";
          environment = [
            {
              name = "ADGUARD_URL";
              value = "http://${hostGatewayIp}:3000";
            }
            {
              name = "ADGUARD_USERNAME";
              value = "admin";
            }
            {
              name = "ADGUARD_PASSWORD_FILE";
              value = "/run/credentials/adguard-password";
            }
          ];
          allowedMetrics = [
            "service_up"
            "service_api_request_duration_seconds"
            "adguard_queries_total"
            "adguard_blocked_queries_total"
            "adguard_query_duration_seconds"
            "adguard_protection_enabled"
            "adguard_filter_enabled"
          ];
        });
    };
  };
}
