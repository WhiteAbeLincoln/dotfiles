{config, ...}: {
  services.k3s.workloads.module = {
    charts,
    k8sLib,
    ...
  }: let
    grafanaHost = "grafana${config.homelab.ingressSuffix}";
    labels = k8sLib.appLabels "monitoring-stack";
    mkManifest = value: builtins.toJSON value;
  in {
    applications.monitoring-stack = {
      namespace = "monitoring";
      createNamespace = true;

      helm.releases.kube-prometheus-stack = {
        chart = charts."kube-prometheus-stack";
        values = {
          prometheus.prometheusSpec = {
            retention = "30d";
            retentionSize = "17GB";
            scrapeInterval = "30s";
            evaluationInterval = "30s";
            serviceMonitorSelectorNilUsesHelmValues = false;
            podMonitorSelectorNilUsesHelmValues = false;
            ruleSelectorNilUsesHelmValues = false;
            serviceMonitorNamespaceSelector = {};
            podMonitorNamespaceSelector = {};
            ruleNamespaceSelector = {};
            storageSpec.volumeClaimTemplate.spec = {
              storageClassName = "local-path";
              accessModes = ["ReadWriteOnce"];
              resources.requests.storage = "20Gi";
            };
            resources = {
              requests = {
                cpu = "250m";
                memory = "1Gi";
              };
              limits.memory = "3Gi";
            };
          };
          alertmanager = {
            enabled = true;
            config = {
              route.receiver = "discard";
              receivers = [{name = "discard";}];
            };
          };
          nodeExporter.enabled = false;
          # k3s embeds these components in one server process instead of exposing the
          # kubeadm-style endpoints expected by the chart.
          kubeControllerManager.enabled = false;
          kubeScheduler.enabled = false;
          kubeProxy.enabled = false;
          kubeEtcd.enabled = false;
          grafana = {
            ingress.enabled = false;
            persistence = {
              enabled = true;
              storageClassName = "local-path";
              accessModes = ["ReadWriteOnce"];
              size = "5Gi";
            };
            resources = {
              requests = {
                cpu = "100m";
                memory = "256Mi";
              };
              limits.memory = "1Gi";
            };
            additionalDataSources = [
              {
                name = "Loki";
                uid = "loki";
                type = "loki";
                access = "proxy";
                url = "http://loki-gateway.monitoring.svc.cluster.local";
                isDefault = false;
              }
            ];
          };
        };
      };

      helm.releases.loki = {
        chart = charts.loki;
        values = {
          deploymentMode = "Monolithic";
          loki = {
            auth_enabled = false;
            commonConfig.replication_factor = 1;
            storage.type = "filesystem";
            schemaConfig.configs = [
              {
                from = "2024-04-01";
                store = "tsdb";
                object_store = "filesystem";
                schema = "v13";
                index = {
                  prefix = "loki_index_";
                  period = "24h";
                };
              }
            ];
            compactor = {
              retention_enabled = true;
              delete_request_store = "filesystem";
            };
            limits_config = {
              retention_period = "720h";
              reject_old_samples = true;
              reject_old_samples_max_age = "168h";
              ingestion_rate_mb = 4;
              ingestion_burst_size_mb = 8;
              max_line_size = 65536;
              max_line_size_truncate = true;
              allow_structured_metadata = true;
            };
          };
          singleBinary = {
            replicas = 1;
            persistence = {
              enabled = true;
              storageClass = "local-path";
              size = "30Gi";
            };
            resources = {
              requests = {
                cpu = "200m";
                memory = "512Mi";
              };
              limits.memory = "2Gi";
            };
          };
          gateway = {
            enabled = true;
            # Reserved for host Alloy; verified unused on 2026-07-26.
            service.clusterIP = "10.43.0.50";
          };
          minio.enabled = false;
          backend.replicas = 0;
          read.replicas = 0;
          write.replicas = 0;
        };
      };

      resources.ingresses.grafana = {
        metadata.labels = labels;
        spec = {
          ingressClassName = "traefik";
          tls = [{hosts = [grafanaHost];}];
          rules = [
            {
              host = grafanaHost;
              http.paths = [
                {
                  path = "/";
                  pathType = "Prefix";
                  backend.service = {
                    name = "kube-prometheus-stack-grafana";
                    port.number = 80;
                  };
                }
              ];
            }
          ];
        };
      };

      yamls = [
        (mkManifest {
          apiVersion = "networking.k8s.io/v1";
          kind = "NetworkPolicy";
          metadata = {
            name = "default-deny-ingress";
            namespace = "monitoring";
            inherit labels;
          };
          spec = {
            podSelector = {};
            policyTypes = ["Ingress"];
          };
        })
        (mkManifest {
          apiVersion = "networking.k8s.io/v1";
          kind = "NetworkPolicy";
          metadata = {
            name = "allow-monitoring-namespace";
            namespace = "monitoring";
            inherit labels;
          };
          spec = {
            podSelector = {};
            policyTypes = ["Ingress"];
            ingress = [
              {
                from = [{podSelector = {};}];
              }
            ];
          };
        })
        (mkManifest {
          apiVersion = "networking.k8s.io/v1";
          kind = "NetworkPolicy";
          metadata = {
            name = "allow-traefik-to-grafana";
            namespace = "monitoring";
            inherit labels;
          };
          spec = {
            podSelector.matchLabels = {
              "app.kubernetes.io/name" = "grafana";
              "app.kubernetes.io/instance" = "kube-prometheus-stack";
            };
            policyTypes = ["Ingress"];
            ingress = [
              {
                from = [
                  {
                    namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                    podSelector.matchLabels."app.kubernetes.io/name" = "traefik";
                  }
                ];
                ports = [
                  {
                    protocol = "TCP";
                    port = 3000;
                  }
                ];
              }
            ];
          };
        })
        (mkManifest {
          apiVersion = "monitoring.coreos.com/v1";
          kind = "PodMonitor";
          metadata = {
            name = "traefik";
            namespace = "monitoring";
            inherit labels;
          };
          spec = {
            namespaceSelector.matchNames = ["kube-system"];
            selector.matchLabels."app.kubernetes.io/name" = "traefik";
            podMetricsEndpoints = [
              {
                port = "metrics";
                interval = "30s";
                path = "/metrics";
              }
            ];
          };
        })
      ];
    };
  };
}
