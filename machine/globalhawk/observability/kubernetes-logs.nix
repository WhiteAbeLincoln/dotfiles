{...}: {
  services.k3s.workloads.module = {charts, ...}: let
    mkManifest = value: builtins.toJSON value;
  in {
    applications.kubernetes-logs = {
      namespace = "monitoring";
      createNamespace = false;

      helm.releases.alloy = {
        chart = charts.alloy;
        values = {
          controller.type = "daemonset";
          crds.create = false;

          alloy = {
            enableReporting = false;
            storagePath = "/var/lib/alloy";
            extraEnv = [
              {
                name = "HOST_NODE_NAME";
                valueFrom.fieldRef.fieldPath = "spec.nodeName";
              }
            ];
            configMap.content = ''
              discovery.kubernetes "pod" {
                role = "pod"
              }

              discovery.relabel "pod_logs" {
                targets = discovery.kubernetes.pod.targets

                rule {
                  source_labels = ["__meta_kubernetes_pod_node_name"]
                  regex         = sys.env("HOST_NODE_NAME")
                  action        = "keep"
                }
                rule {
                  source_labels = ["__meta_kubernetes_namespace"]
                  target_label  = "namespace"
                }
                rule {
                  source_labels = ["__meta_kubernetes_pod_name"]
                  target_label  = "pod"
                }
                rule {
                  source_labels = ["__meta_kubernetes_pod_container_name"]
                  target_label  = "container"
                }
                rule {
                  source_labels = ["__meta_kubernetes_pod_node_name"]
                  target_label  = "node"
                }
                rule {
                  source_labels = ["__meta_kubernetes_pod_controller_name"]
                  target_label  = "workload"
                }
                rule {
                  source_labels = ["workload"]
                  regex         = "^(.+)-[bcdfghjklmnpqrstvwxz2456789]{8,10}$"
                  replacement   = "$1"
                  target_label  = "workload"
                }
                rule {
                  replacement  = "globalhawk"
                  target_label = "cluster"
                }
              }

              loki.source.kubernetes "pod_logs" {
                targets    = discovery.relabel.pod_logs.output
                forward_to = [loki.process.pod_logs.receiver]
              }

              loki.process "pod_logs" {
                forward_to = [loki.write.monitoring.receiver]

                stage.cri {}

                stage.labels {
                  values = {
                    stream = "",
                  }
                }
              }

              loki.source.kubernetes_events "cluster" {
                job_name   = "integrations/kubernetes/eventhandler"
                log_format = "logfmt"
                forward_to = [loki.process.kubernetes_events.receiver]
              }

              loki.process "kubernetes_events" {
                forward_to = [loki.write.monitoring.receiver]

                stage.static_labels {
                  values = {
                    cluster = "globalhawk",
                    source  = "kubernetes-events",
                  }
                }
              }

              loki.write "monitoring" {
                endpoint {
                  url = "http://loki-gateway.monitoring.svc.cluster.local/loki/api/v1/push"
                }
              }
            '';
            mounts = {
              varlog = false;
              dockercontainers = false;
              extra = [
                {
                  name = "alloy-state";
                  mountPath = "/var/lib/alloy";
                }
              ];
            };
            resources = {
              requests = {
                cpu = "100m";
                memory = "128Mi";
              };
              limits.memory = "512Mi";
            };
          };

          controller.volumes.extra = [
            {
              name = "alloy-state";
              persistentVolumeClaim.claimName = "alloy-state";
            }
          ];

          rbac = {
            create = true;
            rules = [
              {
                apiGroups = [""];
                resources = ["pods" "pods/log" "namespaces" "events"];
                verbs = ["get" "list" "watch"];
              }
            ];
            clusterRules = [
              {
                apiGroups = [""];
                resources = ["nodes"];
                verbs = ["get" "list" "watch"];
              }
            ];
          };

          service = {
            enabled = true;
            type = "ClusterIP";
          };
          serviceMonitor.enabled = true;
        };
      };

      yamls = [
        (mkManifest {
          apiVersion = "v1";
          kind = "PersistentVolumeClaim";
          metadata = {
            name = "alloy-state";
            namespace = "monitoring";
          };
          spec = {
            accessModes = ["ReadWriteOnce"];
            storageClassName = "local-path";
            resources.requests.storage = "1Gi";
          };
        })
      ];
    };
  };
}
