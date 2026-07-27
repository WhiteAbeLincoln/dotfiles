{...}: {
  services.k3s.workloads.module = {k8sLib, ...}: let
    labels =
      k8sLib.appLabels "globalhawk-dashboards"
      // {
        grafana_dashboard = "1";
      };
    dashboards = [
      "globalhawk-overview.json"
      "host-health.json"
      "services.json"
      "logs.json"
      "observability.json"
    ];
  in {
    applications.monitoring-stack = {
      helm.releases.kube-prometheus-stack.values.grafana.sidecar.dashboards = {
        folderAnnotation = "grafana_folder";
        provider.foldersFromFilesStructure = true;
      };

      resources.configMaps.globalhawk-dashboards = {
        metadata = {
          namespace = "monitoring";
          inherit labels;
          annotations.grafana_folder = "Globalhawk";
        };
        data = builtins.listToAttrs (map (file: {
            name = file;
            value = builtins.readFile (./dashboards + "/${file}");
          })
          dashboards);
      };
    };
  };
}
