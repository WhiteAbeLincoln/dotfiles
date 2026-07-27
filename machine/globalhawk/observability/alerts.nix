{config, ...}: {
  services.k3s.workloads.module = {k8sLib, ...}: let
    labels = k8sLib.appLabels "globalhawk-alerts";
    mkManifest = value: builtins.toJSON value;
    dashboard = uid: "https://grafana${config.homelab.ingressSuffix}/d/${uid}";
    mkRule = {
      alert,
      expr,
      for,
      severity,
      summary,
      description,
      dashboardUid,
    }: {
      inherit alert expr for;
      labels = {
        inherit severity;
        cluster = "globalhawk";
      };
      annotations = {
        inherit summary description;
        runbook_url = dashboard dashboardUid;
      };
    };
  in {
    applications.monitoring-stack.yamls = [
      (mkManifest {
        apiVersion = "monitoring.coreos.com/v1";
        kind = "PrometheusRule";
        metadata = {
          name = "globalhawk-alerts";
          namespace = "monitoring";
          inherit labels;
        };
        spec.groups = [
          {
            name = "globalhawk.storage";
            interval = "30s";
            rules = [
              (mkRule {
                alert = "GlobalhawkRootFilesystemWarning";
                expr = ''100 * (1 - node_filesystem_avail_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"} / node_filesystem_size_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"}) > 70'';
                for = "15m";
                severity = "warning";
                summary = "Globalhawk root filesystem is over 70% used";
                description = "Root filesystem usage is {{ $value | printf \"%.1f\" }}% based on space currently available to non-root users.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkRootFilesystemCritical";
                expr = ''100 * (1 - node_filesystem_avail_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"} / node_filesystem_size_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"}) > 90'';
                for = "5m";
                severity = "critical";
                summary = "Globalhawk root filesystem is over 90% used";
                description = "Root filesystem usage is {{ $value | printf \"%.1f\" }}% based on space currently available to non-root users.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkRootFilesystemThirtyDayProjection";
                expr = ''node_filesystem_avail_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"} / node_filesystem_size_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"} < 0.30 and predict_linear(node_filesystem_avail_bytes{cluster="globalhawk",mountpoint="/",fstype!~"tmpfs|overlay"}[6h], 30 * 24 * 60 * 60) < 0'';
                for = "1h";
                severity = "warning";
                summary = "Globalhawk root storage may fill within 30 days";
                description = "The six-hour available-space trend projects that the root filesystem will exhaust its current capacity inside the 30-day observability envelope.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkZfsPoolUnhealthy";
                expr = ''zfs_pool_health{cluster="globalhawk",pool="pool"} != 0'';
                for = "0m";
                severity = "critical";
                summary = "Globalhawk ZFS pool is not ONLINE";
                description = "Pool {{ $labels.pool }} reports health code {{ $value }} (ONLINE is 0).";
                dashboardUid = "globalhawk-host";
              })
            ];
          }
          {
            name = "globalhawk.hardware";
            interval = "30s";
            rules = [
              (mkRule {
                alert = "GlobalhawkSmartHealthFailure";
                expr = ''smartctl_device_smart_status{cluster="globalhawk"} == 0 or smartctl_device_critical_warning{cluster="globalhawk"} > 0'';
                for = "0m";
                severity = "critical";
                summary = "Globalhawk disk health check failed";
                description = "Device {{ $labels.device }} reports failed SMART health or an NVMe critical warning.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkDiskTemperatureHigh";
                expr = ''smartctl_device_temperature{cluster="globalhawk"} > 55'';
                for = "15m";
                severity = "warning";
                summary = "Globalhawk disk temperature is high";
                description = "Device {{ $labels.device }} temperature has exceeded 55°C for 15 minutes.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkNvmeWearHigh";
                expr = ''smartctl_device_percentage_used{cluster="globalhawk"} > 90'';
                for = "15m";
                severity = "warning";
                summary = "Globalhawk NVMe endurance is nearly exhausted";
                description = "Device {{ $labels.device }} reports {{ $value | printf \"%.0f\" }}% of its rated endurance used.";
                dashboardUid = "globalhawk-host";
              })
            ];
          }
          {
            name = "globalhawk.host-services";
            interval = "30s";
            rules = [
              (mkRule {
                alert = "GlobalhawkResticBackupStale";
                expr = ''time() - restic_backup_last_success_timestamp_seconds{cluster="globalhawk",backup="media"} > 30 * 60 * 60 or absent(restic_backup_last_success_timestamp_seconds{cluster="globalhawk",backup="media"})'';
                for = "5m";
                severity = "warning";
                summary = "Globalhawk restic backup is stale";
                description = "The last successful media backup is more than 30 hours old or its success telemetry is missing.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkResticBackupFailed";
                expr = ''restic_backup_last_status{cluster="globalhawk",backup="media"} == 0 or time() - restic_backup_last_success_timestamp_seconds{cluster="globalhawk",backup="media"} > 48 * 60 * 60 or absent(restic_backup_last_success_timestamp_seconds{cluster="globalhawk",backup="media"}) or absent(restic_backup_last_status{cluster="globalhawk",backup="media"})'';
                for = "5m";
                severity = "critical";
                summary = "Globalhawk restic backup failed or is critically stale";
                description = "The media backup reports failure, has not succeeded for more than 48 hours, or its telemetry is missing.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkImportantSystemdUnitFailed";
                expr = ''node_systemd_unit_state{cluster="globalhawk",state="failed",name=~"(k3s|plex|adguardhome|smartd|zfs-.*|restic-backups-media|restic-media-failure)\\.service"} == 1'';
                for = "5m";
                severity = "critical";
                summary = "An important Globalhawk systemd unit failed";
                description = "Unit {{ $labels.name }} has remained failed for five minutes.";
                dashboardUid = "globalhawk-host";
              })
              (mkRule {
                alert = "GlobalhawkServiceTargetDown";
                expr = ''up{namespace="monitoring",service=~"plex-exporter|adguard-exporter"} == 0'';
                for = "5m";
                severity = "critical";
                summary = "A Globalhawk media or DNS service is unreachable";
                description = "The {{ $labels.service }} exporter cannot reach its backing service.";
                dashboardUid = "globalhawk-services";
              })
            ];
          }
          {
            name = "globalhawk.logging";
            interval = "30s";
            rules = [
              (mkRule {
                alert = "GlobalhawkLokiRejectingLines";
                expr = ''sum(rate(loki_discarded_samples_total[5m])) > 0'';
                for = "5m";
                severity = "warning";
                summary = "Loki is rejecting log lines";
                description = "Loki has continuously discarded incoming log lines for five minutes.";
                dashboardUid = "globalhawk-telemetry";
              })
            ];
          }
        ];
      })
    ];
  };
}
