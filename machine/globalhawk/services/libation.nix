# Periodic Audible reconciliation. Keep the working pinned image and existing
# state layout; systemd owns scheduling, concurrency, the deadline, and logs.
{
  config,
  lib,
  pkgs,
  ...
}: let
  mediaRoot = config.homelab.media.root;
  stateRoot = "${mediaRoot}/apps/libation";
  mediaUid = toString config.users.users._media.uid;
  mediaGid = toString config.users.groups._media.gid;
  image = "rmcrackan/libation:13.5.1@sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0";
  settings = pkgs.writeText "libation-settings.json" (builtins.toJSON {
    ImportEpisodes = false;
    DownloadEpisodes = false;
    AutoDownloadEpisodes = false;
    # Libation 13.5.1 supports these property names and uses <id> for the
    # Audible product ID. Keep it in both paths so every output form is safe.
    FolderTemplate = "<title short> [<id>]";
    FileTemplate = "<title> [<id>]";
  });
  run = pkgs.writeShellScript "libation-run" ''
    # The 13.5.1 entrypoint copies AccountsSettings.json into
    # /config-internal, where scan may refresh its tokens. Copy
    # that mutable state back without hiding scan/liberate errors.
    set +e
    /libation/liberate.sh
    status=$?
    set -e
    if test "$status" -eq 0; then
      destination="/config/.AccountsSettings.json.$$"
      cp /config-internal/AccountsSettings.json "$destination"
      mv "$destination" /config/AccountsSettings.json
    fi
    exit "$status"
  '';
in {
  virtualisation.oci-containers.containers.libation-reconcile = {
    inherit image;
    serviceName = "libation-reconcile";
    autoStart = false;
    autoRemoveOnStop = true;
    # The OCI module selects the configured runtime. The workload still runs
    # as _media without capabilities or privilege escalation.
    user = "${mediaUid}:${mediaGid}";
    capabilities.ALL = false;
    extraOptions = ["--security-opt=no-new-privileges:true"];
    environment = {
      TZ = config.time.timeZone;
      SLEEP_TIME = "-1";
      LIBATION_BOOKS_DIR = "/data";
      LIBATION_CONFIG_DIR = "/config";
      LIBATION_DB_DIR = "/db";
    };
    volumes = [
      "${mediaRoot}/audiobooks:/data"
      "${stateRoot}/config:/config"
      "${stateRoot}/db:/db"
      # Libation 13.5.1's container forces Settings.InProgress to /tmp.
      "${stateRoot}/in-progress:/tmp"
      "${settings}:/config/Settings.json:ro"
      "${run}:/run-libation:ro"
    ];
    entrypoint = "/bin/bash";
    cmd = ["/run-libation"];
  };

  systemd.services.libation-reconcile = {
    description = "Scan Audible accounts and download new audiobooks";
    unitConfig.RequiresMountsFor = [stateRoot "${mediaRoot}/audiobooks"];
    serviceConfig = {
      # Retain the runtime's generated service type and stop/cleanup handlers.
      # Docker runs attached; Podman notifies systemd via conmon. Neither is
      # a oneshot, so cap active runtime rather than startup time.
      RuntimeMaxSec = "48h";
      Restart = lib.mkForce "no";
    };
  };

  systemd.timers.libation-reconcile = {
    description = "Reconcile Audible twice a month";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "*-*-01,15 04:00:00 ${config.time.timeZone}";
      RandomizedDelaySec = "2h";
      Persistent = true;
      # An already-running service is never started again. Manual starts bypass
      # the timer's delay, replacing the old jitter init container entirely.
    };
  };
}
