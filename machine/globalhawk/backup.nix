{
  lib,
  pkgs,
  config,
  ...
}: let
  secrets = import ../../secrets/globalhawk.nix;
  facts = import ./facts.nix;
  # Immich's UPLOAD_LOCATION, taken from the service config so the two can't
  # drift (becomes services.immich.mediaLocation after the NixOS migration).
  immichRoot = config.services.immich-custom.uploadDir;
  # Where the newest Immich DB dump is staged for inclusion in the backup.
  stagedDbDump = "/var/lib/restic-media/immich-db-latest.sql.gz";
in {
  # Materialize restic credentials from the git-crypt'd secrets file.
  # NOTE: environment.etc.*.text renders content into the world-readable Nix
  # store. This matches the repo's existing secret handling (e.g. the msmtp
  # password in disks.nix); migrating secrets to agenix/sops is the future
  # hardening path, out of scope here.
  environment.etc = {
    "restic/media-password" = {
      text = secrets.restic.b2.restic_repo_pass;
      mode = "0600";
    };
    "restic/media-env" = {
      text = ''
        AWS_ACCESS_KEY_ID=${secrets.restic.b2.key_id}
        AWS_SECRET_ACCESS_KEY=${secrets.restic.b2.app_key}
      '';
      mode = "0600";
    };
  };

  services.restic.backups.media = {
    initialize = true; # create the repo on first run
    repository = secrets.restic.b2.repo; # S3-compatible B2 endpoint + bucket
    passwordFile = "/etc/restic/media-password";
    environmentFile = "/etc/restic/media-env";

    # Direct path backup (no ZFS snapshot): media is write-once and the Immich
    # dump is an atomically-written file. See the design spec for rationale.
    paths = [
      immichRoot
      stagedDbDump
      "${facts.mediaRoot}/photos"
      "${facts.mediaRoot}/books"
      "${facts.mediaRoot}/old_books"
      "${facts.mediaRoot}/audiobooks"
      "${facts.mediaRoot}/documents"
      "${facts.mediaRoot}/music"
    ];

    # thumbs/ and encoded-video/ are regenerable from originals; backups/ holds
    # the rotated DB-dump backlog (the newest is staged separately, below).
    exclude = [
      "${immichRoot}/thumbs"
      "${immichRoot}/encoded-video"
      "${immichRoot}/backups"
    ];

    # Stage only the newest Immich DB dump (full pg_dumpall, ~28 MB) so the repo
    # carries one current restore point rather than the whole rotated backlog.
    backupPrepareCommand = ''
      mkdir -p "$(dirname ${stagedDbDump})"
      latest="$(ls -t ${immichRoot}/backups/immich-db-backup-*.sql.gz 2>/dev/null | head -1)"
      if [ -n "$latest" ]; then
        cp -f "$latest" ${stagedDbDump}
      fi
    '';

    # Run after Immich's built-in 02:00 dump so the staged dump is same-day fresh.
    timerConfig = {
      OnCalendar = "*-*-* 03:30:00";
      RandomizedDelaySec = "30m";
      Persistent = true; # catch up a missed run on next boot
    };

    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 5"
      "--keep-monthly 12"
    ];
  };

  # The prepare command uses coreutils (ls/head/cp/mkdir/dirname); ensure they
  # resolve in the service's PATH.
  systemd.services.restic-backups-media.path = [pkgs.coreutils];

  # Make a failed backup loud: email root (aliased to gmail via /etc/aliases)
  # through the msmtp setup already configured in disks.nix for ZED/smartd.
  systemd.services.restic-backups-media.onFailure = ["restic-media-failure.service"];
  systemd.services.restic-media-failure = {
    description = "Alert on restic media backup failure";
    serviceConfig.Type = "oneshot";
    script = ''
      printf '%s\n' \
        'Subject: [globalhawk] restic media backup FAILED' \
        "" \
        'The restic-backups-media job failed. Investigate with:' \
        '  journalctl -u restic-backups-media --since -1d' \
        | ${pkgs.msmtp}/bin/msmtp root
    '';
  };
}
