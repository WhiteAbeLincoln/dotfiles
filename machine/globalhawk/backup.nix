{
  lib,
  pkgs,
  config,
  ...
}: let
  mediaRoot = config.homelab.media.root;
  # Immich's managed store on k3s (previously derived from the retired custom
  # Immich module's upload dir). Immich writes originals + its built-in DB
  # dumps under here (mounted at /data).
  immichRoot = "${mediaRoot}/immich/library";
  # Where the newest Immich DB dump is staged for inclusion in the backup.
  stagedDbDump = "/var/lib/restic-media/immich-db-latest.sql.gz";
in {
  # restic credentials come from sops-nix at activation (decrypted to
  # /run/secrets, never the store). Repo URL + repo password are single-value
  # secrets; the B2 API creds ride in the sops-rendered EnvironmentFile. See
  # machine/globalhawk/sops.nix.
  services.restic.backups.media = {
    initialize = true; # create the repo on first run
    repositoryFile = config.sops.secrets.restic_repo.path; # S3-compatible B2 endpoint + bucket
    passwordFile = config.sops.secrets.restic_repo_pass.path;
    environmentFile = config.sops.templates."restic-env".path;

    # Direct path backup (no ZFS snapshot): media is write-once and the Immich
    # dump is an atomically-written file. See the design spec for rationale.
    paths = [
      immichRoot
      stagedDbDump
      "${mediaRoot}/photos"
      "${mediaRoot}/books"
      "${mediaRoot}/old_books"
      "${mediaRoot}/audiobooks"
      "${mediaRoot}/documents"
      "${mediaRoot}/music"
      # App state for the k3s ebook/audiobook workloads. SQLite databases and
      # Libation's refreshable account material are captured as a filesystem
      # snapshot (crash-consistent-ish); a sqlite3 .backup pre-hook is a
      # possible later hardening (see design spec).
      "${mediaRoot}/apps/calibre-web-automated"
      "${mediaRoot}/apps/audiobookshelf"
      "${mediaRoot}/apps/libation"
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

  # Make a failed backup loud: email root through the host-wide transport in
  # mail.nix (also used by ZED and smartd).
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
