# globalhawk remote backups — design

**Status:** design, awaiting review
**Date:** 2026-07-22

## Goal

Give globalhawk an off-site, encrypted, deduplicated backup of the
*irreplaceable* subset of `/data/Media` — photos, ebooks, audiobooks,
documents, and music — to Backblaze B2. Bulk regenerable/re-downloadable media
(tv, movies, anime, torrents, transcode caches) is explicitly out of scope to
keep stored size and cost down.

Concretely: replace the half-finished, never-activated custom
`services.restic-b2` module with a working backup built on the upstream NixOS
`services.restic.backups` module, and take the first successful backup.

## Current state (what already exists)

- **`machine/globalhawk/backup.nix`** — a custom `services.restic-b2` module.
  It declares options but the entire implementation is commented out, and the
  file is *not imported* (`./backup.nix` is commented in `default.nix`). It has
  never run. **This file will be deleted.**
- **`secrets/globalhawk.nix`** (git-crypt) — already contains working B2
  credentials: an S3-compatible B2 repo URL (`secrets.restic.b2.repo`), an
  application key pair, and a restic repository password. Verified during
  design: the bucket exists and auth succeeds, but **the restic repo was never
  initialized** — nothing has ever been backed up.
- `/data/Media` is a ZFS dataset (`pool/media`) on a 12.7 TB pool.

## Decisions locked in during brainstorming

- **Service: Backblaze B2.** Cheapest mainstream option at this scale
  (~$6/TB/mo storage); already provisioned. Even with music the bill is under
  ~$5/yr.
- **Tool: restic**, via the upstream **`services.restic.backups`** NixOS module.
  Borg was ruled out (no native object-storage backend — it needs a Borg
  process over SSH, which B2 can't provide). Kopia is a strong engine but has
  no first-class NixOS module; restic's `services.restic.backups` fits the
  declarative dotfiles model with far less to maintain, and restic's historical
  gap vs kopia (no compression) closed in 0.14 with zstd. Plain `rclone sync`
  is not a backup tool (no history/snapshots; propagates deletion/corruption).
- **Music is included.** ~23 GB, adds ~$0.14/mo. Trivial.
- **B2 key will be scoped.** The credential currently in secrets is a
  full-account master key. Before finishing, a new **bucket-restricted**
  application key (limited to the backup bucket) is created in the B2 console
  and swapped into `secrets/globalhawk.nix`.

## What gets backed up

Direct paths under `/data/Media`:

| Path | ~Size | Notes |
|---|---|---|
| `immich/photos` | ~19 G | Originals (`upload/`): ~10.4 G / 4257 photos + ~7.9 G / 260 videos, across two Immich users |
| Immich DB dump (staged) | ~28 M | Newest Postgres dump only (see below) |
| `photos` | 1.6 G | Legacy standalone photo dump |
| `books` | 7.3 G | ebooks |
| `old_books` | 0.6 G | ebooks |
| `audiobooks` | 9.1 G | |
| `documents` | small | 0700-restricted to `abe`; backup runs as root so it is readable |
| `music` | 23 G | |

**Total ~61 GB** before restic dedup/compression (jpg/flac/epub compress little,
so stored size ≈ raw).

Excludes under `immich/photos` (regenerable — Immich rebuilds these from
originals, or handled separately):

```
/data/Media/immich/photos/thumbs          # ~2.3 G — regenerable cache
/data/Media/immich/photos/encoded-video   # ~1.3 G — regenerable transcodes
/data/Media/immich/photos/backups         # dated DB dumps — staged separately, see below
```

Rationale for backing up `immich/photos` as a whole and excluding subdirs
(rather than listing `upload/`): it captures `upload/`, `library/`, and
`profile/` automatically and stays correct if Immich adds new originals subdirs,
while dropping the regenerable caches and the dump backlog. A full Immich
restore needs the originals **and** a DB dump; both are covered (originals here,
DB dump staged below).

### Immich Postgres dump: newest only

The `immich_db_dumper` container writes a **full, standalone** `pg_dump`
(`immich-db-backup-*.sql.gz`, ~28 M) into `immich/photos/backups/` on a
schedule and self-rotates. The dumps are *not* incremental, so the newest one
alone is a complete restore point. Rather than dragging the ~13-dump backlog
(~383 M) into the repo, a `backupPrepareCommand` copies just the latest dump to
a stable path that restic includes:

```nix
backupPrepareCommand = ''
  mkdir -p /var/lib/restic-media
  latest=$(ls -t /data/Media/immich/photos/backups/immich-db-backup-*.sql.gz 2>/dev/null | head -1)
  if [ -n "$latest" ]; then cp -f "$latest" /var/lib/restic-media/immich-db-latest.sql.gz; fi
'';
```

`paths` includes `/var/lib/restic-media/immich-db-latest.sql.gz`; the dated
originals are excluded (above). restic keeps its own version history of this one
file, bounded by the retention prune.

## Consistency approach: direct path backup (no ZFS snapshot)

The old design snapshotted `pool/media` before each run and backed up from
`.zfs/snapshot/…`. We deliberately **do not** do this:

- The data is effectively write-once media; files are not rewritten in place
  mid-walk.
- The one mutable-during-backup artifact, the Immich Postgres dump, is written
  by the `immich_db_dumper` container as a complete file (atomic rename), so a
  live read is consistent.
- Snapshot paths (`/data/Media/.zfs/snapshot/restic_YYYY-MM-DD/…`) are ugly,
  change daily, and require the `dynamicFilesFrom` / snapshot-rollover machinery
  the old module carried.
- restic already provides point-in-time history via its own snapshots.

(The one `backupPrepareCommand` we *do* keep — staging the newest Immich DB
dump — is a trivial file copy, not the ZFS snapshot lifecycle. It reads the
already-consistent dump file the container produced; it does not need a
filesystem snapshot.)

If a future dataset with genuine in-place mutation is added to the backup set,
revisit this.

## Module design

A new `machine/globalhawk/backup.nix` (replacing the deleted custom module),
imported from `default.nix`. It configures a single upstream backup:

```nix
services.restic.backups.media = {
  initialize = true;                       # create the repo on first run
  repository = secrets.restic.b2.repo;     # S3-compatible B2 endpoint + bucket
  passwordFile = "/etc/restic/media-password";
  environmentFile = "/etc/restic/media-env";
  paths = [ …the table above, incl. the staged DB dump… ];
  exclude = [ …thumbs, encoded-video, backups/… ];
  backupPrepareCommand = ''…stage newest DB dump (see above)…'';
  timerConfig = {
    OnCalendar = "*-*-* 03:30:00";          # after Immich's built-in 02:00 dump
    RandomizedDelaySec = "30m";
    Persistent = true;                     # catch up if the box was off
  };
  pruneOpts = [
    "--keep-daily 7"
    "--keep-weekly 5"
    "--keep-monthly 12"
  ];
};
```

Retention is generous because restic dedup makes historical snapshots nearly
free at this data size. `Persistent = true` runs a missed backup on next boot.
The 03:30 schedule sits after Immich's built-in DB backup (default daily 02:00,
keeps last 14), so the dump `backupPrepareCommand` stages is same-day fresh.

## Interaction with the planned Immich 1.124.2 → 2.7.0 (NixOS) upgrade

This backup is being set up partly as insurance for an upcoming Immich upgrade:
from the pinned Docker `v1.124.2` to `2.7.0` via the nixpkgs `services.immich`
module. Findings that shape the design (verified against release notes + by
inspecting an actual dump):

- **The current DB dumps are pgvecto.rs `pg_dumpall` cluster dumps.** Confirmed:
  the newest dump's header is `-- PostgreSQL database cluster dump` and it
  contains `CREATE EXTENSION IF NOT EXISTS vectors WITH SCHEMA vectors`. It can
  only be restored into a Postgres that has the `vectors` (pgvecto.rs)
  extension.
- **The nixpkgs `services.immich` module ships VectorChord + pgvector only** (no
  pgvecto.rs). Immich itself keeps pgvecto.rs support until v3; the engine
  break is the *NixOS module*, not version 2.7.0. So a dump taken now will
  **fail to restore directly into the NixOS instance** (on `CREATE EXTENSION
  vectors`).
- **Consequence for restore strategy:** the **originals** (`upload/` +
  `library/` + `profile/`) are the authoritative, engine-independent restore
  path — re-importable into any fresh Immich. The staged DB dump is a
  *same-engine* restore point: valuable **now** as pre-upgrade rollback
  insurance (restore into the current Docker/pgvecto.rs stack), and best-effort
  across the migration. `profile/` (avatars) is small but **not** regenerable —
  it stays included.

Backup **mechanics are unchanged** by the upgrade: the dump location
(`UPLOAD_LOCATION/backups`) and filename (`immich-db-backup-<epoch>.sql.gz`) are
stable across 1.124.2 → 2.7.0, so the `backupPrepareCommand` glob keeps working;
the include/exclude split is still correct for 2.7.0.

Guidance recorded for the (separate) upgrade work — **not** part of this backup
change:

1. **Run this backup to completion before touching Immich**, so a clean copy of
   originals plus a pre-upgrade pgvecto.rs dump exists as rollback insurance.
2. Then either: (a) migrate pgvecto.rs → VectorChord *on Docker first* (per
   Immich's `postgres-standalone` doc), take a fresh VectorChord dump, then cut
   over to NixOS and restore that dump; or (b) re-import originals into a fresh
   2.7.0 (loses DB-only state: albums, shared links, memories, face groupings,
   edited metadata).
3. When configuring the NixOS module, pin
   `services.immich.mediaLocation = "/data/Media/immich/photos"` so the on-disk
   layout — and therefore these backup paths — do not move.

### Secrets materialization

The restic password and B2 keys live in the git-crypt'd
`secrets/globalhawk.nix`. Two `environment.etc` entries render them to root-only
files the module reads:

- `/etc/restic/media-password` (mode 0600) — `secrets.restic.b2.restic_repo_pass`
- `/etc/restic/media-env` (mode 0600) — the B2 S3 credentials as env:
  ```
  AWS_ACCESS_KEY_ID=<key_id>
  AWS_SECRET_ACCESS_KEY=<app_key>
  ```

The module's own systemd unit runs as root, so no dedicated `_restic` user is
needed (dropping that complexity from the old design). The paths include
`documents` (0700 `abe`), which only root can read — consistent with running
as root.

### Failure alerting

restic's generated unit is `restic-backups-media.service`. Add a drop-in /
companion so a failed run is loud rather than silent, reusing the existing
msmtp path already wired for ZED and smartd:

```nix
systemd.services.restic-backups-media.onFailure = [ "restic-media-failure.service" ];
systemd.services.restic-media-failure = {
  serviceConfig.Type = "oneshot";
  script = ''printf 'Subject: [globalhawk] restic backup FAILED\n\nSee: journalctl -u restic-backups-media\n' | ${pkgs.msmtp}/bin/msmtp root'';
};
```

(Exact wiring validated at implementation; the intent is one email to `root`,
which `/etc/aliases` forwards to the user's gmail.)

## Sequencing / rollout

1. **Scope the B2 key** (manual, user): create an application key in the B2
   console restricted to the backup bucket; update
   `secrets/globalhawk.nix` `restic.b2.key_id` / `app_key` with it.
2. **Delete** the custom `services.restic-b2` module; **write** the new
   `backup.nix`; **import** it in `default.nix`.
3. **`nixos-rebuild build`** to validate evaluation (no `switch` without
   explicit go-ahead).
4. **Activate** (user-approved `switch`), then trigger the first run manually:
   `systemctl start restic-backups-media.service` and watch
   `journalctl -fu restic-backups-media`. First run initializes the repo and
   uploads ~61 GB (bounded by upstream bandwidth — expect hours).
5. **Verify restore** (see below) before trusting it.

## Verification

The backup is only real if a restore works:

```sh
# repo now exists and has a snapshot
restic snapshots
# integrity of structure + a sampled fraction of pack data
restic check --read-data-subset=5%
# actual restore of a known file to a scratch dir, byte-compare
restic restore latest --target /tmp/restore-test --include /data/Media/documents
diff -r /data/Media/documents /tmp/restore-test/data/Media/documents
```

Plus a NixOS-level check that the timer is installed and scheduled:

```sh
systemctl list-timers restic-backups-media.timer
```

## Non-goals

- Not backing up bulk media: `tv`, `movies`, `anime`, `torrents`,
  `docker-services` app state, `apps`.
- Not implementing ZFS-snapshot-based consistency (see rationale above).
- Not building local/second-destination redundancy in this pass (B2 only).
- Not migrating other hosts (nighthawk, campbell) — globalhawk only.
- Not rotating the restic *repository password* (only the B2 API key is
  re-scoped).
- **Not performing the Immich 1.124.2 → 2.7.0 / NixOS migration itself.** That
  is separate work; this spec only records how it interacts with the backup and
  the recommended ordering (back up first).
