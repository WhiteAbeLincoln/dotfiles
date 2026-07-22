# globalhawk Remote Backups Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give globalhawk a working, scheduled, encrypted off-site backup of the irreplaceable subset of `/data/Media` to Backblaze B2 via restic.

**Architecture:** Retire the never-activated custom `services.restic-b2` module and configure the upstream NixOS `services.restic.backups.media` job instead. It backs up Immich originals + selected media directories directly (no ZFS snapshot), stages only the newest Immich Postgres dump, prunes on a retention schedule, and emails root on failure.

**Tech Stack:** NixOS module system, `services.restic.backups` (restic 0.18), Backblaze B2 via restic's S3 backend, git-crypt for secrets, msmtp for failure mail.

**Design spec:** `docs/superpowers/specs/2026-07-22-globalhawk-remote-backups-design.md`

## Global Constraints

- **Public repo, encrypted secrets:** the repo is public; only `secrets/` is git-crypt'd. Never write a secret-derived value (B2 bucket name, S3 endpoint/region, key ID, app key, restic password) into any unencrypted file — plan, spec, `backup.nix`, comments. Reference the Nix attribute path (`secrets.restic.b2.*`) or read it at runtime.
- **Formatter:** all `.nix` edits must pass `nix fmt` (alejandra).
- **No `switch` without explicit user go-ahead.** `nixos-rebuild build` is the eval gate; activation is a separate, user-approved step.
- **Do not remove human-written comments** unless the code they describe is deleted.
- **Immich media location is `/data/Media/immich/photos`** (`services.immich-custom.uploadDir`, `default.nix:221`). All Immich paths derive from it.

### Testing note (Nix reality)

This is a Nix configuration change, not application code — there is no unit-test framework. The equivalent of "the test" is:
1. **Eval gate:** `nixos-rebuild build --flake .#globalhawk` succeeds (catches evaluation/type errors).
2. **Functional gate (post-activation):** `restic snapshots` / `restic check` / a real restore-and-diff.

Each task states its gate explicitly.

---

### Task 1: Swap in the bucket-scoped B2 key

**Files:**
- Modify: `secrets/globalhawk.nix` (git-crypt'd) — `restic.b2.key_id` and `restic.b2.app_key`

**Interfaces:**
- Consumes: a new B2 application key **restricted to the backup bucket**, created by the user (keyID + applicationKey). See the design spec's "Sequencing" step 1 / the chat instructions for creating it.
- Produces: `secrets.restic.b2.key_id` / `secrets.restic.b2.app_key` now hold the scoped key; `secrets.restic.b2.repo` and `secrets.restic.b2.restic_repo_pass` are unchanged.

**Precondition:** git-crypt is unlocked (`nix run .#decrypt-secrets` on a fresh checkout). Confirm with `git-crypt status secrets/globalhawk.nix` showing decrypted content readable.

- [ ] **Step 1: Replace the two key fields**

Edit `secrets/globalhawk.nix`, replacing only these two values inside the `restic.b2` attrset with the new scoped key (leave `pass` and `repo` alone):

```nix
    key_id = "<new scoped keyID>";
    app_key = "<new scoped applicationKey>";
```

- [ ] **Step 2: Verify the new key authenticates to the repo**

The repo is not initialized yet, so the *expected success signal* is restic reaching B2 and reporting the repo is absent — NOT an auth error. Export the repo/creds from secrets (without printing them), then call restic:

```bash
cd /srv/dotfiles
export AWS_ACCESS_KEY_ID="$(nix eval --raw --file secrets/globalhawk.nix restic.b2.key_id)"
export AWS_SECRET_ACCESS_KEY="$(nix eval --raw --file secrets/globalhawk.nix restic.b2.app_key)"
export RESTIC_REPOSITORY="$(nix eval --raw --file secrets/globalhawk.nix restic.b2.repo)"
export RESTIC_PASSWORD="$(nix eval --raw --file secrets/globalhawk.nix restic.b2.restic_repo_pass)"
nix run nixpkgs#restic -- cat config; echo "exit=$?"
unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY RESTIC_REPOSITORY RESTIC_PASSWORD
```

Expected: `Fatal: repository does not exist: ... The specified key does not exist.` (auth succeeded, repo simply not created yet). 
**Fail signal:** a `403`/`Access Denied`/`unauthorized` message → the key lacks capabilities or is scoped to the wrong bucket; recreate it with `listBuckets,listFiles,readFiles,writeFiles,deleteFiles` on the correct bucket.

- [ ] **Step 3: Commit the (encrypted) secrets change**

git-crypt stores ciphertext in git; committing is safe.

```bash
cd /srv/dotfiles
git add secrets/globalhawk.nix
git commit -m "feat(globalhawk): scope the restic B2 key to the backup bucket

Replace the full-account master key with an application key restricted to
the backup bucket, so a leak of globalhawk's on-disk credentials cannot
touch other B2 buckets."
```

Verify the blob is encrypted, not plaintext:

```bash
git show HEAD:secrets/globalhawk.nix | head -c 32 | xxd | head -1
```
Expected: binary/`GITCRYPT` header bytes, not readable Nix.

---

### Task 2: Replace the custom module with the upstream restic backup

**Files:**
- Modify (full rewrite): `machine/globalhawk/backup.nix`
- Modify: `machine/globalhawk/default.nix:42` (uncomment import), `default.nix:213-215` (delete dead commented block)
- Reference: `machine/globalhawk/disks.nix:35-53` (existing msmtp/aliases setup the failure mail reuses)

**Interfaces:**
- Consumes: `secrets.restic.b2.{repo,pass,key_id,app_key}` (from Task 1); `services.immich-custom.uploadDir` layout (`/data/Media/immich/photos`).
- Produces: a systemd service `restic-backups-media.service` + timer `restic-backups-media.timer`; a staged dump at `/var/lib/restic-media/immich-db-latest.sql.gz`.

- [ ] **Step 1: Rewrite `machine/globalhawk/backup.nix`**

Replace the entire file contents (the custom `services.restic-b2` module — options block + commented implementation — is deleted) with:

```nix
{
  lib,
  pkgs,
  config,
  ...
}: let
  secrets = import ../../secrets/globalhawk.nix;
  # Immich's UPLOAD_LOCATION; keep in sync with services.immich-custom.uploadDir
  # in default.nix (and services.immich.mediaLocation after the NixOS migration).
  immichRoot = "/data/Media/immich/photos";
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
      "/data/Media/photos"
      "/data/Media/books"
      "/data/Media/old_books"
      "/data/Media/audiobooks"
      "/data/Media/documents"
      "/data/Media/music"
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
```

- [ ] **Step 2: Enable the module and remove the dead block in `default.nix`**

Uncomment the import at `default.nix:42`:

```nix
    ./backup.nix
```

Delete the dead commented block at `default.nix:213-215`:

```nix
  # services.restic-b2 = {
  #   enable = true;
  # };
```

- [ ] **Step 3: Format**

```bash
cd /srv/dotfiles
nix fmt
```
Expected: exits 0; `git diff --stat` shows only the two touched files (plus any formatting).

- [ ] **Step 4: Eval gate — build the config**

```bash
cd /srv/dotfiles
nixos-rebuild build --flake .#globalhawk
```
Expected: completes with a `./result` symlink, no evaluation errors. This proves the module, secrets wiring, and both systemd units type-check.

- [ ] **Step 5: Confirm the units and schedule made it into the build**

```bash
cd /srv/dotfiles
ls result/etc/systemd/system/ | grep -E 'restic-(backups-media|media-failure)'
grep -h 'OnCalendar' result/etc/systemd/system/restic-backups-media.timer
```
Expected: both `restic-backups-media.service` and `restic-media-failure.service` present, a `restic-backups-media.timer` exists, and `OnCalendar=*-*-* 03:30:00`.

- [ ] **Step 6: Commit**

```bash
cd /srv/dotfiles
git add machine/globalhawk/backup.nix machine/globalhawk/default.nix
git commit -m "feat(globalhawk): off-site restic backup of media to B2

The media pool had no off-site copy; a disk/pool loss or a botched Immich
upgrade would take the photo library with it. Replace the abandoned custom
restic-b2 module with the upstream services.restic.backups job, backing up
Immich originals + ebooks/audiobooks/documents/music (excluding regenerable
Immich caches) and staging the newest Immich DB dump."
```

---

### Task 3: Activate, take the first backup, and verify a restore

**Files:** none (runtime activation + verification only).

**Interfaces:**
- Consumes: the committed config from Task 2 and the scoped key from Task 1.
- Produces: an initialized restic repo in B2 with at least one snapshot; a proven restore.

**Precondition:** explicit user go-ahead to `switch` (this is the outward-facing, hard-to-reverse step). Do not run Step 1 without it.

- [ ] **Step 1: Activate (user-approved)**

```bash
cd /srv/dotfiles
sudo nixos-rebuild switch --flake .#globalhawk
```
Expected: activation succeeds; `systemctl status restic-backups-media.timer` shows the timer active/waiting.

- [ ] **Step 2: Trigger the first backup manually and watch it**

The first run initializes the repo and uploads ~61 GB — expect hours, bound by upstream bandwidth. Run it now rather than waiting for 03:30:

```bash
sudo systemctl start restic-backups-media.service &
journalctl -fu restic-backups-media
```
Expected: log shows repo initialization, the `backupPrepareCommand` staging the dump, then `Files: ... Added to the repository: ...`, ending `processed ... snapshot <id> saved`. Ctrl-C the journal follow once it reports the snapshot saved.

- [ ] **Step 3: Confirm the staged DB dump was captured**

```bash
sudo test -f /var/lib/restic-media/immich-db-latest.sql.gz && echo "staged dump present"
```
Expected: `staged dump present` (the prepare command ran).

- [ ] **Step 4: List snapshots**

```bash
cd /srv/dotfiles
sudo bash -c '
  set -a; . /etc/restic/media-env; set +a
  export RESTIC_PASSWORD_FILE=/etc/restic/media-password
  export RESTIC_REPOSITORY="$(nix eval --raw --file /srv/dotfiles/secrets/globalhawk.nix restic.b2.repo)"
  nix run nixpkgs#restic -- snapshots
'
```
Expected: at least one snapshot whose `Paths` include the Immich root, the staged dump, and the media dirs — and do NOT include `thumbs`/`encoded-video`/`backups`.

- [ ] **Step 5: Integrity check (structure + sampled data)**

```bash
cd /srv/dotfiles
sudo bash -c '
  set -a; . /etc/restic/media-env; set +a
  export RESTIC_PASSWORD_FILE=/etc/restic/media-password
  export RESTIC_REPOSITORY="$(nix eval --raw --file /srv/dotfiles/secrets/globalhawk.nix restic.b2.repo)"
  nix run nixpkgs#restic -- check --read-data-subset=5%
'
```
Expected: `no errors were found`.

- [ ] **Step 6: Real restore-and-diff (the backup is only real if it restores)**

Restore the small `documents` dir to a scratch target and compare byte-for-byte:

```bash
cd /srv/dotfiles
sudo bash -c '
  set -a; . /etc/restic/media-env; set +a
  export RESTIC_PASSWORD_FILE=/etc/restic/media-password
  export RESTIC_REPOSITORY="$(nix eval --raw --file /srv/dotfiles/secrets/globalhawk.nix restic.b2.repo)"
  rm -rf /tmp/restore-test
  nix run nixpkgs#restic -- restore latest --target /tmp/restore-test --include /data/Media/documents
'
sudo diff -r /data/Media/documents /tmp/restore-test/data/Media/documents && echo "RESTORE OK"
sudo rm -rf /tmp/restore-test
```
Expected: `RESTORE OK` with no diff output.

- [ ] **Step 7: Confirm the timer is scheduled for the nightly run**

```bash
systemctl list-timers restic-backups-media.timer
```
Expected: `NEXT` shows the upcoming 03:30 (±30m) run.

---

## Post-implementation notes (not tasks)

- **Immich upgrade ordering:** now that a clean pre-upgrade backup exists (originals + a pgvecto.rs DB dump), the separate Immich 1.124.2 → 2.7.0 / NixOS migration can proceed. Recall the DB dump is engine-specific (see spec §"Interaction with the planned Immich upgrade"): originals are the authoritative restore path.
- **Optional hardening:** a healthchecks.io-style dead-man's-switch (ping a URL on success; alert if pings stop) would catch the timer *silently not firing*, which the `OnFailure` mail cannot. Deferred by design decision.
- **Optional:** migrate restic secrets from `environment.etc` (Nix store) to agenix/sops to keep them out of the world-readable store.
