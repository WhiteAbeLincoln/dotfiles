# globalhawk Immich → k3s — fresh instance, re-upload originals

**Status:** Designed (2026-07-24); not yet implemented.
**Date:** 2026-07-24
**Depends on:** the k3s substrate (`2026-07-22-globalhawk-service-arch-design.md`), the
sops-nix secret mechanism (`2026-07-23-globalhawk-secrets-sops-design.md`), and the
wildcard TLS + AdGuard/mDNS ingress plumbing — all delivered. Completes the last
`oci-containers` migration tracked in `2026-07-22-globalhawk-k3s-migration.md`.

## Goal

Move Immich off `oci-containers` (`services.immich-custom`, currently pinned to the
long-stale `v1.124.2` on the retired `pgvecto-rs` Postgres) and onto the k3s/nixidy
pipeline at the current stable release (**v3.0.0**), authored in Nix like every other
workload. Land Immich fully up to date in one step, and structure the manifests so
future version bumps are a **one-line change**.

The operator's hard constraint: **do not lose the original images.** Everything else in
the old install (albums, people/faces, favorites, shared links, per-asset edits) is
explicitly disposable.

## Decisions locked in (brainstorming 2026-07-24)

- **Fresh database — no data migration.** The old DB is discarded, not migrated. This is
  the pivotal decision: it sidesteps the genuinely delicate `pgvecto-rs → VectorChord`
  vector-extension migration that any in-place catch-up from v1.124.2 would force. A
  fresh v3.0.0 DB simply initializes on the current extension. **Cost, accepted:** albums,
  people/face names, favorites, shared links, and manual edits/descriptions are lost.
  Regenerable data (thumbnails, transcodes, CLIP embeddings, face detection) is rebuilt
  by Immich's background jobs after import.
- **Re-import via the Immich CLI, as first-class managed assets** — *not* an external
  library. The operator wants the originals to be normal, fully-managed Immich assets
  (deletable/editable in-app, storage-template applies, checksum de-dup). The on-disk
  originals are 19 GB; the media pool has 7.4 TB free, so the transient second copy the
  CLI creates is a non-issue. The old tree is deleted only after the operator verifies
  the import.
- **Preserve the two-account split.** The old upload tree has two user UUIDs
  (`a7b1…` ≈ 8.6 GB, `4fac…` ≈ 9.9 GB). Two fresh accounts are created and the CLI is run
  once per account against that account's subfolder, under that account's API key, so
  each person's photos land under their own login.
- **Machine learning included (CPU now).** Runs on CPU on the 16 GB box, memory-limited.
  GPU acceleration is a later, additive change (swap the ML image tag + add a device
  mount); explicitly out of scope here.
- **Mirror the official Helm chart's structure for upgradeability.** We do not run the
  chart (it doesn't compose with our nixidy → `services.k3s.manifests` pipeline), but the
  manifest shape, env vars, and image set track
  `github.com/immich-app/immich-charts` and the release `docker-compose.yml`. Server and
  ML share **one version knob**, exactly as the chart does. See *Upgrade process*.
- **New `immich` namespace** with its own default-deny-ingress NetworkPolicy (allow
  intra-namespace + Traefik), mirroring `media`/`library`.
- **Ingress host:** `photos.h.abrahamwhite.com` (wildcard cert already covers it; mDNS
  alias auto-derived).
- **Storage isolation via POSIX ownership, not shared uid.** This migration is also the
  first step of tightening media-file access: today every workload runs as the single
  `_media` uid (994) and the tree is world-readable (`0755`), so any app can read any
  other's files — and radarr/sonarr in particular bind-mount the *entire* `/data/Media`
  root (for hardlinks), so they can read photos. Immich instead gets its **own** service
  uid (`immich`, 993); its data tree is `0750 immich:immich`, with a new `media-readers`
  group (`abe` + `agent`) granted read via a default ACL. arr is then denied photos by the
  kernel (it's "other" on a `0750` dir it doesn't own) **without any change to the arr
  stack** — see *Isolation & permissions*. Fuller mount-level isolation for arr is
  deferred (*Deferred future work*).

## Target architecture

Four workloads in a new `immich` namespace (`k8s/apps/immich.nix`, plus
`k8s/infra/immich-network.nix` for the NetworkPolicy). All images digest-pinned per repo
convention. Values below are quoted verbatim from the v3.0.0 release
`docker/docker-compose.yml`.

All Immich data lives under one isolated tree, **`${mediaRoot}/immich/`** (`library/`,
`pgdata/`, `model-cache/`), owned `immich:immich` (uid 993) mode `0750` — one owner, one
ACL, one thing to reason about. All pods run as uid 993 (`runAsUser`/`runAsGroup`/`fsGroup
= 993`).

### immich-server — `k8s/apps/immich.nix`

- Image: `ghcr.io/immich-app/immich-server:v3.0.0@sha256:…` (pin the release digest at
  authoring time).
- Port `2283` (`IMMICH_PORT` default). Service + Ingress via `l.mkService`/`l.mkIngress`,
  host `photos${ingressSuffix}`.
- **Storage:** the fresh managed store `${mediaRoot}/immich/library` (hostPath, `Directory`)
  mounted at **`/data`** — the modern mount point (`IMMICH_MEDIA_LOCATION` defaults to
  `/data`; do **not** set it). This replaces the old `${uploadDir}:/usr/src/app/upload`
  mount, which no longer applies.
- Runs as `immich` (993): `runAsUser`/`runAsGroup`/`fsGroup = 993`, so files the server
  writes are `immich`-owned and readable by `abe`/`agent` through the inherited
  `media-readers` default ACL. **Verify at implementation** that the server needs no root
  operation on `/data`; if it insists, fall back to root + `fsGroup = 993`, but Immich's
  own image supports an arbitrary UID.
- Env: `DB_HOSTNAME=immich-postgres`, `DB_USERNAME=postgres`, `DB_DATABASE_NAME=immich`,
  `DB_PASSWORD` (from the `immich-db` Secret via `secretKeyRef`), `REDIS_HOSTNAME=immich-redis`,
  `IMMICH_MACHINE_LEARNING_URL=http://immich-machine-learning:3003`, `TZ=<timezone>`.
- Probes: HTTP `GET /api/server/ping` on 2283 (readiness + liveness).

### immich-machine-learning

- Image: `ghcr.io/immich-app/immich-machine-learning:v3.0.0@sha256:…` — **same tag as the
  server** (the one version knob).
- Port `3003`.
- Model cache: hostPath `${mediaRoot}/immich/model-cache` mounted at `/cache`.
  Env: `TRANSFORMERS_CACHE=/cache`, `HF_XET_CACHE=/cache/huggingface-xet`,
  `MPLCONFIGDIR=/cache/matplotlib-config`.
- Memory is bursty, not constant: near-idle (a few hundred MB) between jobs, climbing as
  CLIP/face models load on demand during work, then dropping back after
  `MACHINE_LEARNING_MODEL_TTL` (default 300 s) unloads idle models. The one sustained
  spell is the initial import backlog (thousands of assets encoded at once); steady-state
  after that is occasional (new uploads only).
- `resources.requests.memory` **low** (~512 Mi–1 Gi) so the scheduler only reserves the
  idle baseline, not the ceiling; `resources.limits.memory` ~3 Gi as a hard cap to protect
  the 16 GB box from a runaway spike (it's a limit, not a reservation — actual RSS is what's
  used). Tune after observing the first CLIP passes. The workload is idempotently
  disable-able (drop the deployment + `IMMICH_MACHINE_LEARNING_URL`) if RAM proves tight —
  search/faces degrade gracefully.

### immich-postgres

- Image: `ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:bcf63357191b76a916ae5eb93464d65c07511da41e3bf7a8416db519b40b1c23`
  (Immich's official DB image; bundles VectorChord + pgvector). Fresh DB ⇒ no extension
  migration; `DB_VECTOR_EXTENSION` auto-detects VectorChord.
- Data: hostPath `${mediaRoot}/immich/pgdata` mounted at `/var/lib/postgresql/data`.
  Runs as uid 993 (`runAsUser`/`fsGroup = 993`); **verify** the Immich Postgres image
  initialises a fresh `PGDATA` as an arbitrary (non-root, non-`postgres`) uid — the
  official `postgres` base supports this when the data dir is owned by the running uid
  (which `fsGroup` + the 993-owned hostPath ensure). If it refuses, run Postgres as its
  default user and set the `pgdata/` subdir ownership to match.
- Env: `POSTGRES_PASSWORD` (from Secret), `POSTGRES_USER=postgres`, `POSTGRES_DB=immich`,
  `POSTGRES_INITDB_ARGS=--data-checksums`.
- `/dev/shm`: mount an `emptyDir{medium=Memory, sizeLimit=128Mi}` at `/dev/shm` (the
  compose `shm_size: 128mb`; k8s defaults to 64Mi, which Postgres can exhaust).
- `strategy.type = Recreate` — never two Postgres pods on the same data dir.
- Probe: `pg_isready -U postgres` exec.

### immich-redis (Valkey)

- Image: `docker.io/valkey/valkey:9@sha256:8e8d64b405ce18f41b8e5ee20aa4687a8ed0022d1298f2ce31cdcf3a76e09411`.
- Port `6379`. Storage: `emptyDir` — it's a job queue/cache; losing it on restart is safe.
- Probe: `redis-cli ping` exec.

### Secret

New sops key `immich_db_password`, rendered as a k8s Secret **`immich-db`** in the
`immich` namespace by `machine/globalhawk/sops.nix` (the same
`sops.templates.<file>.path → /var/lib/rancher/k3s/server/manifests/…` mechanism as
`sops-mullvad-wg.yaml`). Both Postgres (`POSTGRES_PASSWORD`) and the server (`DB_PASSWORD`)
reference it. Fresh DB ⇒ we set a **new** password and retire the old `immich_pass` key.

### Namespace network — `k8s/infra/immich-network.nix`

Default-deny-ingress NetworkPolicy for the `immich` ns, allowing intra-namespace traffic
(server↔postgres↔redis↔ML) and Traefik→server:2283, modeled on `library-network.nix`.

## Isolation & permissions

The goal: **only Immich (and `abe`/`agent` read-only) can reach the photo tree; no other
workload can — including arr, which bind-mounts all of `/data/Media`.** Achieved with host
POSIX ownership + a reader ACL; no arr change, no hardlink risk.

Declared in the NixOS layer (`machine/globalhawk/default.nix`, or a small dedicated
module):

- **`immich` service user + group, uid/gid 993** (993 is free; 994 is `_media`). System
  user, no login. Immich's pods run as it.
- **`media-readers` group** with members `abe` + `agent`. This is the reusable human-read
  handle for tightened per-app trees; `abe` already has `_media` (write) elsewhere, and
  `agent` (the read-only sandbox, uid 1001) is deliberately kept out of `_media` — it only
  ever gets read, via this group.
- **Ownership/mode + ACL on `${mediaRoot}/immich`** via `systemd.tmpfiles` (the same
  mechanism the ebook stack uses):
  - `d ${mediaRoot}/immich 0750 immich immich` (and `library/`, `pgdata/`, `model-cache/`).
  - `A+` ACL granting `media-readers` read, **access + default** so files Immich creates
    inherit it: effectively `g:media-readers:rX` + `d:g:media-readers:rX` (capital `X` =
    traverse dirs, read files). Use `A+` (append), not `A`, per the ebook-stack lesson
    about overlapping tmpfiles ACLs.

**Why this denies arr without touching it.** arr's containers run as `_media` (994). The
dir `${mediaRoot}/immich` is `0750 immich:immich`; 994 is neither owner nor in the `immich`
group nor in `media-readers`, so it falls to "other" = `---` and the kernel refuses even to
traverse into it. arr never scans `immich/` anyway, so nothing breaks, and its single
`/data/Media` mount (hence hardlinks/atomic-moves) is untouched. The old originals subtree
(`${mediaRoot}/immich/photos`, still `_media`-owned `0755` until cleanup) is likewise
shielded, because arr can't traverse the now-`0750` parent to reach it.

**Human read still works.** `abe`/`agent` are in `media-readers` → the ACL grants `r-x`;
they browse the tree normally. `restic` runs as root, so backups read everything regardless
of the `0750` mode, and it preserves + restores POSIX ACLs and ownership.

**The dedicated uid is load-bearing** — it's precisely what lets POSIX distinguish Immich
from the other media apps. Keeping Immich on `_media` (994) would leave photos readable by
every 994 workload; that's the status quo we're fixing.

## Cutover — atomic (operator executes; agent authors + `build`-validates)

Both installs would contend for the same on-disk tree and the same logical service, so
this is an atomic cutover per the arr/ebook precedent (`globalhawk-arr-atomic-cutover`).
The agent (sandbox, uid 1001, read-only) authors the Nix and validates with
`nixos-rebuild build`; the operator runs every `switch`, `kubectl`, `docker`, and CLI step.

1. **(agent)** Author `k8s/apps/immich.nix`, `k8s/infra/immich-network.nix`, the sops
   Secret, the `immich` user + `media-readers` group + tmpfiles ownership/ACL rules, and
   the firewall/backup edits; `nixos-rebuild build --flake .#globalhawk` +
   `nix run .#k3s-drift` clean.
2. **(operator)** Set `services.immich-custom.enable = false`; `switch`. Docker Immich
   (server, ML, redis, postgres, db-dumper) stops and its containers are removed. The old
   originals at `${mediaRoot}/immich/photos/upload` are **not touched** — they stay on disk.
3. **(operator)** `switch` in the k3s stack. Activation creates the `immich` user +
   `media-readers` group and the tmpfiles rules set `${mediaRoot}/immich` to
   `0750 immich:immich` with the reader ACL (immediately shielding the still-present old
   originals from arr). Immich boots **empty**: fresh DB, empty `/data`. Confirm
   `photos.h.abrahamwhite.com` loads the onboarding screen.
4. **(operator)** Create the admin account + the second account in the web UI; mint an API
   key for each.
5. **(operator)** Run the Immich CLI once per account, each against that account's old
   subfolder, under that account's key:
   - `immich upload --recursive ${mediaRoot}/immich/photos/upload/a7b113fc-…`
   - `immich upload --recursive ${mediaRoot}/immich/photos/upload/4fac97df-…`
   Run the CLI via `nix run` or the `ghcr.io/immich-app/immich-cli` container (needs
   network to `:2283` and a read-only mount of the old tree). Checksum de-dup makes each
   run idempotent/resumable — a re-run after interruption skips already-uploaded assets.
6. **Verify (operator):** per-account asset counts are sane, timeline renders (EXIF dates
   reconstructed regardless of the hash filenames), ML jobs are progressing under
   Administration → Jobs. No crash-loops.
7. **Cleanup (only after the operator is satisfied):**
   - Delete the old tree `${mediaRoot}/immich/photos/**` (originals now duplicated into
     `/data`), reclaiming ~24 GB (19 GB originals + thumbs/encoded-video).
   - `docker volume rm` the orphaned `pgdata` + `model-cache`; `docker network rm
     immich-bridge`.
   - Agent PR: remove `program/immich/`, the `services.immich-custom` block +
     `import ../../program/immich`, the `config.services.immich-custom.port` firewall entry,
     and the old `immich_pass` sops key.

**Rollback:** until step 7, rollback is `services.immich-custom.enable = true` + remove the
k3s app + `switch`. The old originals and old DB dump are still on disk, so the pre-cutover
install comes back intact (minus anything uploaded to the new instance in between).

## Backups (`machine/globalhawk/backup.nix`)

Today the module keys off `config.services.immich-custom.uploadDir` and stages the old
container's DB dump. Rework:

- **DB dump:** enable Immich's built-in database backup (Administration → Settings →
  Backup), which writes rotating dumps to `/data/backups` (i.e.
  `${mediaRoot}/immich/library/backups`). Keep the existing "stage only the newest dump"
  `backupPrepareCommand`, repointed at the new path.
- **restic paths:** replace `immichRoot` (old `uploadDir`) with the new managed store
  `${mediaRoot}/immich/library`; keep excludes for `library/thumbs`, `library/encoded-video`,
  and the `library/backups` backlog (all regenerable / staged-separately). The
  `${mediaRoot}/photos` entry (a separate, non-Immich path) is untouched.
- Source the new path from `facts.nix` (add `immichLibrary` or reuse a literal) so
  `backup.nix` no longer depends on the deleted `immich-custom` option.

## Upgrade process (the "painless" payoff)

Routine app upgrade = bump **one** `version` binding at the top of `k8s/apps/immich.nix`
(drives both server + ML), refresh the two digests, `nixos-rebuild build`, then a quick
operator `switch`. Because the DB is now on VectorChord (the current line), version bumps
no longer cross the extension boundary.

The Postgres and Valkey images are pinned **separately** and change rarely. When they do,
Immich's release notes call it out. The tracking discipline: on each upgrade, diff our
manifest against that release's `docker/docker-compose.yml` (and the chart's `values.yaml`)
for **new required env vars or an image bump**, and for any breaking DB-image change follow
Immich's documented step. This keeps us aligned with the official reference without running
the chart.

## Risks / verify items

- **Alpine DNS on k8s.** Immich images are Alpine; Immich documents a resolution bug when
  host nodes carry a DNS *search domain*. globalhawk's host `resolv.conf` has none, so risk
  is low, but if the server/ML can't resolve peers, add a pod `dnsConfig` with
  `options ndots:1` (or use FQDN service names). Verify at bring-up.
- **Run-as-993 vs root** on `/data` (see server section) — verify no chown/permission
  errors in the server log; fall back to root+`fsGroup = 993` if needed.
- **Postgres as uid 993** — verify the Immich Postgres image initialises `PGDATA` as a
  non-`postgres` uid (see immich-postgres section); this is the most likely permission
  snag in the stack.
- **ACL correctness** — after `switch`, confirm `abe` and `agent` can read
  `${mediaRoot}/immich` and a file Immich creates under `library/`, while a process as
  `_media` (994) is denied (`sudo -u <arr-uid> ls ${mediaRoot}/immich` → permission
  denied). This is the behavioural test for the isolation, not a shape assertion.
- **ML memory** on a 16 GB box shared with Plex transcodes + Postgres — watch RSS during
  the first CLIP/face passes; tune the limit or disable ML if it causes pressure.
- **CLI account attribution** — uploading under the wrong API key assigns photos to the
  wrong person; the per-account subfolder mapping above must be followed exactly.

## Deferred future work

The permission model here is the first slice of a broader "each app sees only what it
needs" tightening. Explicitly **not** done now, but designed to compose with what this
migration establishes (the `media-readers` group + per-app uid pattern):

- **True mount-level isolation for arr.** Today radarr/sonarr bind-mount all of
  `/data/Media`; POSIX ownership denies them the tightened trees, but the broad mount
  remains. Fully removing non-arr dirs from their view requires nesting the arr dirs
  (`movies`/`tv`/`anime`/`torrents`) under a common parent (e.g. `${mediaRoot}/lib/`) so a
  single mount can exclude the rest — preserving hardlinks/atomic-moves (which break across
  separate mounts, `EXDEV`, even on one filesystem). The data moves are instant renames on
  the same dataset, but Plex library paths, arr root folders, and qBittorrent's save path
  all need re-pointing — a separate, verifiable change of its own.
- **Per-app uids for the rest of the stack.** Give qBittorrent, the arr apps, CWA, and
  Audiobookshelf their own uids (as Immich now has), each owning only its data, with
  `media-readers` for human read — retiring the shared-994 model entirely. Incremental,
  one app at a time, no data movement (just chown + securityContext).

## Out of scope

GPU/hardware ML + transcode acceleration (later, additive), SSO/forward-auth (parked
Authelia spec — Immich has native OIDC to wire in later), and any preservation of the old
albums/people/DB.
