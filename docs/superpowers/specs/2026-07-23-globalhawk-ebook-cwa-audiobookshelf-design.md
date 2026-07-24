# globalhawk ebook + audiobook stack — CWA + Audiobookshelf on k3s

**Status:** Designed (2026-07-23); not yet implemented.
**Date:** 2026-07-23
**Depends on:** the k3s substrate (`2026-07-22-globalhawk-service-arch-design.md`) and
the sops-nix secret mechanism (`2026-07-23-globalhawk-secrets-sops-design.md`), both
delivered. **Does not depend on** the parked Authelia SSO spec — see Auth below.

## Goal

Replace the native `services.calibre-web` with a k3s-hosted ebook library that does
metadata automation, web upload, and OPDS, and add a dedicated audiobook manager —
both authored in Nix and delivered through the existing nixidy → `services.k3s.manifests`
pipeline. Do it without adding a database server to a 16 GB box that still has Immich
(Postgres) to absorb.

Two workloads, in a new `library` namespace:

- **Calibre-Web-Automated (CWA)** — EPUB/PDF library: automatic metadata + cover
  enforcement, a BookDrop ingest folder, web upload, OPDS, and native OIDC. Consumes the
  **existing** Calibre library at `${mediaRoot}/books` in place (same format, same
  `metadata.db`), so metadata carries over from the current calibre-web with no rebuild.
- **Audiobookshelf (ABS)** — audiobooks: strong audio metadata automation, playback,
  progress sync, native OIDC. Embedded SQLite (no server); no OPDS (accepted).

## Decisions locked in (brainstorming 2026-07-23)

- **App choice: CWA + Audiobookshelf**, *not* BookLore. BookLore was the initial pick
  (single app for books+comics), but it is **MariaDB-only** as of June 2026 (JDBC, no
  SQLite/Postgres driver deps — upstream issue #327) and cannot share Immich's Postgres.
  On a 16 GB box already committed to Immich's Postgres, BookLore would mean a **second
  database engine** (MariaDB) plus a heavier JVM runtime. CWA and ABS both use embedded
  SQLite, so — once Immich lands — **Immich's Postgres is the only database server on the
  box**, which is the consolidation the operator wanted.
- **Comics deferred.** The operator has a single CBR set and no near-term plan to grow it.
  Calibre (hence CWA) has no comic reader, so that set stays parked/unrendered in the
  library for now. **Komga** is the clean additive path later (its own SQLite pod, no
  shared state) if comics ever become a real collection — out of scope here.
- **Auth: local now, native OIDC later.** Authelia is parked. Both CWA and ABS ship with
  local accounts now; both have native OIDC, so registering them as Authelia OIDC clients
  later is purely additive (no rework). This work is therefore *not* blocked on the SSO
  spec.
- **Book data: reuse `${mediaRoot}/books` in place.** CWA points at the existing Calibre
  library (including `metadata.db`); no copy, no rebuild. This is stronger than the
  BookLore plan, which would have rebuilt metadata from scratch.
- **Namespace: new `library` namespace** with its own default-deny-ingress NetworkPolicy
  (allow intra-namespace + Traefik), isolated from the `media` (torrent/arr) stack.
- **calibre-web retirement: atomic cutover with instant rollback** (revised from the
  original "run in parallel" idea — see Cutover; CWA shares the library, so parallel
  writers are unsafe).
- **Friendly ingress hostnames:** `books.h.abrahamwhite.com`, `audiobooks.h.abrahamwhite.com`.

## Workloads

### Calibre-Web-Automated — `k8s/apps/calibre-web-automated.nix`

A LinuxServer-lineage image, so it fits the same container idiom as the arr apps
(root-start, drop to `PUID`/`PGID` = `mediaUid` (994) via s6; `fsGroup = 994`;
`Recreate` strategy because it holds SQLite locks). Authored via the new `mkLsioApp`
helper (see Refactor).

- **Image:** `crocodilestick/calibre-web-automated:latest` (pin a tag at implementation).
- **Port:** 8083 (WebUI + `/opds`).
- **Volumes (hostPath, no data copy):**
  - `${mediaRoot}/books` → `/calibre-library` (RW) — existing Calibre library +
    `metadata.db`, scanned in place.
  - `${mediaRoot}/apps/calibre-web-automated/config` → `/config` — CWA state (its
    `cwa.db`, settings, local users).
  - `${mediaRoot}/apps/calibre-web-automated/ingest` → `/cwa-book-ingest` — BookDrop
    auto-convert/auto-import folder.
- **Auth:** local accounts now; native OIDC (discovery-URL auto-config, JWT field +
  group→role mapping) wired to Authelia later.

### Audiobookshelf — `k8s/apps/audiobookshelf.nix`

Not a LinuxServer image: a Node app that runs as an arbitrary UID and takes a `PORT` env
rather than `PUID`/`PGID`. Authored as a hand-rolled single-container Deployment that
borrows `mkService`/`mkIngress`.

- **Image:** `ghcr.io/advplyr/audiobookshelf:latest` (pin a tag at implementation).
- **Port:** `PORT=13378`, `containerPort: 13378` (non-privileged so it runs fine as
  non-root).
- **securityContext:** `runAsUser` / `runAsGroup` / `fsGroup` = 994, so files it writes to
  the audiobook library stay `_media`-owned.
- **Volumes (hostPath):**
  - `${mediaRoot}/audiobooks` → `/audiobooks` (RW) — library (already exists + backed up).
  - `${mediaRoot}/apps/audiobookshelf/config` → `/config` — ABS SQLite DB, users, progress.
  - `${mediaRoot}/apps/audiobookshelf/metadata` → `/metadata` — cached covers, converted
    files.
- **No OPDS** (upstream gap): do not route e-reader feeds here. Web upload (drag-drop) +
  native OIDC (Authelia-documented) supported.

## LinuxServer abstraction / `k8s/lib.nix` refactor

The Service+Ingress triple is currently duplicated across the arr apps and qbittorrent,
and CWA/ABS would add two more copies. The LinuxServer container env block (`TZ`/`PUID`/
`PGID`, no `runAsUser`) is duplicated across the arr apps and qbittorrent. This refactor
factors both out, in three layers so qbittorrent's bespoke VPN pod is accommodated rather
than forced into a flat mold.

1. **`mkService { name, port }` + `mkIngress { name, port, ingressSuffix }`** — the
   app-agnostic Service + Ingress (traefik class, default `*.h` wildcard TLS, single `/`
   Prefix path → backend). Consumed by the arr apps, qbittorrent, CWA, **and** ABS.
2. **`mkLsioContainer { name, image, containerPort?, mediaUid, timezone, extraEnv ? [], extraMounts ? [], probes ? {} }`**
   — a LinuxServer *container* fragment (the `TZ`/`PUID`/`PGID` env + ports + mounts, no
   `runAsUser`). Reused by `mkLsioApp` **and by qbittorrent's inner `qbittorrent`
   container**, so the VPN pod keeps its gluetun sidecar + shared-netns structure and its
   probes/extra env, but stops hand-rolling the shared env block.
3. **`mkLsioApp { name, image, port, ingressSuffix, mediaUid, timezone, configPath, namespace ? "media", extraVolumes ? [], extraMounts ? [], extraEnv ? [] }`**
   — a single-container LSIO Deployment (`fsGroup`, `Recreate`, `/config` hostPath from an
   explicit `configPath`) composed from `mkLsioContainer` + `mkService` + `mkIngress`.
   **Replaces `mkArrApp`.** The arr apps become `mkLsioApp` calls (with
   `configPath = "${mediaRoot}/docker-services/torrent-config/<name>"`); CWA is a
   `mkLsioApp` call with `configPath = "${mediaRoot}/apps/calibre-web-automated/config"`
   plus the library + ingest `extraVolumes`/`extraMounts`.

ABS is deliberately **not** shoehorned into an LSIO helper — it hand-rolls its Deployment
(needs `runAsUser` + `PORT`) and only borrows `mkService`/`mkIngress`. No generic
plain-app helper is introduced for a single consumer (YAGNI); generalize if a second
non-LSIO web app appears.

**Safety gate (load-bearing):** migrating the arr apps and qbittorrent touches
already-validated production (the arr SQLite cutover, the VPN leak test). The refactor
**must be behavior-preserving**. Validate by rendering `nixidyEnvs...environmentPackage`
before and after and diffing the YAML for `prowlarr`/`radarr`/`sonarr`/`torrent-vpn`/
`qbittorrent`: only cosmetic differences (key ordering, whitespace) are acceptable — **no
changes to image, env values, ports, volumes, securityContext, probes, or strategy**. If
a diff shows a field-value change, the helper is wrong, not the old code.

## Namespace & network — `k8s/infra/network.nix` (or sibling)

A new `library` namespace with a NetworkPolicy mirroring the existing `media-network`
one: `podSelector: {}` + `policyTypes: [Ingress]` makes it default-deny-ingress; re-open
(a) intra-namespace (CWA ↔ ABS need nothing between them today, but keeps parity) and
(b) `kube-system` (Traefik). This keeps the library apps isolated from the torrent/arr
stack, matching the boundary the `media` namespace already draws.

## Secrets

**None required now** — both apps use local auth, neither needs a DB credential (no DB
server). When OIDC is added later, each app's OIDC client secret is delivered via
`sops.templates` (the mechanism in `machine/globalhawk/sops.nix`) as a k8s Secret rendered
into the `library` namespace, consumed by env `secretKeyRef`; the corresponding keys are
added to `secrets/globalhawk.sops.yaml`. That is out of scope here and belongs with the
Authelia resume.

## Cutover — atomic, with one-line rollback

CWA and the native `services.calibre-web` use the **same** on-disk Calibre library and
`metadata.db`. Two processes writing one SQLite `metadata.db` risks corruption (the same
hazard as the arr atomic cutover). Therefore they **must not run concurrently as writers**.

- **Cutover:** disable native `services.calibre-web` in the same `switch` that brings CWA
  up. Because the library is shared and its format is unchanged, CWA reads exactly what
  calibre-web left behind.
- **Rollback:** the native module stays in the flake set to `enable = false`. Re-enabling
  it (`enable = true`) and switching restores the old service against the same, unchanged
  library — no data divergence, no migration to undo. This *is* the fallback; a parallel
  run is neither needed nor safe.

## Storage & ownership

- App-state dirs (`${mediaRoot}/apps/calibre-web-automated/{config,ingest}`,
  `${mediaRoot}/apps/audiobookshelf/{config,metadata}`) are pre-created with owner
  `_media:_media` (994) via `systemd.tmpfiles` in `machine/globalhawk/disks.nix`, matching
  how the media tree is already provisioned. hostPath volumes do not honor `fsGroup`
  chown, so pre-creation is what guarantees the containers can write.
- `${mediaRoot}/apps` already exists (tmpfiles, `0775 _media _media`).
- `${mediaRoot}/books` and `${mediaRoot}/audiobooks` already exist and are `_media`-owned.

## Backups — `machine/globalhawk/backup.nix`

- **Already covered** by the restic path set: `${mediaRoot}/books` (book files **and** the
  Calibre `metadata.db`, which lives inside it) and `${mediaRoot}/audiobooks` (audio
  files).
- **To add:** `${mediaRoot}/apps/calibre-web-automated` and
  `${mediaRoot}/apps/audiobookshelf` — so CWA's `/config` (`cwa.db`, settings, local
  users) and ABS's `config` + `metadata` (SQLite DB, users, listening progress, cached
  cover art) are backed up.
- **Known limitation:** the ABS/CWA SQLite files are captured as a plain filesystem
  snapshot, i.e. crash-consistent-ish, not a transactionally-consistent dump. A
  `sqlite3 .backup` pre-backup hook is a possible later hardening; out of scope for this
  change.

## Ingress / TLS / DNS

- Ingress hosts `books.h.abrahamwhite.com` and `audiobooks.h.abrahamwhite.com`, traefik
  ingress class, served by Traefik's default `*.h.abrahamwhite.com` wildcard cert
  (kube-system TLSStore) — no per-app cert or issuer, same as the arr apps.
- Resolved LAN-privately by AdGuard (the wildcard already answers `facts.lanIp`); never
  published to public DNS.

## Out of scope (explicit)

- Authelia itself, OIDC client registration, and the `/opds` + `/api` forward-auth
  carve-outs (KOReader/Moon+ send app credentials, not interactive redirects) — all belong
  to the parked SSO spec's resume.
- The Immich → k3s migration and its Postgres.
- Komga / comic support.
- Removing the native calibre-web module, its `calibre-web` user/group, the
  `group:calibre-web:rwx` ACL on `books` in `disks.nix`, and port 8083 — a follow-up after
  CWA is validated (see Deferred).

## Validation

- `nix flake check` and the nixidy env evaluate cleanly.
- **Refactor gate:** rendered-manifest diff for `prowlarr`/`radarr`/`sonarr`/`torrent-vpn`/
  `qbittorrent` shows only cosmetic changes (no field-value changes).
- `nix run .#k3s-drift` reports no orphans/drift after switch.
- CWA reachable at `books.h.abrahamwhite.com`; it lists the pre-existing library (proving
  it read the existing `metadata.db`); a web upload and a BookDrop ingest both land in the
  library and get metadata; `/opds` serves the catalog.
- ABS reachable at `audiobooks.h.abrahamwhite.com`; it scans `${mediaRoot}/audiobooks` and
  fetches audiobook metadata; a web upload works.
- Files written by both apps are `_media`-owned (994).
- After cutover, native `services.calibre-web` is disabled and port 8083 is closed once
  the module is removed (Deferred).

## Deferred follow-ups

1. **Decommission native calibre-web** (after CWA validated): remove `services.calibre-web`,
   the `calibre-web` user/group, the `group:calibre-web:rwx` ACL on `books`, and port 8083.
2. **OIDC** for CWA + ABS as Authelia clients, with the `/opds`+`/api` carve-out — with the
   SSO resume.
3. **Komga** for comics, if the collection grows — its own SQLite pod in `library`.
