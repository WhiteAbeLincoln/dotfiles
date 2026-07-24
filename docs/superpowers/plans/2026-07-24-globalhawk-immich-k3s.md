# globalhawk Immich → k3s Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move Immich off the stale `oci-containers` install onto the k3s/nixidy pipeline at v3.0.0, with a fresh DB (re-uploading the on-disk originals) and its own uid-isolated storage.

**Architecture:** Four hand-rolled nixidy workloads (server, machine-learning, Postgres, Valkey) in a new `immich` namespace, backed by hostPath storage under a dedicated `${mediaRoot}/immich/` tree owned by a new `immich` service uid (988, `0750`) with a `media-readers` ACL for `abe`/`agent`. The DB password is a sops-rendered k8s Secret. Cutover is atomic: the operator disables the docker install, switches in the k3s stack, then re-uploads originals via the Immich CLI.

**Tech Stack:** NixOS, nixidy (k8s-as-Nix), k3s + Traefik, sops-nix, systemd-tmpfiles ACLs, restic. This is a Nix repo — the unit of work is a Nix evaluation that succeeds or fails; there is no test framework, so **each authoring task's gate is `nixos-rebuild build --flake .#globalhawk` succeeding**, and isolation/functional checks are behavioural steps the operator runs after `switch`.

## Global Constraints

- **Roles:** the agent (sandbox user, uid 1001, read-only, no sudo) authors Nix and runs `nixos-rebuild build` to validate. The operator (`abe`) runs every `switch`, `sops` edit, `kubectl`, `docker`, and Immich-CLI step. Tasks are labelled **(agent)** or **(operator)**.
- **Images digest-pinned:** every image is `repo:tag@sha256:…`. Server + ML share one `version` binding (`v3.0.0`).
- **All Immich pods run as uid 988** (`runAsUser`/`runAsGroup`/`fsGroup = immichUid`).
- **All Immich data under `${mediaRoot}/immich/`** (`library/`, `pgdata/`, `model-cache/`), owner `immich:immich`, mode `0750`.
- **Public repo:** never write a `secrets/*` literal (the DB password, etc.) into an unencrypted committed file. Reference `config.sops.placeholder.<name>` / the Nix attr path only.
- **Verbatim v3.0.0 image references** (from the release `docker/docker-compose.yml`):
  - `ghcr.io/immich-app/immich-server:v3.0.0` (resolve digest in Task 4)
  - `ghcr.io/immich-app/immich-machine-learning:v3.0.0` (resolve digest in Task 4)
  - `ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:bcf63357191b76a916ae5eb93464d65c07511da41e3bf7a8416db519b40b1c23`
  - `docker.io/valkey/valkey:9@sha256:8e8d64b405ce18f41b8e5ee20aa4687a8ed0022d1298f2ce31cdcf3a76e09411`
- **Spec:** `docs/superpowers/specs/2026-07-24-globalhawk-immich-k3s-design.md`.

## File Structure

- `machine/globalhawk/facts.nix` (modify) — add `immichUid = 988`.
- `machine/globalhawk/immich-storage.nix` (create) — the `immich` user + group, the `media-readers` group, and the tmpfiles ownership/ACL rules for `${mediaRoot}/immich`. Host-side identity only.
- `machine/globalhawk/default.nix` (modify) — import `./immich-storage.nix`; later (Task 7) drop the `services.immich-custom` block, its `program/immich` import, and its firewall port.
- `machine/globalhawk/sops.nix` (modify) — declare the `immich_db_password` secret and render the `immich-db` k8s Secret into the k3s manifests dir.
- `k8s/infra/immich-network.nix` (create) — default-deny-ingress NetworkPolicy + the `immich` namespace.
- `k8s/apps/immich.nix` (create) — the four workloads + Service + Ingress.
- `k8s/default.nix` (modify) — import the two new k8s modules.
- `flake.nix` (modify) — thread `immichUid` into the nixidy env `_module.args`.
- `machine/globalhawk/backup.nix` (modify) — repoint restic at the new library path; drop the `services.immich-custom` dependency.

---

### Task 1: Immich storage identity — dedicated uid, reader group, ACLs

**Files:**
- Modify: `machine/globalhawk/facts.nix`
- Create: `machine/globalhawk/immich-storage.nix`
- Modify: `machine/globalhawk/default.nix` (imports list)
- Modify: `machine/globalhawk/disks.nix` (enable `posixacl`; narrow the blanket `_media` ACL off `immich/` + `documents/`)

**Interfaces:**
- Produces: `facts.immichUid` (= 988); the `immich` user/group (uid/gid 988); the `media-readers` group; the owned/ACL'd `${mediaRoot}/immich/{library,pgdata,model-cache}` tree; `acltype=posixacl` on `pool/media`.

- [ ] **Step 1: Add the immich uid fact**

In `machine/globalhawk/facts.nix`, under the `--- media / storage ---` block (right after the `mediaUid` definition), add:

```nix
  # The `immich` service uid/gid — Immich's k8s pods run as it and its data tree
  # is owned by it, so photos are isolated from the shared `_media` (994) apps
  # (radarr/sonarr bind-mount all of mediaRoot). 988 is free in both the uid and
  # gid namespaces (994 is _media; 993's uid is free but gid 993 is the avahi group).
  immichUid = 988;
```

- [ ] **Step 2: Create the storage-identity module**

Create `machine/globalhawk/immich-storage.nix`:

```nix
# Host-side identity + storage isolation for Immich (the k8s workloads live in
# k8s/apps/immich.nix). Immich runs as its OWN uid (not the shared _media 994),
# and its data tree is 0750 immich:immich, so the media apps that bind-mount all
# of mediaRoot (radarr/sonarr) are denied it by the kernel — no arr change, and
# hardlinks stay intact. `abe`/`agent` keep read access via the media-readers
# group + a default ACL. See docs/superpowers/specs/2026-07-24-globalhawk-immich-k3s-design.md.
{...}: let
  facts = import ./facts.nix;
  immichRoot = "${facts.mediaRoot}/immich";
in {
  users.groups.immich.gid = facts.immichUid;
  users.users.immich = {
    isSystemUser = true;
    uid = facts.immichUid;
    group = "immich";
    description = "Immich service account (k8s workload uid)";
  };

  # Reusable human-read handle for tightened per-app media trees. `abe` also has
  # _media (write) elsewhere; `agent` (read-only sandbox, uid 1001) is kept OUT
  # of _media and only ever gets read, via this group.
  users.groups.media-readers.members = ["abe" "agent"];

  # Ownership + ACLs. The dir mode (0750) denies `_media` (994) — it's neither
  # owner nor in the immich/media-readers groups, so it falls to "other" = ---.
  # media-readers gets r-x on the tree root (traverse) and on library/ (read
  # photos), plus a default ACL on library/ so photos Immich creates inherit it.
  # pgdata/model-cache stay human-inaccessible (no reader ACL) — nobody browses
  # Postgres files. `A+` (append) per the ebook-stack overlapping-ACL lesson.
  systemd.tmpfiles.rules = [
    "d ${immichRoot} 0750 immich immich - -"
    "d ${immichRoot}/library 0750 immich immich - -"
    "d ${immichRoot}/pgdata 0750 immich immich - -"
    "d ${immichRoot}/model-cache 0750 immich immich - -"
    "A+ ${immichRoot} - - - - group:media-readers:r-x,mask::r-x"
    "A+ ${immichRoot}/library - - - - group:media-readers:r-x,default:group:media-readers:r-x,mask::r-x,default:mask::r-x"
  ];
}
```

- [ ] **Step 3: Import the module**

In `machine/globalhawk/default.nix`, add `./immich-storage.nix` to the `imports` list (next to `./backup.nix` / `./adguard.nix`):

```nix
    ./disks.nix
    ./backup.nix
    ./k3s.nix
    ./adguard.nix
    ./sops.nix
    ./immich-storage.nix
    ../../modules/nixos/ai-agent-sandbox.nix
```

- [ ] **Step 4: Build-validate (agent)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds with no evaluation error. (A uid/gid collision or unknown-user error here means 988 or a username is wrong — re-check Step 1/2.)

- [ ] **Step 5: Commit**

```bash
git add machine/globalhawk/facts.nix machine/globalhawk/immich-storage.nix machine/globalhawk/default.nix
git commit -m "feat(globalhawk): dedicated immich uid + media-readers ACL for photo isolation

Give Immich its own uid (988) and a 0750 data tree with a media-readers
default ACL, so the shared-_media apps that bind-mount all of mediaRoot cannot
read photos, while abe/agent keep read access. Host-side half of the k3s
migration."
```

**Steps 1–5 completed (commit `35c3aaf`).** During review it was found that `pool/media`
has `acltype=off`, so those ACL rules are inert until ACLs are enabled — AND the existing
recursive `A ${mediaRoot} … group:_media:rwx` in `disks.nix` (currently inert) would, once
ACLs are on, both wipe Immich's reader ACL and grant `_media` rwx across the whole tree
(including `documents/`). Steps 6–8 fix this: enable `posixacl` and narrow that rule.

- [ ] **Step 6: Enable posixacl + narrow the blanket `_media` ACL in `disks.nix`**

In `machine/globalhawk/disks.nix`, **replace** the single blanket line
`"A ${facts.mediaRoot} - - - - group:_media:rwx"` with explicit per-subtree grants that
omit `immich/` and `documents/` (comment kept/updated, not deleted):

```nix
    # Grant the _media group rwx on the SHARED media dirs so the media apps
    # (which run as _media) can write each other's files despite umask. NOT a
    # blanket rule over ${mediaRoot}: immich/ (isolated, own uid + media-readers
    # ACL — see immich-storage.nix) and documents/ (abe-private) are deliberately
    # omitted so enabling posixacl below doesn't expose them.
    "A+ ${facts.mediaRoot}/anime - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/apps - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/audiobooks - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/docker-services - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/movies - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/music - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/old_books - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/photos - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/torrents - - - - group:_media:rwx"
    "A+ ${facts.mediaRoot}/tv - - - - group:_media:rwx"
    # (books keeps its own A+ line below.)
```

Keep the existing `A+ ${facts.mediaRoot}/books …` line, **but update its comment** — it
currently says the grant is "redundant with the recursive `_media` ACL above"; that recursive
rule is now gone, so this line is the *sole* `_media` grant for `books/`. Reword to reflect
that (e.g. "grants the `_media` group rwx on the books library so the CWA pod (994) can
write; this is now the only `_media` grant for books — the blanket recursive rule was
removed").

Then, in the same file, add a oneshot that ensures `posixacl` is set on the dataset before
tmpfiles applies any ACLs (idempotent; a dataset property, so it persists):

```nix
  # POSIX ACLs are off by default on this pool, which silently no-ops every
  # tmpfiles `A`/`A+` rule. Enable it before systemd-tmpfiles runs so the media
  # + immich ACLs actually take effect. `xattr=sa` is already set.
  systemd.services.zfs-media-posixacl = {
    description = "Ensure acltype=posixacl on pool/media";
    wantedBy = ["local-fs.target"];
    # Order after the dataset is actually mounted (RequiresMountsFor resolves to
    # the generated data-Media.mount, avoiding the imprecise zfs-mount.service),
    # and before BOTH tmpfiles units: -setup (boot) AND -resetup (the unit
    # switch-to-configuration re-runs on `nixos-rebuild switch`). Missing the
    # resetup ordering lets the ACL rules apply before posixacl is on during a
    # switch, silently no-opping the reader ACL until the next reboot.
    before = ["systemd-tmpfiles-setup.service" "systemd-tmpfiles-resetup.service"];
    unitConfig.RequiresMountsFor = facts.mediaRoot;
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.zfs}/bin/zfs set acltype=posixacl pool/media";
    };
  };
```

`disks.nix` already takes `pkgs` in its module args, so `${pkgs.zfs}` resolves; `facts` is
already imported at the top of the module.

- [ ] **Step 7: Build-validate (agent)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds with no evaluation error.

- [ ] **Step 8: Commit**

```bash
git add machine/globalhawk/disks.nix
git commit -m "feat(globalhawk): enable posixacl + narrow the _media grant off immich/documents

pool/media had acltype=off, silently no-opping every tmpfiles ACL (incl. the
blanket recursive _media grant and the ebook books grant). Enable posixacl so
the immich reader ACL actually applies, and replace the whole-tree _media grant
with explicit per-subtree grants that exclude immich/ (own isolation) and
documents/ (abe-private) — otherwise enabling ACLs would expose both."
```

> **Post-switch behavioural checks (operator, in Task 6):** after enabling posixacl and the first `switch`:
> - `getfacl ${mediaRoot}/documents` → **no** `_media` entry (privacy preserved).
> - `getfacl ${mediaRoot}/immich` → a `media-readers` entry, **no** `_media` entry.
> - `sudo -u abe ls ${mediaRoot}/immich/library` and `sudo -u agent …` succeed (abe/agent may need to re-login for the new group).
> - a process as `_media` (994) is **denied** `ls ${mediaRoot}/immich`.
>
> If a reader is wrongly denied, the ACL `mask::` is too tight; if `_media` is allowed, the narrowing missed an entry.

---

### Task 2: sops-rendered DB-password Secret

**Files:**
- Modify: `machine/globalhawk/sops.nix`

**Interfaces:**
- Produces: a k8s Secret `immich-db` (namespace `immich`, key `password`) available to Task 4's server + Postgres via `secretKeyRef`.
- Consumes: the sops mechanism already in `sops.nix`.

- [ ] **Step 1: Declare the secret**

In `machine/globalhawk/sops.nix`, add to the `secrets = { … }` attrset:

```nix
      immich_db_password = {};
```

- [ ] **Step 2: Render the k8s Secret**

In the `templates = { … }` attrset (next to `sops-mullvad-wg.yaml`), add:

```nix
      # Immich Postgres password. name/key/namespace are load-bearing: the
      # immich-server (DB_PASSWORD) and immich-postgres (POSTGRES_PASSWORD)
      # containers in k8s/apps/immich.nix reference immich-db/password.
      "sops-immich-db.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-immich-db.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: immich-db
            namespace: immich
          type: Opaque
          stringData:
            password: ${config.sops.placeholder.immich_db_password}
        '';
      };
```

- [ ] **Step 3: Commit the code**

```bash
git add machine/globalhawk/sops.nix
git commit -m "feat(globalhawk): sops-render the immich-db k8s Secret

Immich's fresh Postgres gets a new password delivered as a k8s Secret via the
same sops->manifests mechanism as the mullvad/cloudflare secrets."
```

- [ ] **Step 4: Add the secret value (operator) — REQUIRED BEFORE build-validation, not just switch**

**Important:** `sops-nix` validates that every declared secret *exists* in the encrypted
file at **build** time (`validateSopsFiles`, part of `system.build.toplevel`) — not only at
activation. So `nixos-rebuild build` **fails** with `secret immich_db_password ... cannot be
found` until this value exists. Because the declaration is committed on the branch, this also
blocks build-validation of every later task. The operator therefore adds the value now, before
Step 5. From the repo root:

```bash
sops secrets/globalhawk.sops.yaml
# add a top-level line:  immich_db_password: <a fresh strong password, e.g. openssl rand -base64 32>
```

Do **not** reuse the old `immich_pass`; this is a fresh DB. (Agent cannot do this — no key.)

- [ ] **Step 5: Build-validate (agent, after Step 4)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds cleanly now that the key exists. (Before Step 4 it fails on the missing key — that failure is expected and is *not* a code defect.)

---

### Task 3: Immich namespace + NetworkPolicy

**Files:**
- Create: `k8s/infra/immich-network.nix`
- Modify: `k8s/default.nix` (imports)

**Interfaces:**
- Produces: the `immich` namespace (`createNamespace = true`) and a default-deny-ingress NetworkPolicy allowing intra-namespace + Traefik (kube-system) traffic. Task 4's workloads land in this namespace.

- [ ] **Step 1: Create the network module**

Create `k8s/infra/immich-network.nix` (mirrors `library-network.nix`):

```nix
# Immich lives in its own namespace with the same boundary as media/library:
# default-deny-ingress, re-opened only for intra-namespace traffic (server <->
# postgres/redis/ML) and the Traefik ingress controller (kube-system).
{...}: {
  applications.immich-network = {
    namespace = "immich";
    createNamespace = true;
    yamls = [
      (builtins.toJSON {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "allow-intra-and-ingress";
          namespace = "immich";
        };
        spec = {
          podSelector = {};
          policyTypes = ["Ingress"];
          ingress = [
            {from = [{podSelector = {};}];}
            {
              from = [
                {
                  namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                }
              ];
            }
          ];
        };
      })
    ];
  };
}
```

- [ ] **Step 2: Import it**

In `k8s/default.nix`, add to `imports` (in the infra group):

```nix
    ./infra/network.nix
    ./infra/library-network.nix
    ./infra/immich-network.nix
    ./infra/cert-manager.nix
    ./infra/wildcard-tls.nix
```

- [ ] **Step 3: Build-validate (agent)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds (this renders the nixidy env, so a malformed manifest fails here).

- [ ] **Step 4: Commit**

```bash
git add k8s/infra/immich-network.nix k8s/default.nix
git commit -m "feat(globalhawk): immich namespace + default-deny-ingress NetworkPolicy"
```

---

### Task 4: The Immich workloads

**Files:**
- Create: `k8s/apps/immich.nix`
- Modify: `k8s/default.nix` (imports)
- Modify: `flake.nix` (nixidy `_module.args`)

**Interfaces:**
- Consumes: module args `lib`, `ingressSuffix`, `mediaRoot`, `timezone`, `immichUid`; the `immich-db` Secret (Task 2); the `immich` namespace (Task 3); `l.appLabels`/`l.mkService`/`l.mkIngress` from `k8s/lib.nix`.
- Produces: Deployments `immich-postgres`, `immich-redis`, `immich-machine-learning`, `immich-server`; Services `immich-postgres`/`immich-redis`/`immich-machine-learning`/`immich-server`; Ingress `immich-server` at `photos${ingressSuffix}`.

- [ ] **Step 1: Thread `immichUid` into the nixidy env**

In `flake.nix`, extend the `inherit (facts) …` line in the nixidy env `_module.args` (around line 137):

```nix
                inherit (facts) ingressSuffix podCidr serviceCidr hostGatewayIp mediaRoot mediaUid timezone immichUid;
```

- [ ] **Step 2: Resolve the server + ML image digests (agent)**

```bash
nix run nixpkgs#skopeo -- inspect --format '{{.Digest}}' docker://ghcr.io/immich-app/immich-server:v3.0.0
nix run nixpkgs#skopeo -- inspect --format '{{.Digest}}' docker://ghcr.io/immich-app/immich-machine-learning:v3.0.0
```

Expected: two `sha256:…` digests. Use them in Step 3 in place of `sha256:REPLACE_SERVER` / `sha256:REPLACE_ML`.

- [ ] **Step 3: Write the workloads manifest**

Create `k8s/apps/immich.nix`:

```nix
# Immich on k3s (v3.0.0), fresh DB. Four workloads in the `immich` namespace:
# server + machine-learning (share ONE version tag — the upgrade knob), the
# official VectorChord Postgres, and Valkey. All run as the dedicated `immich`
# uid (988, NOT _media) so the data tree stays isolated; storage is hostPath
# under ${mediaRoot}/immich (owned/ACL'd by machine/globalhawk/immich-storage.nix).
# DB password from the sops `immich-db` Secret. Mirrors the official Helm chart's
# shape. See docs/superpowers/specs/2026-07-24-globalhawk-immich-k3s-design.md.
{
  lib,
  ingressSuffix,
  mediaRoot,
  timezone,
  immichUid,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  version = "v3.0.0";
  serverImage = "ghcr.io/immich-app/immich-server:${version}@sha256:REPLACE_SERVER";
  mlImage = "ghcr.io/immich-app/immich-machine-learning:${version}@sha256:REPLACE_ML";
  pgImage = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:bcf63357191b76a916ae5eb93464d65c07511da41e3bf7a8416db519b40b1c23";
  valkeyImage = "docker.io/valkey/valkey:9@sha256:8e8d64b405ce18f41b8e5ee20aa4687a8ed0022d1298f2ce31cdcf3a76e09411";

  serverLabels = l.appLabels "immich-server";
  mlLabels = l.appLabels "immich-machine-learning";
  pgLabels = l.appLabels "immich-postgres";
  redisLabels = l.appLabels "immich-redis";

  secCtx = {
    runAsUser = immichUid;
    runAsGroup = immichUid;
    fsGroup = immichUid;
  };
  dbPassword = {
    name = "DB_PASSWORD";
    valueFrom.secretKeyRef = {
      name = "immich-db";
      key = "password";
    };
  };
in {
  applications.immich = {
    namespace = "immich";
    createNamespace = false;
    resources = {
      deployments = {
        # --- Postgres (VectorChord) ---
        immich-postgres.spec = {
          replicas = 1;
          selector.matchLabels = pgLabels;
          strategy.type = "Recreate"; # single writer on the data dir
          template = {
            metadata.labels = pgLabels;
            spec = {
              securityContext = secCtx;
              containers.postgres = {
                image = pgImage;
                env = [
                  (dbPassword // {name = "POSTGRES_PASSWORD";})
                  {
                    name = "POSTGRES_USER";
                    value = "postgres";
                  }
                  {
                    name = "POSTGRES_DB";
                    value = "immich";
                  }
                  {
                    name = "POSTGRES_INITDB_ARGS";
                    value = "--data-checksums";
                  }
                ];
                ports.postgres.containerPort = 5432;
                volumeMounts = [
                  {
                    name = "pgdata";
                    mountPath = "/var/lib/postgresql/data";
                  }
                  {
                    name = "shm";
                    mountPath = "/dev/shm";
                  }
                ];
                readinessProbe.exec.command = ["pg_isready" "-U" "postgres" "-d" "immich"];
              };
              volumes = [
                {
                  name = "pgdata";
                  hostPath = {
                    path = "${mediaRoot}/immich/pgdata";
                    type = "Directory";
                  };
                }
                {
                  name = "shm";
                  emptyDir = {
                    medium = "Memory";
                    sizeLimit = "128Mi";
                  };
                }
              ];
            };
          };
        };

        # --- Valkey (redis) ---
        immich-redis.spec = {
          replicas = 1;
          selector.matchLabels = redisLabels;
          template = {
            metadata.labels = redisLabels;
            spec = {
              securityContext = secCtx;
              containers.redis = {
                image = valkeyImage;
                ports.redis.containerPort = 6379;
                readinessProbe.exec.command = ["redis-cli" "ping"];
              };
              # Job queue/cache only — safe to lose on restart.
            };
          };
        };

        # --- Machine learning (CPU) ---
        immich-machine-learning.spec = {
          replicas = 1;
          selector.matchLabels = mlLabels;
          strategy.type = "Recreate"; # holds the model-cache hostPath
          template = {
            metadata.labels = mlLabels;
            spec = {
              securityContext = secCtx;
              containers.machine-learning = {
                image = mlImage;
                env = [
                  {
                    name = "TRANSFORMERS_CACHE";
                    value = "/cache";
                  }
                  {
                    name = "HF_XET_CACHE";
                    value = "/cache/huggingface-xet";
                  }
                  {
                    name = "MPLCONFIGDIR";
                    value = "/cache/matplotlib-config";
                  }
                ];
                ports.http.containerPort = 3003;
                volumeMounts = [
                  {
                    name = "model-cache";
                    mountPath = "/cache";
                  }
                ];
                resources = {
                  requests.memory = "512Mi";
                  limits.memory = "3Gi";
                };
              };
              volumes = [
                {
                  name = "model-cache";
                  hostPath = {
                    path = "${mediaRoot}/immich/model-cache";
                    type = "Directory";
                  };
                }
              ];
            };
          };
        };

        # --- Server (API + web) ---
        immich-server.spec = {
          replicas = 1;
          selector.matchLabels = serverLabels;
          strategy.type = "Recreate"; # single writer on the library hostPath
          template = {
            metadata.labels = serverLabels;
            spec = {
              securityContext = secCtx;
              containers.server = {
                image = serverImage;
                env = [
                  dbPassword
                  {
                    name = "DB_HOSTNAME";
                    value = "immich-postgres";
                  }
                  {
                    name = "DB_USERNAME";
                    value = "postgres";
                  }
                  {
                    name = "DB_DATABASE_NAME";
                    value = "immich";
                  }
                  {
                    name = "REDIS_HOSTNAME";
                    value = "immich-redis";
                  }
                  {
                    name = "IMMICH_MACHINE_LEARNING_URL";
                    value = "http://immich-machine-learning:3003";
                  }
                  {
                    name = "TZ";
                    value = timezone;
                  }
                ];
                ports.http.containerPort = 2283;
                volumeMounts = [
                  {
                    name = "library";
                    mountPath = "/data";
                  }
                ];
                readinessProbe.httpGet = {
                  path = "/api/server/ping";
                  port = 2283;
                };
                livenessProbe.httpGet = {
                  path = "/api/server/ping";
                  port = 2283;
                };
              };
              volumes = [
                {
                  name = "library";
                  hostPath = {
                    path = "${mediaRoot}/immich/library";
                    type = "Directory";
                  };
                }
              ];
            };
          };
        };
      };

      services =
        (l.mkService {
          name = "immich-server";
          port = 2283;
        })
        // (l.mkService {
          name = "immich-postgres";
          port = 5432;
          portName = "postgres";
        })
        // (l.mkService {
          name = "immich-redis";
          port = 6379;
          portName = "redis";
        })
        // (l.mkService {
          name = "immich-machine-learning";
          port = 3003;
        });

      ingresses = l.mkIngress {
        name = "immich-server";
        port = 2283;
        host = "photos${ingressSuffix}";
      };
    };
  };
}
```

- [ ] **Step 4: Import the app**

In `k8s/default.nix`, add to `imports` (apps group):

```nix
    ./apps/calibre-web-automated.nix
    ./apps/audiobookshelf.nix
    ./apps/immich.nix
```

- [ ] **Step 5: Build-validate (agent)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds. (If it complains about `immichUid` being an unexpected argument, Step 1's `_module.args` wiring is missing.)

- [ ] **Step 6: Commit**

```bash
git add k8s/apps/immich.nix k8s/default.nix flake.nix
git commit -m "feat(globalhawk): Immich v3.0.0 workloads on k3s (server/ML/postgres/valkey)

Fresh-DB Immich in the immich namespace, all pods as uid 988 on the isolated
${mediaRoot}/immich tree. Server + ML share one version tag; DB image is the
official VectorChord build; DB password from the sops immich-db Secret."
```

---

### Task 5: Backup rework

**Files:**
- Modify: `machine/globalhawk/backup.nix`

**Interfaces:**
- Consumes: `facts.mediaRoot`.
- Produces: a `backup.nix` that no longer depends on `config.services.immich-custom` and backs up the new `${mediaRoot}/immich/library`.

- [ ] **Step 1: Repoint the immich paths**

In `machine/globalhawk/backup.nix`, replace the `immich-custom`-derived binding. Change the `let` block:

```nix
  facts = import ./facts.nix;
  # Immich's managed store on k3s (was services.immich-custom.uploadDir). Immich
  # writes originals + its built-in DB dumps under here (mounted at /data).
  immichRoot = "${facts.mediaRoot}/immich/library";
  # Where the newest Immich DB dump is staged for inclusion in the backup.
  stagedDbDump = "/var/lib/restic-media/immich-db-latest.sql.gz";
```

The rest of the module (the `paths`, `exclude`, `backupPrepareCommand`, `timerConfig`) already references `immichRoot`, so repointing the binding flows through. **Verify** the `backupPrepareCommand` glob still matches Immich v3's built-in dump filenames — update `immich-db-backup-*.sql.gz` to whatever v3 writes under `${immichRoot}/backups` (confirm at operator verify time; v3's built-in backup writes `immich-db-backup-*.sql.gz.` — check the exact suffix in `${immichRoot}/backups` after first run and adjust the glob if needed).

- [ ] **Step 2: Confirm no other `immich-custom` reference remains**

Run: `grep -n immich-custom machine/globalhawk/backup.nix`
Expected: no output. (If any remains, it will break at Task 7 when the option is removed.)

- [ ] **Step 3: Build-validate (agent)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds.

- [ ] **Step 4: Commit**

```bash
git add machine/globalhawk/backup.nix
git commit -m "refactor(globalhawk): back up Immich's k3s library, drop immich-custom dep

Repoint restic + the staged DB dump at the new managed store so backup.nix no
longer depends on the services.immich-custom option (removed at decommission)."
```

---

### Task 6: Atomic cutover (operator runbook)

This task is executed by the operator on globalhawk after Tasks 1–5 are merged and the sops value (Task 2, Step 5) is added. The agent does not run these steps. Each is a checkbox so the operator can track progress.

- [ ] **Step 1: Final validation on the branch**

```bash
nixos-rebuild build --flake .#globalhawk    # eval clean
nix run .#k3s-drift                          # current cluster vs desired (pre-switch baseline)
```

- [ ] **Step 1a: Enable posixacl on the pool up front (belt-and-suspenders)**

The `zfs-media-posixacl` oneshot enables it declaratively, but the *first* activation's
systemd ordering + ZFS's live-vs-remount behaviour for an `acltype` change is enough of a
grey area that we set it by hand once first (it's a persistent dataset property — set once,
true forever). Then prove ACLs are actually live before relying on them:

```bash
sudo zfs set acltype=posixacl pool/media
zfs get -o value acltype pool/media          # posixacl
# prove the kernel now accepts ACLs on this dataset (immich dir exists after any build/switch,
# or test on any dir); a clean setfacl + getfacl round-trip means ACLs are live, no remount needed:
sudo setfacl -m g:media-readers:r-x /data/Media/immich 2>/dev/null && getfacl /data/Media/immich | grep media-readers && echo "ACLs live"
```

If `setfacl` errors with "Operation not supported", the mount hasn't picked up posixacl —
remount it (`sudo zfs mount -o remount pool/media`, or defer to the reboot after cutover) and
re-test before continuing.

- [ ] **Step 2: Stop the docker Immich**

Set `services.immich-custom.enable = false;` in `machine/globalhawk/default.nix` (leave the rest of the block for now), then:

```bash
sudo nixos-rebuild switch --flake .#globalhawk
docker ps | grep immich   # expect: nothing
```

The old originals at `${mediaRoot}/immich/photos` are untouched.

- [ ] **Step 3: Switch in the k3s stack**

```bash
sudo nixos-rebuild switch --flake .#globalhawk
kubectl -n immich get pods            # server/ML/postgres/redis all Ready
kubectl -n immich get ingress
```

- [ ] **Step 3a: Postgres-as-988 health check (do this FIRST — most likely failure)**

The final review flagged this as the single most-likely-to-trip item: the VectorChord/official
Postgres image runs `getpwuid()` on its effective uid, and uid 988 has no passwd entry inside
the container, so `initdb` on a fresh data dir can fail. Check the DB pod before investing in
the photo re-upload:

```bash
kubectl -n immich logs deploy/immich-postgres --tail=50
kubectl -n immich get pod -l app.kubernetes.io/name=immich-postgres   # Running, not CrashLoopBackOff
```

- **Healthy:** logs show `database system is ready to accept connections`. Proceed.
- **Failed** with `could not look up local user ID 988`, `initdb: … permission denied`, or a
  crash loop: this is a **design tweak, not an operator workaround** — stop and hand back to the
  agent. The fix (validated then, not now): drop the forced `runAsUser`/`runAsGroup` from the
  `immich-postgres` pod so the image runs its default `root→postgres` entrypoint (which owns a
  passwd entry and chowns `PGDATA` itself), and adjust the `pgdata` tmpfiles rule so it doesn't
  fight that ownership on the next switch. Server + ML stay at 988 (they must own `library/`
  and `model-cache/`). Immich's own `docker-compose.yml` runs Postgres with no user override,
  so this is the reference-aligned fallback.

- [ ] **Step 4: Behavioural isolation check (the Task 1 deferred check)**

```bash
sudo -u abe   ls ${mediaRoot}/immich/library      # succeeds
sudo -u agent ls ${mediaRoot}/immich/library      # succeeds
sudo -u _media ls ${mediaRoot}/immich             # PERMISSION DENIED (the point)
```

If a reader is wrongly denied, loosen the tmpfiles ACL `mask::` (Task 1); if `_media` is *not* denied, the dir mode/ownership is wrong.

- [ ] **Step 5: Confirm the app is up and empty**

Browse `https://photos.h.abrahamwhite.com` → Immich onboarding screen loads over TLS. Create the **admin** account and the **second** account. For each, Account Settings → API Keys → mint a key.

- [ ] **Step 6: Re-upload originals per account**

Map the two old upload subfolders to the two accounts (decide which UUID is whose by spot-checking a few photos). Then, per account:

```bash
# account A's key + account A's subfolder
IMMICH_INSTANCE_URL=https://photos.h.abrahamwhite.com/api \
IMMICH_API_KEY=<accountA-key> \
nix run nixpkgs#immich-cli -- upload --recursive ${mediaRoot}/immich/photos/upload/a7b113fc-b9c9-4bd3-b935-7b1b112cbc22

# account B's key + account B's subfolder
IMMICH_INSTANCE_URL=https://photos.h.abrahamwhite.com/api \
IMMICH_API_KEY=<accountB-key> \
nix run nixpkgs#immich-cli -- upload --recursive ${mediaRoot}/immich/photos/upload/4fac97df-37dc-4c0a-843b-5bda8d3f7f8a
```

Checksum de-dup makes each run idempotent/resumable — re-run after any interruption. (If `nixpkgs#immich-cli` lags v3, use the `ghcr.io/immich-app/immich-cli` container with the old tree mounted read-only.)

- [ ] **Step 7: Verify the import**

- Per-account asset counts look right; timeline renders (EXIF dates reconstructed).
- Administration → Jobs: thumbnail/CLIP/face jobs progressing.
- Administration → Settings → Backup: enable the built-in database backup (writes to `/data/backups`). Trigger one, confirm a dump appears in `${mediaRoot}/immich/library/backups`, and confirm the restic `backupPrepareCommand` glob (Task 5) matches its filename.
- `kubectl -n immich logs deploy/immich-server` and `… deploy/immich-postgres` — no permission/crash loops (the **Postgres-as-988** risk; if PGDATA init fails, run Postgres as its default user per the spec's immich-postgres note).

- [ ] **Step 8: STOP — hand back for decommission**

Do not delete anything yet. Confirm to the agent that the import is verified; proceed to Task 7. Rollback until this point: re-enable `services.immich-custom`, remove the k3s app import, `switch` — old originals + old DB dump are still on disk.

---

### Task 7: Decommission the docker install (agent, after operator sign-off)

**Files:**
- Modify: `machine/globalhawk/default.nix` (remove the `services.immich-custom` block, the `program/immich` import, the immich firewall port)
- Modify: `machine/globalhawk/sops.nix` (remove the now-unused `immich_pass` secret)
- Delete: `program/immich/` (the whole directory)

**Interfaces:**
- Consumes: operator confirmation that Task 6 verified successfully.

- [ ] **Step 1: Remove the service config + import + firewall port**

In `machine/globalhawk/default.nix`, delete the `services.immich-custom = { … };` block, remove `../../program/immich` from `imports`, and remove the firewall line `config.services.immich-custom.port` from `allowedTCPPorts`.

- [ ] **Step 2: Remove the old secret**

In `machine/globalhawk/sops.nix`, remove `immich_pass = {};` from `secrets`. (Operator separately drops the value from the encrypted file with `sops secrets/globalhawk.sops.yaml` — optional cleanup.)

- [ ] **Step 3: Delete the custom module**

```bash
git rm -r program/immich
```

- [ ] **Step 4: Build-validate (agent)**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds with no reference to `services.immich-custom` (a leftover reference — e.g. in `backup.nix` — fails here; that's why Task 5 removed it).

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(globalhawk): decommission the oci-containers Immich

The k3s Immich is validated and serving the re-uploaded originals; remove the
native services.immich-custom install, its custom module, firewall port, and
the stale immich_pass secret."
```

- [ ] **Step 6: Operator physical cleanup**

```bash
# reclaim ~24 GB: old originals (now duplicated into /data) + regenerable dirs
sudo rm -rf ${mediaRoot}/immich/photos
# orphaned docker state from the old install
docker volume rm pgdata model-cache
docker network rm immich-bridge
sudo nixos-rebuild switch --flake .#globalhawk   # apply the decommission
```

- [ ] **Step 7: Finish the branch**

Use `superpowers:finishing-a-development-branch` to merge `globalhawk-immich-k3s` to `master`.

---

## Self-Review

**Spec coverage:**
- Fresh v3.0.0 install, no DB migration → Task 4 (fresh Postgres, no restore step). ✓
- Re-upload via CLI, two accounts → Task 6 Steps 5–6. ✓
- ML included, CPU, memory-limited (low request / 3 Gi cap) → Task 4 ML resources. ✓
- Mirror the chart / one version knob → Task 4 `version` binding. ✓
- New `immich` namespace + NetworkPolicy → Task 3. ✓
- Ingress `photos.h.…` → Task 4 ingress. ✓
- Secret via sops → Task 2. ✓
- Storage isolation (uid 988, 0750, media-readers ACL, arr untouched) → Task 1 + behavioural check in Task 6 Step 4. ✓
- Postgres shm/`--data-checksums`/Recreate → Task 4 postgres. ✓
- Backup rework → Task 5 + Task 6 Step 7. ✓
- Atomic cutover + rollback → Task 6. ✓
- Decommission + cleanup → Task 7. ✓
- Deferred arr restructure → out of scope for this plan (documented in the spec). ✓

**Placeholder scan:** the only intentional placeholders are the two image digests (`sha256:REPLACE_SERVER`/`sha256:REPLACE_ML`), resolved by a concrete command in Task 4 Step 2. No other TBD/TODO.

**Type/name consistency:** Secret `immich-db`/key `password` is defined in Task 2 and referenced identically in Task 4. Service names (`immich-postgres`, `immich-redis`, `immich-machine-learning`, `immich-server`) match the env values (`DB_HOSTNAME`, `REDIS_HOSTNAME`, `IMMICH_MACHINE_LEARNING_URL`). `immichUid` fact (Task 1) → `_module.args` (Task 4 Step 1) → module arg (Task 4 Step 3) is threaded end-to-end.
