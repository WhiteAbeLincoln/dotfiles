# globalhawk ebook + audiobook stack (CWA + Audiobookshelf) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace native `services.calibre-web` with Calibre-Web-Automated (CWA) on k3s and add Audiobookshelf (ABS), both authored in Nix and delivered via the existing nixidy pipeline, while extracting the duplicated LinuxServer/ingress boilerplate into shared `k8s/lib.nix` helpers and pinning all workload images.

**Architecture:** Two new nixidy workloads in a new `library` namespace, delivered through the existing `nixidyCombined` → `services.k3s.manifests` lane (no ArgoCD, no Helm). CWA reuses the existing Calibre library at `${mediaRoot}/books` in place. The arr apps and qbittorrent are migrated onto new `mkService`/`mkIngress`/`mkLsioContainer`/`mkLsioApp` helpers in a behavior-preserving refactor gated on a rendered-manifest diff.

**Tech Stack:** Nix (flake-parts), nixidy (k8s workloads as Nix modules → plain YAML), k3s, Traefik ingress, sops-nix (not needed this plan — local auth), alejandra (formatter).

## Global Constraints

- **Formatter:** run `nix fmt` (alejandra) after every `.nix` edit before validating.
- **GNU sed** (not BSD); prefer the Edit tool over sed regardless.
- **Never `switch`.** The implementer is the read-only sandbox user (uid 1001, no sudo, no `kubectl` writes). Validate with `nix build` / rendered-YAML diffs only. All `switch` + write-`kubectl` + browser checks are **OPERATOR** steps, collected in the final task.
- **Public repo:** never write a value from `secrets/*.nix` into a committed file. Nothing in this plan needs a secret (local auth); reference attribute paths if one ever arises.
- **Run as `_media` (uid/gid 994):** every workload touching the media tree runs as 994 (`PUID`/`PGID` for LSIO images, `runAsUser`/`fsGroup` for others) so on-disk files stay `_media`-owned.
- **hostPath, no data copy;** `strategy.type = "Recreate"` for any workload holding a SQLite/config lock.
- **Refactor byte-identity gate:** after Tasks 1 and 2, the rendered YAML for `prowlarr`/`radarr`/`sonarr`/`torrent` must be **identical** to the Task 0 baseline. After Task 3, the *only* differences are `image:` lines.
- **Pin every image** to an immutable digest (`@sha256:…`); no floating `:latest` ships.

**Render/validate commands used throughout:**

```bash
# SCRATCH dir for baselines/diffs (session scratchpad):
SCRATCH=/tmp/claude-1001/-srv-dotfiles/4e496b2e-08e7-4458-8ccb-4e2cf753dfc8/scratchpad

# Build the rendered nixidy env, print its store path:
render() { nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'; }

# Build the whole NixOS system (evaluation gate for host-layer edits):
sysbuild() { nix build --no-link '.#nixosConfigurations.globalhawk.config.system.build.toplevel'; }
```

---

### Task 0: Capture refactor baseline

**Files:** none (produces a scratchpad baseline only).

**Interfaces:**
- Produces: `$SCRATCH/baseline/` — a copy of the current rendered YAML for the four workloads the refactor touches, used by Tasks 1–3 as the equality oracle.

- [ ] **Step 1: Build the current env and snapshot the touched workloads**

```bash
SCRATCH=/tmp/claude-1001/-srv-dotfiles/4e496b2e-08e7-4458-8ccb-4e2cf753dfc8/scratchpad
OUT=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
rm -rf "$SCRATCH/baseline" && mkdir -p "$SCRATCH/baseline"
cp -rL "$OUT"/prowlarr "$OUT"/radarr "$OUT"/sonarr "$OUT"/torrent "$SCRATCH/baseline/"
find "$SCRATCH/baseline" -type f | sort
```

Expected: lists `Deployment-*.yaml`, `Service-*.yaml`, `Ingress-*.yaml` for prowlarr/radarr/sonarr and `torrent/{Deployment-torrent-vpn,Service-qbittorrent,Ingress-qbittorrent}.yaml`.

- [ ] **Step 2: Define the diff helper (used by Tasks 1–3)**

```bash
# Re-run after each refactor task; empty output = byte-identical.
diff_workloads() {
  local OUT; OUT=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
  rm -rf "$SCRATCH/after" && mkdir -p "$SCRATCH/after"
  cp -rL "$OUT"/prowlarr "$OUT"/radarr "$OUT"/sonarr "$OUT"/torrent "$SCRATCH/after/"
  diff -ru "$SCRATCH/baseline" "$SCRATCH/after"
}
```

No commit (scratchpad only).

---

### Task 1: Extract `mkService` + `mkIngress`; migrate arr + qbittorrent exposure

**Files:**
- Modify: `k8s/lib.nix` (add `mkService`, `mkIngress`; use them inside `mkArrApp`)
- Modify: `k8s/apps/torrent.nix` (replace hand-written Service+Ingress; drop now-unused `host` local)

**Interfaces:**
- Consumes: `appLabels` (existing in `lib.nix`).
- Produces:
  - `mkService = { name, port, portName ? "http" }: { "${name}".spec = {...}; }`
  - `mkIngress = { name, port, host }: { "${name}".spec = {...}; }`

- [ ] **Step 1: Add the two helpers to `k8s/lib.nix`**

Insert into the `rec { … }` body (after `appLabels`):

```nix
  # A ClusterIP Service selecting this app's pods on a single named port.
  # portName defaults to "http"; the torrent pod uses "webui" to match its
  # existing manifest.
  mkService = {
    name,
    port,
    portName ? "http",
  }: {
    "${name}".spec = {
      selector = appLabels name;
      ports.${portName} = {
        inherit port;
        targetPort = port;
      };
    };
  };

  # A Traefik Ingress routing `host` to this app's Service on `port`. No
  # secretName: Traefik serves its default *.h wildcard cert. `host` is passed
  # explicitly so an app's ingress hostname can differ from its resource name
  # (e.g. calibre-web-automated -> books.h.…).
  mkIngress = {
    name,
    port,
    host,
  }: {
    "${name}".spec = {
      ingressClassName = "traefik";
      tls = [{hosts = [host];}];
      rules = [
        {
          inherit host;
          http.paths = [
            {
              path = "/";
              pathType = "Prefix";
              backend.service = {
                inherit name;
                port.number = port;
              };
            }
          ];
        }
      ];
    };
  };
```

- [ ] **Step 2: Rewire `mkArrApp` to use them**

In `mkArrApp`, replace the inline `services."${name}".spec = {…}` and `ingresses."${name}" = {…}` blocks (keep everything else) so the `resources` attr ends with:

```nix
        services = mkService {inherit name port;};
        ingresses = mkIngress {
          inherit name port;
          host = "${name}${ingressSuffix}";
        };
```

The `host` local binding at the top of `mkArrApp` (`host = "${name}${ingressSuffix}";`) is now used only in that call — inline it as above and delete the standalone `let … host = …` binding if it becomes unused.

- [ ] **Step 3: Rewire `k8s/apps/torrent.nix` exposure**

Replace the `services.qbittorrent.spec = {…}` and `ingresses.qbittorrent = {…}` blocks (the tail of `resources`) with:

```nix
      services = l.mkService {
        name = "qbittorrent";
        port = 9091;
        portName = "webui";
      };
      ingresses = l.mkIngress {
        name = "qbittorrent";
        port = 9091;
        host = "qbittorrent${ingressSuffix}";
      };
```

Delete the now-unused `host = "qbittorrent${ingressSuffix}";` local at the top of the `let`. Keep `labels`.

- [ ] **Step 4: Format and build**

```bash
nix fmt && nix build --no-link '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
```

Expected: builds successfully (a store path prints).

- [ ] **Step 5: Assert byte-identity**

```bash
diff_workloads && echo "IDENTICAL"
```

Expected: prints only `IDENTICAL` (no diff lines). If any diff appears, fix the helper until it matches — do not change the baseline.

- [ ] **Step 6: Commit**

```bash
git add k8s/lib.nix k8s/apps/torrent.nix
git commit -m "refactor(k8s): extract mkService/mkIngress, dedup arr+qbittorrent exposure"
```

---

### Task 2: Extract `mkLsioContainer` + `mkLsioApp`; replace `mkArrApp`

**Files:**
- Modify: `k8s/lib.nix` (add `lsioEnv`, `mkLsioContainer`, `mkLsioApp`; remove `mkArrApp`)
- Modify: `k8s/apps/arr.nix` (call `mkLsioApp`)
- Modify: `k8s/apps/torrent.nix` (build qbittorrent container via `mkLsioContainer`)

**Interfaces:**
- Consumes: `appLabels`, `mkService`, `mkIngress` (Task 1).
- Produces:
  - `lsioEnv = { mediaUid, timezone }: [ … TZ/PUID/PGID env … ]`
  - `mkLsioContainer = { name, image, port, mediaUid, timezone, portName ? "http", configVolumeName ? "config", configMountPath ? "/config", extraEnv ? [], extraMounts ? [], probes ? {} }: <container attrs>`
  - `mkLsioApp = { name, image, port, ingressSuffix, mediaUid, timezone, configPath, namespace ? "media", portName ? "http", host ? "${name}${ingressSuffix}", extraVolumes ? [], extraMounts ? [], extraEnv ? [] }: { "${name}" = { namespace; createNamespace = false; resources = {…}; }; }`

- [ ] **Step 1: Add the three helpers to `k8s/lib.nix`**

Insert into the `rec { … }` body:

```nix
  # The env every LinuxServer.io image shares: timezone + PUID/PGID set to the
  # _media uid/gid (994) so files land _media-owned. LSIO images start as root
  # and drop to PUID/PGID via s6, so no runAsUser.
  lsioEnv = {
    mediaUid,
    timezone,
  }: [
    {
      name = "TZ";
      value = timezone;
    }
    {
      name = "PUID";
      value = toString mediaUid;
    }
    {
      name = "PGID";
      value = toString mediaUid;
    }
  ];

  # A single LinuxServer.io container: shared env (+ extraEnv), one named port,
  # a /config mount (+ extraMounts), optional probes. Used standalone inside the
  # torrent pod and as the container of mkLsioApp.
  mkLsioContainer = {
    name,
    image,
    port,
    mediaUid,
    timezone,
    portName ? "http",
    configVolumeName ? "config",
    configMountPath ? "/config",
    extraEnv ? [],
    extraMounts ? [],
    probes ? {},
  }:
    {
      inherit image;
      env = lsioEnv {inherit mediaUid timezone;} ++ extraEnv;
      ports.${portName}.containerPort = port;
      volumeMounts =
        [
          {
            name = configVolumeName;
            mountPath = configMountPath;
          }
        ]
        ++ extraMounts;
    }
    // probes;

  # A standalone single-container LSIO app: Deployment (fsGroup=994, Recreate,
  # /config hostPath from configPath) + Service + Ingress. Replaces mkArrApp.
  # `host` defaults to name-based but can be overridden (books.h.… for CWA).
  mkLsioApp = {
    name,
    image,
    port,
    ingressSuffix,
    mediaUid,
    timezone,
    configPath,
    namespace ? "media",
    portName ? "http",
    host ? "${name}${ingressSuffix}",
    extraVolumes ? [],
    extraMounts ? [],
    extraEnv ? [],
  }: let
    labels = appLabels name;
  in {
    "${name}" = {
      inherit namespace;
      createNamespace = false;
      resources = {
        deployments."${name}".spec = {
          replicas = 1;
          selector.matchLabels = labels;
          # Holds a SQLite/config lock on /config; never run two at once.
          strategy.type = "Recreate";
          template = {
            metadata.labels = labels;
            spec = {
              securityContext.fsGroup = mediaUid;
              containers."${name}" = mkLsioContainer {
                inherit name image port portName mediaUid timezone extraEnv extraMounts;
              };
              volumes =
                [
                  {
                    name = "config";
                    hostPath = {
                      path = configPath;
                      type = "Directory";
                    };
                  }
                ]
                ++ extraVolumes;
            };
          };
        };
        services = mkService {inherit name port portName;};
        ingresses = mkIngress {inherit name port host;};
      };
    };
  };
```

- [ ] **Step 2: Remove `mkArrApp` from `k8s/lib.nix`**

Delete the entire `mkArrApp = { … }: …;` definition (and its leading comment). `mkLsioApp` replaces it.

- [ ] **Step 3: Migrate `k8s/apps/arr.nix` to `mkLsioApp`**

Replace the three `l.mkArrApp (facts // { … })` calls with `l.mkLsioApp (facts // { … })`, adding the explicit `configPath` each formerly derived internally:

```nix
  applications = lib.mkMerge [
    (l.mkLsioApp (facts
      // {
        name = "prowlarr";
        image = "lscr.io/linuxserver/prowlarr:latest";
        port = 9696;
        configPath = "${mediaRoot}/docker-services/torrent-config/prowlarr";
      }))
    (l.mkLsioApp (facts
      // {
        name = "radarr";
        image = "lscr.io/linuxserver/radarr:latest";
        port = 7878;
        configPath = "${mediaRoot}/docker-services/torrent-config/radarr";
        extraVolumes = [mediaVolume];
        extraMounts = [mediaMount];
      }))
    (l.mkLsioApp (facts
      // {
        name = "sonarr";
        image = "lscr.io/linuxserver/sonarr:latest";
        port = 8989;
        configPath = "${mediaRoot}/docker-services/torrent-config/sonarr";
        extraVolumes = [mediaVolume];
        extraMounts = [mediaMount];
      }))
  ];
```

Keep the `mediaVolume`/`mediaMount` locals and the `facts` binding unchanged. (Images stay `:latest` here — pinning is Task 3, so this task stays byte-identical.)

- [ ] **Step 4: Migrate the qbittorrent container in `k8s/apps/torrent.nix`**

Replace the `qbittorrent = { … };` container block (inside `containers = { gluetun = …; qbittorrent = …; }`) with a `mkLsioContainer` call. Preserve the two explanatory comments above it:

```nix
              # Shares gluetun's netns (same pod) -> all its traffic transits the
              # VPN, exactly as --network=container:vpn did. VueTorrent installed
              # by the LinuxServer mod at /vuetorrent (matches the existing
              # WebUI\RootFolder=/vuetorrent config).
              qbittorrent = l.mkLsioContainer {
                name = "qbittorrent";
                image = "lscr.io/linuxserver/qbittorrent:latest";
                port = 9091;
                portName = "webui";
                inherit mediaUid timezone;
                configVolumeName = "qbt-config";
                extraEnv = [
                  {
                    name = "WEBUI_PORT";
                    value = "9091";
                  }
                  {
                    name = "TORRENTING_PORT";
                    value = "6881";
                  }
                  {
                    name = "DOCKER_MODS";
                    value = "ghcr.io/gabe565/linuxserver-mod-vuetorrent";
                  }
                ];
                extraMounts = [
                  {
                    name = "downloads";
                    mountPath = "/data/torrents/downloads";
                  }
                ];
                probes = {
                  readinessProbe = {
                    httpGet = {
                      path = "/";
                      port = 9091;
                    };
                    initialDelaySeconds = 20;
                    periodSeconds = 15;
                    timeoutSeconds = 8;
                    failureThreshold = 4;
                  };
                  livenessProbe = {
                    exec.command = [
                      "sh"
                      "-c"
                      "wget -q -T 8 -O /dev/null http://connectivitycheck.gstatic.com/generate_204 || curl -fsS -m 8 -o /dev/null http://connectivitycheck.gstatic.com/generate_204"
                    ];
                    initialDelaySeconds = 90;
                    periodSeconds = 30;
                    timeoutSeconds = 12;
                    failureThreshold = 6;
                  };
                };
              };
```

Leave the `gluetun` container, the pod `securityContext.fsGroup`, the `volumes` (tun/qbt-config/downloads), and the Deployment name `torrent-vpn` exactly as they are.

- [ ] **Step 5: Format and build**

```bash
nix fmt && nix build --no-link '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
```

Expected: builds successfully.

- [ ] **Step 6: Assert byte-identity**

```bash
diff_workloads && echo "IDENTICAL"
```

Expected: only `IDENTICAL`. Fix helpers until the diff is empty (common culprit: a stray `runAsUser`, a renamed volume, or a missing `extraEnv` entry).

- [ ] **Step 7: Commit**

```bash
git add k8s/lib.nix k8s/apps/arr.nix k8s/apps/torrent.nix
git commit -m "refactor(k8s): replace mkArrApp with layered mkLsioContainer/mkLsioApp"
```

---

### Task 3: Pin existing workload images

**Files:**
- Modify: `k8s/apps/arr.nix` (3 image lines)
- Modify: `k8s/apps/torrent.nix` (gluetun + qbittorrent image lines)

**Interfaces:** none new.

The digests below were read from the live cluster on 2026-07-23 (`kubectl … .status.containerStatuses[*].imageID`); pinning to them is a no-op on the running pods.

- [ ] **Step 1: Pin the arr images in `k8s/apps/arr.nix`**

```nix
        image = "lscr.io/linuxserver/prowlarr@sha256:2f3d31307beba3ba2dd226d191f5f5c14ee3b4d8b49277c64683f5ed97083179";
```
```nix
        image = "lscr.io/linuxserver/radarr@sha256:e35056574cdc695a9ee745aa1ecda9eab3842450bf4b7b8471b023790fa3861d";
```
```nix
        image = "lscr.io/linuxserver/sonarr@sha256:24acea2956a0ccb11f103877d9f4f8576600fb34bff34820ed749c2256dab89f";
```

- [ ] **Step 2: Pin the torrent-pod images in `k8s/apps/torrent.nix`**

gluetun:
```nix
                image = "qmcgaw/gluetun@sha256:ad6b604e0cecc917a5cb6a8de55cd167ba415da8b7ec13456abb871a84be3c30";
```
qbittorrent (in the `mkLsioContainer` call):
```nix
                image = "lscr.io/linuxserver/qbittorrent@sha256:b024436f8ca665d16d9a997d26fd27fdf867ee5566ba09f32764e7b2976d3e02";
```

- [ ] **Step 3: Format, build, assert only image lines changed**

```bash
nix fmt && nix build --no-link '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
diff_workloads
```

Expected: every diff line is a `- image: …:latest` / `+ image: …@sha256:…` pair — no other fields. (gluetun's digest is under `torrent/Deployment-torrent-vpn.yaml`.)

- [ ] **Step 4: Commit**

```bash
git add k8s/apps/arr.nix k8s/apps/torrent.nix
git commit -m "chore(k8s): pin arr/qbittorrent/gluetun images to running digests"
```

---

### Task 4: `library` namespace + NetworkPolicy

**Files:**
- Create: `k8s/infra/library-network.nix`
- Modify: `k8s/default.nix` (import it)

**Interfaces:**
- Produces: the `library` namespace (with `createNamespace = true`) that Tasks 5–6 deploy into.

- [ ] **Step 1: Create `k8s/infra/library-network.nix`**

```nix
# The ebook/audiobook apps live in their own namespace with the same boundary
# the `media` (torrent/arr) namespace has: default-deny-ingress, re-opened only
# for intra-namespace traffic and the Traefik ingress controller (kube-system).
# This keeps these apps isolated from the torrent stack.
{...}: {
  applications.library-network = {
    namespace = "library";
    createNamespace = true;
    yamls = [
      (builtins.toJSON {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "allow-intra-and-ingress";
          namespace = "library";
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

- [ ] **Step 2: Import it in `k8s/default.nix`**

Add to the `imports` list, next to `./infra/network.nix`:

```nix
    ./infra/library-network.nix
```

- [ ] **Step 3: Format and build**

```bash
nix fmt && nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
```

- [ ] **Step 4: Verify the namespace + policy render**

```bash
OUT=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
ls "$OUT"/library-network/
```

Expected: `Namespace-library.yaml` and `NetworkPolicy-allow-intra-and-ingress.yaml`.

- [ ] **Step 5: Commit**

```bash
git add k8s/infra/library-network.nix k8s/default.nix
git commit -m "feat(k8s): add isolated library namespace for the ebook/audiobook apps"
```

---

### Task 5: Calibre-Web-Automated workload

**Files:**
- Create: `k8s/apps/calibre-web-automated.nix`
- Modify: `k8s/default.nix` (import it)

**Interfaces:**
- Consumes: `mkLsioApp` (Task 2), the `library` namespace (Task 4), `_module.args` (`ingressSuffix`, `mediaRoot`, `mediaUid`, `timezone`).

- [ ] **Step 1: Resolve and pin the CWA image**

```bash
nix run nixpkgs#skopeo -- list-tags docker://ghcr.io/crocodilestick/calibre-web-automated | tail -20
# pick the newest stable release tag VERSION (not "latest"/"dev"/"nightly"), then:
nix run nixpkgs#skopeo -- inspect --format '{{.Digest}}' docker://ghcr.io/crocodilestick/calibre-web-automated:VERSION
```

Record `ghcr.io/crocodilestick/calibre-web-automated:VERSION@sha256:<digest>` for Step 2. (If registry access is blocked in the sandbox, the OPERATOR provides the digest.)

- [ ] **Step 2: Create `k8s/apps/calibre-web-automated.nix`**

Replace `IMAGE_PIN` with the value from Step 1.

```nix
# Calibre-Web-Automated: the EPUB/PDF library, replacing native calibre-web. A
# LinuxServer-lineage image, so it uses the shared mkLsioApp helper (root->994
# via PUID/PGID, fsGroup, Recreate). Reads the EXISTING Calibre library at
# ${mediaRoot}/books in place (same format + metadata.db as the old service);
# /cwa-book-ingest is CWA's BookDrop auto-import/convert folder. Local accounts
# now; native OIDC wired to Authelia later. See the design spec.
{
  lib,
  ingressSuffix,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
in {
  applications = l.mkLsioApp {
    name = "calibre-web-automated";
    image = "IMAGE_PIN";
    port = 8083;
    namespace = "library";
    host = "books${ingressSuffix}";
    configPath = "${mediaRoot}/apps/calibre-web-automated/config";
    inherit ingressSuffix mediaUid timezone;
    extraVolumes = [
      {
        name = "calibre-library";
        hostPath = {
          path = "${mediaRoot}/books";
          type = "Directory";
        };
      }
      {
        name = "ingest";
        hostPath = {
          path = "${mediaRoot}/apps/calibre-web-automated/ingest";
          type = "Directory";
        };
      }
    ];
    extraMounts = [
      {
        name = "calibre-library";
        mountPath = "/calibre-library";
      }
      {
        name = "ingest";
        mountPath = "/cwa-book-ingest";
      }
    ];
  };
}
```

- [ ] **Step 3: Import it in `k8s/default.nix`**

Add to `imports`:

```nix
    ./apps/calibre-web-automated.nix
```

- [ ] **Step 4: Format, build, verify render**

```bash
nix fmt && OUT=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
ls "$OUT"/calibre-web-automated/
grep -E 'host:|namespace:|@sha256' "$OUT"/calibre-web-automated/*.yaml
```

Expected: `Deployment-`, `Service-`, `Ingress-calibre-web-automated.yaml`; Ingress `host: books.h.abrahamwhite.com`; namespace `library`; the pinned digest present.

- [ ] **Step 5: Commit**

```bash
git add k8s/apps/calibre-web-automated.nix k8s/default.nix
git commit -m "feat(k8s): deploy Calibre-Web-Automated in the library namespace"
```

---

### Task 6: Audiobookshelf workload

**Files:**
- Create: `k8s/apps/audiobookshelf.nix`
- Modify: `k8s/default.nix` (import it)

**Interfaces:**
- Consumes: `appLabels`, `mkService`, `mkIngress` (Tasks 1–2), the `library` namespace (Task 4).

Audiobookshelf is **not** a LinuxServer image (Node app; `PORT` env, runs as an arbitrary UID), so it is hand-rolled and only borrows `mkService`/`mkIngress`.

- [ ] **Step 1: Resolve and pin the ABS image**

```bash
nix run nixpkgs#skopeo -- list-tags docker://ghcr.io/advplyr/audiobookshelf | tail -20
nix run nixpkgs#skopeo -- inspect --format '{{.Digest}}' docker://ghcr.io/advplyr/audiobookshelf:VERSION
```

Record `ghcr.io/advplyr/audiobookshelf:VERSION@sha256:<digest>` for Step 2.

- [ ] **Step 2: Create `k8s/apps/audiobookshelf.nix`**

Replace `IMAGE_PIN` with the value from Step 1.

```nix
# Audiobookshelf: the audiobook manager. Embedded SQLite (no DB server), no
# OPDS. Not a LinuxServer image (Node app: PORT env, arbitrary UID), so it is
# hand-rolled and runs as _media (994) so files it writes to the library stay
# _media-owned. Reads ${mediaRoot}/audiobooks (already backed up). Local auth
# now; native OIDC wired to Authelia later. See the design spec.
{
  lib,
  ingressSuffix,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  labels = l.appLabels "audiobookshelf";
in {
  applications.audiobookshelf = {
    namespace = "library";
    createNamespace = false;
    resources = {
      deployments.audiobookshelf.spec = {
        replicas = 1;
        selector.matchLabels = labels;
        # Holds a SQLite lock on /config; never run two at once.
        strategy.type = "Recreate";
        template = {
          metadata.labels = labels;
          spec = {
            securityContext = {
              runAsUser = mediaUid;
              runAsGroup = mediaUid;
              fsGroup = mediaUid;
            };
            containers.audiobookshelf = {
              image = "IMAGE_PIN";
              env = [
                {
                  name = "TZ";
                  value = timezone;
                }
                {
                  # Non-privileged port so it binds fine as non-root (994).
                  name = "PORT";
                  value = "13378";
                }
              ];
              ports.http.containerPort = 13378;
              volumeMounts = [
                {
                  name = "audiobooks";
                  mountPath = "/audiobooks";
                }
                {
                  name = "config";
                  mountPath = "/config";
                }
                {
                  name = "metadata";
                  mountPath = "/metadata";
                }
              ];
            };
            volumes = [
              {
                name = "audiobooks";
                hostPath = {
                  path = "${mediaRoot}/audiobooks";
                  type = "Directory";
                };
              }
              {
                name = "config";
                hostPath = {
                  path = "${mediaRoot}/apps/audiobookshelf/config";
                  type = "Directory";
                };
              }
              {
                name = "metadata";
                hostPath = {
                  path = "${mediaRoot}/apps/audiobookshelf/metadata";
                  type = "Directory";
                };
              }
            ];
          };
        };
      };
      services = l.mkService {
        name = "audiobookshelf";
        port = 13378;
      };
      ingresses = l.mkIngress {
        name = "audiobookshelf";
        port = 13378;
        host = "audiobooks${ingressSuffix}";
      };
    };
  };
}
```

- [ ] **Step 3: Import it in `k8s/default.nix`**

Add to `imports`:

```nix
    ./apps/audiobookshelf.nix
```

- [ ] **Step 4: Format, build, verify render**

```bash
nix fmt && OUT=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
ls "$OUT"/audiobookshelf/
grep -E 'host:|namespace:|@sha256' "$OUT"/audiobookshelf/*.yaml
```

Expected: three resources; Ingress `host: audiobooks.h.abrahamwhite.com`; namespace `library`; pinned digest present.

- [ ] **Step 5: Commit**

```bash
git add k8s/apps/audiobookshelf.nix k8s/default.nix
git commit -m "feat(k8s): deploy Audiobookshelf in the library namespace"
```

---

### Task 7: Pre-create app-state dirs + ensure books is _media-writable

**Files:**
- Modify: `machine/globalhawk/disks.nix` (tmpfiles rules)

**Interfaces:** none (host-layer).

hostPath volumes do not honor `fsGroup` chown, so the app-state dirs must exist with the right owner before the pods mount them. CWA runs as 994 and writes to `${mediaRoot}/books`, so that dir must be `_media`-group-writable (today it carries only a `calibre-web`-group ACL).

- [ ] **Step 1: Add tmpfiles rules in `machine/globalhawk/disks.nix`**

In the `systemd.tmpfiles.rules` list, after the existing `A ${facts.mediaRoot}/books …` line, add:

```nix
    # App state for the k3s ebook/audiobook workloads (hostPath ignores fsGroup,
    # so the dirs must pre-exist _media-owned for the pods to write).
    "d ${facts.mediaRoot}/apps/calibre-web-automated 0775 _media _media -"
    "d ${facts.mediaRoot}/apps/calibre-web-automated/config 0775 _media _media -"
    "d ${facts.mediaRoot}/apps/calibre-web-automated/ingest 0775 _media _media -"
    "d ${facts.mediaRoot}/apps/audiobookshelf 0775 _media _media -"
    "d ${facts.mediaRoot}/apps/audiobookshelf/config 0775 _media _media -"
    "d ${facts.mediaRoot}/apps/audiobookshelf/metadata 0775 _media _media -"
    # CWA runs as _media (994); grant the _media group rwx on the library so it
    # can write (the calibre-web-group ACL above is removed at decommission).
    "A ${facts.mediaRoot}/books - - - - group:_media:rwx"
```

- [ ] **Step 2: Format and build the system**

```bash
nix fmt && nix build --no-link '.#nixosConfigurations.globalhawk.config.system.build.toplevel'
```

Expected: builds successfully.

- [ ] **Step 3: Commit**

```bash
git add machine/globalhawk/disks.nix
git commit -m "feat(globalhawk): provision app-state dirs + _media ACL for the k3s book apps"
```

---

### Task 8: Back up the new app state

**Files:**
- Modify: `machine/globalhawk/backup.nix` (restic `paths`)

**Interfaces:** none.

Book/audiobook *files* and CWA's `metadata.db` (inside `books/`) are already in the restic set; the app-state dirs (CWA `/config`, ABS `config`+`metadata`) are not.

- [ ] **Step 1: Add the app-state dirs to `paths` in `machine/globalhawk/backup.nix`**

After the existing `"${facts.mediaRoot}/music"` entry, add:

```nix
      # App state for the k3s ebook/audiobook workloads. SQLite files are
      # captured as a filesystem snapshot (crash-consistent-ish); a sqlite3
      # .backup pre-hook is a possible later hardening (see design spec).
      "${facts.mediaRoot}/apps/calibre-web-automated"
      "${facts.mediaRoot}/apps/audiobookshelf"
```

- [ ] **Step 2: Format and build the system**

```bash
nix fmt && nix build --no-link '.#nixosConfigurations.globalhawk.config.system.build.toplevel'
```

- [ ] **Step 3: Commit**

```bash
git add machine/globalhawk/backup.nix
git commit -m "feat(globalhawk): back up CWA + audiobookshelf app state"
```

---

### Task 9: Cutover — disable native calibre-web

**Files:**
- Modify: `program/calibre-web/default.nix` (`enable = false` + rollback note)

**Interfaces:** none.

CWA and native calibre-web share `${mediaRoot}/books/metadata.db`; two writers risk SQLite corruption, so the native service is disabled at cutover. The module, user/group, `group:calibre-web:rwx` ACL, and firewall port 8083 stay for now — re-enabling is the rollback; full decommission is a deferred follow-up.

- [ ] **Step 1: Disable the service in `program/calibre-web/default.nix`**

Change `enable = true;` to `enable = false;` and add a comment:

```nix
  services.calibre-web = {
    # Cutover to k3s Calibre-Web-Automated (books.h.…), which shares this same
    # ${mediaRoot}/books library + metadata.db — so the two must not both run as
    # writers. Disabled, not removed: `enable = true;` + switch is the one-line
    # rollback (library format is unchanged). Full decommission (module, user,
    # ACL, port 8083) is a deferred follow-up once CWA is validated.
    enable = false;
    listen.ip = "0.0.0.0";
    options = {
      enableBookUploading = true;
    };
  };
```

- [ ] **Step 2: Format and build the system**

```bash
nix fmt && nix build --no-link '.#nixosConfigurations.globalhawk.config.system.build.toplevel'
```

- [ ] **Step 3: Full flake check**

```bash
nix flake check
```

Expected: evaluates cleanly (all outputs).

- [ ] **Step 4: Commit**

```bash
git add program/calibre-web/default.nix
git commit -m "feat(globalhawk): disable native calibre-web ahead of the CWA cutover"
```

---

### Task 10: OPERATOR — deploy & validate

**These steps require sudo / write-`kubectl` / a browser and are run by the operator (abe), not the sandbox agent.** Do them in order; the cutover (native calibre-web off, CWA on) happens atomically in the single `switch`.

- [ ] **Step 1: Apply**

```bash
sudo nixos-rebuild switch --flake .#globalhawk
```

Note: a `switch` may exit 101 at a dbus reload while activation still applies — verify with `readlink /run/current-system` if so.

- [ ] **Step 2: Cluster health**

```bash
kubectl get ns library
kubectl get pods -n library -o wide          # calibre-web-automated + audiobookshelf Running
kubectl get pods -n media -o wide            # prowlarr/radarr/sonarr/torrent-vpn still Running
nix run .#k3s-drift                          # no orphans / no hand-created drift
```

- [ ] **Step 3: Regression — the refactor/pin didn't disturb the torrent stack**

```bash
# VPN egress leak test (must still report connected):
kubectl exec -n media deploy/torrent-vpn -c gluetun -- wget -qO- https://am.i.mullvad.net/connected
# arr -> qbit reachability unaffected (spot-check radarr UI at radarr.h.abrahamwhite.com)
```

- [ ] **Step 4: CWA validation** (`https://books.h.abrahamwhite.com`)

- Create the CWA admin account; confirm the **existing library is listed** (proves it read the pre-existing `metadata.db`).
- Upload an EPUB via the web UI → it appears with metadata.
- Drop a file into `${mediaRoot}/apps/calibre-web-automated/ingest` → it auto-imports/converts into the library.
- Fetch `https://books.h.abrahamwhite.com/opds` → catalog served.
- Confirm native calibre-web is down: `systemctl status calibre-web` → inactive; port 8083 no longer answers.

- [ ] **Step 5: Audiobookshelf validation** (`https://audiobooks.h.abrahamwhite.com`)

- Create the ABS admin account; add `/audiobooks` as a library → it scans and fetches audiobook metadata.
- Upload an audiobook via the web UI → appears and plays.

- [ ] **Step 6: Ownership check**

```bash
# Files written by both apps must be _media-owned (994):
ls -ln ${mediaRoot}/apps/calibre-web-automated/config ${mediaRoot}/apps/audiobookshelf/config
```

Expected: owner/group `994`.

- [ ] **Step 7: Report back** — if CWA is confirmed good, the deferred decommission (remove the calibre-web module, user/group, `group:calibre-web:rwx` ACL, and firewall port 8083) can be scheduled. If not, rollback = set `services.calibre-web.enable = true;` and switch.

---

## Deferred follow-ups (separate plans)

1. **Decommission native calibre-web:** remove `services.calibre-web` (the `program/calibre-web` import), `users.users.calibre-web`, the `group:calibre-web:rwx` ACL on `books`, and port `8083` from the firewall.
2. **OIDC:** register CWA + ABS as Authelia OIDC clients; carve `/opds` + `/api` out of any forward-auth middleware. Lands with the parked SSO spec.
3. **Komga** for comics, if the CBR collection grows — its own SQLite pod in `library`.

## Self-Review

- **Spec coverage:** app stack (T5, T6), `library` ns + NetworkPolicy (T4), CWA-in-place library reuse (T5), local auth / no secrets (all — none added), atomic cutover + rollback (T9, T10), backups (T8), storage/ownership tmpfiles + ACL (T7), LinuxServer abstraction + render-equivalence gate (T1–T2, T0), image pinning incl. existing (T3, and new in T5/T6), ingress/TLS/friendly hostnames (T5/T6 via `host`), validation (T10). Out-of-scope items (decommission, OIDC, Komga) listed as deferred. ✅
- **Placeholder scan:** the only literal placeholders are `IMAGE_PIN`/`VERSION` for the two new images, each with an explicit resolve command (T5S1, T6S1) — deliberate, since the current release is resolved at implementation time. No TODO/TBD/"handle errors" left.
- **Type consistency:** `mkService {name,port,portName?}`, `mkIngress {name,port,host}`, `mkLsioContainer {…,port,portName?,configVolumeName?,probes?,…}`, `mkLsioApp {…,configPath,namespace?,host?,extra*?}` — names/params match across their definitions (T1/T2) and every call site (arr T2, torrent T1/T2, CWA T5, ABS T6). ✅
