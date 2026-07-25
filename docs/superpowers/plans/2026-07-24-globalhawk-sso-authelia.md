# globalhawk SSO (Authelia) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stand up Authelia in the k3s cluster as the single sign-on provider — native OIDC for Immich/Audiobookshelf/Calibre-Web, Traefik forward-auth for the arr stack + qBittorrent, with AdGuard surfaced by name but SSO-excepted — and migrate host mail off Gmail onto a provider-neutral SMTP secret.

**Architecture:** Authelia is a nixidy Helm release (chart vendored as a fixed-output derivation via `lib.helm.downloadHelmChart`, values authored in Nix under `configMap.*`) in a new `auth` namespace. Its ~6 secrets and the two-user store are sops-rendered k8s Secrets. Forward-auth is one Traefik `Middleware` CRD applied per-ingress by annotation. Each OIDC app is configured by a small reconciler Job that writes only the OIDC block through the app's own settings API, leaving the rest of the app UI editable. AdGuard is surfaced Plex-style (selector-less Service + EndpointSlice → host `:3000`).

**Tech Stack:** NixOS, nixidy (k8s-as-Nix), k3s + bundled Traefik, cert-manager (wildcard cert, already present), sops-nix, msmtp. This is a Nix repo — the unit of work is a Nix evaluation that succeeds or fails; there is no test framework, so **each authoring task's gate is `nixos-rebuild build --flake .#globalhawk` succeeding**, and functional checks are behavioural steps the operator runs after `switch`.

## Global Constraints

- **Roles:** the **agent** (sandbox user, uid 1001, read-only, no sudo) authors Nix, runs `nixos-rebuild build --flake .#globalhawk` and the read-only `nix run .#k3s-drift` to validate, and computes fixed-output hashes. The **operator** (`abe`) runs every `switch`, `sops edit`, `kubectl`, and each app's one-time admin/API-key bootstrap. Steps are labelled **(agent)** or **(operator)**.
- **Ingress suffix:** every app host is `<name>${facts.ingressSuffix}` = `<name>.h.abrahamwhite.com`. The portal is `auth.h.abrahamwhite.com`. Traefik's default wildcard cert covers all of `*.h.abrahamwhite.com` — **no per-app cert/secretName** (see `mkIngress` in `k8s/lib.nix`).
- **Secrets via sops only:** runtime secrets are declared in `machine/globalhawk/sops.nix` and rendered as k8s Secrets straight into `/var/lib/rancher/k3s/server/manifests/` (mode 0400, root, `sops-` filename prefix). No plaintext ever enters `/nix/store`.
- **Public repo:** never write a `secrets/*` literal (email addresses, passwords, keys, client secrets) into an unencrypted committed file. Reference `config.sops.placeholder.<name>`, the `secrets.*` Nix attr path, or a generic description only.
- **k3s prune-on-switch:** all workloads flow through the single `nixidyCombined` manifest; adding/removing a workload is edit + `switch` (auto-prunes). Never delete the `nixidy` manifests source. Verify desired-vs-live with `nix run .#k3s-drift`.
- **k3s-drift owner prefixes:** sops-rendered Secrets carry the `sops-` Addon prefix; the drift tool already knows it. New sops Secrets follow the `sops-<name>.yaml` filename convention.
- **Auth patterns (from the spec):** OIDC = Immich, Audiobookshelf, Calibre-Web-Automated (family, one-factor). Forward-auth = radarr/sonarr/prowlarr/qbittorrent (admins, two-factor). SSO-excepted, ingress-surfaced = Plex (done) + AdGuard. `default_policy: deny`.
- **Spec:** `docs/superpowers/specs/2026-07-23-globalhawk-sso-authelia-design.md`.

## File Structure

- `machine/globalhawk/facts.nix` (modify) — add `autheliaUid = 987`.
- `machine/globalhawk/sops.nix` (modify) — retire `gmail_password`; add `smtp_password`; add the Authelia secret scalars + the two-user store; render the `authelia-secrets`, `authelia-users`, and per-app OIDC-client k8s Secrets.
- `machine/globalhawk/disks.nix` (modify) — repoint msmtp from Gmail to the new provider; `passwordeval` → `smtp_password`.
- `machine/globalhawk/backup.nix` (modify) — update the root alias / `from` address.
- `machine/globalhawk/authelia-storage.nix` (create) — the `authelia` user/group + the tmpfiles-owned SQLite dir (mirrors `immich-storage.nix`).
- `machine/globalhawk/default.nix` (modify) — import `./authelia-storage.nix`.
- `charts/authelia/default.nix` (create) — the vendored chart pin (`repo`/`chart`/`version`/`chartHash`).
- `k8s/default.nix` (modify) — set `nixidy.chartsDir = ../charts`; import the new infra + app modules.
- `k8s/infra/auth-network.nix` (create) — the `auth` namespace + default-deny-ingress NetworkPolicy.
- `k8s/infra/forward-auth.nix` (create) — the Traefik `Middleware` CRD.
- `k8s/apps/authelia.nix` (create) — the Authelia Helm release + values + the persistence/OIDC-clients `resources` patch.
- `k8s/apps/adguard.nix` (create) — AdGuard ExternalName/EndpointSlice + plain Ingress (mirrors `plex.nix`).
- `k8s/apps/arr.nix` (modify) — add the forward-auth middleware annotation to each ingress.
- `k8s/apps/torrent.nix` (modify) — add the forward-auth middleware annotation to the qbittorrent ingress.
- `k8s/lib.nix` (modify) — extend `mkIngress` with an optional `annotations` arg.
- `k8s/apps/oidc-reconcilers.nix` (create) — the Immich / ABS / CWA OIDC reconciler Jobs + their ConfigMap scripts.
- `flake.nix` (modify) — thread `autheliaUid` into the nixidy env `_module.args`.

---

## Phase A — Email migration (provider-neutral SMTP, host-wide)

Independent of the cluster; done first because it also proves SMTP for Authelia's password-reset notifier.

### Task A1: Retire `gmail_password`, add provider-neutral `smtp_password`

**Files:**
- Modify: `machine/globalhawk/sops.nix` (secrets block)
- Modify: `machine/globalhawk/disks.nix:78-104` (the `programs.msmtp` block)
- Modify: `machine/globalhawk/backup.nix` (root alias / from address)

**Interfaces:**
- Produces: `config.sops.secrets.smtp_password.path` (host file consumed by msmtp and, later, Authelia).

- [ ] **Step 1 (agent): swap the secret declaration**

In `machine/globalhawk/sops.nix`, in the `secrets = { … }` block, remove the `gmail_password = {};` line and add:

```nix
      # Outbound SMTP password (an Apple app-specific password today). Named for
      # the ROLE, not the provider — a future mail-provider swap is a value
      # change, not a rename. Consumed by host msmtp (disks.nix) and Authelia's
      # notifier (k8s/apps/authelia.nix, via the authelia-secrets Secret).
      smtp_password = {};
```

- [ ] **Step 2 (agent): repoint msmtp**

In `machine/globalhawk/disks.nix`, in the `programs.msmtp` account, change the provider fields. Replace the Gmail host/user/from and the `passwordeval`:

```nix
        host = "smtp.mail.me.com";
        port = 587;
        tls = true;
        tls_starttls = true;
        auth = true;
        # The account username + From must be the operator's iCloud custom-domain
        # address (kept in secrets/, git-crypt). Reference the attr path, never
        # the literal — this is a public repo.
        user = secrets.mail.smtpUser;
        from = secrets.mail.fromAddress;
        passwordeval = "cat ${config.sops.secrets.smtp_password.path}";
```

> If `disks.nix` does not already `import ../../secrets/globalhawk.nix`, add `secrets = import ../../secrets/globalhawk.nix;` to its `let` block (see `adguard.nix:10` for the pattern). The `mail.smtpUser` / `mail.fromAddress` attrs are added to `secrets/globalhawk.nix` by the operator in Step 5.

- [ ] **Step 3 (agent): update the alias / from in backup.nix**

In `machine/globalhawk/backup.nix`, wherever the failure mail targets `root` (aliased to the Gmail address) or sets a `from`, update the comment and any literal to reference `secrets.mail.fromAddress` instead of the Gmail address. If the alias lives in `disks.nix`'s `aliases` block (`root: …@gmail.com`), change it there to `root: ${secrets.mail.fromAddress}`.

- [ ] **Step 4 (agent): build-validate**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds successfully. (Build does not decrypt sops, so a not-yet-populated `smtp_password` is fine here; the missing secret only surfaces at `switch`/activation.)

- [ ] **Step 5 (operator): populate the secret + custom-domain address**

```sh
# Generate an app-specific password at appleid.apple.com, then:
sops secrets/globalhawk.sops.yaml   # add: smtp_password: <app-specific-password>
$EDITOR secrets/globalhawk.nix      # add: mail = { smtpUser = "…"; fromAddress = "…"; };
```
(The `.nix` file is git-crypt, not sops — see the immich `immich_pass` precedent.)

- [ ] **Step 6 (operator): switch + verify mail end-to-end**

```sh
sudo nixos-rebuild switch --flake .#globalhawk
echo "authelia-smtp preflight $(date)" | msmtp -a default <operator-address>
# Force a ZED/smartd path if desired; confirm the mail arrives via iCloud.
```
Expected: mail delivered from the custom-domain address. If it bounces, check the iCloud constraint that `from` must be a verified alias (spec open item).

- [ ] **Step 7 (agent): commit**

```bash
git add machine/globalhawk/sops.nix machine/globalhawk/disks.nix machine/globalhawk/backup.nix
git commit -m "feat(globalhawk): move host mail to a provider-neutral SMTP secret

Password-reset for SSO needs a notifier, and the operator is leaving Gmail
for an iCloud custom domain. Naming the secret for its role (smtp_password)
rather than the provider keeps a future swap to a value change."
```

---

## Phase B — Authelia core

### Task B1: Authelia service identity + persistent SQLite dir

**Files:**
- Modify: `machine/globalhawk/facts.nix`
- Create: `machine/globalhawk/authelia-storage.nix`
- Modify: `machine/globalhawk/default.nix` (imports list)
- Modify: `flake.nix` (thread `autheliaUid` into the nixidy env args)

**Interfaces:**
- Produces: `facts.autheliaUid` (= 987); the `authelia` user/group; the owned dir `/var/lib/authelia` (mode 0750) for the SQLite storage volume.

- [ ] **Step 1 (agent): add the uid fact**

In `machine/globalhawk/facts.nix`, under `--- media / storage ---` (after `immichUid`), add:

```nix
  # The `authelia` service uid/gid — Authelia's k8s pod runs as it and its
  # SQLite state dir (/var/lib/authelia) is owned by it. 987 is free in both
  # namespaces (988 = immich, 994 = _media).
  autheliaUid = 987;
```

- [ ] **Step 2 (agent): create the storage-identity module**

Create `machine/globalhawk/authelia-storage.nix` (mirrors `immich-storage.nix`, but the dir is on the root fs, not the media pool — Authelia state is tiny):

```nix
# Host-side identity + a persistent dir for Authelia's SQLite storage (the k8s
# workload lives in k8s/apps/authelia.nix). Authelia runs as its OWN uid and its
# state dir is 0750 authelia:authelia. The pod mounts /var/lib/authelia via a
# hostPath patched onto the Helm-rendered Deployment.
{
  config,
  facts,
  ...
}: let
  uid = facts.autheliaUid;
in {
  users.users.authelia = {
    isSystemUser = true;
    group = "authelia";
    uid = uid;
    description = "Authelia SSO";
  };
  users.groups.authelia.gid = uid;

  systemd.tmpfiles.rules = [
    "d /var/lib/authelia 0750 authelia authelia - -"
  ];
}
```

> `facts` reaches this module the same way `immich-storage.nix` receives it. If host modules import facts directly (`facts = import ./facts.nix;` in a `let`), use that form instead of the `facts` arg — match whatever `immich-storage.nix` does.

- [ ] **Step 3 (agent): import it**

In `machine/globalhawk/default.nix`, add `./authelia-storage.nix` to the imports list (next to `./immich-storage.nix`).

- [ ] **Step 4 (agent): thread the uid into nixidy**

In `flake.nix` at the `nixidyEnvs` `_module.args` / `inherit (facts) …` site (line ~136, where `immichUid` is threaded), add `autheliaUid`:

```nix
                inherit (facts) ingressSuffix podCidr serviceCidr hostGatewayIp mediaRoot mediaUid timezone immichUid autheliaUid;
```

- [ ] **Step 5 (agent): build-validate + commit**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds. Then:

```bash
git add machine/globalhawk/facts.nix machine/globalhawk/authelia-storage.nix machine/globalhawk/default.nix flake.nix
git commit -m "feat(globalhawk): authelia service uid + persistent state dir

Authelia's SQLite (TOTP/WebAuthn registrations, reset tokens) must survive
pod restarts and stay isolated from other service uids."
```

### Task B2: sops — Authelia secrets + two-user store

**Files:**
- Modify: `machine/globalhawk/sops.nix`

**Interfaces:**
- Produces (k8s Secrets in `auth` ns): `authelia-secrets` (keys `jwt`, `session`, `storage-encryption`, `oidc-hmac`, `oidc-issuer-key`, `smtp-password`) and `authelia-users` (key `users_database.yml`). Secret/key names are load-bearing — referenced by the chart values in Task B5.

- [ ] **Step 1 (agent): declare the scalar secrets**

In `machine/globalhawk/sops.nix` `secrets = { … }`, add (these render to `/run/secrets/<name>` and, more importantly, define `config.sops.placeholder.<name>` for the templates below):

```nix
      authelia_jwt = {};
      authelia_session = {};
      authelia_storage_encryption = {};
      authelia_oidc_hmac = {};
      authelia_oidc_issuer_key = {};          # RSA private key PEM (multiline)
      authelia_users = {};                     # the whole users_database.yml
      # per-app OIDC client secrets (plaintext side lives with the app reconciler
      # in Task D; the HASH used by Authelia is in authelia_oidc_clients below)
      authelia_oidc_clients = {};              # rendered clients config fragment
```

- [ ] **Step 2 (agent): render the multi-key `authelia-secrets` Secret**

In the `templates = { … }` block, add (follows the exact `sops-<name>.yaml` pattern already used for `sops-immich-db.yaml`):

```nix
      "sops-authelia-secrets.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-secrets.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-secrets
            namespace: auth
          type: Opaque
          stringData:
            jwt: ${config.sops.placeholder.authelia_jwt}
            session: ${config.sops.placeholder.authelia_session}
            storage-encryption: ${config.sops.placeholder.authelia_storage_encryption}
            oidc-hmac: ${config.sops.placeholder.authelia_oidc_hmac}
            smtp-password: ${config.sops.placeholder.smtp_password}
        '';
      };
```

> The OIDC issuer private key is multiline PEM; k8s `stringData` cannot inline it via a single-line placeholder cleanly. Render it as its own Secret so YAML indentation is controlled:

```nix
      "sops-authelia-oidc-key.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-oidc-key.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-oidc-key
            namespace: auth
          type: Opaque
          stringData:
            issuer.pem: |
          ${lib.concatMapStringsSep "\n" (l: "      " + l) (lib.splitString "\n" config.sops.placeholder.authelia_oidc_issuer_key)}
        '';
      };
```

> If the indentation helper proves fiddly at eval, the simpler robust alternative is `data:` with a pre-base64'd value in sops — but prefer `stringData` + the indent helper first; validate by inspecting the rendered file on the box in Step 5.

- [ ] **Step 3 (agent): render the users store**

```nix
      "sops-authelia-users.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-users.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-users
            namespace: auth
          type: Opaque
          stringData:
            users_database.yml: |
          ${lib.concatMapStringsSep "\n" (l: "      " + l) (lib.splitString "\n" config.sops.placeholder.authelia_users)}
        '';
      };
```

- [ ] **Step 4 (agent): build-validate**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds. (Not yet applied — the `auth` namespace does not exist until Task B3+B5 land, so these Secrets should be committed but the operator applies them together with the namespace in Task B5's switch. Ordering is safe: k3s retries applying a Secret until its namespace exists.)

- [ ] **Step 5 (operator): populate the secrets**

```sh
sops secrets/globalhawk.sops.yaml
```
Add: 64-char random values for `authelia_jwt`, `authelia_session`, `authelia_storage_encryption`, `authelia_oidc_hmac` (`openssl rand -hex 32` each); an RSA private key for `authelia_oidc_issuer_key` (`openssl genrsa 4096`); and `authelia_users` = a `users_database.yml` with two users (operator in group `admins`, wife in group `family`), argon2id hashes from `authelia crypto hash generate argon2` (run in a throwaway `authelia:latest` container). Leave `authelia_oidc_clients` empty until Phase D.

- [ ] **Step 6 (agent): commit**

```bash
git add machine/globalhawk/sops.nix
git commit -m "feat(globalhawk): sops secrets for Authelia (keys + user store)

Reuses the established sops.templates -> k3s manifests lane so no Authelia
secret enters the world-readable store."
```

### Task B3: `auth` namespace + NetworkPolicy

**Files:**
- Create: `k8s/infra/auth-network.nix`
- Modify: `k8s/default.nix` (imports)

**Interfaces:**
- Produces: the `auth` namespace with default-deny-ingress, re-opened for intra-namespace + `kube-system` (Traefik). Consumed by the Authelia release (Task B5).

- [ ] **Step 1 (agent): author the module** (copy of `immich-network.nix`, ns renamed)

Create `k8s/infra/auth-network.nix`:

```nix
# Authelia lives in its own namespace with the same boundary as media/immich:
# default-deny-ingress, re-opened only for intra-namespace traffic and the
# Traefik ingress controller (kube-system). Traefik is the sole external source
# and it fronts the portal, the forward-auth callback, AND the OIDC endpoints,
# so no other cross-namespace ingress is needed.
{...}: {
  applications.auth-network = {
    namespace = "auth";
    createNamespace = true;
    yamls = [
      (builtins.toJSON {
        apiVersion = "networking.k8s.io/v1";
        kind = "NetworkPolicy";
        metadata = {
          name = "allow-intra-and-ingress";
          namespace = "auth";
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

- [ ] **Step 2 (agent): import it** — add `./infra/auth-network.nix` to `k8s/default.nix` imports.

- [ ] **Step 3 (agent): build-validate + commit**

Run: `nixos-rebuild build --flake .#globalhawk` → builds.

```bash
git add k8s/infra/auth-network.nix k8s/default.nix
git commit -m "feat(globalhawk): auth namespace with default-deny-ingress"
```

### Task B4: vendor the Authelia Helm chart

**Files:**
- Create: `charts/authelia/default.nix`
- Modify: `k8s/default.nix` (`nixidy.chartsDir`)

**Interfaces:**
- Produces: `charts.authelia.authelia` (the chart derivation, via `nixidy.chartsDir` → `lib.helm.mkChartAttrs`), consumed by the release in Task B5.

- [ ] **Step 1 (agent): pin the chart**

Create `charts/authelia/default.nix` (the `chartsDir` structure is `<repoName>/<chartName>/default.nix`; here the repo folder is `authelia`):

```nix
{
  repo = "https://charts.authelia.com";
  chart = "authelia";
  version = "0.9.x"; # PLACEHOLDER — replace with the exact version resolved in Step 2
  chartHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; # replace in Step 2
}
```

- [ ] **Step 2 (agent): resolve the real version + hash**

Resolve the latest chart version and its FOD hash (mirrors resolving image digests in the immich plan). With network available to the Nix daemon:

```sh
# Discover the latest version:
nix run nixpkgs#kubernetes-helm -- repo add authelia https://charts.authelia.com
nix run nixpkgs#kubernetes-helm -- search repo authelia/authelia --versions | head
```
Put the chosen `version` into `charts/authelia/default.nix`, then obtain `chartHash` by building with the placeholder hash and reading the `got:` value:

```sh
nix build --impure --expr '(builtins.getFlake (toString ./.)).inputs.nixidy.lib.helm.downloadHelmChart { repo = "https://charts.authelia.com"; chart = "authelia"; version = "<chosen>"; chartHash = ""; }' 2>&1 | sed -n 's/.*got: *//p'
```
Paste the returned `sha256-…` into `chartHash`.

- [ ] **Step 3 (agent): point nixidy at the charts dir**

In `k8s/default.nix`, add to the module body (top level, alongside `nixidy.target.*`):

```nix
  # Vendored Helm charts (FODs). mkChartAttrs walks this dir for default.nix
  # files and exposes them as the `charts` arg to every module.
  nixidy.chartsDir = ../charts;
```

- [ ] **Step 4 (agent): build-validate + commit**

Run: `nixos-rebuild build --flake .#globalhawk`
Expected: builds — the FOD fetch succeeds with the correct hash. (This validates the vendored chart resolves before we consume it.)

```bash
git add charts/authelia/default.nix k8s/default.nix
git commit -m "feat(globalhawk): vendor the Authelia Helm chart as a FOD

First use of nixidy's Helm path in this repo; chart pinned + hashed so the
build stays pure and offline after fetch."
```

### Task B5: the Authelia Helm release + values + persistence patch

**Files:**
- Create: `k8s/apps/authelia.nix`
- Modify: `k8s/default.nix` (imports)
- Modify: `flake.nix` (thread `smtpSender`/`smtpUser` into the nixidy env `_module.args`, mirroring `acmeEmail`)

**Interfaces:**
- Consumes: `charts.authelia.authelia`; the `authelia-secrets` / `authelia-oidc-key` / `authelia-users` Secrets (Task B2); `facts.autheliaUid`, `facts.ingressSuffix`; `smtpSender`/`smtpUser` (git-crypt, threaded via flake).
- Produces: the Authelia Deployment/Service/ConfigMap/Ingress in `auth` ns, reachable at `auth.h.abrahamwhite.com`; session cookie domain `h.abrahamwhite.com`.

- [ ] **Step 1 (agent): confirm the chart's exact value paths**

The chart templates Authelia's whole config under `configMap.*` and references secrets inline via `.secret_name`. Confirm the exact keys for THIS version before authoring:

```sh
nix build --impure --expr '(builtins.getFlake (toString ./.)).inputs.nixidy.lib.helm.downloadHelmChart { repo = "https://charts.authelia.com"; chart = "authelia"; version = "<chosen>"; chartHash = "<hash>"; }'
sed -n '1,400p' result/values.yaml | less   # locate: configMap.session, .storage.local, .notifier.smtp,
                                            # .authentication_backend.file, .access_control, .identity_providers.oidc,
                                            # .identity_validation.reset_password.secret.secret_name,
                                            # pod.securityContext, ingress, service
```
Record the real nesting; the block below is the intended configuration — adjust key paths to match the version's schema.

- [ ] **Step 2 (agent): author the release**

Create `k8s/apps/authelia.nix`:

```nix
# Authelia SSO — a nixidy Helm release (chart vendored in ../../charts/authelia).
# The whole Authelia config is authored here as Nix under `values.configMap.*`;
# secrets are referenced inline by `.secret_name` pointing at the sops-rendered
# Secrets (machine/globalhawk/sops.nix). Persistence (SQLite) is added by a
# `resources` patch mounting the host /var/lib/authelia (see authelia-storage.nix).
{
  lib,
  charts,
  ingressSuffix,
  autheliaUid,
  smtpSender,
  smtpUser,
  ...
}: let
  host = "auth${ingressSuffix}";
  cookieDomain = "h.abrahamwhite.com";
in {
  applications.authelia = {
    namespace = "auth";
    createNamespace = false; # created by infra/auth-network.nix
    helm.releases.authelia = {
      chart = charts.authelia.authelia;
      values = {
        # Run as the dedicated uid; fsGroup lets it own the mounted state dir.
        pod.securityContext = {
          pod.fsGroup = autheliaUid;
          container = {
            runAsUser = autheliaUid;
            runAsGroup = autheliaUid;
          };
        };
        service.port = 9091;
        ingress = {
          enabled = true;
          className = "traefik";
          certManager = false; # Traefik default wildcard cert
          tls.enabled = true;
          annotations = {}; # no forward-auth on the portal itself
          hosts = [{host = host; path = "/";}]; # confirm exact shape in Step 1
        };
        configMap = {
          theme = "auto";
          server.address = "tcp://0.0.0.0:9091";
          log.level = "info";

          # Single replica -> in-memory session store, no Redis.
          session = {
            name = "authelia_session";
            same_site = "lax";
            cookies = [
              {
                domain = cookieDomain;
                authelia_url = "https://${host}";
                default_redirection_url = "https://${host}";
              }
            ];
            encryption_key.secret_name = "authelia-secrets"; # key: session (confirm)
          };

          # SQLite on the mounted host dir (persistence patch below).
          storage = {
            encryption_key.secret_name = "authelia-secrets"; # key: storage-encryption
            local = {
              enabled = true;
              path = "/data/db.sqlite3";
            };
          };

          # File-based user store from the sops-rendered Secret (mounted below).
          authentication_backend.file = {
            path = "/users/users_database.yml";
            password.algorithm = "argon2";
          };

          # Password-reset over the provider-neutral SMTP secret.
          identity_validation.reset_password.secret.secret_name = "authelia-secrets"; # key: jwt
          notifier.smtp = {
            address = "smtp://smtp.mail.me.com:587"; # provider host is public, not secret
            # sender/username are the operator's email — a git-crypt value threaded
            # into the nixidy env exactly like acmeEmail (flake _module.args ->
            # cert-manager.nix). Same accepted "low-sensitivity, git-crypt at rest,
            # baked into the store on-host" posture as acme_email; NO literal in
            # this committed file.
            sender = smtpSender;
            username = smtpUser;
            password.secret_name = "authelia-secrets"; # key: smtp-password
          };

          # Two-factor for admin domains; family surfaces are governed by the
          # OIDC authorization_policies (Phase D). default deny.
          access_control = {
            default_policy = "deny";
            rules = [
              {
                domain = ["radarr${ingressSuffix}" "sonarr${ingressSuffix}" "prowlarr${ingressSuffix}" "qbittorrent${ingressSuffix}"];
                policy = "two_factor";
                subject = ["group:admins"];
              }
              {
                domain = ["books${ingressSuffix}"];
                policy = "one_factor";
                subject = ["group:family" "group:admins"];
              }
            ];
          };

          identity_providers.oidc = {
            hmac_secret.secret_name = "authelia-secrets"; # key: oidc-hmac
            # issuer key from the dedicated Secret (mounted file, see patch)
            jwks = [{key.path = "/oidc/issuer.pem";}];
            authorization_policies = {
              family = {
                default_policy = "one_factor";
                rules = [{policy = "one_factor"; subject = ["group:family" "group:admins"];}];
              };
            };
            # clients added in Phase D
            clients = [];
          };
        };
      };
    };

    # Patch the Helm-rendered Deployment: mount the host state dir (SQLite), the
    # users store Secret, and the OIDC issuer key Secret. Names/paths must match
    # the config above. `deployments.authelia` is the chart's Deployment name
    # (confirm with `kubectl get deploy -n auth` after the first apply if unsure).
    resources.deployments.authelia.spec.template.spec = {
      volumes = lib.mkForce [
        {name = "data"; hostPath = {path = "/var/lib/authelia"; type = "Directory";};}
        {name = "users"; secret.secretName = "authelia-users";}
        {name = "oidc"; secret.secretName = "authelia-oidc-key";}
      ];
      containers.authelia.volumeMounts = lib.mkForce [
        {name = "data"; mountPath = "/data";}
        {name = "users"; mountPath = "/users"; readOnly = true;}
        {name = "oidc"; mountPath = "/oidc"; readOnly = true;}
      ];
    };
  };
}
```

> The `resources.*` patch is the nixidy-documented way to modify chart output. The exact container name inside the pod (`authelia`) and Deployment name come from the chart — confirm from `result/` templates in Step 1 or after the first apply, and adjust the `containers.<name>` / `deployments.<name>` keys.

- [ ] **Step 3 (agent): thread the SMTP identity into nixidy**

In `flake.nix`, at the nixidy env `_module.args` (line ~133, alongside `acmeEmail = s.acme_email;`), add:

```nix
                smtpSender = s.mail.fromAddress;
                smtpUser = s.mail.smtpUser;
```

(These read the git-crypt `secrets/globalhawk.nix` `mail` attr the operator added in Task A1 — same eval-time, git-crypt, low-sensitivity posture as `acmeEmail`.)

- [ ] **Step 4 (agent): import + build-validate**

Add `./apps/authelia.nix` to `k8s/default.nix` imports. Run:
`nixos-rebuild build --flake .#globalhawk` → builds (helm template renders offline, values type-check).

- [ ] **Step 5 (agent): drift preview**

Run: `nix run .#k3s-drift`
Expected: reports the `auth` namespace, Authelia Deployment/Service/Ingress/ConfigMap, and the sops Secrets as "not yet applied" (desired-but-absent). No orphans.

- [ ] **Step 6 (operator): switch + verify the portal**

```sh
sudo nixos-rebuild switch --flake .#globalhawk
kubectl -n auth rollout status deploy/authelia
kubectl -n auth get ingress,secret
```
Open `https://auth.h.abrahamwhite.com` → the Authelia login page loads over the wildcard cert. Log in as the operator; enrol TOTP (Authelia emails the registration link via the Phase-A SMTP — confirms mail too). Log in as the family user (one-factor).

- [ ] **Step 7 (agent): commit**

```bash
git add k8s/apps/authelia.nix k8s/default.nix flake.nix
git commit -m "feat(globalhawk): deploy Authelia as a nixidy Helm release

Whole config authored in Nix under configMap.*; secrets referenced inline
from the sops Secrets; SQLite persisted via a hostPath resources-patch."
```

---

## Phase C — Forward-auth for the arr stack + qBittorrent

### Task C1: the Traefik forward-auth Middleware

**Files:**
- Create: `k8s/infra/forward-auth.nix`
- Modify: `k8s/default.nix` (imports)

**Interfaces:**
- Produces: a Traefik `Middleware` named `forward-auth` in the `media` namespace (Middlewares are namespaced; the arr/qbit ingresses that reference it live in `media`). Referenced by ingresses as `media-forward-auth@kubernetescrd`.

- [ ] **Step 1 (agent): author the Middleware**

Create `k8s/infra/forward-auth.nix`:

```nix
# Traefik forward-auth middleware: unauthenticated requests to the protected
# ingresses are sent to Authelia's forward-auth endpoint, which 302s to the
# portal. Authelia's identity headers are copied back to the backend. The
# Middleware is namespaced; it lives in `media` (where the protected apps are)
# and is referenced by ingress annotation as `media-forward-auth@kubernetescrd`.
{...}: {
  applications.forward-auth = {
    namespace = "media";
    createNamespace = false;
    yamls = [
      (builtins.toJSON {
        apiVersion = "traefik.io/v1alpha1";
        kind = "Middleware";
        metadata = {
          name = "forward-auth";
          namespace = "media";
        };
        spec.forwardAuth = {
          address = "http://authelia.auth.svc.cluster.local:9091/api/authz/forward-auth";
          authResponseHeaders = [
            "Remote-User"
            "Remote-Groups"
            "Remote-Email"
            "Remote-Name"
          ];
        };
      })
    ];
  };
}
```

- [ ] **Step 2 (agent): import + build + commit**

Add `./infra/forward-auth.nix` to `k8s/default.nix`. `nixos-rebuild build …` → builds.

```bash
git add k8s/infra/forward-auth.nix k8s/default.nix
git commit -m "feat(globalhawk): Traefik forward-auth middleware -> Authelia"
```

### Task C2: apply the middleware to the arr + qbit ingresses

**Files:**
- Modify: `k8s/lib.nix` (`mkIngress` gains an optional `annotations`)
- Modify: `k8s/apps/arr.nix`
- Modify: `k8s/apps/torrent.nix`

**Interfaces:**
- Consumes: the `forward-auth` Middleware (Task C1).
- Produces: forward-auth-gated ingresses for radarr/sonarr/prowlarr/qbittorrent.

- [ ] **Step 1 (agent): extend `mkIngress` with annotations**

In `k8s/lib.nix`, change `mkIngress` to accept an optional `annotations` arg and set it on the ingress metadata:

```nix
  mkIngress = {
    name,
    port,
    host,
    annotations ? {},
  }: {
    "${name}" = {
      metadata.annotations = annotations;
      spec = {
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
  };
```

Then extend `mkLsioApp` to thread an optional `ingressAnnotations` through to `mkIngress` (add `ingressAnnotations ? {}` to its args and pass `annotations = ingressAnnotations` into the `mkIngress` call).

- [ ] **Step 2 (agent): annotate the arr ingresses**

In `k8s/apps/arr.nix`, add to each `mkLsioApp` call:

```nix
        ingressAnnotations = {
          "traefik.ingress.kubernetes.io/router.middlewares" = "media-forward-auth@kubernetescrd";
        };
```

- [ ] **Step 3 (agent): annotate the qbittorrent ingress**

In `k8s/apps/torrent.nix`, add the same annotation to the qbittorrent ingress (via `mkIngress`'s new `annotations` arg or the app's `ingressAnnotations`, matching how torrent.nix builds its ingress).

- [ ] **Step 4 (agent): build + drift + commit**

`nixos-rebuild build …` → builds. `nix run .#k3s-drift` → shows the ingresses changing (annotation added), Middleware present.

```bash
git add k8s/lib.nix k8s/apps/arr.nix k8s/apps/torrent.nix
git commit -m "feat(globalhawk): gate arr + qbit ingresses behind forward-auth"
```

- [ ] **Step 5 (operator): switch + set app-side single-login + verify**

```sh
sudo nixos-rebuild switch --flake .#globalhawk
```
- Browse `https://radarr.h.abrahamwhite.com` (logged out of Authelia) → redirected to the portal; after two-factor login → radarr loads.
- In each arr (Settings → General → Security) set **Authentication Required = "Disabled for Local Addresses"** so the arr app trusts the in-cluster proxy and does not show its own login (no double login).
- In qBittorrent (Options → Web UI) enable **Bypass authentication for clients in whitelisted IP subnets** and add `10.42.0.0/16` (pod CIDR).
- Confirm: fresh browser → one Authelia login → all four apps reachable with no second login.

---

## Phase D — OIDC clients + reconcilers

Each reconciler is a Job (keyed by a hash of its desired OIDC config in the Job name, so it re-runs only when the config changes) that reads the client secret + a bootstrap admin credential from a mounted sops Secret and writes ONLY the OIDC block via the app's own API. The app UI stays editable.

### Task D1: Immich OIDC (system-config API reconciler)

**Files:**
- Modify: `machine/globalhawk/sops.nix` (Immich client secret + admin API key; add the client to `authelia_oidc_clients`)
- Modify: `k8s/apps/authelia.nix` (add the Immich client to `configMap.identity_providers.oidc.clients`)
- Create: `k8s/apps/oidc-reconcilers.nix` (Immich reconciler Job + ConfigMap script)
- Modify: `k8s/default.nix` (imports)

**Interfaces:**
- Consumes: Authelia OIDC provider; the Immich Service `immich-server.immich.svc:2283`.
- Produces: Immich configured for OIDC login (family + admins), local password login left enabled.

- [ ] **Step 1 (agent): register the client in Authelia**

In `k8s/apps/authelia.nix`, add to `configMap.identity_providers.oidc.clients`:

```nix
              {
                client_id = "immich";
                client_name = "Immich";
                client_secret = "$SOPS_IMMICH_CLIENT_HASH"; # rendered via the oidc-clients Secret; see note
                public = false;
                authorization_policy = "family";
                redirect_uris = [
                  "app.immich:///oauth-callback"
                  "https://photos${ingressSuffix}/auth/login"
                  "https://photos${ingressSuffix}/user-settings"
                ];
                scopes = ["openid" "profile" "email"];
                token_endpoint_auth_method = "client_secret_post";
              }
```

> The client **secret hash** must not sit in the committed file. Keep the `clients` list's secret fields sourced from the `authelia_oidc_clients` sops value (rendered as a merged second config file passed to Authelia via `configMap.extraConfigs`, per the chart's `configMap.extraConfigs` option). Concretely: move the whole `clients` list into the sops-rendered `authelia-oidc-clients` Secret mounted at a path added to `extraConfigs`, and leave `clients = []` in this file. Confirm `extraConfigs` mount semantics from the chart values in B5-Step1.

- [ ] **Step 2 (agent): declare the Immich secrets**

In `sops.nix` `secrets`, add `immich_oidc_client_secret = {};` and `immich_admin_api_key = {};`. Render a Secret in the `immich` ns:

```nix
      "sops-immich-oidc.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-immich-oidc.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: immich-oidc
            namespace: immich
          type: Opaque
          stringData:
            client-secret: ${config.sops.placeholder.immich_oidc_client_secret}
            admin-api-key: ${config.sops.placeholder.immich_admin_api_key}
        '';
      };
```

- [ ] **Step 3 (agent): author the reconciler Job + script**

Create `k8s/apps/oidc-reconcilers.nix`. The Immich reconciler uses a stock `curl`+`jq` image; its script GETs the current system-config, merges the `oauth` block, and PUTs it back:

```nix
{lib, ingressSuffix, ...}: let
  # Bump this when the desired OIDC config changes so the Job re-runs.
  immichCfgHash = "v1";
  immichScript = ''
    set -eu
    API=http://immich-server.immich.svc.cluster.local:2283/api
    KEY=$(cat /secret/admin-api-key)
    CS=$(cat /secret/client-secret)
    cur=$(curl -sf -H "x-api-key: $KEY" "$API/system-config")
    echo "$cur" | jq \
      --arg cs "$CS" \
      --arg iss "https://auth${ingressSuffix}" \
      '.oauth = (.oauth + {
         enabled: true,
         issuerUrl: $iss,
         clientId: "immich",
         clientSecret: $cs,
         scope: "openid email profile",
         buttonText: "Login with SSO",
         autoRegister: true,
         mobileRedirectUri: "app.immich:///oauth-callback"
       })' > /tmp/new.json
    curl -sf -X PUT -H "x-api-key: $KEY" -H "Content-Type: application/json" \
      --data @/tmp/new.json "$API/system-config" >/dev/null
    echo "immich oauth reconciled"
  '';
in {
  applications.oidc-reconcilers = {
    namespace = "immich";
    createNamespace = false;
    resources = {
      configMaps.immich-oidc-script.data."reconcile.sh" = immichScript;
      jobs."immich-oidc-${immichCfgHash}".spec = {
        backoffLimit = 6;
        template.spec = {
          restartPolicy = "OnFailure";
          containers.reconcile = {
            image = "docker.io/curlimages/curl:8.11.1@sha256:REPLACE"; # resolve digest (Step 4)
            command = ["sh" "/script/reconcile.sh"];
            volumeMounts = [
              {name = "script"; mountPath = "/script";}
              {name = "secret"; mountPath = "/secret"; readOnly = true;}
            ];
          };
          volumes = [
            {name = "script"; configMap = {name = "immich-oidc-script"; defaultMode = 493;};}
            {name = "secret"; secret.secretName = "immich-oidc";}
          ];
        };
      };
    };
  };
}
```

> `curlimages/curl` includes `jq`? It does NOT — use `docker.io/badouralix/curl-jq` or add jq. Simpler: use `ghcr.io/jqlang/jq` won't have curl. Pick a stock image with BOTH curl and jq (e.g. `docker.io/dwdraju/alpine-curl-jq` — resolve+pin a digest in Step 4), or use `alpine` and `apk add --no-cache curl jq` at container start. Confirm and pin in Step 4.

- [ ] **Step 4 (agent): resolve the reconciler image digest**

Pick a stock curl+jq image and pin it by digest (same discipline as immich image pins):

```sh
nix run nixpkgs#skopeo -- inspect docker://docker.io/dwdraju/alpine-curl-jq:latest --format '{{.Digest}}'
```
Put `repo@sha256:…` into the Job's `image`.

- [ ] **Step 5 (agent): import + build + drift + commit**

Add `./apps/oidc-reconcilers.nix` to `k8s/default.nix`. `nixos-rebuild build …` → builds. `nix run .#k3s-drift` → shows the Job + ConfigMap + Secret desired.

```bash
git add machine/globalhawk/sops.nix k8s/apps/authelia.nix k8s/apps/oidc-reconcilers.nix k8s/default.nix
git commit -m "feat(globalhawk): Immich OIDC via a system-config reconciler Job

Writes only the oauth block through Immich's API so the admin settings UI
stays editable; the client secret + admin key come from sops."
```

- [ ] **Step 6 (operator): bootstrap + switch + verify**

```sh
# One-time: in Immich admin UI, create an admin API key; put it + a random
# client secret into sops (immich_admin_api_key, immich_oidc_client_secret),
# and the matching argon2 hash into authelia_oidc_clients.
sops secrets/globalhawk.sops.yaml
sudo nixos-rebuild switch --flake .#globalhawk
kubectl -n immich get job          # immich-oidc-v1 -> Complete
kubectl -n immich logs job/immich-oidc-v1
```
Verify: Immich admin UI shows OAuth enabled (and still editable); the wife logs into the **Immich mobile app** via "Login with SSO". Local password login still works (safety).

### Task D2: Audiobookshelf OIDC reconciler

**Files:**
- Modify: `machine/globalhawk/sops.nix` (ABS client secret + admin token; add client to `authelia_oidc_clients`)
- Modify: `k8s/apps/authelia.nix` (ABS client)
- Modify: `k8s/apps/oidc-reconcilers.nix` (ABS Job + script)

**Interfaces:**
- Consumes: the ABS Service in the `library` ns.
- Produces: ABS OIDC login (family + admins).

- [ ] **Step 1 (agent): DISCOVER the ABS write endpoint**

The read-only `/auth/openid/config` helper only populates discovery URLs; the actual **write** is the call the ABS web UI makes when saving auth settings. Confirm it before coding (open item in the spec):

```sh
# In a browser devtools Network tab, save the OIDC settings once in the ABS UI
# and capture the request: method + path (likely PATCH /api/auth-settings or a
# settings PATCH) and the JSON body field names (authOpenIDIssuerURL,
# authOpenIDClientID, authOpenIDClientSecret, authOpenIDMobileRedirectURIs, …).
```
Record the exact method/path/fields. If no clean write endpoint exists, fall back to a **documented one-time UI setup** for ABS (spec-allowed) and skip the Job for ABS only.

- [ ] **Step 2 (agent): register the ABS client in Authelia**

In `k8s/apps/authelia.nix`, add an `audiobookshelf` client to the (sops-sourced) clients list: `authorization_policy = "family"`, redirect_uris = the ABS web callback + `https://<abs-host>/auth/openid/mobile-redirect`, scopes `["openid" "profile" "email"]`.

- [ ] **Step 3 (agent): add the ABS reconciler**

Add to `k8s/apps/oidc-reconcilers.nix` a second application block (namespace `library`) with an ABS script that authenticates as admin (API key/token from the mounted sops Secret) and PATCHes the discovered endpoint with the OIDC fields. Reuse the pinned curl+jq image + the hash-keyed Job-name pattern from D1. Declare `abs_oidc_client_secret` + `abs_admin_token` in `sops.nix` and render an `abs-oidc` Secret in the `library` ns.

- [ ] **Step 4 (agent): build + drift + commit**

`nixos-rebuild build …` → builds; `nix run .#k3s-drift` → ABS Job/Secret desired.

```bash
git add machine/globalhawk/sops.nix k8s/apps/authelia.nix k8s/apps/oidc-reconcilers.nix
git commit -m "feat(globalhawk): Audiobookshelf OIDC reconciler"
```

- [ ] **Step 5 (operator): bootstrap + switch + verify**

Create the ABS admin token, populate sops, `switch`, confirm `kubectl -n library logs job/abs-oidc-…` completes and the ABS **web** login offers SSO. Note the carried risk: OIDC-created users may hit the Oct-2025 ABS mobile-app login bug — web is the validation target.

### Task D3: Calibre-Web-Automated OIDC reconciler

**Files:**
- Modify: `machine/globalhawk/sops.nix` (CWA client secret + admin creds)
- Modify: `k8s/apps/authelia.nix` (CWA client)
- Modify: `k8s/apps/oidc-reconcilers.nix` (CWA Job + script)

**Interfaces:**
- Consumes: the CWA Service in `library` ns; CWA config in `app.db` on its `/config` hostPath.
- Produces: CWA OIDC login (family + admins).

- [ ] **Step 1 (agent): DISCOVER the CWA config mechanism**

CWA has no REST settings API; config lives in `app.db` (ConfigSQL). Prefer POSTing the `/admin/config` form (survives schema changes); fall back to a direct `app.db` UPSERT. Discover both on the box:

```sh
# Form path: capture the POST /admin/config request (fields + CSRF token flow)
# from the CWA UI devtools when saving OAuth settings.
# DB path: inspect the schema to find the OAuth columns:
sqlite3 /data/Media/docker-services/torrent-config/<cwa-config>/app.db '.schema settings' | grep -i oauth
sqlite3 …/app.db '.tables' | tr ' ' '\n' | grep -i oauth
```
Record the exact field/column names for the deployed CWA version.

- [ ] **Step 2 (agent): register the CWA client in Authelia**

Add a `calibre-web` client (sops-sourced secret): `authorization_policy = "family"`, redirect_uri = the CWA OAuth callback for `books${ingressSuffix}`, scopes `["openid" "profile" "email"]`. CWA also needs `config_oauth_redirect_host` = `https://books${ingressSuffix}` set in its own config (Step 3).

- [ ] **Step 3 (agent): add the CWA reconciler**

Add a `library`-ns block to `k8s/apps/oidc-reconcilers.nix`. Preferred: a curl-based script that logs in as CWA admin, scrapes the CSRF token, and POSTs `/admin/config` with the OAuth fields. Fallback: an `alpine`+`sqlite3` script that UPSERTs the OAuth columns/table in `app.db` (requires mounting CWA's `/config` hostPath into the Job and CWA using `strategy: Recreate` so the DB isn't concurrently written — coordinate timing). Declare `cwa_oidc_client_secret` + CWA admin creds in `sops.nix`; render a `cwa-oidc` Secret.

- [ ] **Step 4 (agent): build + drift + commit**

`nixos-rebuild build …` → builds; drift shows the CWA Job desired.

```bash
git add machine/globalhawk/sops.nix k8s/apps/authelia.nix k8s/apps/oidc-reconcilers.nix
git commit -m "feat(globalhawk): Calibre-Web-Automated OIDC reconciler"
```

- [ ] **Step 5 (operator): bootstrap + switch + verify** — populate sops, `switch`, confirm the CWA Job completes and CWA offers SSO login at `books.h.abrahamwhite.com`.

---

## Phase E — AdGuard surfacing + final tighten

### Task E1: surface AdGuard by name (SSO-excepted)

**Files:**
- Create: `k8s/apps/adguard.nix`
- Modify: `k8s/default.nix` (imports)

**Interfaces:**
- Consumes: `facts.hostGatewayIp`, `facts.ingressSuffix`.
- Produces: `adguard.h.abrahamwhite.com` → host AdGuard `:3000`, no forward-auth (its own login stays).

- [ ] **Step 1 (agent): author the module** (mirror `plex.nix`, port 3000, own namespace)

Create `k8s/apps/adguard.nix`:

```nix
# AdGuard Home stays host-native (machine/globalhawk/adguard.nix) and is SSO-
# EXCEPTED (no proxy-header trust -> forward-auth would only double-login one
# admin page). We only give it a hostname + TLS + a routing-table row, exactly
# like Plex: Traefik -> selector-less Service -> manual EndpointSlice -> the
# host's AdGuard web UI on :3000 (reachable from pods over the trusted cni0
# bridge; no firewall change). AdGuard's own admin login remains the gate.
{
  lib,
  ingressSuffix,
  hostGatewayIp,
  ...
}: let
  host = "adguard${ingressSuffix}";
  port = 3000;
in {
  applications.adguard = {
    namespace = "adguard";
    createNamespace = true;
    resources = {
      services.adguard.spec.ports.web = {
        inherit port;
        targetPort = port;
      };
      ingresses.adguard.spec = {
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
                  name = "adguard";
                  port.number = port;
                };
              }
            ];
          }
        ];
      };
    };
    yamls = [
      (builtins.toJSON {
        apiVersion = "discovery.k8s.io/v1";
        kind = "EndpointSlice";
        metadata = {
          name = "adguard";
          namespace = "adguard";
          labels."kubernetes.io/service-name" = "adguard";
        };
        addressType = "IPv4";
        endpoints = [{addresses = [hostGatewayIp];}];
        ports = [{name = "web"; port = port; protocol = "TCP";}];
      })
    ];
  };
}
```

- [ ] **Step 2 (agent): import + build + drift + commit**

Add `./apps/adguard.nix` to `k8s/default.nix`. `nixos-rebuild build …` → builds. `nix run .#k3s-drift` → adguard ns + Service + Ingress + EndpointSlice desired.

```bash
git add k8s/apps/adguard.nix k8s/default.nix
git commit -m "feat(globalhawk): surface AdGuard by name, SSO-excepted like Plex"
```

- [ ] **Step 3 (operator): switch + verify**

```sh
sudo nixos-rebuild switch --flake .#globalhawk
```
Open `https://adguard.h.abrahamwhite.com` → AdGuard's own login page loads over the wildcard cert (no Authelia redirect). Confirm `:3000` is no longer needed for access.

### Task E2: final verification

- [ ] **Step 1 (operator): drift + posture check**

```sh
nix run .#k3s-drift          # no orphans, no hand-created drift
sudo nmap -sT -p- 192.168.1.50   # from another LAN host: only 22, 80/443, 53, Samba, tailscale
```

- [ ] **Step 2 (operator): end-to-end matrix**

Confirm, in one browser session: one Authelia login carries into all four forward-auth apps with no second login; the three OIDC apps offer SSO and log in the family user; two-factor is enforced on admin domains and one-factor on family; Plex + AdGuard reachable by name with their own logins; a password-reset email arrives via the new SMTP.

- [ ] **Step 3 (agent): finalize** — invoke `superpowers:finishing-a-development-branch` to decide merge/PR for `globalhawk-sso-authelia`.

---

## Self-review notes (gaps deliberately left as in-task discovery)

These are **not** placeholders to fill before execution — they are values that only exist on the running box and are resolved by the exact commands given in-task, per the repo's established plan style:

- **Task B4/B5:** the chart `version` + `chartHash`, and the chart's exact `configMap.*`/`secret_name`/`ingress`/`pod.securityContext` value paths (confirmed via `helm search` + reading `result/values.yaml`). The authored config block is the intended state; key paths adjust to the resolved chart version.
- **Task D1–D3:** the reconciler image digest; the ABS write endpoint + field names; the CWA config mechanism (form vs `app.db`) + column names. Each has an explicit discovery command; D2/D3 name the UI-fallback if no clean write path exists.
- **Secret VALUES** (all phases) are operator `sops edit` steps — the agent authors wiring only and is not a sops recipient, so `build` validates structure without decryption.
