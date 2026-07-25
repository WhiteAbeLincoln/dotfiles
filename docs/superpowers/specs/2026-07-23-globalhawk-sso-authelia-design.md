# globalhawk SSO (Authelia) — design

**Status:** Designed (2026-07-24). Ready for an implementation plan.
**Supersedes:** the PARKED decisions-only version of this file (2026-07-23), whose
locked decisions are preserved verbatim in the appendix.
**Builds on (both now DELIVERED):**
- `2026-07-23-globalhawk-secrets-sops-design.md` — sops-nix renders k8s Secrets
  straight into k3s's auto-deploy dir. Authelia's secrets reuse this lane.
- `2026-07-24-globalhawk-immich-k3s-design.md` — Immich now runs in k3s, so its
  OIDC client is self-contained (no `oci-containers` coordination).

## Goal

Give globalhawk one unified login. The operator gets UX uniformity across the
admin apps; **family users** (starting with one — the operator's wife needs
Immich from the mobile app) get a single account that reaches the family-facing
surfaces. The design is multi-user from day one but small: a file-based user
store with two users and two groups, structured so more family members are one
more entry.

This is a follow-on to the service-architecture spec, which deliberately left the
IdP choice, forward-auth wiring, and access policy out of scope. This spec takes
all three on.

## Prerequisites — already satisfied

- **Secret delivery:** `sops.templates."sops-<name>.yaml"` renders a k8s Secret
  (mode 0400, root) into `/var/lib/rancher/k3s/server/manifests/`; k3s applies it
  with no controller. Verified live for the Cloudflare, Mullvad, and Immich-DB
  secrets. Authelia's secrets are more of the same.
- **Hostname + TLS:** `auth.h.abrahamwhite.com` resolves LAN-privately via AdGuard
  (`*.h.abrahamwhite.com` rewrite) and is covered by Traefik's default wildcard
  cert. Already proven end-to-end by `photos.h.abrahamwhite.com` (Immich).
- **Ingress:** k3s's bundled Traefik, whose `Middleware`/`IngressRoute` CRDs are
  installed by default — the forward-auth middleware and per-app routing use them.

## The two auth patterns, and which apps use which

The decisive split is **not** "web vs mobile" but **"does the app authenticate
itself against an IdP, or must the proxy do it for the app."** Each app's *current*
(mid/late-2025) capability was verified against upstream docs/issues, not memory —
this corrected the parked spec's classification of Calibre-Web (see below).

### Native OIDC — the app logs the user in against Authelia

| App | Namespace | Host | Client type | Notes |
|---|---|---|---|---|
| Immich | immich | photos.h | web + mobile | mobile redirect `app.immich:///oauth-callback` |
| Audiobookshelf | library | (abs host) | web + mobile | Authelia ships an official ABS integration guide |
| Calibre-Web-Automated | library | books.h | web | CWA supports full OAuth2/OIDC (Keycloak/Authentik/Authelia) |

**Why OIDC for these:** all three have native/mobile clients that cannot perform
an interactive forward-auth redirect, or (CWA) a first-class OIDC integration that
gives a cleaner login than header trust. They authenticate against Authelia's
OIDC provider directly.

**Calibre-Web-Automated is a correction.** The parked spec listed calibre-web as
forward-auth-only — true of the old native `services.calibre-web`, but CWA (now in
k3s) supports full OAuth2/OIDC *and* Remote-User header auth. Since it is a family
surface, OIDC keeps it consistent with Immich/ABS.

### Forward-auth — Traefik gates the door via Authelia

| App | Namespace | Host | Policy |
|---|---|---|---|
| radarr | media | radarr.h | admins, two-factor |
| sonarr | media | sonarr.h | admins, two-factor |
| prowlarr | media | prowlarr.h | admins, two-factor |
| qbittorrent | media | (qbit host) | admins, two-factor |

None of these support OIDC (Servarr ships only None/Basic/Forms; qbit ships only
its own WebUI login). Traefik's forward-auth middleware calls Authelia's
`/api/authz/forward-auth`; unauthenticated requests are redirected to the portal.

**Avoiding the double-login trap.** Forward-auth only *gates* these apps — each
still has its own login unless configured to trust the already-authenticated,
already-proxied request. This is an app-side setting, done once per app:

- **radarr / sonarr / prowlarr:** set *Authentication Required =
  "Disabled for Local Addresses"* (Settings → General → Security). Requests arrive
  from Traefik's in-cluster pod IP (RFC1918), so the arr app skips its own login
  and Authelia's forward-auth is the sole gate. The `media` NetworkPolicy already
  restricts ingress to Traefik + intra-namespace, so "trust local" opens no hole.
- **qbittorrent:** enable *Bypass authentication for clients in whitelisted IP
  subnets* and whitelist the **cluster pod CIDR** (`10.42.0.0/16`). qbit sees
  Traefik's pod IP as the client (the reverse-proxy-IP quirk works in our favor),
  so proxied traffic bypasses qbit's login and relies on forward-auth.

### SSO-excepted — own login, but surfaced via ingress

| App | Substrate | Host | Why excepted |
|---|---|---|---|
| Plex | host-native | plex.h | plex.tv accounts + native clients; already surfaced |
| AdGuard Home | host-native | adguard.h | no OIDC and no proxy-header trust; its own login stays |

**AdGuard is deliberately *not* behind Authelia.** It has no header-auth mode, so
forward-auth would only ever be a double login for one infrequent admin page. It
is treated exactly like Plex: surfaced at `adguard.h.abrahamwhite.com` via a
selector-less Service + a hand-authored EndpointSlice pointing at the host
(`hostGatewayIp:3000`, reachable over the trusted `cni0` bridge with no firewall
change) + a **plain** Ingress (no forward-auth middleware). This buys name-based
`*.h` access — retiring `192.168.1.50:3000` for monitoring/config — while keeping
AdGuard's existing declarative admin login (`adguard_password_hash`) as the gate.
The network posture (LAN + Tailscale only) already keeps it off the internet.

## Identity provider — Authelia (locked)

Authelia was chosen in the 2026-07-23 session over Authentik (heavier: server +
worker + Postgres + Redis, config largely DB/UI) and Kanidm (no forward-auth →
would need oauth2-proxy for exactly the arr apps). Re-confirmed for this design.
It is a single Go binary + SQLite that provides **both** a forward-auth endpoint
and an OIDC provider, plus TOTP/WebAuthn MFA and SMTP-based password reset — the
whole surface this design needs, from one config.

### Packaging — the upstream Helm chart, consumed through nixidy

nixidy can render a Helm chart with **values authored as structured Nix** (not
chart text-templating), which sidesteps the objection to hand-authoring Helm
YAML while avoiding a bespoke re-implementation of Authelia's manifests. The
chart output is folded into the same `nixidyCombined` multi-doc file k3s already
delivers; no ArgoCD, no Helm at apply time. This is the one workload authored via
a chart rather than hand-written; the rest of the repo's conventions (single
always-present manifest file, prune-on-switch, `k3s-drift` verification) are
unchanged.

### Deployment shape

- **Namespace `auth`**, created by a new `k8s/infra/auth-network.nix` that reuses
  the established default-deny-ingress NetworkPolicy (re-open intra-namespace +
  the `kube-system` Traefik source). Traefik is the only ingress source, which
  covers the portal, forward-auth calls, *and* the OIDC endpoints — apps reach the
  issuer at `https://auth.h.abrahamwhite.com` through the ingress, so no
  cross-namespace direct paths are needed.
- **One replica.** At this scale Authelia uses in-memory session state (no Redis).
- **Persistent state** (registered TOTP/WebAuthn devices, identity-verification
  and password-reset tokens) in SQLite on a hostPath, owned by a dedicated
  `authelia` uid — mirroring `immich-storage.nix`, a NixOS oneshot pre-creates and
  chowns the directory before k3s starts. A new uid is allocated in `facts.nix`
  (free in both uid and gid namespaces, like `immichUid = 988`).
- **Service** on :9091; **Ingress** for `auth.h.abrahamwhite.com` (default
  wildcard cert, like every other app).
- **Session cookie domain `h.abrahamwhite.com`** so a single Authelia session is
  shared across all `*.h` forward-auth apps; `authelia_url =
  https://auth.h.abrahamwhite.com`.

## Users, groups, and access policy

- **User store:** file-based `users_database.yml` (argon2id hashes), two users:
  the operator in group `admins`, the wife in group `family`. Adding a family
  member is one more entry — no schema change.
- **Groups → policy:**
  - `admins` → every surface; **two-factor** on the admin apps.
  - `family` → the family surfaces (Immich, Audiobookshelf, Calibre-Web) at
    **one-factor** (password only — no MFA enrollment friction on the wife's
    Immich app). The `family` group is also reserved for a future
    Jellyseerr/Overseerr requests page, so wiring it now costs nothing later.
- **Two enforcement mechanisms**, because the app set spans both patterns:
  - `access_control.rules` (matched by domain) governs the **forward-auth** apps —
    admin domains at `two_factor` for `group:admins`, `default_policy: deny`.
  - `identity_providers.oidc.authorization_policies` (named `family` / `admins`
    policies, attached per client via `authorization_policy`) governs the **OIDC**
    apps — Immich/ABS/CWA get the `family` policy (one-factor; `family` + `admins`
    subjects).
- **MFA:** TOTP (issuer `auth.h.abrahamwhite.com`) with WebAuthn available; the
  operator enrolls once. Family stays one-factor.

## OIDC client delivery — script OIDC only, keep every app's UI editable

Each OIDC client needs a `client_id` + a client secret: Authelia stores the
**hash**; the app holds the **plaintext**. Redirect URIs are pinned per client.

**Authelia side.** The client definitions (issuer JWKS private key + the
per-client hashed secrets) are rendered by sops as a **second config file** merged
via an additional `--config` argument, so the hashes never enter the world-readable
Nix store. The non-secret Authelia config (server, access rules, policies,
authentication backend path, notifier host) is authored in Nix via the chart
values.

**App side — the design goal is discoverability.** A static, whole-config
approach (e.g. Immich's `IMMICH_CONFIG_FILE`) would make the app's settings UI
**entirely read-only** — a real loss when the operator wants to explore options in
the GUI. Verified against primary sources, all three OIDC apps keep their config
in a **mutable server-side store (DB)** with a **programmatic write path** — the
same one the web UI uses. So we script **only the OIDC block** and leave
everything else editable and discoverable in each app's UI:

- **Immich** — `PUT /api/system-config` (admin), *not* `IMMICH_CONFIG_FILE`. The
  reconciler authenticates with an admin API key, `GET`s the current config,
  merges the `oauth` block, and `PUT`s it back (the endpoint is a full-object
  replace). With no config file set, the admin settings page stays fully editable.
  The `oauth` fields are exactly those in the [official docs](https://docs.immich.app/administration/oauth):
  `issuerUrl`, `clientId`, `clientSecret`, `scope`, claim mappings
  (`storageLabelClaim`/`roleClaim`/`storageQuotaClaim`), `buttonText`,
  `autoRegister`, `autoLaunch`, `mobileRedirectUriOverride`, etc. Redirect URIs
  registered in Authelia: `app.immich:///oauth-callback` (mobile),
  `https://photos.h.abrahamwhite.com/auth/login`, and `…/user-settings`.
- **Audiobookshelf** — OIDC settings live in the mutable server settings; the
  reconciler writes them via the admin settings API the UI uses (the
  `/auth/openid/config` route is only a read-only discovery-populate helper).
  Mobile redirect via `/auth/openid/mobile-redirect`. **Known caveat:** an open
  (Oct-2025) ABS bug leaves OIDC-created users unable to log into some mobile
  player apps ("User has no password set"); ABS **web** OIDC is unaffected.
  Carried as a risk, not a blocker.
- **Calibre-Web-Automated** — config lives in `app.db` (ConfigSQL); no REST
  settings API. The reconciler either POSTs CWA's `/admin/config` form (preferred
  — survives schema changes) or, as a fallback, UPSERTs the OIDC columns directly
  in `app.db`. Either sets `config_oauth_redirect_host` for the `books.h`
  hostname. CWA is web-only, so no mobile redirect concerns.

**Reconciler mechanism.** A small per-app reconciler — a stock image
(`curl`+`jq`, or `sqlite3` for the CWA fallback) running a script from a ConfigMap
— waits for the app to be ready, reads the client secret **and** a bootstrap admin
credential from a mounted sops Secret, and applies the OIDC block idempotently. It
is a **Job keyed by a hash of the desired OIDC config**, so it re-applies on a
GitOps *change* rather than continuously fighting an admin who is editing other
settings in the UI. (A CronJob is the alternative if drift-healing is later
wanted.) It touches only OIDC — the rest of each app's config remains
GUI-managed. This keeps `k8s/apps/immich.nix` (and the arr/ABS/CWA apps) unchanged
except for the reconciler workload alongside them; Immich's **local password login
stays enabled** so a bad OIDC config can never lock the family out.

**Bootstrap credential.** Each reconciler needs an admin credential for its app
(Immich/ABS API key; CWA admin session). On these fresh installs the first admin
already exists, so the operator generates the key/credential once, stores it in
sops, and the reconciler consumes it thereafter. Documented as a one-time step.

## Forward-auth wiring

- One Traefik **`Middleware`** (`forward-auth`) → `http://authelia.auth.svc.
  cluster.local:9091/api/authz/forward-auth`, copying Authelia's response headers
  (`Remote-User`, `Remote-Groups`, `Remote-Email`, `Remote-Name`) to the backend.
- Applied to the arr + qbit ingresses. With no external API-key clients today
  (web UI is primary), the middleware protects the whole app.
- **Deferred:** a per-path `/api` exemption (a high-priority no-auth route via a
  Traefik `IngressRoute`, or a second Ingress) so external API-key mobile clients
  can bypass forward-auth. Not needed now — recorded here so it is a known,
  additive change if arr mobile apps are ever introduced. Intra-cluster
  prowlarr→arr and arr→qbit traffic already goes via cluster DNS, never through
  Traefik, so it is unaffected regardless.

## Email — iCloud SMTP, host-wide

Self-service password reset needs an SMTP notifier. The operator is moving off
Gmail to **iCloud Mail with a custom domain**, and this switch is taken
**host-wide**, not just for Authelia:

- One new sops secret, **`smtp_password`**, holds the outbound SMTP password (an
  Apple app-specific password today). The name is deliberately provider-neutral —
  iCloud is the current provider, not a permanent one — so a future provider swap
  is a value change, not a rename. `gmail_password` is retired. The
  provider-specific host/username/`from` are ordinary config (in `secrets/` where
  they carry the operator's address), not baked into the secret name.
- The existing host `msmtp` (ZFS ZED, smartd, and restic backup-failure alerts in
  `disks.nix` / `backup.nix`) is repointed from `smtp.gmail.com` to
  **`smtp.mail.me.com:587`** (STARTTLS), authenticating with the Apple ID / custom
  -domain address, `from` = the custom-domain address. `/etc/aliases` root target
  is updated to the new address.
- Authelia's `notifier.smtp` uses the same server and the same `smtp_password`
  secret (`AUTHELIA_NOTIFIER_SMTP_PASSWORD_FILE`), sender = the custom-domain
  address.

The email addresses and the app-specific password live in `secrets/` (git-crypt /
sops) and are referenced by attribute path — never written into any committed
file, per the public-repo rule.

## Secrets inventory (all via sops)

Rendered as k8s Secrets into k3s's manifests dir, or as host files:

| Secret | Consumer |
|---|---|
| `authelia_jwt_secret` | Authelia (identity-validation / reset JWTs) |
| `authelia_session_secret` | Authelia session encryption |
| `authelia_storage_encryption_key` | Authelia SQLite at-rest encryption |
| `authelia_oidc_hmac_secret` | Authelia OIDC provider |
| `authelia_oidc_issuer_key` | Authelia OIDC issuer (RSA private key / JWKS) |
| `authelia_users` (whole `users_database.yml`) | Authelia auth backend (argon2 hashes + emails) |
| `authelia_oidc_clients` (merged config fragment) | Authelia OIDC clients (hashed secrets) |
| `smtp_password` (provider-neutral name) | host msmtp **and** Authelia notifier |
| per-app OIDC client secret (plaintext) | the app's reconciler (Immich/ABS/CWA) |
| per-app bootstrap admin credential | the app's reconciler (Immich/ABS API key, CWA admin) |

No secret is ever rendered into `/nix/store`.

## Rollout — additive, reversible, lock-out-safe

`kubectl` is always an out-of-band admin path, and each phase leaves the previous
one working, so no phase can lock the operator or family out.

- **A — Email.** Migrate host msmtp to iCloud; add the sops secret; retire
  `gmail_password`. **Gate:** a test mail (and a forced ZED/smartd or restic-fail
  notification) arrives via iCloud. This also proves SMTP for Authelia.
- **B — Authelia core.** `auth` namespace + NetworkPolicy, the chart deployment,
  secrets, storage oneshot, portal Ingress, the two-user store, TOTP. **Gate:**
  portal login at `auth.h.abrahamwhite.com`; operator enrolls TOTP.
- **C — Forward-auth.** The middleware + one app (radarr) incl. the local-address
  bypass; validate SSO with no double login; fan out to sonarr/prowlarr/qbit
  (qbit subnet whitelist). **Gate:** each admin app reachable only after
  two-factor; no second app login.
- **D — OIDC clients.** The reconcilers apply the OIDC block to Immich (system-
  config API), then Audiobookshelf and CWA. Each app's UI stays editable; Immich
  local login stays on. **Gate:** the wife logs into the Immich **app** via OIDC;
  ABS/CWA web login via SSO; each app's settings page still editable in the GUI.
- **E — AdGuard surfacing + tighten.** Add the AdGuard ExternalName/EndpointSlice
  + plain Ingress. Confirm `default_policy: deny`, two-factor on admin, one-factor
  on family. **Gate:** `k3s-drift` clean; `nmap` shows only the intended ports;
  every app reachable by name.

## Validation

- `nixos-rebuild build` and `nix flake check` evaluate cleanly; `nix run
  .#k3s-drift` reports no drift.
- Portal login + TOTP enrollment succeed; password-reset email is received via
  iCloud.
- Forward-auth: admin apps redirect to the portal when unauthenticated and grant
  access after two-factor, with **no** second (app-native) login.
- OIDC: the wife authenticates to the Immich mobile app via Authelia; ABS/CWA web
  login via SSO.
- Host mail: a deliberately failed backup/SMART event emails via iCloud.
- Plex + AdGuard reachable by name via ingress with their own logins intact.

## Open items (resolved during implementation, not blocking the design)

- Confirm the Immich `PUT /api/system-config` payload shape and that an admin API
  key is sufficient (full-object replace → GET/merge/PUT).
- Confirm the exact Audiobookshelf admin endpoint that **writes** OIDC settings
  (the UI's save call), vs. the read-only `/auth/openid/config` helper.
- Decide the CWA reconciler mechanism: POST `/admin/config` (preferred) vs.
  seeding `app.db` — confirm the OAuth field/column names for the deployed CWA
  version.
- Confirm iCloud custom-domain SMTP `from`/username constraints (must be a
  verified iCloud alias).

---

## Appendix — original PARKED decisions (2026-07-23), preserved verbatim

The following is the decisions-only note this file held before being expanded into
the design above. Kept for provenance; where it and the design differ (Calibre-Web
now OIDC; AdGuard now SSO-excepted rather than forward-auth), the design above
governs.

> **Goal:** UX uniformity for the operator now, architected so **family users**
> can be added later. The family-facing surface is Immich, calibre, Plex, and an
> arr **requests** page (Overseerr/Jellyseerr), possibly Jellyfin later.
>
> **IdP: Authelia.** Lightweight single Go binary + SQLite, local in-memory
> sessions (no Redis at single-user scale), one declarative YAML config that fits
> the nixidy/GitOps model. Provides **both** forward-auth and an OIDC provider,
> plus TOTP/WebAuthn MFA. Chosen over Authentik (heavier: server + worker +
> Postgres + Redis, config largely DB/UI) and Kanidm (no forward-auth → would
> need oauth2-proxy for exactly the arr apps).
>
> **Deployment: in-cluster** (nixidy-authored), in an `auth` namespace. Rationale:
> immich and calibre are moving into k3s, and Plex may be dropped after a Jellyfin
> evaluation, so the substrate becomes uniformly k3s.
>
> **Two auth patterns**, split by client type: **Native OIDC** for apps with
> native/mobile clients (Immich, future Jellyfin); **forward-auth** (Traefik
> middleware → Authelia) for web-only apps with no OIDC (the arr apps,
> qbittorrent, AdGuard, calibre-web).
>
> **Plex is excepted** (its own account system + native clients).
>
> **Hostname:** `auth.h.abrahamwhite.com`, covered by the existing Traefik default
> wildcard cert; resolved LAN-privately by AdGuard.
>
> Known remaining work when resumed: arr `/api` forward-auth exemption; Authelia
> secrets via the new sops-nix mechanism; file-based user store with a path to
> family users; forward-auth middleware wiring; Immich OIDC client registration
> (coordinate with the immich→k3s migration).
