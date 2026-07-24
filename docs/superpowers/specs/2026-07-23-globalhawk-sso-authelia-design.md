# globalhawk SSO (Authelia) — PARKED

**Status:** PARKED — decisions captured, full design not yet written.
**Blocked on:** `2026-07-23-globalhawk-secrets-sops-design.md` (secrets migration
must ship first — Authelia's ~6 secrets will use the new sops-nix mechanism, not
sealed-secrets).

This is not a complete design. It records the decisions locked in during the
brainstorming session on 2026-07-23 so they survive until SSO is resumed. Resume
by completing the brainstorming flow (approaches → full design → plan) on top of
the delivered sops-nix foundation.

## Decisions locked in

- **Goal:** UX uniformity for the operator now, architected so **family users**
  can be added later. The family-facing surface is Immich, calibre, Plex, and an
  arr **requests** page (Overseerr/Jellyseerr), possibly Jellyfin later.
- **IdP: Authelia.** Lightweight single Go binary + SQLite, local in-memory
  sessions (no Redis at single-user scale), one declarative YAML config that fits
  the nixidy/GitOps model. Provides **both** forward-auth and an OIDC provider,
  plus TOTP/WebAuthn MFA. Chosen over Authentik (heavier: server + worker +
  Postgres + Redis, config largely DB/UI) and Kanidm (no forward-auth → would
  need oauth2-proxy for exactly the arr apps).
- **Deployment: in-cluster** (nixidy-authored), in an `auth` namespace. Rationale:
  immich and calibre are moving into k3s, and Plex may be dropped after a Jellyfin
  evaluation, so the substrate becomes uniformly k3s.
- **Two auth patterns**, split by client type:
  - **Native OIDC** for apps with native/mobile clients that cannot do an
    interactive forward-auth redirect — Immich, future Jellyfin (the same reason
    Plex is an SSO exception). They authenticate against Authelia's OIDC provider.
  - **Forward-auth** (Traefik middleware → Authelia) for web-only apps with no
    OIDC — the arr apps, qbittorrent, AdGuard, calibre-web.
- **Plex is excepted** (its own account system + native clients), per the service
  architecture spec.
- **Hostname:** `auth.h.abrahamwhite.com`, covered by the existing Traefik default
  wildcard cert; resolved LAN-privately by AdGuard.

## Known design work remaining (when resumed)

- **arr API exemption:** forward-auth on the arr/qbit WebUIs must not break
  API-key access. Intra-cluster prowlarr→arr and arr→qbit traffic goes via
  cluster DNS (not through Traefik), so it is unaffected; but *external* API-key
  clients (mobile apps hitting `radarr.h.abrahamwhite.com/api`) would be blocked
  by forward-auth. Design a per-path middleware that exempts `/api`.
- **Authelia secrets** (JWT/session/storage-encryption keys, OIDC HMAC + issuer
  private key, per-client secrets, user password hashes) → all via the new
  sops-nix mechanism (`sops.templates` → k3s manifests dir).
- **User store:** file-based `users_database.yml` (hashed) for the operator now;
  path to family users (SMTP for password reset, per-app access rules).
- **Forward-auth wiring:** Traefik middleware pointing at Authelia's
  `/api/authz/forward-auth`, applied per-ingress (or as a default) with the arr
  API exemptions.
- **Immich OIDC client** registration (immich is still on `oci-containers`;
  coordinate with the immich→k3s migration).
