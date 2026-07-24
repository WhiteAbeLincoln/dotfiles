# globalhawk secrets → sops-nix

**Status:** design approved, ready for implementation plan
**Date:** 2026-07-23
**Depends on:** nothing (this is itself a prerequisite)
**Blocks:** the SSO / Authelia spec (`2026-07-23-globalhawk-sso-authelia-design.md`)

## Why

globalhawk currently handles secrets with **two mechanisms, each with a distinct
flaw**, and the imminent SSO work would pile ~6 more secrets onto the broken
model. This spec consolidates all *runtime* secrets onto **sops-nix** first, so
SSO (and the later immich/calibre k3s migrations) build on a clean foundation.

The two current mechanisms:

1. **git-crypt** (`secrets/*.nix`, `machine/globalhawk/secrets.nix`) encrypts the
   files *in the repo*, but they are plain Nix values imported at evaluation
   time. Every value that gets interpolated into system config is written into
   the **world-readable `/nix/store`** on the running host. git-crypt gives repo
   confidentiality, not runtime confidentiality. `machine/globalhawk/backup.nix`
   even documents this leak in a comment and names sops as the fix.

2. **sealed-secrets** (k8s) keeps two secrets out of the store (Cloudflare API
   token, Mullvad WireGuard private key), but it **duplicates the source of
   truth** (plaintext in `secrets/globalhawk.nix` *and* a sealed blob committed
   in a `.nix` file) and its decrypt key is **cluster-ephemeral** — lost on a k3s
   rebuild, forcing a manual re-seal.

The organizing concept:

- **eval-time secrets** — values Nix needs at build time, which end up baked into
  store files (e.g. git `user.email` written into `~/.gitconfig`). git-crypt is
  the only tool that fits these, and they cannot be de-leaked with a runtime file
  without templating.
- **activation-time (runtime) secrets** — values a service reads at runtime,
  which can be a *file path* that sops decrypts on the host at activation, never
  in the store or git. This is what sops-nix handles natively.

This migration moves every runtime secret to sops-nix and retires the
sealed-secrets controller. git-crypt is kept **only** for the residual eval-time
values (see [Scope](#scope)).

## Scope

**In scope** — globalhawk runtime secrets:

- Replace both k8s sealed-secrets (Cloudflare token, Mullvad key) with
  sops-rendered k8s Secret manifests, and retire the sealed-secrets controller.
- Move the host secrets that currently leak into the store (restic credentials,
  `gmail_password`, wifi PSK) to sops runtime files.
- Delete the dead `sorensen_psk` (no consumer anywhere in the tree).

**Out of scope / deliberately retained on git-crypt:**

- `secrets/common.nix` cross-host eval-time emails (`work_email`, `bw_email`) —
  consumed at eval time on nighthawk (darwin) and campbell (WSL), where host-key
  decryption is not set up. They stay git-crypt.
- `acme_email`, `wireguard_addresses`, `vpn_server_cities` — injected into the
  nixidy-rendered YAML at eval time. Low sensitivity (a contact email and VPN
  account metadata; the WG addresses/cities are, per the existing code comment,
  "not a credential"). They remain git-crypt in `secrets/globalhawk.nix`.
- `machine/globalhawk/secrets.nix` (proprietary t2 firmware) — unrelated to this
  work; stays git-crypt.

**Deferred (decided defaults):**

- **`immich_pass`** stays git-crypt/store until immich moves to k3s. It is a DB
  password for a container about to be re-homed; adding a host `dbPasswordFile`
  now would be throwaway work. Migrate it as part of the immich→k3s pass.
- **`adguard_password_hash`** stays as-is (the one accepted residual). It is
  already a bcrypt *hash*, and `services.adguardhome` renders `settings` to the
  store with no per-user file option. Rendering the whole `AdGuardHome.yaml` via
  sops would wrest config ownership from the module — not worth it for a hash.

## Key & recipient model

- **Encryption:** age (not GPG).
- **Host decryption key:** derived from globalhawk's SSH host key
  (`/etc/ssh/ssh_host_ed25519_key`) via `ssh-to-age`. No new key to provision;
  sops-nix decrypts at *activation*. The public half goes in `.sops.yaml`.
- **Operator editing key:** a dedicated age keypair (`age-keygen`), private key at
  `~/.config/sops/age/keys.txt` on the operator's workstation(s), backed up
  out-of-band. Public half in `.sops.yaml`.
- **Recipients:** every secret is encrypted to **both** the host key and the
  operator key, via a `.sops.yaml` creation rule matching the data file.
- **The agent (sandbox uid 1001) is deliberately NOT a recipient.** It has no
  host-key access and is read-only. It authors the Nix wiring and `.sops.yaml`
  (public keys only); the operator generates keys and populates values. This
  preserves the established division of labour: the agent authors +
  build-validates, the operator applies.
- **Build vs activation:** `nixos-rebuild build` does **not** decrypt — sops
  paths are static strings computed without reading plaintext — so the agent can
  build-validate against the committed *encrypted* file. Only `switch`
  (activation) decrypts, which the operator runs.

## Repo layout & git-crypt narrowing

- **New** `secrets/globalhawk.sops.yaml` — the age-encrypted data file. Safe in
  this public repo *without* git-crypt (it is already encrypted to recipients).
- **New** `.sops.yaml` — recipients (host + operator age public keys) and a
  creation rule for the data file. Public keys only; safe to commit.
- **New** `machine/globalhawk/sops.nix` — the `sops-nix` module: declares
  `sops.secrets.*` (runtime files) and `sops.templates.*` (k8s Secret manifests),
  and wires `sops.age` to the host key.
- **`.gitattributes`:** narrow the git-crypt filter so it no longer matches the
  new sops data file. The filter must continue to cover only the plaintext-Nix
  files (`secrets/common.nix`, and `machine/globalhawk/secrets.nix`). The
  `secrets/*.sops.yaml` path is explicitly excluded — double-wrapping it with
  git-crypt would make it opaque to `sops` tooling on checkout.
- **Flake:** add the `sops-nix` input and import its NixOS module into
  `nixosConfigurations.globalhawk`.

## Secret inventory & migration mapping

Values are never reproduced here (public repo); only names, locations, and the
consuming option are referenced.

### k8s secrets (currently sealed-secrets)

| Secret | k8s object (name / key / ns) | Consumer | sops treatment |
|---|---|---|---|
| Cloudflare API token | `cloudflare-api-token` / `api-token` / `cert-manager` | cert-manager DNS-01 ClusterIssuers (`k8s/infra/cert-manager.nix`) | `sops.templates` → Secret manifest into k3s manifests dir |
| Mullvad WG private key | `mullvad-wg` / `WIREGUARD_PRIVATE_KEY` / `media` | gluetun container `secretKeyRef` (`k8s/apps/torrent.nix`) | `sops.templates` → Secret manifest into k3s manifests dir |

The rendered Secret objects **must** keep these exact name/key/namespace triples,
because the existing Deployments reference them by name.

### Host runtime secrets (currently git-crypt → store)

| Secret (current attr) | Consumer / option | sops treatment |
|---|---|---|
| `restic.b2.restic_repo_pass` | `services.restic.backups.media.passwordFile` (`backup.nix`) | `sops.secrets` file; repoint `passwordFile` |
| `restic.b2.key_id`, `restic.b2.app_key`, `restic.b2.repo` | `services.restic` `environmentFile` (`backup.nix`) | `sops.templates` env file containing `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, and `RESTIC_REPOSITORY` (move the repo URL into the env file too, off the store) |
| `gmail_password` (in `common.nix`) | msmtp account password (`disks.nix`) | `sops.secrets` file; msmtp `passwordeval = "cat <path>"` |
| `pokestop_psk` (in `common.nix`) | `networking.wireless.networks.pokestop.psk` (`default.nix`) | `sops.secrets` file; `networking.wireless.secretsFile` + `psk=ext:` reference |

After migration, `secrets/common.nix` retains **only** `work_email` and
`bw_email` (the cross-host eval-time emails). `gmail_password` and `pokestop_psk`
move out to sops (they are globalhawk-only).

### Dead / removed

| Secret | Action |
|---|---|
| `sorensen_psk` (in `common.nix`) | delete — no consumer exists anywhere in the tree |

### Retained on git-crypt (eval-time, out of scope)

| Secret | Reason |
|---|---|
| `work_email`, `bw_email` | cross-host eval-time (darwin/WSL git config) |
| `acme_email`, `wireguard_addresses`, `vpn_server_cities` | injected into nixidy YAML at eval; low-sensitivity account metadata |
| `immich_pass` | deferred to the immich→k3s migration |
| `adguard_password_hash` | accepted residual (bcrypt hash; module has no file option) |

## k8s Secret rendering mechanism

Each k8s secret is materialized by a `sops.templates` entry that renders a
complete `v1/Secret` manifest, with the secret value interpolated via
`config.sops.placeholder.<name>`, to a path inside k3s's auto-deploy directory:

```
sops.templates."cloudflare-token.yaml" = {
  content = <Secret manifest embedding ${config.sops.placeholder."cf_api_token"}>;
  path    = "/var/lib/rancher/k3s/server/manifests/sops-cloudflare-token.yaml";
  mode    = "0400";      # root only
  owner   = "root";
};
```

- Decryption happens at **activation**; the plaintext manifest lands only at that
  root-only path (on the persistent disk, not the store, not git). This is
  consistent with k8s storing Secret objects unencrypted-at-rest in its datastore
  anyway, so it introduces no new class of exposure.
- k3s watches the manifests dir and applies the Secret. No controller involved —
  this **removes** a controller (sealed-secrets) rather than adding one.
- **Ordering:** cert-manager needs its token, gluetun needs its key. k3s applies
  the whole manifests dir and retries, so eventual consistency is fine; no
  explicit ordering is required, but the rendered files should exist before the
  workloads' first reconcile (they will, since activation renders them before
  k3s re-reads the dir on the same `switch`).
- **Filename prefix `sops-`** distinguishes operator-rendered secret manifests
  from the NixOS `services.k3s.manifests` files, so it is unambiguous which files
  are sops-owned.

**Verification item for the plan:** confirm that the NixOS `services.k3s.manifests`
mechanism does not purge sibling files it did not place (k3s itself writes
`traefik.yaml`, `ccm.yaml`, etc. there, so it should not — but validate that the
sops-rendered files survive a `switch` and are not clobbered, and that activation
ordering writes the sops files relative to any k3s-module directory management).

## Removing sealed-secrets

- The **SealedSecret CRs** (`mullvad-wg`, `cloudflare-api-token`) are authored
  inside `k8s/apps/torrent.nix` and `k8s/infra/cert-manager.nix` as `yamls`
  entries, so they render into the single combined nixidy Addon. Deleting them
  from those modules is a *content change* to that Addon, which k3s **auto-prunes**
  on `switch` (confirmed prune-on-switch behaviour, 2026-07-23). No manual
  `kubectl delete` needed for the CRs.
- The sealed-secrets **controller** is a *separate* Addon installed via
  `services.k3s.manifests.sealed-secrets.source` (pinned upstream YAML in
  `machine/globalhawk/k3s.nix`). k3s does **not** prune resources when an
  auto-deploy manifest *file* is removed — so retiring the controller requires an
  explicit operator `kubectl delete` of its resources, in addition to removing
  the `sealed-secrets` manifest entry and its `fetchurl`. This is an explicit
  cutover step, not automatic.

## Cutover sequence

Authoring is the agent's; anything touching key material or plaintext is the
operator's.

1. **Operator:** run `age-keygen` (operator key) and `ssh-to-age` on the host
   public key; provide both **public** keys to the agent.
2. **Agent:** add the `sops-nix` flake input and import its module into
   `nixosConfigurations.globalhawk`; write `.sops.yaml`, `machine/globalhawk/sops.nix`,
   the `sops.templates` for the two k8s secrets, and repoint every host consumer
   (`backup.nix`, `disks.nix`, `default.nix` wireless). Do **not** create or edit
   any secret value.
3. **Operator:** create `secrets/globalhawk.sops.yaml` and populate it from the
   current plaintext values (`sops secrets/globalhawk.sops.yaml`).
4. **Build-validate:** `nixos-rebuild build --flake .#globalhawk` (no decryption
   required) and `nix flake check`. Either party can run this.
5. **Operator:** `nixos-rebuild switch`; then:
   - verify each host consumer reads its secret from the runtime path (restic,
     msmtp, wifi);
   - confirm the two k8s Secrets exist and cert-manager + gluetun are healthy;
   - `kubectl delete` the sealed-secrets controller resources.
6. **Cleanup (agent authors, operator applies):** remove the migrated keys from
   `secrets/globalhawk.nix` / `secrets/common.nix`, delete the SealedSecret CRs
   from the nixidy modules, and drop the `sealed-secrets` manifest entry +
   `fetchurl` from `k3s.nix`. Delete dead `sorensen_psk`.

## Rollback

Each step is reversible before cleanup (step 6): if `switch` misbehaves, the
previous generation still has the git-crypt/sealed-secrets values baked in, so
`nixos-rebuild switch --rollback` restores a fully-working system. The migrated
`secrets/*.nix` keys and SealedSecret CRs are only deleted in step 6, *after* the
sops path is proven — so a rollback before then loses nothing.

## Validation / success criteria

- `nix flake check` and `nixos-rebuild build --flake .#globalhawk` evaluate
  cleanly.
- For every migrated secret, `grep -r <marker> /nix/store` (run by the operator
  on-host) finds **no** plaintext — the store leak is closed for restic creds,
  `gmail_password`, and the wifi PSK.
- cert-manager still issues/renews the wildcard cert (DNS-01 works → the sops
  Cloudflare Secret is correct).
- gluetun VPN leak test passes (`curl am.i.mullvad.net/connected` from the pod →
  connected → the sops Mullvad Secret is correct).
- A `services.restic` run completes against B2 (repo pass + env from sops).
- The sealed-secrets controller is gone; `nix run .#k3s-drift` reports no drift.
- msmtp can send (smartd test notification) and wifi still associates (if used).

## Risks & open items

- **k3s manifests-dir coexistence** (see verification item above) — the main
  implementation unknown; validate before relying on it.
- **sops-nix on the flake** — a new input; pin it and run `nix flake check` to
  confirm it composes with the existing flake-parts + determinate setup.
- **Operator key custody** — the dedicated age private key is now a
  single-point-of-failure for editing secrets; it must be backed up out-of-band.
  Host decryption is independent (host SSH key), so losing the operator key does
  not break the running system, only future edits — recoverable by re-keying the
  data file from any surviving recipient.
- **`immich_pass` / `adguard_password_hash`** remain residual store leaks by
  decision; revisit `immich_pass` in the immich→k3s migration.
