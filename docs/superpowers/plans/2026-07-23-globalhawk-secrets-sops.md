# globalhawk secrets → sops-nix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Consolidate all of globalhawk's *runtime* secrets onto sops-nix (age, host-key decryption), render k8s Secrets into the k3s auto-deploy manifests dir, and retire the sealed-secrets controller — leaving git-crypt only for residual eval-time values.

**Architecture:** sops-nix decrypts at *activation* using an age key derived from globalhawk's SSH host key. Host secrets land at `/run/secrets/*` (tmpfs) and are consumed by file-based NixOS options; k8s Secrets are rendered by `sops.templates` directly into `/var/lib/rancher/k3s/server/manifests/` where k3s applies them. The operator holds a dedicated age editing key; the read-only agent authors Nix wiring only and is **not** a decryption recipient.

**Tech Stack:** NixOS (flakehub nixpkgs 0.2605), sops-nix (`github:Mic92/sops-nix`), age, `ssh-to-age`, k3s auto-deploy manifests, nixidy.

## Global Constraints

- **Public repo:** never write a literal secret value into any unencrypted committed file (spec, comment, `.nix` outside git-crypt). Reference attribute *paths* only. (Project CLAUDE.md.)
- **Agent is read-only (uid 1001, no sudo):** the agent authors Nix + `.sops.yaml`; the **operator** generates keys, populates the encrypted file, runs every `nixos-rebuild switch`, and runs `kubectl`. Tasks are labelled **[AGENT]**, **[OPERATOR]**, or **[EITHER]**.
- **No `switch` until all authoring is committed:** activation is batched at Task 6, after Tasks 4–5, so the k8s cutover (add sops template + remove SealedSecret) applies atomically with no dual-owner conflict on the same Secret.
- **`nixos-rebuild build` does not decrypt** — it is the agent's validation gate. Only `switch` decrypts.
- **Spec:** `docs/superpowers/specs/2026-07-23-globalhawk-secrets-sops-design.md`. This plan is the prerequisite that unblocks the parked SSO spec.
- **k8s Secret identity is load-bearing:** rendered Secrets MUST keep exact `name`/`key`/`namespace`: `cloudflare-api-token`/`api-token`/`cert-manager` and `mullvad-wg`/`WIREGUARD_PRIVATE_KEY`/`media`.
- Commit trailers per repo convention (Co-Authored-By + Claude-Session). Keep `.git` group-writable (shared repo).

## File Structure

- **Create** `.sops.yaml` — recipients (host + operator age public keys) + creation rule. Public keys only.
- **Create** `secrets/globalhawk.sops.yaml` — the age-encrypted data file (operator-populated).
- **Create** `machine/globalhawk/sops.nix` — the sops-nix module: `sops.age`, `sops.defaultSopsFile`, `sops.secrets.*` (host runtime files), `sops.templates.*` (k8s Secret manifests + restic env + wireless env).
- **Modify** `flake.nix` — add the `sops-nix` input.
- **Modify** `machine/globalhawk/default.nix` — import `./sops.nix`.
- **Modify** `machine/globalhawk/backup.nix` — repoint restic `passwordFile`/`environmentFile` to sops paths; drop the store-leaking `environment.etc."restic/*"`.
- **Modify** `machine/globalhawk/disks.nix` — msmtp `passwordeval` from the sops file.
- **Modify** `machine/globalhawk/default.nix` — wifi `pskRaw = "ext:..."` + `networking.wireless.secretsFile`.
- **Modify** `k8s/infra/cert-manager.nix` — delete the `cloudflare-api-token` SealedSecret CR.
- **Modify** `k8s/apps/torrent.nix` — delete the `mullvad-wg` SealedSecret CR.
- **Modify** `machine/globalhawk/k3s.nix` — remove the `sealed-secrets` manifest entry + its `fetchurl`; drop `kubeseal` from `systemPackages`; add `sops`.
- **Modify** `.gitattributes` — narrow the git-crypt filter off `secrets/**` onto the two remaining plaintext-Nix files.
- **Modify** `secrets/globalhawk.nix`, `secrets/common.nix` — remove migrated keys (cleanup); delete dead `sorensen_psk`.

---

### Task 1: [OPERATOR] Provision age keys + probe the manifests dir

**Files:** none committed this task (produces two public keys + a go/no-go on the manifests-dir mechanism).

**Interfaces:**
- Produces: `HOST_AGE_PUBKEY`, `OPERATOR_AGE_PUBKEY` (age `age1...` strings) handed to the agent for Task 2; a confirmed placement strategy for k3s Secret manifests.

- [ ] **Step 1: Generate the operator age key**

```bash
mkdir -p ~/.config/sops/age
nix run nixpkgs#age -- -keygen -o ~/.config/sops/age/keys.txt
# note the "Public key: age1..." line -> OPERATOR_AGE_PUBKEY
grep -o 'age1[0-9a-z]*' ~/.config/sops/age/keys.txt
```
Back this file up out-of-band (it is the single point of failure for *editing* secrets; host decryption is independent).

- [ ] **Step 2: Derive the host age public key from the SSH host key**

```bash
sudo cat /etc/ssh/ssh_host_ed25519_key.pub | nix run nixpkgs#ssh-to-age
# -> HOST_AGE_PUBKEY (age1...)
```
Expected: one `age1...` line.

- [ ] **Step 3: Probe how k3s auto-deploy manifests are placed (de-risk)**

```bash
sudo ls -la /var/lib/rancher/k3s/server/manifests/
# For each NixOS-managed file (nixidy.yaml, cert-manager.yaml, sealed-secrets.yaml):
sudo stat /var/lib/rancher/k3s/server/manifests/nixidy.yaml
# Is it a symlink into /nix/store, or a regular file? Note which.
# Then test survival of a hand-placed sibling across a rebuild:
sudo touch /var/lib/rancher/k3s/server/manifests/zz-probe.yaml
sudo nixos-rebuild build --flake .#globalhawk   # build only, no switch
sudo test -f /var/lib/rancher/k3s/server/manifests/zz-probe.yaml && echo "SURVIVES build" 
sudo rm /var/lib/rancher/k3s/server/manifests/zz-probe.yaml
```
Expected: the probe file survives a `build` (build never touches runtime dirs). The real question is `switch`: **if** the k3s NixOS module purges unmanaged files on `switch` (unlikely — k3s writes its own `traefik.yaml`/`ccm.yaml` there), the sops template path in Task 4 must instead be written by an activation script ordered *after* the k3s module. Record the finding; the default assumption (Task 4/5) is that sibling files survive.

- [ ] **Step 4: Hand the two public keys to the agent**

Paste `HOST_AGE_PUBKEY` and `OPERATOR_AGE_PUBKEY` into the branch (e.g. a comment on the tracking task) so the agent can fill `.sops.yaml`. No commit.

---

### Task 2: [AGENT] Add the sops-nix input, `.sops.yaml`, and narrow git-crypt

**Files:**
- Modify: `flake.nix` (inputs block, after the `nixidy.url` line ~32)
- Create: `.sops.yaml`
- Modify: `.gitattributes`

**Interfaces:**
- Consumes: `HOST_AGE_PUBKEY`, `OPERATOR_AGE_PUBKEY` from Task 1.
- Produces: `inputs.sops-nix` available to modules; `.sops.yaml` creation rule matching `secrets/globalhawk.sops.yaml`; git-crypt no longer claims the sops data file.

- [ ] **Step 1: Add the flake input**

In `flake.nix`, inside `inputs = { ... }`, after the `nixidy.url` entry:

```nix
    # sops-nix: activation-time secret decryption (age, host-SSH-key derived).
    # Replaces sealed-secrets (k8s) and the git-crypt->/nix/store leak (host).
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
```

- [ ] **Step 2: Create `.sops.yaml`** (substitute the two real pubkeys from Task 1)

```yaml
keys:
  - &host age1HOST_PUBKEY_FROM_TASK1
  - &operator age1OPERATOR_PUBKEY_FROM_TASK1
creation_rules:
  - path_regex: secrets/globalhawk\.sops\.yaml$
    key_groups:
      - age:
          - *host
          - *operator
```

- [ ] **Step 3: Narrow the git-crypt filter**

Replace the broad `secrets/**` line in `.gitattributes` so git-crypt claims only the remaining plaintext-Nix files and never the (already-encrypted) sops YAML:

```gitattributes
machine/globalhawk/secrets.nix filter=git-crypt diff=git-crypt
secrets/common.nix filter=git-crypt diff=git-crypt
secrets/globalhawk.nix filter=git-crypt diff=git-crypt
```
(Remove the old `secrets/** filter=git-crypt diff=git-crypt` line. `secrets/globalhawk.sops.yaml` is intentionally unmatched.)

- [ ] **Step 4: Update the lock and validate** *(needs network; if the agent sandbox blocks fetches, hand this step to the operator)*

```bash
nix flake lock
nix flake check
```
Expected: `sops-nix` added to `flake.lock`; `nix flake check` evaluates cleanly (no sops module imported yet, so nothing references the not-yet-existing data file).

- [ ] **Step 5: Verify git-crypt will not grab the sops file**

```bash
git check-attr filter -- secrets/globalhawk.sops.yaml
```
Expected: `secrets/globalhawk.sops.yaml: filter: unspecified` (NOT `git-crypt`). Also confirm the existing files still match:
```bash
git check-attr filter -- secrets/globalhawk.nix   # -> filter: git-crypt
```

- [ ] **Step 6: Commit**

```bash
git add flake.nix flake.lock .sops.yaml .gitattributes
git commit -m "feat(globalhawk): add sops-nix input + age recipients; narrow git-crypt off the sops data file"
```

---

### Task 3: [OPERATOR] Create and populate the encrypted secrets file

**Files:**
- Create: `secrets/globalhawk.sops.yaml`

**Interfaces:**
- Consumes: `.sops.yaml` creation rule from Task 2; the current plaintext values in `secrets/globalhawk.nix` / `secrets/common.nix`.
- Produces: `secrets/globalhawk.sops.yaml` with these exact keys, consumed by Task 4's `sops.nix`: `cf_api_token`, `mullvad_wg_key`, `restic_repo_pass`, `restic_b2_key_id`, `restic_b2_app_key`, `restic_repo`, `gmail_password`, `pokestop_psk`.

- [ ] **Step 1: Open the encrypted file in sops and add every key**

```bash
cd /srv/dotfiles
SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix run nixpkgs#sops -- secrets/globalhawk.sops.yaml
```
In the editor, enter a flat YAML mapping (copy values from the current git-crypt secrets — `cf_api_token` is the Cloudflare token currently only present as the sealed blob, so paste the raw token you sealed originally):

```yaml
cf_api_token: "<cloudflare API token>"
mullvad_wg_key: "<wireguard_private_key from secrets/globalhawk.nix>"
restic_repo_pass: "<restic.b2.restic_repo_pass>"
restic_b2_key_id: "<restic.b2.key_id>"
restic_b2_app_key: "<restic.b2.app_key>"
restic_repo: "<restic.b2.repo>"
gmail_password: "<gmail_password from secrets/common.nix>"
pokestop_psk: "<pokestop_psk from secrets/common.nix>"
```
Save/exit — sops writes ciphertext.

- [ ] **Step 2: Confirm it is encrypted and not git-crypt-wrapped**

```bash
head -20 secrets/globalhawk.sops.yaml         # values show as ENC[AES256_GCM,...]; sops: block present
git check-attr filter -- secrets/globalhawk.sops.yaml   # -> unspecified
```
Expected: ciphertext visible; filter unspecified.

- [ ] **Step 3: Confirm the host can decrypt (dry run)**

```bash
sudo SOPS_AGE_SSH_PRIVATE_KEY_FILE=/etc/ssh/ssh_host_ed25519_key \
  nix run nixpkgs#sops -- -d secrets/globalhawk.sops.yaml >/dev/null && echo "HOST CAN DECRYPT"
```
Expected: `HOST CAN DECRYPT`. If this fails, the host pubkey in `.sops.yaml` is wrong — fix Task 2 Step 2 and re-key (`sops updatekeys secrets/globalhawk.sops.yaml`).

- [ ] **Step 4: Commit**

```bash
git add secrets/globalhawk.sops.yaml
git commit -m "secret(globalhawk): add sops-encrypted runtime secrets (age; host+operator recipients)"
```

---

### Task 4: [AGENT] Write `sops.nix`; migrate the host secrets (restic, msmtp, wifi)

**Files:**
- Create: `machine/globalhawk/sops.nix`
- Modify: `machine/globalhawk/default.nix` (imports list ~line 39; wifi block ~line 127-132)
- Modify: `machine/globalhawk/backup.nix` (`environment.etc` block + `services.restic.backups.media`)
- Modify: `machine/globalhawk/disks.nix` (msmtp account password ~line 48)

**Interfaces:**
- Consumes: `secrets/globalhawk.sops.yaml` keys from Task 3; `inputs.sops-nix` from Task 2.
- Produces: `config.sops.secrets."restic_repo_pass".path`, `config.sops.secrets."gmail_password".path`, `config.sops.templates."restic-env".path`, `config.sops.templates."wireless.env".path` — consumed by the modified NixOS options in this task.

- [ ] **Step 1: Create `machine/globalhawk/sops.nix`** (host secrets + k8s templates; k8s ones are inert until Task 5 removes the SealedSecrets, but defining them now keeps the module in one place)

```nix
# sops-nix: the single mechanism for globalhawk RUNTIME secrets. Decryption key
# is derived from the host's SSH ed25519 key (ssh-to-age), so the host decrypts
# at activation with no key to provision; plaintext never enters the Nix store
# or git. Host secrets render to /run/secrets (tmpfs); k8s Secrets render as
# manifests into k3s's auto-deploy dir. See
# docs/superpowers/specs/2026-07-23-globalhawk-secrets-sops-design.md.
{
  config,
  inputs,
  ...
}: {
  imports = [inputs.sops-nix.nixosModules.sops];

  sops = {
    defaultSopsFile = ../../secrets/globalhawk.sops.yaml;
    # Derive the age identity from the SSH host key. No separate age key on disk.
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];

    # --- host runtime secrets (single-value files under /run/secrets) ---
    secrets = {
      restic_repo_pass = {};
      gmail_password = {};
    };

    # --- composite / rendered files (placeholders substituted at activation) ---
    templates = {
      # restic environment file: B2 creds + repo URL, kept off the store.
      "restic-env".content = ''
        AWS_ACCESS_KEY_ID=${config.sops.placeholder.restic_b2_key_id}
        AWS_SECRET_ACCESS_KEY=${config.sops.placeholder.restic_b2_app_key}
        RESTIC_REPOSITORY=${config.sops.placeholder.restic_repo}
      '';

      # wpa_supplicant external-secrets file: pskRaw = "ext:pokestop_psk".
      "wireless.env".content = ''
        pokestop_psk=${config.sops.placeholder.pokestop_psk}
      '';

      # --- k8s Secrets: rendered straight into k3s's auto-deploy dir (root 0400,
      # never in store/git). k3s applies them; no controller. Name/key/namespace
      # are load-bearing (referenced by cert-manager + gluetun). ---
      "sops-cloudflare-token.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-cloudflare-token.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: cloudflare-api-token
            namespace: cert-manager
          type: Opaque
          stringData:
            api-token: ${config.sops.placeholder.cf_api_token}
        '';
      };
      "sops-mullvad-wg.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-mullvad-wg.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: mullvad-wg
            namespace: media
          type: Opaque
          stringData:
            WIREGUARD_PRIVATE_KEY: ${config.sops.placeholder.mullvad_wg_key}
        '';
      };
    };
  };
}
```

Note: the k8s templates need the `cf_api_token` etc. placeholders, which require those keys in the sops file (Task 3) and their declaration — sops-nix auto-registers a placeholder for any key referenced in a template, so no extra `secrets.*` entry is needed for template-only keys.

- [ ] **Step 2: Import `sops.nix` into the host**

In `machine/globalhawk/default.nix`, add to the `imports` list (after `./adguard.nix`, ~line 38):

```nix
    ./sops.nix
```

- [ ] **Step 3: Repoint restic in `backup.nix`**

Delete the store-leaking `environment.etc."restic/media-password"` and `environment.etc."restic/media-env"` entries. Repoint the backup:

```nix
  services.restic.backups.media = {
    initialize = true;
    # repository now comes from RESTIC_REPOSITORY in the sops-rendered env file.
    passwordFile = config.sops.secrets.restic_repo_pass.path;
    environmentFile = config.sops.templates."restic-env".path;
    # ... (paths, timerConfig, etc. unchanged) ...
  };
```
Remove the now-unused `repository = secrets.restic.b2.repo;` line (it is supplied via `RESTIC_REPOSITORY` in the env file). Ensure `config` is in the module's argument set.

- [ ] **Step 4: Repoint msmtp in `disks.nix`**

Replace the account password value with a `passwordeval` that reads the sops file:

```nix
        # was: password = secrets.gmail_password;
        passwordeval = "cat ${config.sops.secrets.gmail_password.path}";
```
Ensure `config` is in the module args; drop the `secrets.gmail_password` reference.

- [ ] **Step 5: Repoint wifi in `default.nix`**

```nix
  networking.wireless = {
    enable = true;
    secretsFile = config.sops.templates."wireless.env".path;
    networks = {
      pokestop.pskRaw = "ext:pokestop_psk";   # was: pokestop.psk = secrets.pokestop_psk;
    };
  };
```

- [ ] **Step 6: Build-validate**

```bash
nixos-rebuild build --flake .#globalhawk
nix flake check
```
Expected: both succeed. (Build does not decrypt; it only needs the encrypted file to exist, which it does after Task 3.)

- [ ] **Step 7: Commit**

```bash
git add machine/globalhawk/sops.nix machine/globalhawk/default.nix \
        machine/globalhawk/backup.nix machine/globalhawk/disks.nix
git commit -m "feat(globalhawk): move restic/msmtp/wifi secrets to sops-nix runtime files"
```

---

### Task 5: [AGENT] k8s cutover — add sops Secret templates, remove the SealedSecret CRs

**Files:**
- Modify: `k8s/infra/cert-manager.nix` (delete the `cloudflare-api-token` SealedSecret CR + its `cloudflareTokenSealed` blob)
- Modify: `k8s/apps/torrent.nix` (delete the `mullvad-wg` SealedSecret CR + its sealed blob)

**Interfaces:**
- Consumes: the two `sops.templates` k8s manifests defined in Task 4 (they will supply the same-named Secrets once switched).
- Produces: a nixidy render that no longer contains SealedSecret CRs; the Secrets now come from the sops-rendered manifests.

- [ ] **Step 1: Remove the Cloudflare SealedSecret from `cert-manager.nix`**

Delete the `cloudflareTokenSealed = "...";` binding and the `SealedSecret` entry in the `yamls` list (the `bitnami.com/v1alpha1` block for `cloudflare-api-token`). Keep the two ClusterIssuers and the `acmeSolver` (which still references `secretKeyRef` name `cloudflare-api-token` / key `api-token` — now satisfied by the sops-rendered Secret). Update the module comment to say the token is sops-rendered, not sealed.

- [ ] **Step 2: Remove the Mullvad SealedSecret from `torrent.nix`**

Delete the `SealedSecret` block (the `yamls`/`resources` entry named `mullvad-wg`). The gluetun container's `valueFrom.secretKeyRef { name = "mullvad-wg"; key = "WIREGUARD_PRIVATE_KEY"; }` stays unchanged — now satisfied by the sops-rendered Secret. Update the comment.

- [ ] **Step 3: Build-validate the render**

```bash
nixos-rebuild build --flake .#globalhawk
# confirm no SealedSecret survives in the rendered nixidy output:
nix build .#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage --print-out-paths
grep -rL "SealedSecret" ./result 2>/dev/null >/dev/null; grep -r "kind: SealedSecret" ./result && echo "STILL PRESENT (fail)" || echo "no SealedSecret CRs — ok"
```
Expected: build succeeds; no `SealedSecret` in the render.

- [ ] **Step 4: Commit**

```bash
git add k8s/infra/cert-manager.nix k8s/apps/torrent.nix
git commit -m "feat(globalhawk): source CF token + Mullvad key from sops, drop SealedSecret CRs"
```

---

### Task 6: [OPERATOR] Activate and verify; retire the sealed-secrets controller

**Files:** none (activation + live verification + manual controller deletion).

**Interfaces:**
- Consumes: everything committed in Tasks 2–5.
- Produces: a running system on sops-rendered secrets; sealed-secrets controller removed.

- [ ] **Step 1: Switch**

```bash
cd /srv/dotfiles
sudo nixos-rebuild switch --flake .#globalhawk
```
(Exit code 101 at a dbus reload is a known false-failure on this host — verify via `/run/current-system` if it occurs.)

- [ ] **Step 2: Verify host secrets are live and off the store**

```bash
sudo test -f /run/secrets/restic_repo_pass && echo "restic pass rendered"
sudo test -f /run/secrets/gmail_password && echo "gmail rendered"
# the wifi + restic-env templates:
sudo ls -l /run/secrets/rendered/ 2>/dev/null || sudo ls -l /run/secrets*/
# no plaintext in the store for the migrated values:
sudo grep -rl "$(sudo sed -n 's/^AWS_SECRET_ACCESS_KEY=//p' /run/secrets/rendered/restic-env 2>/dev/null)" /nix/store 2>/dev/null && echo "LEAK (fail)" || echo "no app_key in store — ok"
```
Expected: files present; no leak.

- [ ] **Step 3: Verify the k8s Secrets rendered and applied**

```bash
sudo ls -l /var/lib/rancher/k3s/server/manifests/sops-*.yaml
sudo k3s kubectl -n cert-manager get secret cloudflare-api-token
sudo k3s kubectl -n media get secret mullvad-wg
```
Expected: both manifest files exist (0400 root); both Secrets present. If the manifest files are missing after switch, the k3s module purged them — apply the Task 1 Step 3 fallback (activation-script placement ordered after the k3s module) and re-switch.

- [ ] **Step 4: Verify dependent workloads are healthy**

```bash
sudo k3s kubectl -n cert-manager get certificaterequests,certificates -A | tail
sudo k3s kubectl -n media get pods
# VPN leak test from inside the torrent pod:
sudo k3s kubectl -n media exec deploy/torrent-vpn -c gluetun -- wget -qO- https://am.i.mullvad.net/connected
```
Expected: wildcard Certificate `Ready=True`; torrent-vpn pod Running; Mullvad reports **connected**.

- [ ] **Step 5: Delete the sealed-secrets controller (k3s does not prune it on file removal yet)**

```bash
# Remove the controller's resources. The SealedSecret CRs are already gone
# (auto-pruned as a content change to the nixidy Addon).
sudo k3s kubectl delete -n kube-system deployment sealed-secrets-controller 2>/dev/null || true
sudo k3s kubectl delete crd sealedsecrets.bitnami.com 2>/dev/null || true
sudo k3s kubectl -n kube-system get all | grep -i sealed || echo "sealed-secrets controller gone"
```
Expected: no sealed-secrets resources remain. (The `sealed-secrets` manifest *file* is removed from Nix in Task 7; deleting resources here prevents k3s re-applying it before then — so keep Task 7 close behind.)

---

### Task 7: [AGENT] Cleanup — remove migrated plaintext, drop the sealed-secrets install

**Files:**
- Modify: `secrets/globalhawk.nix` (remove `wireguard_private_key`, `restic`, `adguard_password_hash` stays, `immich_pass` stays; remove the ones now in sops)
- Modify: `secrets/common.nix` (remove `gmail_password`, `pokestop_psk`, `sorensen_psk`)
- Modify: `machine/globalhawk/k3s.nix` (remove `sealed-secrets` manifest entry + `sealedSecretsVersion`/`sealedSecretsYaml`; swap `kubeseal` for `sops` in `systemPackages`)

**Interfaces:**
- Consumes: a proven-live sops path (Task 6).
- Produces: a repo with no duplicated plaintext for migrated secrets and no sealed-secrets machinery.

- [ ] **Step 1: Prune `secrets/globalhawk.nix`**

Remove the keys now sourced from sops: `wireguard_private_key`, and the `restic = { ... }` block. **Keep** `acme_email`, `adguard_password_hash`, `immich_pass`, `wireguard_addresses`, `vpn_server_cities` (eval-time / deferred, per spec).

- [ ] **Step 2: Prune `secrets/common.nix`**

Remove `gmail_password`, `pokestop_psk` (now sops), and `sorensen_psk` (dead — no consumer). **Keep** `work_email`, `bw_email`.

- [ ] **Step 3: Remove sealed-secrets from `k3s.nix`**

Delete the `sealedSecretsVersion` + `sealedSecretsYaml` `let`-bindings and the `sealed-secrets.source = sealedSecretsYaml;` line under `manifests`. In `environment.systemPackages`, remove `pkgs.kubeseal` and add `pkgs.sops` (for operator edits). Keep `cert-manager.source`.

- [ ] **Step 4: Build-validate**

```bash
nixos-rebuild build --flake .#globalhawk
nix flake check
```
Expected: succeeds. (No module references the removed keys — confirm with `grep -rn "wireguard_private_key\|restic_repo_pass\|gmail_password\|pokestop_psk\|sorensen_psk\|kubeseal\|sealedSecrets" machine/ k8s/ flake.nix` → only the sops.nix placeholder names / none.)

- [ ] **Step 5: Commit**

```bash
git add secrets/globalhawk.nix secrets/common.nix machine/globalhawk/k3s.nix
git commit -m "chore(globalhawk): drop migrated plaintext + sealed-secrets machinery"
```

---

### Task 8: [OPERATOR] Final activation + full validation suite

**Files:** none.

- [ ] **Step 1: Switch and confirm**

```bash
cd /srv/dotfiles
sudo nixos-rebuild switch --flake .#globalhawk
sudo k3s kubectl -n cert-manager get secret cloudflare-api-token >/dev/null && echo "CF secret still present after sealed-secrets removal"
```

- [ ] **Step 2: Run the acceptance checks**

```bash
# 1. cert renewal path intact (issuer still works):
sudo k3s kubectl -n kube-system get certificate wildcard-h -o jsonpath='{.status.conditions[?(@.type=="Ready")].status}'; echo
# 2. VPN leak test:
sudo k3s kubectl -n media exec deploy/torrent-vpn -c gluetun -- wget -qO- https://am.i.mullvad.net/connected
# 3. restic reaches the repo:
sudo systemctl start restic-backups-media.service && sudo journalctl -u restic-backups-media.service -n 20 --no-pager
# 4. drift check:
nix run .#k3s-drift
# 5. no plaintext leak for migrated host secrets:
for v in restic_repo_pass gmail_password pokestop_psk; do \
  p=$(sudo cat /run/secrets/$v 2>/dev/null || sudo grep -h "$v" /run/secrets/rendered/* 2>/dev/null); \
  [ -n "$p" ] && (sudo grep -rlF "$p" /nix/store >/dev/null 2>&1 && echo "$v LEAKS" || echo "$v clean"); \
done
```
Expected: Certificate `Ready=True`; Mullvad **connected**; restic run completes; `k3s-drift` reports no drift; every migrated secret reports `clean`.

- [ ] **Step 3: Update the migration memory / mark the SSO spec unblocked**

The secrets prerequisite is done — the SSO/Authelia spec (`docs/superpowers/specs/2026-07-23-globalhawk-sso-authelia-design.md`) is now unblocked and can be resumed through brainstorming → plan.

---

## Self-Review

**Spec coverage:** scope §in-scope (CF token, Mullvad key, restic, gmail, wifi PSK, drop sorensen) → Tasks 3–7; sealed-secrets retirement → Tasks 5–7; git-crypt narrowing → Task 2; key model (host ssh-to-age + operator age, agent non-recipient) → Tasks 1–3; k8s rendering into manifests dir → Task 4; deferred immich/adguard → left untouched (Task 7 Step 1 keeps them); validation suite → Task 8; manifests-dir risk → Task 1 Step 3 probe + Task 6 Step 3 fallback. All covered.

**Placeholder scan:** no TBD/TODO; every code step shows concrete content; secret *values* are intentionally `<...>` in the operator-only Task 3 (per the public-repo constraint — the operator fills them locally), which is correct, not a plan placeholder.

**Type consistency:** sops key names are identical across Task 3 (file), Task 4 (`config.sops.secrets.*` / `config.sops.placeholder.*` / `config.sops.templates.*`), and Task 8 (verification): `restic_repo_pass`, `gmail_password`, `restic_b2_key_id`, `restic_b2_app_key`, `restic_repo`, `pokestop_psk`, `cf_api_token`, `mullvad_wg_key`. k8s Secret identities match the consumers verified in the spec.
