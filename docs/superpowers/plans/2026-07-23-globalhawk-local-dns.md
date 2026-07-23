# globalhawk Local DNS (AdGuard + Tailscale + DNS-01) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make globalhawk's k3s services reachable by a real dotted name (`radarr.h.abewhite.dev`) that resolves identically on the LAN and over Tailscale, with browser-trusted wildcard TLS and nothing exposed to the WAN — replacing the interim `*-globalhawk.local` mDNS scheme.

**Architecture:** A NixOS-native AdGuard Home resolver on globalhawk answers `*.h.abewhite.dev` with the box's LAN IP (ad-blocking folded in). Remote devices resolve the same name via Tailscale split-DNS and reach it via a Tailscale subnet route (topology "2c"). cert-manager issues one `*.h.abewhite.dev` wildcard via Let's Encrypt **DNS-01** (Cloudflare solver, token stored as a SealedSecret), served by Traefik as its **default certificate** via a `TLSStore`. Design: `docs/superpowers/specs/2026-07-23-globalhawk-local-dns-design.md`.

**Tech Stack:** NixOS 25.05 (`services.adguardhome`, `services.tailscale`), k3s + bundled Traefik, nixidy (Nix→YAML, no ArgoCD/Helm), cert-manager v1.16.2, sealed-secrets 0.27.3, Cloudflare DNS.

## Global Constraints

- **Roles.** The agent authoring this plan runs as the read-only sandbox user (uid 1001, **no sudo, no cluster admin**). It can edit Nix and run **build-only** validation (`nixos-rebuild build`, `nix build …environmentPackage`). Anything that mutates the live system or cluster — `nixos-rebuild switch`, `kubectl`, `kubeseal`, `tailscale set`, the Cloudflare/Fiber consoles — is an **operator (abe)** action. Operator steps are labelled **[OPERATOR]**.
- **Never activate unasked.** Do not run `nixos-rebuild switch`. Validate with `build`. Activation happens once, by the operator, in Task 7.
- **Public repo.** Only `secrets/` and the t2 firmware are git-crypt encrypted. Never write a credential (passwords, hashes, API tokens, emails) into any unencrypted committed file. Emails and the AdGuard admin hash live in `secrets/globalhawk.nix`; the Cloudflare token lives only as a `SealedSecret` (encrypted to the cluster key). RFC1918 IPs and the domain suffix are not secret and live in `facts.nix` (the existing convention — `abewhite.dev` is already in a committed comment there).
- **Pruning is nixidy-scoped.** `services.k3s.manifests` *does* prune resources removed from the single combined nixidy manifest — removing a workload from Nix and running `switch` deletes it (verified 2026-07-23; the whole nixidy output is one always-present Addon that k3s re-applies with pruning). What auto-prune can NOT reach is resources created by *other* controllers — the per-app `*-tls` secrets and the `globalhawk-ca-key-pair` secret that cert-manager minted and never handed to nixidy. Those (and only those) need an explicit `kubectl delete` in Task 7. `nix run .#k3s-drift` reports exactly this class as "untracked / orphaned cert-manager secret."
- **Commit messages document the *why*, not the *what*.** End every commit body with:
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`
- **Placeholders the operator must confirm** (used verbatim below; correct them in Task/Phase 0 if different):
  - Domain suffix: **`.h.abrahamwhite.com`** (child label `h`; deliberately on abrahamwhite.com, not the identity domain abewhite.dev). Other `abewhite.dev` strings in the design/plan prose are illustrative — the source of truth is `facts.ingressSuffix`.
  - globalhawk reserved LAN IP: **`192.168.1.50`**.
  - LAN subnet: **`192.168.1.0/24`**.

---

## Phase 0 — Operator Prerequisites [OPERATOR]

These produce four values the Nix tasks consume: the **Cloudflare-sealed token blob**, the **ACME email**, the **AdGuard admin password hash**, and the confirmed **LAN IP / subnet / domain**. Do these first — they unblock everything else. Each command shows expected output.

- [ ] **Step 0.1 — Create a Cloudflare account and add the zone.**
  1. Sign up at https://dash.cloudflare.com/sign-up (free plan).
  2. **Add a Site** → enter your registered domain (e.g. `abewhite.dev`) → select **Free**.
  3. Cloudflare imports existing records and shows **two assigned nameservers**, e.g. `dana.ns.cloudflare.com` / `rob.ns.cloudflare.com`. Copy both.

- [ ] **Step 0.2 — Repoint nameservers at Porkbun (registrar stays Porkbun).**
  1. Porkbun → **Domain Management** → your domain → **Authoritative Nameservers** → **Edit**.
  2. Replace the Porkbun nameservers with the two Cloudflare nameservers from 0.1. Save.
  3. Verify delegation propagated (may take minutes to a few hours):
     ```bash
     nix run nixpkgs#dnsutils -- dig +short NS abewhite.dev
     ```
     Expected: the two `*.ns.cloudflare.com` names. Cloudflare's dashboard will also flip the zone to **Active**.

- [ ] **Step 0.3 — Mint a scoped Cloudflare API token.**
  1. Cloudflare → **My Profile** → **API Tokens** → **Create Token** → template **Edit zone DNS** → **Use template**.
  2. **Permissions:** the token needs **two** — `Zone` → `DNS` → **Edit** (create/delete the challenge TXT) **and** `Zone` → `Zone` → **Read** (so cert-manager can resolve the zone ID). The "Edit zone DNS" template bundles both; if you build the policy by hand, `DNS:Edit` alone is NOT enough — it fails at issuance with a zone-lookup 403. Leave all other rows (Zone Custom Asset, Zone DNS Settings, Zone Settings, Zone Versioning) unchecked.
  3. **Zone Resources:** `Include` → `Specific zone` → your domain.
  4. **TTL:** leave blank = **no expiry** (avoids future rotation).
  5. Create, copy the token, then verify it. **Which endpoint depends on the token type:**
     - A **User API token** (created under *My Profile → API Tokens*) verifies at `/user/tokens/verify`.
     - An **account-owned token** (prefix `cfat_`, created under *Manage Account → API Tokens*) verifies at `/accounts/{account_id}/tokens/verify` — the user endpoint returns a misleading `code 1000 "Invalid API Token"` for a *valid* account-owned token. Account-owned is the recommended type for a service integration and works fine with cert-manager (it's just a bearer credential for the DNS API).
     ```bash
     export CF_TOKEN='paste-token-here'
     # User token:
     nix run nixpkgs#curl -- -s -H "Authorization: Bearer $CF_TOKEN" \
       https://api.cloudflare.com/client/v4/user/tokens/verify | nix run nixpkgs#jq -- .
     # Account-owned (cfat_) token — ACCOUNT_ID from the dashboard URL / zone Overview:
     nix run nixpkgs#curl -- -s -H "Authorization: Bearer $CF_TOKEN" \
       "https://api.cloudflare.com/client/v4/accounts/$ACCOUNT_ID/tokens/verify" | nix run nixpkgs#jq -- .
     ```
     Expected: JSON containing `"status": "active"` and `"success": true`.

- [ ] **Step 0.4 — Seal the token into the encrypted blob.**
  The token becomes `encryptedData.api-token` in the SealedSecret authored in Task 4
  (name `cloudflare-api-token`, namespace `cert-manager`, default *strict* scope). The
  sealed-secrets controller is **`sealed-secrets-controller`** in `kube-system` (NOT
  `sealed-secrets` — confirm with `kubectl -n kube-system get deploy | grep sealed`).

  Sealing is easiest **offline** with `kubeseal --raw` — the plaintext token never
  touches disk or the cluster. First get the controller's *public* cert (safe to
  share; it's only the encryption half), which needs cluster read access once:
  ```bash
  # As abe (root kubeconfig), or any read-capable kubeconfig:
  sudo env KUBECONFIG=/etc/rancher/k3s/k3s.yaml kubeseal \
    --controller-namespace kube-system --controller-name sealed-secrets-controller \
    --fetch-cert > /tmp/cf-sealed-cert.pem
  ```
  Then seal (no sudo, no cluster needed):
  ```bash
  CF_TOKEN=$(rbw get -f 'globalhawk-dns01-cert token' dash.cloudflare.com)
  printf '%s' "$CF_TOKEN" | kubeseal --raw --scope strict \
    --namespace cert-manager --name cloudflare-api-token \
    --cert /tmp/cf-sealed-cert.pem
  ```
  **Use `printf '%s'`, never `echo`** — a trailing newline gets encrypted into the
  token and silently corrupts it. The output is a long `AgA…` blob. **Save it** — it
  goes into `k8s/infra/cert-manager.nix` `cloudflareTokenSealed` in Task 4. The
  `--scope strict` + name + namespace must match that SealedSecret exactly.

- [ ] **Step 0.5 — Generate the AdGuard admin password hash.**
  ```bash
  nix shell nixpkgs#apacheHttpd -c htpasswd -B -n -b admin 'CHOOSE-A-STRONG-PASSWORD'
  ```
  Expected: `admin:$2y$05$…`. **Take only the part after `admin:`** (the `$2y$…` bcrypt hash).

- [ ] **Step 0.6 — Pick globalhawk's static LAN IP + confirm the subnet.**
  **No router config needed.** globalhawk is assigned a **static IP in Nix** (`default.nix`, from `facts.lanIp`) rather than a DHCP reservation. The Fiber router only leases from `.100` up, so any address in `.2–.99` is collision-free without a reservation.
  1. Choose an address below the DHCP pool floor (placeholder `192.168.1.50`) and set `facts.lanIp`. AdGuard answers the wildcard with it, and it's set statically on `facts.lanInterface` (`enp1s0`).
  2. Confirm `facts.lanSubnet` (`192.168.1.0/24`) and `facts.lanGateway` (`192.168.1.1`). ⚠️ If the subnet is the ultra-common `192.168.0/1.0/24`, remote subnet-route collisions are likelier (see design); just be aware.
  3. ⚠️ Activation changes globalhawk's wired IP from its current DHCP lease to the static value — have Tailscale or console access ready when abe runs `switch` (Task 7) so a dropped SSH session isn't a lockout.

- [ ] **Step 0.7 — Add the two secret values to git-crypt'd secrets.**
  Edit `secrets/globalhawk.nix` (decrypted in the working tree) and add, inside the top-level attrset:
  ```nix
    # ACME account email for Let's Encrypt registration (cert-manager DNS-01).
    acme_email = "you@example.com";
    # bcrypt hash for the AdGuard Home admin user (from `htpasswd -B`, Step 0.5).
    adguard_password_hash = "$2y$05$REPLACE_WITH_HASH_FROM_STEP_0_5";
  ```
  Commit (the file re-encrypts on commit via the git-crypt filter):
  ```bash
  git add secrets/globalhawk.nix
  git commit -m "secret(globalhawk): add ACME email + AdGuard admin hash for local DNS"
  ```

---

## Nix Authoring Tasks (agent)

### Task 1: Retire the mDNS alias scheme

Removes the `go-avahi-cname` CNAME publisher; AdGuard replaces it. Avahi itself stays (Samba/SSH discovery). No functional resolution until Task 3 lands, but the tree still evaluates — nothing is switched until Task 7.

**Files:**
- Delete: `machine/globalhawk/mdns-aliases.nix`
- Modify: `machine/globalhawk/default.nix:38` (remove the import)
- Modify: `flake.nix:35-38` (remove the `go-avahi-cname` input)

**Interfaces:**
- Consumes: nothing.
- Produces: nothing (pure removal).

- [ ] **Step 1: Delete the mDNS module**
```bash
git rm machine/globalhawk/mdns-aliases.nix
```

- [ ] **Step 2: Remove its import from the host**
In `machine/globalhawk/default.nix`, delete the line:
```nix
    ./mdns-aliases.nix
```

- [ ] **Step 3: Remove the flake input**
In `flake.nix`, delete the entire `go-avahi-cname` input block:
```nix
    # mDNS CNAME publisher (upstream ships a flake) — publishes the k3s ingress
    # hostnames as CNAMEs -> globalhawk.local so they resolve on the LAN.
    go-avahi-cname = {
      url = "github:grishy/go-avahi-cname/v2.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
```

- [ ] **Step 4: Update the flake lock and validate the NixOS layer evaluates**
```bash
nix flake lock
nixos-rebuild build --flake .#globalhawk 2>&1 | tail -5
```
Expected: a `./result` symlink is produced with no evaluation error (no reference to `go-avahi-cname` or `mdns-aliases`). `git status` shows `flake.lock` modified (input dropped).

- [ ] **Step 5: Commit**
```bash
git add -A
git commit -m "refactor(globalhawk): retire go-avahi-cname mDNS aliases

AdGuard Home will own name resolution (real dotted names, not single-label
.local), so the CNAME-to-globalhawk.local publisher is no longer needed.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Add local-DNS host facts and flip the ingress suffix

`ingressSuffix` is the single source of truth for every ingress host and for the AdGuard wildcard; flipping it here propagates to both the k3s manifests and (in Task 3) the resolver.

**Files:**
- Modify: `machine/globalhawk/facts.nix`

**Interfaces:**
- Consumes: nothing.
- Produces: `facts.ingressSuffix` (`".h.abewhite.dev"`), `facts.lanIp` (`"192.168.1.50"`), `facts.lanSubnet` (`"192.168.1.0/24"`) — consumed by Task 3 (AdGuard, Tailscale) and by the nixidy env (ingress hosts).

- [ ] **Step 1: Flip `ingressSuffix` to the real dotted suffix**
In `machine/globalhawk/facts.nix`, replace the `ingressSuffix` assignment (keep the existing comment above it, updating its wording to reflect the real value):
```nix
  # Suffix appended to each app name to form its ingress host: "<app>" + suffix,
  # e.g. "radarr" -> "radarr.h.abewhite.dev". The dedicated child label `h` keeps
  # the wildcard cert + AdGuard rewrite scoped to the homelab and prevents the
  # wildcard from shadowing anything on the apex. Resolved LAN-privately by
  # AdGuard (machine/globalhawk/adguard.nix); never published to public DNS.
  # Not secret (domains are public via WHOIS; internal names already appear in
  # the committed k8s manifests). OPERATOR: set to your real registered domain.
  ingressSuffix = ".h.abewhite.dev";
```

- [ ] **Step 2: Add the LAN facts**
In the same file, inside the `--- cluster network ---` area (or a new `--- lan ---` block), add:
```nix
  # --- lan ---
  # globalhawk's reserved LAN IP (DHCP reservation in the Fiber app). AdGuard
  # answers the `*${ingressSuffix}` wildcard with this address. OPERATOR: must
  # match the reservation from Phase 0 Step 0.6.
  lanIp = "192.168.1.50";
  # The LAN CIDR globalhawk advertises as a Tailscale subnet route so the same
  # name resolves+connects remotely (topology 2c). OPERATOR: confirm.
  lanSubnet = "192.168.1.0/24";
```

- [ ] **Step 3: Validate both layers evaluate with the new suffix**
```bash
nixos-rebuild build --flake .#globalhawk 2>&1 | tail -3
nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
```
Expected: both succeed. Confirm the suffix reached the manifests:
```bash
env=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
grep -rho 'host: [a-z].*\.h\.abewhite\.dev' "$env" | sort -u
```
Expected: `host: radarr.h.abewhite.dev`, `host: qbittorrent.h.abewhite.dev`, `host: plex.h.abewhite.dev`, etc.

- [ ] **Step 4: Commit**
```bash
git add machine/globalhawk/facts.nix
git commit -m "feat(globalhawk): adopt real dotted ingress suffix + LAN facts

Names move from single-label .local to <app>.h.abewhite.dev so they can carry
public-CA TLS and resolve the same on LAN and Tailscale. lanIp/lanSubnet feed
the AdGuard wildcard answer and the Tailscale subnet route.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: AdGuard Home resolver + Tailscale subnet-router prep

Adds the LAN resolver (ad-blocking + the wildcard rewrite), opens DNS on the LAN interface only, and enables IP forwarding so globalhawk can act as a Tailscale subnet router (the route is *advertised* by the operator in Task 7).

**Files:**
- Create: `machine/globalhawk/adguard.nix`
- Modify: `machine/globalhawk/default.nix` (add import; add firewall DNS port; add `services.tailscale.useRoutingFeatures`)

**Interfaces:**
- Consumes: `facts.ingressSuffix`, `facts.lanIp`, `facts.lanInterface`; `secrets.adguard_password_hash` (Phase 0).
- Produces: a resolver on `:53` (LAN + `tailscale0`) answering `*.h.abewhite.dev → lanIp`; web UI on `:3000` (reachable via `tailscale0`/localhost only).

- [ ] **Step 1: Create the AdGuard module**
Create `machine/globalhawk/adguard.nix`:
```nix
# LAN DNS resolver: answers the homelab wildcard privately (never in public DNS)
# and does network-wide ad-blocking. Replaces the mDNS alias scheme. Host-level
# (NOT a k3s workload) so it keeps resolving even if the cluster is unhealthy.
#
# The web UI (:3000) is bound on all interfaces but only reachable over
# tailscale0 (a trusted firewall interface) + localhost — port 53 is the only
# thing opened to the LAN, and only on the LAN interface.
{pkgs, ...}: let
  facts = import ./facts.nix;
  secrets = import ../../secrets/globalhawk.nix;
in {
  services.adguardhome = {
    enable = true;
    # Fully declarative: UI edits are reverted on restart, config lives in Nix.
    mutableSettings = false;
    # Web UI bind (module maps these to http.address). Firewall gates LAN access.
    host = "0.0.0.0";
    port = 3000;
    openFirewall = false; # we scope :53 by interface below; UI stays off the LAN
    settings = {
      users = [
        {
          name = "admin";
          password = secrets.adguard_password_hash;
        }
      ];
      dns = {
        bind_hosts = ["0.0.0.0"];
        port = 53;
        # DoH upstreams; bootstrap_dns resolves the upstream hostnames and
        # satisfies the module's bootstrap assertion under mutableSettings=false.
        upstream_dns = [
          "https://dns.cloudflare.com/dns-query"
          "https://dns.quad9.net/dns-query"
        ];
        bootstrap_dns = ["1.1.1.1" "9.9.9.9"];
      };
      filtering = {
        protection_enabled = true;
        filtering_enabled = true;
        # Split-horizon: the homelab wildcard resolves to globalhawk on the LAN.
        # These A records exist ONLY here — never in public DNS.
        rewrites = [
          {
            domain = "*${facts.ingressSuffix}";
            answer = facts.lanIp;
          }
        ];
      };
      # Ad-blocking blocklist(s).
      filters = [
        {
          enabled = true;
          name = "AdGuard DNS filter";
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_1.txt";
          id = 1;
        }
      ];
    };
  };
}
```

- [ ] **Step 2: Wire the module, DNS firewall port, and Tailscale routing into the host**
In `machine/globalhawk/default.nix`:

(a) Add to the `imports` list (near `./k3s.nix`):
```nix
    ./adguard.nix
```

(b) Enable Tailscale server routing features (replace the existing `services.tailscale.enable = true;` line):
```nix
  # stuck behind a double-NAT with no router control. useRoutingFeatures
  # "server" enables IP forwarding so globalhawk can advertise the LAN subnet as
  # a Tailscale route (operator runs `tailscale set --advertise-routes` once).
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
  };
```

(c) Open DNS on the LAN interface only (AdGuard's `:53`). Add after the existing `networking.firewall = { … };` block (a separate statement, keeping the existing block intact):
```nix
  # AdGuard Home DNS: reachable on the LAN interface and (implicitly, via the
  # trusted tailscale0 interface) the tailnet. The web UI (:3000) is deliberately
  # NOT opened here, so it stays off the LAN.
  networking.firewall.interfaces.${facts.lanInterface} = {
    allowedTCPPorts = [53];
    allowedUDPPorts = [53];
  };
```

- [ ] **Step 3: Validate the NixOS layer evaluates**
```bash
nixos-rebuild build --flake .#globalhawk 2>&1 | tail -8
```
Expected: builds with no error. In particular no assertion failure about AdGuard bootstrap DNS, and no `bind_host`/`bind_port`-in-settings assertion (we use top-level `host`/`port` + `dns.bind_hosts`).

- [ ] **Step 4: Confirm the rendered AdGuard config has the wildcard rewrite**
```bash
sys=$(nixos-rebuild build --flake .#globalhawk --print-out-paths 2>/dev/null | tail -1)
grep -RhoE '"?domain"?:\s*"\*\.h\.abewhite\.dev"' "$sys" 2>/dev/null | head
```
Expected: at least one match (the rewrite is baked into the AdGuard config the service reads). If the grep finds nothing, the config is generated at service start from the Nix `settings`; instead confirm the setting is present in the evaluated value:
```bash
nix eval --json '.#nixosConfigurations.globalhawk.config.services.adguardhome.settings.filtering.rewrites'
```
Expected: `[{"answer":"192.168.1.50","domain":"*.h.abewhite.dev"}]`.

- [ ] **Step 5: Commit**
```bash
git add machine/globalhawk/adguard.nix machine/globalhawk/default.nix
git commit -m "feat(globalhawk): AdGuard Home LAN resolver + Tailscale route prep

Answers *.h.abewhite.dev with the box's LAN IP (records private to the LAN, never
public) and blocks ads network-wide. useRoutingFeatures=server lets globalhawk
advertise the LAN subnet so the same name works over Tailscale. DNS opens only on
the LAN interface; the admin UI stays off the LAN.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: cert-manager Let's Encrypt DNS-01 issuers + Cloudflare SealedSecret

Replaces the interim self-signed internal CA with staging + prod ACME `ClusterIssuer`s that solve DNS-01 via Cloudflare, plus the sealed Cloudflare token.

**Files:**
- Modify: `k8s/infra/cert-manager.nix` (full rewrite of the module body)
- Modify: `flake.nix` (thread `acmeEmail` into the nixidy env `_module.args`)

**Interfaces:**
- Consumes: `acmeEmail` (new `_module.args`, from `secrets.acme_email`); the sealed Cloudflare blob from Phase 0 Step 0.4.
- Produces: `ClusterIssuer/letsencrypt-staging`, `ClusterIssuer/letsencrypt-prod` (both DNS-01/Cloudflare), and `Secret/cloudflare-api-token` (via SealedSecret) in namespace `cert-manager`. Consumed by Task 5's wildcard `Certificate`.

- [ ] **Step 1: Thread the ACME email into the nixidy env**
In `flake.nix`, in the `nixidyEnvs` `_module.args` block, add `acmeEmail` alongside the existing secret-derived args:
```nix
              _module.args = {
                wireguardAddresses = s.wireguard_addresses;
                vpnServerCities = s.vpn_server_cities;
                acmeEmail = s.acme_email;
                inherit (facts) ingressSuffix podCidr serviceCidr hostGatewayIp mediaRoot mediaUid timezone;
              };
```

- [ ] **Step 2: Rewrite `k8s/infra/cert-manager.nix`**
Replace the entire file with (paste the Cloudflare blob from Phase 0 Step 0.4 into `cloudflareTokenSealed`):
```nix
# Public-trusted TLS via Let's Encrypt DNS-01. No inbound 80/443 exists
# (double-NAT), so HTTP-01 is impossible; DNS-01 needs only outbound API calls to
# create a transient _acme-challenge TXT — the sole thing that ever hits public
# DNS (service A records stay private in AdGuard). Replaces the earlier
# self-signed internal CA. Two issuers: `letsencrypt-staging` for validating the
# plumbing without burning prod rate limits, `letsencrypt-prod` for real certs.
#
# The Cloudflare API token (Zone:DNS:Edit) is a SealedSecret: encrypted to the
# cluster key, safe in the world-readable store + git. Authored as raw CRs via
# `yamls` (nixidy has no typed options for cert-manager / sealed-secrets CRDs).
{acmeEmail, ...}: let
  # From Phase 0 Step 0.4: `.spec.encryptedData.api-token` of the kubeseal output.
  cloudflareTokenSealed = "PASTE_SEALED_BLOB_FROM_PHASE_0_STEP_0_4";
  acmeSolver = {
    dns01.cloudflare.apiTokenSecretRef = {
      name = "cloudflare-api-token";
      key = "api-token";
    };
  };
  mkIssuer = name: server: privateKeyRef: {
    apiVersion = "cert-manager.io/v1";
    kind = "ClusterIssuer";
    metadata.name = name;
    spec.acme = {
      inherit server;
      email = acmeEmail;
      privateKeySecretRef.name = privateKeyRef;
      solvers = [acmeSolver];
    };
  };
in {
  applications.cert-manager-config = {
    namespace = "cert-manager"; # created by the upstream controller manifest
    createNamespace = false;
    yamls = map builtins.toJSON [
      (mkIssuer "letsencrypt-staging"
        "https://acme-staging-v02.api.letsencrypt.org/directory"
        "letsencrypt-staging-account-key")
      (mkIssuer "letsencrypt-prod"
        "https://acme-v02.api.letsencrypt.org/directory"
        "letsencrypt-prod-account-key")
      {
        apiVersion = "bitnami.com/v1alpha1";
        kind = "SealedSecret";
        metadata = {
          name = "cloudflare-api-token";
          namespace = "cert-manager";
        };
        spec = {
          encryptedData.api-token = cloudflareTokenSealed;
          template.metadata = {
            name = "cloudflare-api-token";
            namespace = "cert-manager";
          };
        };
      }
    ];
  };
}
```

- [ ] **Step 3: Validate the nixidy env renders the issuers**
```bash
env=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
grep -rl 'letsencrypt-prod' "$env" && grep -rl 'kind: SealedSecret' "$env"
grep -rho 'kind: ClusterIssuer' "$env" | wc -l
```
Expected: files matched for both greps; the `ClusterIssuer` count is `2`. No occurrence of the old `selfsigned-bootstrap` / `globalhawk-ca` issuer:
```bash
grep -rl 'selfsigned-bootstrap\|globalhawk-ca' "$env" || echo "OK: self-signed CA gone from render"
```
Expected: `OK: self-signed CA gone from render`.

- [ ] **Step 4: Commit**
```bash
git add k8s/infra/cert-manager.nix flake.nix
git commit -m "feat(globalhawk): DNS-01 Let's Encrypt issuers, drop self-signed CA

The interim self-signed ClusterIssuer required importing a root CA on every
device. DNS-01 issues browser-trusted wildcard certs with no inbound and no
per-client trust, fitting the double-NAT posture. The Cloudflare token is a
SealedSecret so no plaintext token lands in the world-readable /nix/store.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Wildcard certificate + Traefik default TLSStore

One `*.h.abewhite.dev` `Certificate` in `kube-system`, served by Traefik as its **default** certificate. This is why individual app ingresses (Task 6) need no per-app cert — every host on the wildcard is covered by Traefik's default cert. Issued against **staging** first; flipped to prod in Task 7.

**Files:**
- Create: `k8s/infra/wildcard-tls.nix`
- Modify: `k8s/default.nix` (import it)

**Interfaces:**
- Consumes: `ingressSuffix`; `ClusterIssuer/letsencrypt-staging` (Task 4).
- Produces: `Secret/wildcard-h-tls` (in `kube-system`) and Traefik `TLSStore/default` (in `kube-system`) referencing it — the default cert for all ingresses.

- [ ] **Step 1: Create the wildcard TLS module**
Create `k8s/infra/wildcard-tls.nix`:
```nix
# A single wildcard cert for the whole homelab, served by Traefik as its DEFAULT
# certificate. Because Traefik has a default cert, app Ingresses need no per-app
# secret — a `tls` entry with `hosts` but no `secretName` uses this. The cert +
# secret live in kube-system (Traefik's namespace); a Traefik TLSStore named
# `default` (the reserved name Traefik looks up) points at the secret.
#
# issuerRef starts at `letsencrypt-staging` to validate issuance without burning
# prod rate limits; Task 7 flips it to `letsencrypt-prod`.
{ingressSuffix, ...}: {
  applications.wildcard-tls = {
    namespace = "kube-system";
    createNamespace = false;
    yamls = map builtins.toJSON [
      {
        apiVersion = "cert-manager.io/v1";
        kind = "Certificate";
        metadata = {
          name = "wildcard-h";
          namespace = "kube-system";
        };
        spec = {
          secretName = "wildcard-h-tls";
          dnsNames = ["*${ingressSuffix}"];
          issuerRef = {
            name = "letsencrypt-staging";
            kind = "ClusterIssuer";
            group = "cert-manager.io";
          };
          privateKey = {
            algorithm = "ECDSA";
            size = 256;
          };
        };
      }
      {
        apiVersion = "traefik.io/v1alpha1";
        kind = "TLSStore";
        metadata = {
          name = "default";
          namespace = "kube-system";
        };
        spec.defaultCertificate.secretName = "wildcard-h-tls";
      }
    ];
  };
}
```

- [ ] **Step 2: Import it in the nixidy env**
In `k8s/default.nix`, add to the `imports` list (under `./infra/cert-manager.nix`):
```nix
    ./infra/wildcard-tls.nix
```

- [ ] **Step 3: Validate the render**
```bash
env=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
grep -rl 'kind: TLSStore' "$env" && grep -rho 'dnsNames' "$env" | head -1
grep -rhoE '\*\.h\.abewhite\.dev' "$env" | sort -u
```
Expected: TLSStore present; `*.h.abewhite.dev` appears (the wildcard dnsName).

- [ ] **Step 4: Commit**
```bash
git add k8s/infra/wildcard-tls.nix k8s/default.nix
git commit -m "feat(globalhawk): wildcard cert as Traefik default certificate

One *.h.abewhite.dev cert served as Traefik's default means adding an app needs
no new cert and no per-namespace secret replication — every ingress host on the
wildcard is covered. Starts on the ACME staging issuer to prove issuance.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Point every ingress at the default wildcard cert

Removes the per-app `cert-manager.io/cluster-issuer` annotation and the per-app `secretName` from each ingress `tls` entry. A `tls` entry with `hosts` and no `secretName` makes Traefik serve its default cert (the wildcard from Task 5).

**Files:**
- Modify: `k8s/lib.nix:90-99` (the `mkArrApp` ingress — covers prowlarr/radarr/sonarr)
- Modify: `k8s/apps/plex.nix:27-36`
- Modify: `k8s/apps/torrent.nix:204-213`
- Modify: `k8s/apps/whoami.nix:36-45`

**Interfaces:**
- Consumes: Traefik `TLSStore/default` (Task 5).
- Produces: ingresses with no per-app issuer/secret (all serve the wildcard).

- [ ] **Step 1: Update the shared `mkArrApp` ingress in `k8s/lib.nix`**
Replace the `ingresses."${name}"` block (currently starting with the `metadata.annotations` line) with:
```nix
        ingresses."${name}" = {
          spec = {
            ingressClassName = "traefik";
            # No secretName: Traefik serves its default cert (the *.h wildcard,
            # kube-system TLSStore/default). No per-app issuer or cert needed.
            tls = [{hosts = [host];}];
            rules = [
              {
                inherit host;
                http.paths = [
                  {
                    path = "/";
                    pathType = "Prefix";
                    backend.service = {
                      name = name;
                      port.number = port;
                    };
                  }
                ];
              }
            ];
          };
        };
```

- [ ] **Step 2: Update `k8s/apps/plex.nix`**
Replace its `ingresses.plex` block with (drop the annotation line and the `secretName`):
```nix
      ingresses.plex = {
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
                    name = "plex";
                    port.number = plexPort;
                  };
                }
              ];
            }
          ];
        };
      };
```

- [ ] **Step 3: Update `k8s/apps/torrent.nix`**
Replace its `ingresses.qbittorrent` block with:
```nix
      ingresses.qbittorrent = {
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
                    name = "qbittorrent";
                    port.number = 9091;
                  };
                }
              ];
            }
          ];
        };
      };
```

- [ ] **Step 4: Update `k8s/apps/whoami.nix`**
Replace its `ingresses.whoami` block with:
```nix
      ingresses.whoami = {
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
                    name = "whoami";
                    port.number = 80;
                  };
                }
              ];
            }
          ];
        };
      };
```

- [ ] **Step 5: Validate no ingress still references the old issuer or a per-app secret**
```bash
env=$(nix build --no-link --print-out-paths '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')
grep -rl 'cluster-issuer\|globalhawk-ca' "$env" || echo "OK: no per-app issuer annotations"
grep -rho 'secretName' "$env" | grep -c 'secretName'
```
Expected: `OK: no per-app issuer annotations`; the only `secretName` occurrences are the wildcard (`wildcard-h-tls`) and the SealedSecret templates — no `*-tls` per app. Spot-check one ingress:
```bash
grep -rA12 'name: radarr' "$env" | grep -A12 'kind: Ingress' | head -20 || true
```
Expected: the radarr ingress shows `tls: [{hosts: [radarr.h.abewhite.dev]}]` with no `secretName`.

- [ ] **Step 6: Commit**
```bash
git add k8s/lib.nix k8s/apps/plex.nix k8s/apps/torrent.nix k8s/apps/whoami.nix
git commit -m "refactor(globalhawk): serve all ingresses via Traefik default wildcard cert

Per-app issuer annotations + per-app cert secrets are redundant once Traefik has
a default wildcard cert: a tls entry with hosts and no secretName uses it. Adding
an app now needs zero cert wiring.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Activation & Validation [OPERATOR]

Runs on globalhawk as abe. Activates the switch, verifies staging issuance, flips to prod, brings up the Tailscale route + Fiber DNS, and runs the spec's validation gates. Cleans up superseded cert-manager leftovers (auto-prune covers nixidy-authored resources but not secrets other controllers created).

- [ ] **Step 1: Activate**
```bash
sudo nixos-rebuild switch --flake .#globalhawk
```
Note: a `101` exit at a dbus reload is a known cosmetic issue on this box — verify activation actually applied via `readlink /run/current-system` rather than trusting the exit code.

- [ ] **Step 2: Confirm AdGuard is up and answering the wildcard locally**
```bash
systemctl status adguardhome --no-pager | head -5
dig @127.0.0.1 radarr.h.abewhite.dev +short
```
Expected: service `active (running)`; the dig returns `192.168.1.50` (the `lanIp`).

- [ ] **Step 3: Confirm the cloudflare secret unsealed and the staging cert issued**
```bash
sudo k3s kubectl -n cert-manager get secret cloudflare-api-token
sudo k3s kubectl -n kube-system get certificate wildcard-h
sudo k3s kubectl -n kube-system describe certificate wildcard-h | tail -20
```
Expected: the secret exists; the Certificate reaches `READY=True`. If it stays False, the describe output shows the DNS-01 challenge state (token/permission/propagation issues surface here).

- [ ] **Step 4: Flip the wildcard cert to prod** (only after staging shows `READY=True`)
Edit `k8s/infra/wildcard-tls.nix`: change the Certificate `issuerRef.name` from `"letsencrypt-staging"` to `"letsencrypt-prod"`. Then:
```bash
sudo nixos-rebuild switch --flake .#globalhawk
# Force reissue from the prod issuer (delete the staging-signed secret):
sudo k3s kubectl -n kube-system delete secret wildcard-h-tls
sudo k3s kubectl -n kube-system get certificate wildcard-h -w   # wait for READY=True
```
Commit the flip:
```bash
git add k8s/infra/wildcard-tls.nix
git commit -m "feat(globalhawk): promote wildcard cert to Let's Encrypt prod

Staging validated the DNS-01 plumbing end-to-end; switch to a browser-trusted
prod cert now that issuance is proven.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 5: Bring up the Tailscale subnet route (topology 2c)**
```bash
sudo tailscale set --advertise-routes=192.168.1.0/24
```
Then in the Tailscale admin console → **Machines** → globalhawk → **Edit route settings** → approve `192.168.1.0/24`. Configure **split-DNS**: admin console → **DNS** → **Nameservers** → **Add nameserver** → **Custom** → globalhawk's `100.x` Tailscale IP, **Restrict to domain** `h.abewhite.dev`. On Linux/Windows tailnet clients, enable `tailscale set --accept-routes=true` (macOS/iOS accept by default).

- [ ] **Step 6: Point the LAN at AdGuard (Google Fiber app)**
Set **primary DNS = globalhawk's LAN IP** (`192.168.1.50`) and **secondary DNS = `1.1.1.1`** (so the internet survives an AdGuard outage). Renew a client's DHCP lease (or toggle Wi-Fi) to pick up the new resolver.

- [ ] **Step 7: Run the spec's validation gates**
```bash
# LAN resolution + trusted TLS (from a LAN client):
dig radarr.h.abewhite.dev +short          # -> 192.168.1.50
curl -sSI https://radarr.h.abewhite.dev | head -1   # -> HTTP/2 200/302/401, NO cert warning
# Ad-blocking active:
dig doubleclick.net @192.168.1.50 +short  # -> 0.0.0.0 or blocked/empty
# Internet survives AdGuard being down:
sudo systemctl stop adguardhome
dig google.com +short                      # still resolves via the 1.1.1.1 secondary
sudo systemctl start adguardhome
# Remote (device on the tailnet, off the LAN):
dig radarr.h.abewhite.dev +short           # resolves via split-DNS
curl -sSI https://radarr.h.abewhite.dev | head -1   # connects via the subnet route
```
Expected: each line as annotated. The remote `curl` returning a normal HTTP status with no TLS error is the end-to-end gate.

- [ ] **Step 8: Clean up cert-manager leftovers** (the only resources auto-prune can't reach)
```bash
# nixidy-authored resources below (the ClusterIssuers + the globalhawk-ca
# Certificate) are ALREADY gone — removing them from Nix pruned them on the
# switch above. These deletes are belt-and-suspenders (--ignore-not-found):
sudo k3s kubectl delete clusterissuer selfsigned-bootstrap globalhawk-ca --ignore-not-found
sudo k3s kubectl -n cert-manager delete certificate globalhawk-ca --ignore-not-found
# GENUINELY manual — cert-manager minted these Secrets and never handed them to
# nixidy, so auto-prune leaves them behind when their Certificate is deleted:
sudo k3s kubectl -n cert-manager delete secret globalhawk-ca-key-pair --ignore-not-found
# Old per-app cert secrets (each app now uses the default wildcard):
for ns in media plex whoami; do
  sudo k3s kubectl -n "$ns" delete secret -l 'controller.cert-manager.io/fao=true' --ignore-not-found 2>/dev/null || true
  sudo k3s kubectl -n "$ns" get secret | grep -- '-tls'   # review any leftover *-tls and delete by name
done
```
Expected: old issuers/secrets gone; `kubectl get clusterissuer` shows only `letsencrypt-staging` and `letsencrypt-prod`. Confirm with `nix run .#k3s-drift` — the "untracked / orphaned cert-manager secret" entries should be gone.

---

## Self-Review

**Spec coverage:**
- Naming (`*.h.abewhite.dev`, dedicated label, private A records, `ingressSuffix` flip) → Task 2 + Task 3 rewrite.
- AdGuard Home NixOS-native, ad-blocking, LAN+Tailscale bind, replaces `mdns-aliases.nix` → Task 1 (removal) + Task 3.
- Wildcard rewrite derived from `ingressSuffix` (collapses to one record) → Task 3 Step 1.
- Remote via Tailscale 2c (subnet route + split-DNS + accept-routes) → Task 3 (routing prep) + Task 7 Steps 5–6.
- TLS DNS-01 wildcard, Cloudflare solver, SealedSecret token, staging→prod, replaces self-signed → Tasks 4–5 + Task 7 Steps 3–4.
- External one-time setup (Cloudflare account/delegation/token, DHCP reservation, hash) → Phase 0.
- Public secondary resolver for internet-never-fails → Task 7 Step 6; validated Task 7 Step 7.
- Cleanup of cert-manager leftovers (CA key-pair + per-app `*-tls` secrets auto-prune can't reach) → Task 7 Step 8.
- Phase 2 (Pi backup resolver) → intentionally out of scope per spec; not in this plan.

**Placeholder scan:** The only intentional fill-ins are operator-supplied values flagged **OPERATOR**/**PASTE** (sealed blob, email, hash, LAN IP/subnet/domain) — these cannot be known at authoring time and are produced in Phase 0. No `TODO`/`TBD`/"add error handling"-style gaps.

**Type consistency:** `ingressSuffix`/`lanIp`/`lanSubnet` fact names match across facts.nix, adguard.nix, and the flake args. `acmeEmail` arg name matches between `flake.nix` `_module.args` and `cert-manager.nix`'s function head. Secret names (`cloudflare-api-token`/`api-token`, `wildcard-h-tls`) are identical across the SealedSecret, the issuer `apiTokenSecretRef`, the `Certificate.secretName`, and the `TLSStore.defaultCertificate.secretName`. Issuer names (`letsencrypt-staging`/`letsencrypt-prod`) match between Task 4 and Task 5/7.
