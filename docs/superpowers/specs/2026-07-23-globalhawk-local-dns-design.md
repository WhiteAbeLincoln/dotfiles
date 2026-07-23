# globalhawk local DNS — real names on the LAN via AdGuard Home + Tailscale

**Status:** Design (2026-07-23). Phase 1 in scope; Phase 2 (backup resolver) deferred.
**Depends on:** the landed k3s/nixidy service architecture
(`2026-07-22-globalhawk-service-arch-design.md`) — this replaces that design's
interim `.local` naming and its self-signed cert issuer.

## Goal

Reach globalhawk's k3s services by a **real, dotted name that works identically on
the LAN and remotely over Tailscale** — `radarr.h.abewhite.dev`, `plex.h.abewhite.dev`
— with **browser-trusted TLS** and **no service ever exposed to the public internet**.
This replaces the current mDNS alias scheme (`mdns-aliases.nix`, single-label
`*-globalhawk.local` CNAMEs published via `go-avahi-cname`).

Two hard constraints from the operator shaped every choice below:

1. **The internet must not depend on a home-grown box.** A spouse-test failure — "the
   internet is down" because a media box hiccuped — is unacceptable. Local *services*
   going down with the box is fine; general DNS/internet is not.
2. **Nothing is exposed to the WAN.** globalhawk sits behind a double-NAT and is never
   public (LAN + Tailscale only). See the host's network-posture notes.

`h` here is a **placeholder child label** under the operator's registered domain
(written `abewhite.dev` throughout, matching the existing placeholder in
`machine/globalhawk/facts.nix`). A dedicated child label is deliberate — see Naming.

## Decision: local resolver (AdGuard Home), not a public wildcard record

Three approaches were weighed:

- **Public wildcard `*.h.abewhite.dev → <LAN IP>` in the public zone (rejected).**
  Zero infrastructure and immune to the box being down (the record is served by the
  registrar's DNS, not by globalhawk), which is attractive against constraint 1. But it
  publishes a private-IP mapping into *public* DNS — an information leak the operator's
  cautious posture disfavours, and one the IETF/hardening guidance flags. It also gives
  no ad-blocking and no clean same-name-both-places story for Tailscale.
- **`home.arpa` (RFC 8375) (rejected).** The standards-blessed local-only name, maximally
  private (nothing in public DNS at all). But no public CA will issue for it, forcing an
  **internal CA** whose root must be imported onto every client (painful on iOS / Android
  / Roku / TVs). It also still requires a local resolver — all the cost of the chosen
  approach, plus the cert tax.
- **Local resolver on a registered subdomain (chosen).** AdGuard Home answers
  `*.h.abewhite.dev` on the LAN with globalhawk's private IP; the *name* is a registered
  subdomain so **public-CA (Let's Encrypt) wildcard certs** work via DNS-01. This is the
  common homelab consensus, folds in ad-blocking, and — crucially — keeps the
  **service A records out of public DNS** while still getting trusted TLS (only a
  transient ACME TXT is ever public). The internet-dependency risk (constraint 1) is
  handled by a public secondary resolver now and a second AdGuard instance in Phase 2.

## Naming

- **`*.h.abewhite.dev`**, one label deep under the registered domain.
- A **dedicated child label** (`h`) rather than the apex: the wildcard cannot shadow
  anything hosted on the apex, the wildcard cert is scoped to the homelab, and only the
  single label `h` ever appears in public DNS (never individual app names).
- The A record set lives **only in AdGuard** and is never published to the public zone.
- `machine/globalhawk/facts.nix`: `ingressSuffix` flips from `-globalhawk.local` to
  `.h.abewhite.dev`. Because ingress hosts are `<app>` + `ingressSuffix`, every k3s
  ingress host name updates from this one edit, and the mDNS derivation that consumed the
  old value is removed.

## Components

### Resolver — AdGuard Home, NixOS-native on globalhawk

- Runs as a **host-level NixOS service** (`services.adguardhome`, declarative config),
  in the same host/cluster tier as Avahi and Tailscale — *not* as a k3s workload. It must
  keep resolving even if k3s is unhealthy, so it cannot depend on the cluster.
- **Records are derived in-flake, not hand-maintained.** The existing derivation in
  `mdns-aliases.nix` already extracts every Ingress host from the nixidy env; that logic
  is repurposed to generate AdGuard's DNS-rewrite entries. Because *all* ingress traffic
  terminates at one Traefik IP, this collapses to a **single wildcard rewrite**
  `*.h.abewhite.dev → <globalhawk LAN IP>` (per-host rewrites remain an option but are
  unnecessary). Adding a k3s app with an ingress needs no DNS edit.
- **Ad-blocking** is enabled (a primary motivation for choosing AdGuard over a minimal
  resolver). Upstream resolvers are public (e.g. Cloudflare/Quad9); blocklists are
  declared in the module config.
- AdGuard listens on the LAN interface **and its Tailscale interface** (bind addresses),
  so remote tailnet devices can query it (see Remote access).
- `mdns-aliases.nix` and the `go-avahi-cname` input are removed; Avahi itself stays for
  other mDNS uses.

### Wiring AdGuard in as the LAN resolver

- AdGuard is set as the LAN's DNS server via the Google Fiber app.
- **A public secondary resolver (e.g. `1.1.1.1`) is configured alongside it** to satisfy
  constraint 1: if AdGuard is unavailable, clients still resolve public names and the
  internet keeps working. The known cost is that during an AdGuard outage local names and
  ad-blocking are lost and client failover to the secondary is slow/OS-dependent — an
  accepted trade for "the internet never hard-fails." Phase 2 upgrades this to a real
  second resolver.
- The box's **LAN IP must be static / DHCP-reserved** — the wildcard rewrite points at a
  fixed address (mDNS previously tracked a changing IP automatically; a flat record does
  not).

### Remote access — Tailscale, topology "2c" (subnet route)

The same `*.h.abewhite.dev` name must resolve for the operator's personal devices when
remote. AdGuard's rewrites are **global** (one answer per name), which fits a single-answer
topology and rules out per-client "views":

- **globalhawk (and, in Phase 2, the Pi) advertises the LAN subnet as a Tailscale subnet
  route.** AdGuard always returns the **LAN IP**; remote tailnet clients reach it through
  the route, LAN clients reach it directly.
- **Tailscale split-DNS** (admin console) points `h.abewhite.dev` at AdGuard's Tailscale
  (100.x) address, so remote devices resolve the homelab zone via AdGuard automatically
  (pushed to all tailnet devices; not per-device config).
- **On-LAN devices — including TVs/Rokus that cannot join the tailnet — need no Tailscale
  and no config**; they use the LAN IP directly at native speed. This is why the
  single-answer LAN-IP topology was chosen over always-returning the Tailscale IP.

Accepted downsides of this topology:

- **Remote client config:** `--accept-routes` is required to use the route — default-on
  for macOS/iOS, **off by default on Linux/Windows** (one-time flag per such device).
- **Subnet collision:** if a remote network uses the *same* RFC1918 range as the home LAN
  (`192.168.1.0/24` is extremely common), the advertised route collides and traffic may
  reach the wrong host. Mitigated by running the home LAN on an **unusual subnet**; noted
  as a risk that may be unfixable behind the Fiber box's DHCP, and one that only bites
  while remote on a colliding network.
- **Route SPOF / scope:** globalhawk down takes the route down (moot — the services are
  down too); the /24 is reachable by any tailnet member (fine on a solo tailnet; tighten
  with Tailscale ACLs if nodes are ever shared). Phase 2's Pi doubles as a backup subnet
  router (Tailscale supports multiple routers for one route with automatic failover).

### TLS — cert-manager DNS-01 wildcard (Let's Encrypt)

- cert-manager issues a **`*.h.abewhite.dev` wildcard** via **DNS-01**. This is the only
  challenge type that supports wildcards and the only one compatible with the no-inbound
  posture (no WAN 80/443). It **replaces the k3s design's interim self-signed
  `ClusterIssuer`**; that issuer was always documented as the swap point.
- **Solver: Cloudflare (via zone delegation).** The zone is registered at Porkbun with no
  Porkbun solver built into cert-manager. The chosen path (the operator's already-preferred
  one) delegates authoritative DNS to a free Cloudflare account and uses cert-manager's
  **native Cloudflare DNS-01 solver**. *Alternative:* keep DNS at Porkbun and run the
  community **Porkbun webhook** solver — rejected as the default because it adds a
  maintained in-cluster component, but it is a drop-in `ClusterIssuer` swap if moving DNS
  hosting is undesirable.
- **Secret handling:** the Cloudflare `Zone:DNS:Edit` API token is stored as a
  **SealedSecret** (already the cluster's secret mechanism), never as plaintext in the
  store.
- **Staging then prod:** a Let's Encrypt **staging** `ClusterIssuer` is used while
  validating to avoid burning prod rate limits; the prod issuer is the same shape.
- **Fully automatic after setup:** renewal (~day 60 of 90) is unattended — cert-manager
  creates/cleans the `_acme-challenge` TXT via the token each time. The only recurring
  human task is **API-token rotation**, avoided by minting a **no-expiry** token.
- The challenge path does **not** traverse AdGuard: cert-manager talks to the Cloudflare
  API directly and Let's Encrypt validates against public DNS, so split-horizon does not
  interfere. (If cert-manager's DNS self-check misbehaves under cluster DNS, point its
  recursive nameservers at a public resolver — noted as a possible gotcha, not expected.)

## External (out-of-repo) setup — one-time checklist

1. Create a free Cloudflare account; add the `abewhite.dev` zone; repoint nameservers at
   Porkbun (registrar) → Cloudflare (authoritative DNS host).
2. Mint a Cloudflare API token scoped `Zone:DNS:Edit` for that zone, **no expiry**.
3. Seal the token into a `SealedSecret` and commit it.
4. Reserve/static-assign globalhawk's LAN IP in the Fiber app.
5. Set AdGuard as the LAN's primary DNS + a public secondary (e.g. `1.1.1.1`) in the
   Fiber app.
6. In the Tailscale admin console: approve globalhawk's subnet route and configure
   split-DNS for `h.abewhite.dev` → AdGuard's 100.x address.
7. On Linux/Windows tailnet clients: enable `--accept-routes`.

Everything else (AdGuard config + records, the two `ClusterIssuer`s, the wildcard
`Certificate`, the `ingressSuffix` flip, removal of the mDNS units) is in this flake and
delivered by `nixos-rebuild switch`.

## Phases

- **Phase 1 (in scope).** globalhawk AdGuard Home (records derived from nixidy ingress
  hosts, ad-blocking, LAN + Tailscale bind); flip `ingressSuffix`; remove
  `mdns-aliases.nix` / `go-avahi-cname`; Tailscale subnet route + split-DNS; cert-manager
  DNS-01 wildcard (staging → prod) replacing the self-signed issuer; Fiber-app DNS
  (primary = AdGuard, secondary = public). Completion is **not** gated on any second
  device.
- **Phase 2 (deferred).** A spare Pi added as another `nixosConfigurations.<pi>` host in
  this flake, running a twin AdGuard fed the **same in-flake-generated records** (Nix is
  the replication mechanism — no zone transfer). It becomes the DHCP **secondary
  resolver** (replacing the public secondary) and a **backup Tailscale subnet router**.
  Deferred because it requires physically setting up hardware and Phase 1 already meets
  the internet-never-fails bar via the public secondary. Kept out of k3s deliberately — a
  backup resolver managed by the control plane running on the node it backs up is a
  dependency loop.

## Security posture

- No service is exposed to the WAN; DNS-01 needs no inbound ports. Only a transient
  `_acme-challenge` TXT and the operator's pre-existing public records are ever in public
  DNS — never service names or private IPs.
- The Mullvad/WireGuard-style secret precedent stands: the Cloudflare token lives only as
  a SealedSecret, never plaintext in `/nix/store`.
- Tailscale route scope is the one widened surface (whole /24 to the tailnet); acceptable
  on a solo tailnet, tightenable via ACLs.

## Validation

- `nix flake check` and `nixidy build` evaluate cleanly; the mDNS derivation is gone with
  no dangling references to `ingressSuffix`'s old value.
- From a LAN client: `dig radarr.h.abewhite.dev` returns globalhawk's LAN IP via AdGuard;
  the service loads over HTTPS with a **publicly-trusted** cert (no warning, no imported
  root).
- Ad-blocking active (a known ad domain resolves to `0.0.0.0`/blocked in AdGuard).
- With AdGuard stopped, a LAN client still resolves public names via the secondary (the
  internet-never-fails gate).
- From a remote device on the tailnet (not on the LAN): the same
  `radarr.h.abewhite.dev` resolves (split-DNS) and connects (subnet route) with the same
  cert.
- cert-manager reports the `*.h.abewhite.dev` Certificate `Ready`; a forced renewal
  against staging succeeds unattended.

## Out of scope

- The IdP / forward-auth / SSO work (still a separate follow-on, unchanged by this spec).
- Migrating Plex/calibre-web behind ingress beyond what the k3s design already covers —
  they simply gain the new hostname via the `ingressSuffix` change.
- Per-client DNS "views" (topology 2a) — precluded by AdGuard's global rewrites and
  unnecessary given the subnet-route topology.
