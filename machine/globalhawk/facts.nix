# Host facts — the single source of truth for values that recur across BOTH the
# NixOS layer and the k8s (nixidy) workloads, so changing one here propagates
# everywhere instead of needing edits in a dozen files. Imported directly by the
# NixOS modules and threaded into the nixidy env via the flake (see flake.nix).
{
  # --- media / storage ---
  # Root of the media pool (ZFS mount). Everything else is a subpath of this.
  mediaRoot = "/data/Media";
  # The `_media` service uid/gid that owns the media tree; k8s workloads run with
  # it as PUID/PGID/fsGroup to preserve on-disk ownership.
  mediaUid = 994;

  timezone = "America/Denver";

  # The LAN network interface (hardware-stable for this box). avahi is restricted
  # to it so mDNS isn't advertised/resolved across docker bridges and k3s veths.
  lanInterface = "enp1s0";

  # Suffix appended to each app name to form its ingress host: "<app>" + suffix,
  # e.g. "radarr" -> "radarr.h.abewhite.dev". The dedicated child label `h` keeps
  # the wildcard cert + AdGuard rewrite scoped to the homelab and prevents the
  # wildcard from shadowing anything on the apex. Resolved LAN-privately by
  # AdGuard (machine/globalhawk/adguard.nix); never published to public DNS.
  # Not secret (domains are public via WHOIS; internal names already appear in
  # the committed k8s manifests). OPERATOR: set to your real registered domain.
  ingressSuffix = ".h.abewhite.dev";

  # --- cluster network ---
  # k3s is *pinned* to these CIDRs (see k3s.nix), which is what makes the
  # host-from-pods gateway a fixed, known value rather than a k3s default we're
  # implicitly relying on.
  podCidr = "10.42.0.0/16";
  serviceCidr = "10.43.0.0/16";
  # The host as seen from any pod: the node's flannel bridge (cni0), the first
  # address of the node's pod subnet. With podCidr 10.42.0.0/16 and k3s's default
  # /24 node mask, node 0 is 10.42.0.0/24 -> gateway 10.42.0.1. Pinning podCidr
  # is what guarantees this holds across a rebuild.
  hostGatewayIp = "10.42.0.1";

  # --- lan ---
  # globalhawk's reserved LAN IP (DHCP reservation in the Fiber app). AdGuard
  # answers the `*${ingressSuffix}` wildcard with this address. OPERATOR: must
  # match the reservation.
  lanIp = "192.168.1.50";
  # The LAN CIDR globalhawk advertises as a Tailscale subnet route so the same
  # name resolves+connects remotely (topology 2c). OPERATOR: confirm.
  lanSubnet = "192.168.1.0/24";
}
