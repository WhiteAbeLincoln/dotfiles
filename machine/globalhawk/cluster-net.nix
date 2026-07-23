# Cluster network constants — the single source of truth so a rebuild is
# deterministic (cattle, not pets). k3s is *pinned* to these CIDRs (see
# k3s.nix), which is what makes the host-from-pods gateway a fixed, known value
# rather than a k3s-default we're implicitly relying on. Imported by both the
# NixOS layer (k3s.nix) and the flake's nixidy env args, so nothing downstream
# hardcodes an address.
{
  podCidr = "10.42.0.0/16";
  serviceCidr = "10.43.0.0/16";
  # The host as seen from any pod: the node's flannel bridge (cni0), which is the
  # first address of the node's pod subnet. With podCidr 10.42.0.0/16 and k3s's
  # default /24 node mask, node 0 is 10.42.0.0/24 -> gateway 10.42.0.1. Pinning
  # podCidr above is what guarantees this holds across a rebuild.
  hostGatewayIp = "10.42.0.1";
}
