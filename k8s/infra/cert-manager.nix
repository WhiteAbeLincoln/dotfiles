# Public-trusted TLS via Let's Encrypt DNS-01. No inbound 80/443 exists
# (double-NAT), so HTTP-01 is impossible; DNS-01 needs only outbound API calls to
# create a transient _acme-challenge TXT — the sole thing that ever hits public
# DNS (service A records stay private in AdGuard). Replaces the earlier
# self-signed internal CA. Two issuers: `letsencrypt-staging` for validating the
# plumbing without burning prod rate limits, `letsencrypt-prod` for real certs.
#
# The Cloudflare API token (Zone:DNS:Edit + Zone:Read) is a k8s Secret
# (cloudflare-api-token / key api-token) rendered by sops-nix into the k3s
# manifests dir at activation — see machine/globalhawk/sops.nix. The issuers
# below reference it by name only. Authored as raw CRs via `yamls` (nixidy has
# no typed options for the cert-manager CRDs).
{acmeEmail, ...}: let
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
    ];
  };
}
