# Public-trusted TLS via Let's Encrypt DNS-01. No inbound 80/443 exists
# (double-NAT), so HTTP-01 is impossible; DNS-01 needs only outbound API calls to
# create a transient _acme-challenge TXT — the sole thing that ever hits public
# DNS (service A records stay private in AdGuard). Replaces the earlier
# self-signed internal CA. Two issuers: `letsencrypt-staging` for validating the
# plumbing without burning prod rate limits, `letsencrypt-prod` for real certs.
#
# The Cloudflare API token (Zone:DNS:Edit + Zone:Read) is a SealedSecret:
# encrypted to the cluster key, safe in the world-readable store + git. Authored
# as raw CRs via `yamls` (nixidy has no typed options for cert-manager /
# sealed-secrets CRDs).
{acmeEmail, ...}: let
  # Sealed to Secret/cloudflare-api-token in the cert-manager namespace (strict
  # scope). Produced by `kubeseal --raw --scope strict --namespace cert-manager
  # --name cloudflare-api-token`. Safe to commit — it's encrypted to the cluster.
  cloudflareTokenSealed = "AgAtsq/xdaI4a9WZA/qE9D77WXqSTQaIb0UuIAFnh6oeb0bjUa8QaRuyXckegfJe0O8Ln0W0CaFzNEEdyDNCDk0PO9SuUblliJ5/s1QIaoH2BjmuOHt6mDdsoj5Ke4D9QaVa0+8dpS4P4zx5bLR6Rqy1PxlJdtsqhN6O5ERiu+D4VJsHlAhS/ugEqOYuExlU5jkgHnMRjvWr5AYiQ8eLC9UYO1sV+7D8q+kb2cD31JnbGA/t93/yQToJ4FaY6z7UqTgsoEpRVvsOAwh6bVZTCaVju0FxZ2Vbdrq61CC6RvoALCwBSUjVRs8lSZPqR2hmkgz7gHTiETq4eIGcAuIDNEHxXPeVL0uy2dqRvTG8OBIj3GhKGmusoWEwgPRoh/90LjYDsfVXISYPM5EvIrSv7bY9EPhO1J9PpTYyeD1ZMy8BFJaLWaeUww1MhkdkXV1DaUEcjiK9ovicQhKaBmg8gY6X2j7OeniUTBgH3VixUz9jlHLGfWcpxHuB125ZzMh3+QYy1lB6nbyznanaAZ1DaqKaRzLfCNqqh7oNjP0nS1gbORpFJFmsWDhTUlYhRiHA+MENooWkfYarrK5mwNZ1t8C1NavIeuRYcO6naWJO9Rxw2SUzEK25vg4EnqXoCOlneIwDKvZeJEprrWqNo3z4AoxEc8Pz21fI5V00XQDYPyQfm1nXQ8kaDDhjsMdNt4Fw/HoQtBqSerfI6kro0NBEsBkD9HcmEsqgAytBIXW88bJ/ifztqNg524DkH6Q62hOC68koqXiUHA==";
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
