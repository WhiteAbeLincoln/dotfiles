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
