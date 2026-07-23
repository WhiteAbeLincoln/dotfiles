# Internal CA for LAN TLS. No inbound 80/443 exists (double-NAT), so ACME
# HTTP-01 is impossible; DNS-01/Let's Encrypt is deferred to the SSO/ingress
# follow-up. Here: a self-signed issuer bootstraps a root CA, which becomes the
# ClusterIssuer every app Ingress uses. Clients trust the root CA once imported.
#
# Authored as raw CRs via `yamls` (nixidy has no typed options for cert-manager
# CRDs, and no `resources.raw`); builtins.toJSON is valid YAML.
{...}: {
  applications.cert-manager-config = {
    namespace = "cert-manager"; # created by the upstream controller manifest
    createNamespace = false;
    yamls = map builtins.toJSON [
      {
        apiVersion = "cert-manager.io/v1";
        kind = "ClusterIssuer";
        metadata.name = "selfsigned-bootstrap";
        spec.selfSigned = {};
      }
      {
        apiVersion = "cert-manager.io/v1";
        kind = "Certificate";
        metadata = {
          name = "globalhawk-ca";
          namespace = "cert-manager";
        };
        spec = {
          isCA = true;
          commonName = "globalhawk-internal-ca";
          secretName = "globalhawk-ca-key-pair";
          duration = "87600h"; # 10y
          privateKey = {
            algorithm = "ECDSA";
            size = 256;
          };
          issuerRef = {
            name = "selfsigned-bootstrap";
            kind = "ClusterIssuer";
            group = "cert-manager.io";
          };
        };
      }
      {
        apiVersion = "cert-manager.io/v1";
        kind = "ClusterIssuer";
        metadata.name = "globalhawk-ca";
        spec.ca.secretName = "globalhawk-ca-key-pair";
      }
    ];
  };
}
