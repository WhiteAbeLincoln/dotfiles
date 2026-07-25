# Traefik forward-auth middleware: unauthenticated requests to the protected
# ingresses are sent to Authelia's forward-auth endpoint, which redirects to the
# portal. Authelia's identity headers are copied back to the backend.
{...}: {
  applications.forward-auth = {
    namespace = "media";
    createNamespace = false;
    yamls = [
      (builtins.toJSON {
        apiVersion = "traefik.io/v1alpha1";
        kind = "Middleware";
        metadata = {
          name = "forward-auth";
          namespace = "media";
        };
        spec.forwardAuth = {
          address = "http://authelia.auth.svc.cluster.local:9091/api/authz/forward-auth";
          authResponseHeaders = [
            "Remote-User"
            "Remote-Groups"
            "Remote-Email"
            "Remote-Name"
          ];
        };
      })
    ];
  };
}
