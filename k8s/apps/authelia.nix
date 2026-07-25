# Authelia SSO — a nixidy Helm release (chart vendored in ../../charts/authelia).
# Core config authored as Nix under `values.configMap.*`; every secret field is
# pinned to a sops-rendered Secret via {secret_name,path} (declared in
# secret.additionalSecrets), so the chart generates NO secret of its own. SQLite
# persists on the host /var/lib/authelia (authelia-storage.nix) mounted at /config.
# The OIDC provider + clients are added in Phase D. Value paths were render-
# validated against chart 0.11.6 (app v4.39.x).
{
  lib,
  charts,
  ingressSuffix,
  autheliaUid,
  smtpSender,
  smtpUser,
  smtp,
  ...
}: let
  host = "auth${ingressSuffix}"; # auth.h.abrahamwhite.com
  cookieDomain = "h.abrahamwhite.com";
in {
  applications.authelia = {
    namespace = "auth";
    createNamespace = false; # created by infra/auth-network.nix
    helm.releases.authelia = {
      chart = charts.authelia;
      # Authelia's values.schema.json $refs a handful of shared k8s-type
      # definitions from https://charts.authelia.com/definitions.json (for
      # Gateway/NetworkPolicy/HPA fields we don't set). `helm template` fetches
      # that over the network to validate, which the sandboxed Nix build
      # forbids — skip schema validation rather than grant network access.
      extraOpts = ["--skip-schema-validation"];
      values = {
        pod = {
          kind = "Deployment";
          replicas = 1;
          strategy.type = "Recreate"; # single writer on the SQLite hostPath
          securityContext = {
            pod.fsGroup = autheliaUid;
            container = {
              runAsUser = autheliaUid;
              runAsGroup = autheliaUid;
            };
          };
          extraVolumes = [
            {
              name = "data";
              hostPath = {
                path = "/var/lib/authelia";
                type = "Directory";
              };
            }
          ];
          extraVolumeMounts = [
            {
              name = "data";
              mountPath = "/config";
            }
          ];
        };
        persistence.enabled = false; # using the hostPath above, not a PVC
        service.port = 9091;
        ingress = {
          enabled = true;
          className = "traefik";
          tls.enabled = true; # secretName dropped by the resources patch below
        };
        secret.additionalSecrets = {
          "authelia-secrets" = {};
          "authelia-users" = {};
          "authelia-oidc-key" = {};
          "authelia-oidc-client-hashes" = {};
        };
        configMap = {
          theme = "auto";
          identity_validation.reset_password.secret = {
            secret_name = "authelia-secrets";
            path = "jwt";
          };
          session = {
            cookies = [
              {
                subdomain = "auth";
                domain = cookieDomain;
              }
            ];
            encryption_key = {
              secret_name = "authelia-secrets";
              path = "session";
            };
          };
          storage = {
            encryption_key = {
              secret_name = "authelia-secrets";
              path = "storage-encryption";
            };
            local.enabled = true;
          };
          authentication_backend.file = {
            enabled = true;
            path = "/secrets/authelia-users/users_database.yml";
          };
          identity_providers.oidc = {
            enabled = true;
            hmac_secret = {
              secret_name = "authelia-secrets";
              path = "oidc-hmac";
            };
            authorization_policies.family = {
              default_policy = "deny";
              rules = [
                {
                  policy = "one_factor";
                  subject = ["group:family" "group:admins"];
                }
              ];
            };
            jwks = [
              {
                key_id = "main";
                algorithm = "RS256";
                use = "sig";
                key.path = "/secrets/authelia-oidc-key/issuer.pem";
              }
            ];
            clients = [
              {
                client_id = "immich";
                client_name = "Immich";
                client_secret.path = "/secrets/authelia-oidc-client-hashes/immich";
                authorization_policy = "family";
                redirect_uris = [
                  "app.immich:///oauth-callback"
                  "https://photos${ingressSuffix}/auth/login"
                  "https://photos${ingressSuffix}/user-settings"
                ];
                scopes = ["openid" "profile" "email"];
                token_endpoint_auth_method = "client_secret_post";
              }
              {
                client_id = "audiobookshelf";
                client_name = "Audiobookshelf";
                client_secret.path = "/secrets/authelia-oidc-client-hashes/audiobookshelf";
                authorization_policy = "family";
                redirect_uris = [
                  "https://audiobooks${ingressSuffix}/auth/openid/callback"
                  "https://audiobooks${ingressSuffix}/auth/openid/mobile-redirect"
                ];
                scopes = ["openid" "profile" "email"];
                token_endpoint_auth_method = "client_secret_basic";
              }
              {
                client_id = "calibre-web";
                client_name = "Calibre-Web";
                client_secret.path = "/secrets/authelia-oidc-client-hashes/calibre-web";
                authorization_policy = "family";
                redirect_uris = [
                  "https://books${ingressSuffix}/login/generic/authorized"
                ];
                scopes = ["openid" "profile" "email"];
                token_endpoint_auth_method = "client_secret_basic";
              }
            ];
          };
          notifier.smtp = {
            enabled = true;
            address = "smtp://${smtp.host}:${toString smtp.port}";
            sender = smtpSender;
            username = smtpUser;
            password = {
              secret_name = "authelia-secrets";
              path = "smtp-password";
            };
          };
          access_control = {
            default_policy = "deny";
            rules = [
              {
                domain = [
                  "radarr${ingressSuffix}"
                  "sonarr${ingressSuffix}"
                  "prowlarr${ingressSuffix}"
                  "qbittorrent${ingressSuffix}"
                ];
                policy = "two_factor";
                subject = ["group:admins"];
              }
              {
                domain = ["books${ingressSuffix}"];
                policy = "one_factor";
                subject = ["group:family" "group:admins"];
              }
            ];
          };
        };
      };
    };
    # Use Traefik's default *.h wildcard cert (like every other app): drop the
    # chart's per-app secretName from the rendered Ingress named `authelia`.
    resources.ingresses.authelia.spec.tls = lib.mkForce [{hosts = [host];}];
  };
}
