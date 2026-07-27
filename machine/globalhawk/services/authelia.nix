# Authelia SSO — a nixidy Helm release (chart vendored in ../../charts/authelia).
# Core config authored as Nix under `values.configMap.*`; every secret field is
# pinned to a sops-rendered Secret via {secret_name,path} (declared in
# secret.additionalSecrets), so the chart generates NO secret of its own. SQLite
# persists on the host /var/lib/authelia (authelia-storage.nix) mounted at /config.
# The OIDC provider + clients are added in Phase D. Value paths were render-
# validated against chart 0.11.6 (app v4.39.x).
{config, ...}: let
  secrets = import ../../../secrets/globalhawk.nix;
  ingressSuffix = config.homelab.ingressSuffix;
  autheliaUid = config.users.users.authelia.uid;
  smtpSender = secrets.mail.fromAddress;
  smtpUser = secrets.mail.smtpUser;
  smtp = {
    inherit (config.programs.msmtp.accounts.default) host port;
  };
  cookieDomain = "h.abrahamwhite.com";
  host = "auth${ingressSuffix}"; # auth.h.abrahamwhite.com
in {
  # Host-side identity + a persistent dir for Authelia's SQLite storage (the k8s
  # workload lives in k8s/apps/authelia.nix). Authelia runs as its OWN uid and its
  # state dir is 0750 authelia:authelia. The pod mounts /var/lib/authelia via a
  # hostPath patched onto the Helm-rendered Deployment.
  users.users.authelia = {
    isSystemUser = true;
    group = "authelia";
    uid = 989;
    description = "Authelia SSO";
  };
  users.groups.authelia.gid = config.users.users.authelia.uid;

  systemd.tmpfiles.rules = [
    "d /var/lib/authelia 0750 authelia authelia - -"
  ];

  services.k3s.workloads.module = {
    lib,
    charts,
    ...
  }: {
    # Authelia lives in its own namespace with the same boundary as media/immich:
    # default-deny-ingress, re-opened only for intra-namespace traffic and the
    # Traefik ingress controller (kube-system). Traefik is the sole external source
    # and it fronts the portal, the forward-auth callback, AND the OIDC endpoints,
    # so no other cross-namespace ingress is needed.
    applications.auth-network = {
      namespace = "auth";
      createNamespace = true;
      yamls = [
        (builtins.toJSON {
          apiVersion = "networking.k8s.io/v1";
          kind = "NetworkPolicy";
          metadata = {
            name = "allow-intra-and-ingress";
            namespace = "auth";
          };
          spec = {
            podSelector = {};
            policyTypes = ["Ingress"];
            ingress = [
              {from = [{podSelector = {};}];}
              {
                from = [
                  {
                    namespaceSelector.matchLabels."kubernetes.io/metadata.name" = "kube-system";
                  }
                ];
              }
            ];
          };
        })
      ];
    };

    # OIDC relying parties must resolve Authelia's canonical HTTPS issuer from
    # inside the cluster. The name is LAN-private in AdGuard, while k3s CoreDNS
    # forwards through the host's non-AdGuard resolver. Rewrite it to Traefik's
    # cluster Service; an exact CoreDNS rewrite also rewrites the answer name back,
    # so clients still see and validate the canonical TLS/issuer hostname.
    applications.coredns-custom = {
      namespace = "kube-system";
      createNamespace = false;
      resources.configMaps.coredns-custom.data."private-ingress.override" = ''
        rewrite name exact auth${ingressSuffix} traefik.kube-system.svc.cluster.local
      '';
    };

    # Traefik forward-auth middleware: unauthenticated requests to the protected
    # ingresses are sent to Authelia's forward-auth endpoint, which redirects to the
    # portal. Authelia's identity headers are copied back to the backend.
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
              authorization_policies = {
                family = {
                  default_policy = "deny";
                  rules = [
                    {
                      policy = "one_factor";
                      subject = ["group:family" "group:admins"];
                    }
                  ];
                };
                admin = {
                  default_policy = "deny";
                  rules = [
                    {
                      policy = "two_factor";
                      subject = ["group:admins"];
                    }
                  ];
                };
              };
              claims_policies.grafana.id_token = [
                "email"
                "name"
                "groups"
                "preferred_username"
              ];
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
                {
                  client_id = "grafana";
                  client_name = "Grafana";
                  client_secret.path = "/secrets/authelia-oidc-client-hashes/grafana";
                  authorization_policy = "admin";
                  claims_policy = "grafana";
                  public = false;
                  require_pkce = true;
                  pkce_challenge_method = "S256";
                  redirect_uris = [
                    "https://grafana${ingressSuffix}/login/generic_oauth"
                  ];
                  scopes = ["openid" "profile" "email" "groups"];
                  response_types = ["code"];
                  grant_types = ["authorization_code"];
                  token_endpoint_auth_method = "client_secret_basic";
                  access_token_signed_response_alg = "none";
                  userinfo_signed_response_alg = "none";
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
  };
}
