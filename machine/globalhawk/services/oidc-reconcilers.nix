# One-shot OIDC reconcilers. Job names include the desired-config hash, so a
# declarative OIDC change creates a new Job without continuously fighting UI
# edits elsewhere. Secrets are mounted from sops-rendered Kubernetes Secrets.
{
  config,
  lib,
  ...
}: let
  ingressSuffix = config.homelab.ingressSuffix;
  mediaRoot = config.homelab.media.root;
  mediaUid = config.users.users._media.uid;
in {
  services.k3s.workloads.module = {...}: let
    curlJqImage = "docker.io/dwdraju/alpine-curl-jq@sha256:eb00b3d4864c03814885a1c15ed1f5b2b569ca102ad4d02c27d582affb4fd6b1";
    sqliteImage = "docker.io/keinos/sqlite3@sha256:2d168434d3de0f65ebd742cf6340e06dfe550b419c3acd2e6d286687deccd69a";
    issuer = "https://auth${ingressSuffix}";

    immichScript = ''
      set -eu
      api=http://immich-server.immich.svc.cluster.local:2283/api
      key=$(cat /secret/admin-api-key)
      client_secret=$(cat /secret/client-secret)

      until current=$(curl -fsS -H "x-api-key: $key" "$api/system-config"); do
        echo "waiting for Immich"
        sleep 5
      done

      echo "$current" | jq \
        --arg issuer "${issuer}" \
        --arg client_secret "$client_secret" \
        '.oauth = (.oauth + {
          enabled: true,
          issuerUrl: $issuer,
          clientId: "immich",
          clientSecret: $client_secret,
          scope: "openid profile email",
          buttonText: "Login with SSO",
          autoRegister: true,
          autoLaunch: false,
          tokenEndpointAuthMethod: "client_secret_post",
          mobileOverrideEnabled: false,
          mobileRedirectUri: ""
        })' >/tmp/config.json

      curl -fsS -X PUT \
        -H "x-api-key: $key" \
        -H "Content-Type: application/json" \
        --data-binary @/tmp/config.json \
        "$api/system-config" >/dev/null
      echo "Immich OIDC reconciled"
    '';
    immichHash = builtins.substring 0 12 (builtins.hashString "sha256" immichScript);

    absScript = ''
      set -eu
      api=http://audiobookshelf.library.svc.cluster.local:13378
      token=$(cat /secret/admin-token)
      client_secret=$(cat /secret/client-secret)
      issuer="${issuer}"

      until discovery=$(curl -fsS "$issuer/.well-known/openid-configuration"); do
        echo "waiting for Authelia discovery"
        sleep 5
      done

      payload=$(echo "$discovery" | jq \
        --arg issuer "$issuer" \
        --arg client_secret "$client_secret" \
        '{
          authActiveAuthMethods: ["local", "openid"],
          authOpenIDIssuerURL: $issuer,
          authOpenIDAuthorizationURL: .authorization_endpoint,
          authOpenIDTokenURL: .token_endpoint,
          authOpenIDUserInfoURL: .userinfo_endpoint,
          authOpenIDJwksURL: .jwks_uri,
          authOpenIDLogoutURL: .end_session_endpoint,
          authOpenIDClientID: "audiobookshelf",
          authOpenIDClientSecret: $client_secret,
          authOpenIDTokenSigningAlgorithm: "RS256",
          authOpenIDButtonText: "Login with SSO",
          authOpenIDAutoLaunch: false,
          authOpenIDAutoRegister: true,
          authOpenIDMatchExistingBy: "email",
          authOpenIDMobileRedirectURIs: ["audiobookshelf://oauth"],
          authOpenIDSubfolderForRedirectURLs: ""
        }')

      until curl -fsS -X PATCH \
        -H "Authorization: Bearer $token" \
        -H "Content-Type: application/json" \
        --data-binary "$payload" \
        "$api/api/auth-settings" >/dev/null; do
        echo "waiting for Audiobookshelf"
        sleep 5
      done
      echo "Audiobookshelf OIDC reconciled"
    '';
    absHash = builtins.substring 0 12 (builtins.hashString "sha256" absScript);

    # CWA has no settings API. Its own ConfigSQL schema is updated transactionally
    # in SQLite; the operator restarts the CWA pod after this Job completes so its
    # Flask-Dance blueprints reload the new provider.
    cwaScript = ''
      set -eu
      client_secret_hex=$(od -An -v -tx1 /secret/client-secret | tr -d ' \n')
      sqlite3 /config/app.db <<SQL
      BEGIN IMMEDIATE;
      UPDATE oauthProvider
         SET oauth_client_id = 'calibre-web',
             oauth_client_secret = CAST(X'$client_secret_hex' AS TEXT),
             oauth_base_url = '${issuer}',
             oauth_authorize_url = '${issuer}/api/oidc/authorization',
             oauth_token_url = '${issuer}/api/oidc/token',
             oauth_userinfo_url = '${issuer}/api/oidc/userinfo',
             metadata_url = '${issuer}/.well-known/openid-configuration',
             scope = 'openid profile email',
             username_mapper = 'preferred_username',
             email_mapper = 'email',
             login_button = 'Login with SSO',
             active = 1
       WHERE provider_name = 'generic';
      UPDATE settings
         SET config_login_type = 2,
             config_oauth_redirect_host = 'https://books${ingressSuffix}',
             config_disable_standard_login = 0
       WHERE id = 1;
      COMMIT;
      SQL
      echo "Calibre-Web OIDC reconciled; restart its pod to load the provider"
    '';
    cwaHash = builtins.substring 0 12 (builtins.hashString "sha256" cwaScript);

    scriptMode = 493;
  in {
    applications = {
      immich-oidc-reconciler = {
        namespace = "immich";
        createNamespace = false;
        resources = {
          configMaps.immich-oidc-script.data."reconcile.sh" = immichScript;
          jobs."immich-oidc-${immichHash}".spec = {
            backoffLimit = 6;
            template.spec = {
              restartPolicy = "OnFailure";
              containers.reconcile = {
                image = curlJqImage;
                command = ["sh" "/script/reconcile.sh"];
                volumeMounts = [
                  {
                    name = "script";
                    mountPath = "/script";
                  }
                  {
                    name = "secret";
                    mountPath = "/secret";
                    readOnly = true;
                  }
                ];
              };
              volumes = [
                {
                  name = "script";
                  configMap = {
                    name = "immich-oidc-script";
                    defaultMode = scriptMode;
                  };
                }
                {
                  name = "secret";
                  secret.secretName = "immich-oidc";
                }
              ];
            };
          };
        };
      };

      abs-oidc-reconciler = {
        namespace = "library";
        createNamespace = false;
        resources = {
          configMaps.abs-oidc-script.data."reconcile.sh" = absScript;
          jobs."abs-oidc-${absHash}".spec = {
            backoffLimit = 6;
            template.spec = {
              restartPolicy = "OnFailure";
              containers.reconcile = {
                image = curlJqImage;
                command = ["sh" "/script/reconcile.sh"];
                volumeMounts = [
                  {
                    name = "script";
                    mountPath = "/script";
                  }
                  {
                    name = "secret";
                    mountPath = "/secret";
                    readOnly = true;
                  }
                ];
              };
              volumes = [
                {
                  name = "script";
                  configMap = {
                    name = "abs-oidc-script";
                    defaultMode = scriptMode;
                  };
                }
                {
                  name = "secret";
                  secret.secretName = "abs-oidc";
                }
              ];
            };
          };
        };
      };

      cwa-oidc-reconciler = {
        namespace = "library";
        createNamespace = false;
        resources = {
          configMaps.cwa-oidc-script.data."reconcile.sh" = cwaScript;
          jobs."cwa-oidc-${cwaHash}".spec = {
            backoffLimit = 3;
            template.spec = {
              restartPolicy = "OnFailure";
              securityContext = {
                runAsUser = mediaUid;
                runAsGroup = mediaUid;
                fsGroup = mediaUid;
              };
              containers.reconcile = {
                image = sqliteImage;
                command = ["sh" "/script/reconcile.sh"];
                volumeMounts = [
                  {
                    name = "script";
                    mountPath = "/script";
                  }
                  {
                    name = "secret";
                    mountPath = "/secret";
                    readOnly = true;
                  }
                  {
                    name = "config";
                    mountPath = "/config";
                  }
                ];
              };
              volumes = [
                {
                  name = "script";
                  configMap = {
                    name = "cwa-oidc-script";
                    defaultMode = scriptMode;
                  };
                }
                {
                  name = "secret";
                  secret.secretName = "cwa-oidc";
                }
                {
                  name = "config";
                  hostPath = {
                    path = "${mediaRoot}/apps/calibre-web-automated/config";
                    type = "Directory";
                  };
                }
              ];
            };
          };
        };
      };
    };
  };
}
