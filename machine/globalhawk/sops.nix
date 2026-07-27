# sops-nix: the single mechanism for globalhawk RUNTIME secrets. The decryption
# key is derived from the host's SSH ed25519 key (ssh-to-age), so the host
# decrypts at ACTIVATION with no key to provision, and plaintext never enters
# the world-readable Nix store or git. Host secrets render to /run/secrets
# (tmpfs); k8s Secrets render as manifests into k3s's auto-deploy dir. See
# docs/superpowers/specs/2026-07-23-globalhawk-secrets-sops-design.md.
{
  config,
  inputs,
  ...
}: {
  imports = [inputs.sops-nix.nixosModules.sops];

  # Kubernetes Secrets are declared through the typed runtime-secret interface.
  # The module renders these into k3s's auto-deploy directory at activation.
  services.k3s.runtimeSecrets = {
    # These names and keys are load-bearing: cert-manager reads
    # cloudflare-api-token/api-token, and gluetun reads
    # mullvad-wg/WIREGUARD_PRIVATE_KEY.
    cloudflare-api-token = {
      namespace = "cert-manager";
      # Preserve the existing activation output consumed by the running host.
      manifestStem = "cloudflare-token";
      stringData.api-token.sopsSecret = "cf_api_token";
    };
    mullvad-wg = {
      namespace = "media";
      stringData.WIREGUARD_PRIVATE_KEY.sopsSecret = "mullvad_wg_key";
    };
    # Both immich-server (DB_PASSWORD) and immich-postgres
    # (POSTGRES_PASSWORD) read immich-db/password.
    immich-db = {
      namespace = "immich";
      stringData.password.sopsSecret = "immich_db_password";
    };
    # Authelia's JWT, session, storage-encryption, OIDC HMAC, and SMTP secrets
    # are single-line values, so the runtime-secret module emits stringData.
    authelia-secrets = {
      namespace = "auth";
      stringData = {
        jwt.sopsSecret = "authelia_jwt";
        session.sopsSecret = "authelia_session";
        storage-encryption.sopsSecret = "authelia_storage_encryption";
        oidc-hmac.sopsSecret = "authelia_oidc_hmac";
        smtp-password.sopsSecret = "smtp_password";
      };
    };
    # Kubernetes data requires base64 input. Keep the multi-line issuer PEM
    # pre-base64-encoded in sops so activation can substitute it as one value.
    authelia-oidc-key = {
      namespace = "auth";
      data."issuer.pem".sopsSecret = "authelia_oidc_issuer_key";
    };
    # The complete multi-line users database is likewise stored pre-base64 in
    # sops and must remain under data rather than stringData.
    authelia-users = {
      namespace = "auth";
      data."users_database.yml".sopsSecret = "authelia_users";
    };
    # Authelia receives only the argon2 client-secret hashes here; each relying
    # party receives its matching plaintext client secret in its Secret below.
    authelia-oidc-client-hashes = {
      namespace = "auth";
      stringData = {
        immich.sopsSecret = "immich_oidc_client_secret_hash";
        audiobookshelf.sopsSecret = "abs_oidc_client_secret_hash";
        calibre-web.sopsSecret = "cwa_oidc_client_secret_hash";
        grafana.sopsSecret = "grafana_oidc_client_secret_hash";
      };
    };
    immich-oidc = {
      namespace = "immich";
      stringData = {
        client-secret.sopsSecret = "immich_oidc_client_secret";
        admin-api-key.sopsSecret = "immich_admin_api_key";
      };
    };
    abs-oidc = {
      namespace = "library";
      stringData = {
        client-secret.sopsSecret = "abs_oidc_client_secret";
        admin-token.sopsSecret = "abs_admin_token";
      };
    };
    cwa-oidc = {
      namespace = "library";
      stringData.client-secret.sopsSecret = "cwa_oidc_client_secret";
    };
    grafana-secrets = {
      namespace = "monitoring";
      stringData = {
        admin-password.sopsSecret = "grafana_admin_password";
        oidc-client-secret.sopsSecret = "grafana_oidc_client_secret";
      };
    };
    plex-exporter = {
      namespace = "monitoring";
      stringData.token.sopsSecret = "plex_api_token";
    };
    adguard-exporter = {
      namespace = "monitoring";
      stringData.password.sopsSecret = "adguard_metrics_password";
    };
  };

  sops = {
    defaultSopsFile = ../../secrets/globalhawk.sops.yaml;
    # Derive the age identity from the SSH host key — nothing else to manage.
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];

    # Every key is declared here so it renders to /run/secrets/<name> for direct
    # file consumers AND so config.sops.placeholder.<name> exists for the
    # templates below and typed runtime-secret declarations above — sops-nix
    # only defines a placeholder for a declared secret. The template-only keys
    # (b2 creds, psk, k8s tokens) get an
    # unused /run/secrets file too; harmless (root-only tmpfs).
    secrets = {
      restic_repo_pass = {};
      restic_repo = {};
      # Outbound SMTP password (an Apple app-specific password today). Named for
      # the ROLE, not the provider — a future mail-provider swap is a value
      # change, not a rename. Consumed by host msmtp (disks.nix) and Authelia's
      # notifier (k8s/apps/authelia.nix, via the authelia-secrets Secret).
      smtp_password = {};
      restic_b2_key_id = {};
      restic_b2_app_key = {};
      pokestop_psk = {};
      cf_api_token = {};
      mullvad_wg_key = {};
      immich_db_password = {};
      authelia_jwt = {};
      authelia_session = {};
      authelia_storage_encryption = {};
      authelia_oidc_hmac = {};
      authelia_oidc_issuer_key = {}; # base64 of the RSA private key PEM (multi-line -> data:)
      authelia_users = {}; # base64 of the whole users_database.yml (multi-line -> data:)
      # per-app OIDC client secrets (plaintext side lives with the app reconciler
      # in Task D; the HASH used by Authelia is in authelia_oidc_clients below)
      authelia_oidc_clients = {}; # rendered clients config fragment
      immich_oidc_client_secret = {};
      immich_oidc_client_secret_hash = {};
      immich_admin_api_key = {};
      abs_oidc_client_secret = {};
      abs_oidc_client_secret_hash = {};
      abs_admin_token = {};
      cwa_oidc_client_secret = {};
      cwa_oidc_client_secret_hash = {};
      grafana_admin_password = {};
      grafana_oidc_client_secret = {};
      grafana_oidc_client_secret_hash = {};
      plex_api_token = {};
      adguard_metrics_password = {};
    };

    templates = {
      # restic B2 credentials as an EnvironmentFile (systemd reads it as root
      # before the unit starts). The repo URL + repo password are separate
      # single-value secrets consumed via repositoryFile/passwordFile.
      "restic-env".content = ''
        AWS_ACCESS_KEY_ID=${config.sops.placeholder.restic_b2_key_id}
        AWS_SECRET_ACCESS_KEY=${config.sops.placeholder.restic_b2_app_key}
      '';

      # wpa_supplicant external-password file (wired as ext_password_backend=
      # file:<path>). networks.pokestop.pskRaw = "ext:pokestop_psk" resolves the
      # value from here, keeping the passphrase out of the store.
      "wireless.env".content = ''
        pokestop_psk=${config.sops.placeholder.pokestop_psk}
      '';
    };
  };
}
