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

  sops = {
    defaultSopsFile = ../../secrets/globalhawk.sops.yaml;
    # Derive the age identity from the SSH host key — nothing else to manage.
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];

    # Every key is declared here so it renders to /run/secrets/<name> for direct
    # file consumers AND so config.sops.placeholder.<name> exists for the
    # composite templates below — sops-nix only defines a placeholder for a
    # declared secret. The template-only keys (b2 creds, psk, k8s tokens) get an
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

      # k8s Secrets rendered straight into k3s's auto-deploy dir (root 0400,
      # never in the store or git); k3s applies them — no controller, which is
      # what lets us drop sealed-secrets. name/key/namespace are load-bearing:
      # referenced by cert-manager (cloudflare-api-token/api-token) and gluetun
      # (mullvad-wg/WIREGUARD_PRIVATE_KEY).
      "sops-cloudflare-token.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-cloudflare-token.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: cloudflare-api-token
            namespace: cert-manager
          type: Opaque
          stringData:
            api-token: ${config.sops.placeholder.cf_api_token}
        '';
      };
      "sops-mullvad-wg.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-mullvad-wg.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: mullvad-wg
            namespace: media
          type: Opaque
          stringData:
            WIREGUARD_PRIVATE_KEY: ${config.sops.placeholder.mullvad_wg_key}
        '';
      };
      # Immich Postgres password. name/key/namespace are load-bearing: the
      # immich-server (DB_PASSWORD) and immich-postgres (POSTGRES_PASSWORD)
      # containers in k8s/apps/immich.nix reference immich-db/password.
      "sops-immich-db.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-immich-db.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: immich-db
            namespace: immich
          type: Opaque
          stringData:
            password: ${config.sops.placeholder.immich_db_password}
        '';
      };
      # Authelia's scalar secrets (JWT/session/storage-encryption keys, OIDC HMAC,
      # SMTP password). All single-line, so plain stringData is safe. name/keys
      # are load-bearing: referenced by the Authelia chart values in Task B5.
      "sops-authelia-secrets.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-secrets.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-secrets
            namespace: auth
          type: Opaque
          stringData:
            jwt: ${config.sops.placeholder.authelia_jwt}
            session: ${config.sops.placeholder.authelia_session}
            storage-encryption: ${config.sops.placeholder.authelia_storage_encryption}
            oidc-hmac: ${config.sops.placeholder.authelia_oidc_hmac}
            smtp-password: ${config.sops.placeholder.smtp_password}
        '';
      };
      # OIDC issuer RSA private key. Multi-line PEM, so it MUST use data: (base64)
      # rather than stringData: — a single-token placeholder substituted into a
      # stringData block scalar is not re-indented by sops-nix, which breaks the
      # YAML at switch (passes build, fails switch). The sops value is stored
      # base64-encoded so the placeholder substitutes on one line cleanly.
      "sops-authelia-oidc-key.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-oidc-key.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-oidc-key
            namespace: auth
          type: Opaque
          data:
            issuer.pem: ${config.sops.placeholder.authelia_oidc_issuer_key}
        '';
      };
      # Authelia's users_database.yml (two-user store). Multi-line, so same rule
      # as the OIDC key above: data: with the sops value stored base64-encoded.
      "sops-authelia-users.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-users.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-users
            namespace: auth
          type: Opaque
          data:
            users_database.yml: ${config.sops.placeholder.authelia_users}
        '';
      };
      # Authelia reads the argon2 client-secret hashes while each relying party
      # receives only its matching plaintext secret below.
      "sops-authelia-oidc-client-hashes.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-authelia-oidc-client-hashes.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: authelia-oidc-client-hashes
            namespace: auth
          type: Opaque
          stringData:
            immich: ${config.sops.placeholder.immich_oidc_client_secret_hash}
            audiobookshelf: ${config.sops.placeholder.abs_oidc_client_secret_hash}
            calibre-web: ${config.sops.placeholder.cwa_oidc_client_secret_hash}
        '';
      };
      "sops-immich-oidc.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-immich-oidc.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: immich-oidc
            namespace: immich
          type: Opaque
          stringData:
            client-secret: ${config.sops.placeholder.immich_oidc_client_secret}
            admin-api-key: ${config.sops.placeholder.immich_admin_api_key}
        '';
      };
      "sops-abs-oidc.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-abs-oidc.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: abs-oidc
            namespace: library
          type: Opaque
          stringData:
            client-secret: ${config.sops.placeholder.abs_oidc_client_secret}
            admin-token: ${config.sops.placeholder.abs_admin_token}
        '';
      };
      "sops-cwa-oidc.yaml" = {
        path = "/var/lib/rancher/k3s/server/manifests/sops-cwa-oidc.yaml";
        mode = "0400";
        owner = "root";
        content = ''
          apiVersion: v1
          kind: Secret
          metadata:
            name: cwa-oidc
            namespace: library
          type: Opaque
          stringData:
            client-secret: ${config.sops.placeholder.cwa_oidc_client_secret}
        '';
      };
    };
  };
}
