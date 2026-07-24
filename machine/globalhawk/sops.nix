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
      gmail_password = {};
      restic_b2_key_id = {};
      restic_b2_app_key = {};
      pokestop_psk = {};
      cf_api_token = {};
      mullvad_wg_key = {};
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
    };
  };
}
