# Host-wide outbound mail transport. ZED, smartd, restic, and anything using
# sendmail(1) deliver through this shared msmtp account; Authelia consumes the
# same non-secret endpoint from the configured account through the nixidy module
# arguments.
{config, ...}: let
  secrets = import ../../secrets/globalhawk.nix;
in {
  programs.msmtp = {
    enable = true;
    setSendmail = true;
    defaults = {
      tls = "on";
      aliases = "/etc/aliases";
      tls_trust_file = "/etc/ssl/certs/ca-certificates.crt";
    };
    accounts.default = {
      auth = "on";
      host = "smtp.mail.me.com";
      port = 587;
      tls = true;
      tls_starttls = true;
      # The account username + From must be the operator's iCloud custom-domain
      # address (kept in secrets/, git-crypt). Reference the attr path, never
      # the literal — this is a public repo.
      user = secrets.mail.smtpUser;
      # Read at send time from the sops runtime file (root-owned), keeping the
      # app password out of the world-readable store. Current senders run as
      # root, which can read /run/secrets.
      passwordeval = "cat ${config.sops.secrets.smtp_password.path}";
      from = secrets.mail.fromAddress;
    };
  };

  environment.etc.aliases = {
    text = ''
      root: ${secrets.mail.fromAddress}
    '';
    mode = "0644";
  };
}
