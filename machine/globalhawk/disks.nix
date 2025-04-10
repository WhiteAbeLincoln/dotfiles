{ config, pkgs, ... }:

let
  secrets = import ../../secrets/common.nix;
in
{
  boot.supportedFilesystems = ["zfs"];
  boot.zfs.forceImportRoot = false;
  # boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  networking.hostId = "077fcbce";

  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  fileSystems = {
    "/data/Media" = {
      device = "pool/media";
      fsType = "zfs";
    };
  };

  systemd.tmpfiles.rules = [
    # user rwx, group rwx, other rx
    "d /data/Media 0775 _media _media -"
    "d /data/Media/apps 0775 _media _media -"
    # ensure new files are created with the correct permissions using ACL
    "A /data/Media - - - - group:_media:rwx"
    "A /data/Media/books - - - - group:calibre-web:rwx"
  ];

  programs.msmtp = {
    enable = true;
    setSendmail = true;
    defaults = {
      tls = "on";
      aliases = "/etc/aliases";
      tls_trust_file = "/etc/ssl/certs/ca-certificates.crt";
    };
    accounts = {
      default = {
        auth = "on";
        host = "smtp.gmail.com";
        port = 587;
        user = "abelincoln.white@gmail.com";
        password = secrets.gmail_password;
        from = "abelincoln.white@gmail.com";
      };
    };
  };

  environment.etc = {
    "aliases" = {
      text = ''
        root: abelincoln.white@gmail.com
      '';
      mode = "0644";
    };
  };

  services.zfs.zed.settings = {
    ZED_DEBUG_LOG = "/tmp/zed.debug.log";
    ZED_EMAIL_ADDR = [ "root" ];
    ZED_EMAIL_PROG = "${pkgs.msmtp}/bin/msmtp";
    ZED_EMAIL_OPTS = "@ADDRESS@";

    ZED_NOTIFY_INTERVAL_SECS = 3600;
    ZED_NOTIFY_VERBOSE = true;

    ZED_USE_ENCLOSURE_LEDS = true;
    ZED_SCRUB_AFTER_RESILVER = true;
  };
  # this option does not work; will return error
  services.zfs.zed.enableMail = false;

  services.smartd = {
    enable = true;
    autodetect = true;
    notifications.mail.enable = true;
  };
}
