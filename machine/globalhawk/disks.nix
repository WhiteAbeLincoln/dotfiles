{
  config,
  pkgs,
  ...
}: let
  mediaRoot = config.homelab.media.root;
in {
  boot.supportedFilesystems = ["zfs"];
  boot.zfs.forceImportRoot = false;
  # boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  networking.hostId = "077fcbce";

  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  fileSystems.${mediaRoot} = {
    device = "pool/media";
    fsType = "zfs";
  };

  systemd.tmpfiles.rules = [
    # user rwx, group rwx, other rx
    "d ${mediaRoot} 0775 _media _media -"
    "d ${mediaRoot}/apps 0775 _media _media -"
    # Grant the _media group rwx on the SHARED media dirs so the media apps
    # (which run as _media) can write each other's files despite umask. NOT a
    # blanket rule over ${mediaRoot}: immich/ (isolated, own uid + media-readers
    # ACL — see immich-storage.nix) and documents/ (abe-private) are deliberately
    # omitted so enabling posixacl below doesn't expose them.
    "A+ ${mediaRoot}/anime - - - - group:_media:rwx"
    "A+ ${mediaRoot}/apps - - - - group:_media:rwx"
    "A+ ${mediaRoot}/audiobooks - - - - group:_media:rwx"
    "A+ ${mediaRoot}/docker-services - - - - group:_media:rwx"
    "A+ ${mediaRoot}/movies - - - - group:_media:rwx"
    "A+ ${mediaRoot}/music - - - - group:_media:rwx"
    "A+ ${mediaRoot}/old_books - - - - group:_media:rwx"
    "A+ ${mediaRoot}/photos - - - - group:_media:rwx"
    "A+ ${mediaRoot}/torrents - - - - group:_media:rwx"
    "A+ ${mediaRoot}/tv - - - - group:_media:rwx"
    # (books keeps its own A+ line below.)
    # App state for the k3s ebook/audiobook workloads (hostPath ignores fsGroup,
    # so the dirs must pre-exist _media-owned for the pods to write).
    "d ${mediaRoot}/apps/calibre-web-automated 0775 _media _media -"
    "d ${mediaRoot}/apps/calibre-web-automated/config 0775 _media _media -"
    "d ${mediaRoot}/apps/calibre-web-automated/ingest 0775 _media _media -"
    "d ${mediaRoot}/apps/audiobookshelf 0775 _media _media -"
    "d ${mediaRoot}/apps/audiobookshelf/config 0775 _media _media -"
    "d ${mediaRoot}/apps/audiobookshelf/metadata 0775 _media _media -"
    "d ${mediaRoot}/apps/libation 0775 _media _media -"
    "d ${mediaRoot}/apps/libation/config 0770 _media _media -"
    "d ${mediaRoot}/apps/libation/db 0770 _media _media -"
    "d ${mediaRoot}/apps/libation/in-progress 0770 _media _media -"
    # CWA runs as _media (994); this A+ grants the _media group rwx on the books
    # library so the CWA pod can write. Now the only _media grant for books — the
    # blanket recursive rule was removed.
    "A+ ${mediaRoot}/books - - - - group:_media:rwx"
  ];

  # POSIX ACLs are off by default on this pool, which silently no-ops every
  # tmpfiles `A`/`A+` rule. Enable it before systemd-tmpfiles runs so the media
  # + immich ACLs actually take effect. `xattr=sa` is already set.
  systemd.services.zfs-media-posixacl = {
    description = "Ensure acltype=posixacl on pool/media";
    wantedBy = ["local-fs.target"];
    # Order after the dataset is actually mounted (RequiresMountsFor resolves to
    # the generated data-Media.mount, avoiding the imprecise zfs-mount.service),
    # and before BOTH tmpfiles units: -setup (boot) AND -resetup (the unit
    # switch-to-configuration re-runs on `nixos-rebuild switch`). Missing the
    # resetup ordering lets the ACL rules apply before posixacl is on during a
    # switch, silently no-opping the reader ACL until the next reboot.
    before = ["systemd-tmpfiles-setup.service" "systemd-tmpfiles-resetup.service"];
    # A default-dependencies service implicitly gains After=sysinit.target and
    # After=basic.target. Combined with the Before=systemd-tmpfiles-setup above
    # (tmpfiles-setup is itself ordered Before=sysinit.target), that forms a boot
    # ordering cycle. systemd breaks the cycle by dropping the tmpfiles-setup job,
    # which then skips the `d /run/avahi-daemon - avahi avahi -` rule — so the
    # avahi socket auto-creates that dir as root and avahi-daemon dies with
    # "Failed to create runtime directory". Dropping the default deps lets this
    # oneshot sit cleanly between the media mount and tmpfiles in early boot.
    unitConfig.DefaultDependencies = false;
    unitConfig.RequiresMountsFor = mediaRoot;
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.zfs}/bin/zfs set acltype=posixacl pool/media";
    };
  };

  services.zfs.zed.settings = {
    ZED_DEBUG_LOG = "/tmp/zed.debug.log";
    ZED_EMAIL_ADDR = ["root"];
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
