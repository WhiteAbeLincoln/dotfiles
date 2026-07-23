# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{
  config,
  pkgs,
  ...
}: let
  lib = pkgs.lib;
  user = config.meta.user;
  facts = import ./facts.nix;
  secrets = (import ../../secrets/common.nix) // (import ../../secrets/globalhawk.nix);
  mkMediaFs = uuid: fsType: {
    device = "/dev/disk/by-uuid/" + uuid;
    fsType = fsType;
    options = [
      "defaults"
      "nofail"
      "x-systemd.device-timeout=30"
      "x-systemd.automount"
    ];
  };
  linuxPkgs = pkgs.linuxKernel.packages.linux_6_12;
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # <home-manager/nixos>
    ../../program/plex
    ../../program/calibre-web
    ../../program/immich
    # TODO: homebridge has been added to nixos
    # remove custom module
    # ../../program/homebridge
    ./disks.nix
    ./backup.nix
    ./k3s.nix
    ./adguard.nix
    ../../modules/nixos/ai-agent-sandbox.nix
  ];

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    # Prebuilt coding agents from numtide/llm-agents.nix (pkgs.llm-agents.*).
    # This host is plain NixOS (no Determinate), so the cache goes here rather
    # than in the darwin `determinateNix.customSettings`.
    extra-substituters = ["https://cache.numtide.com"];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };

  # {{{ Boot & System
  # upgrade the linux kernel to support our Intel AX101 wifi
  boot.kernelPackages = linuxPkgs;
  # boot.extraModulePackages = with linuxPkgs; [ it87 ];
  # boot.kernelModules = ["coretemp", "it87"];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.tmp.useTmpfs = true;
  boot.loader.systemd-boot.configurationLimit = 6;

  # Do NOT reboot unattended: a reboot restarts k3s, which re-triggers the media
  # EndpointSlice reconcile gap (Traefik 503s until the Services are nudged) and
  # briefly disrupts DNS/certs. Updates should be observed, not silent. (Also
  # appears to have been inert — ~130d uptime despite allowReboot=true — likely
  # because no autoUpgrade `flake`/channel is configured on this flakes host.)
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = false;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  # needed for large linux kernel builds caused by nixos-hardware
  # services.logind.extraConfig = ''
  #   RuntimeDirectorySize=20G
  #   RuntimeDirectoryInodesMax=1048576
  # '';
  powerManagement.enable = false;
  # systemd.sleep.extraConfig = ''
  #   AllowSuspend=no
  #   AllowHibernation=no
  #   AllowSuspendThenHibernate=no
  #   AllowHybridSleep=no
  # '';
  # }}}

  # {{{ Users
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups._media = {gid = facts.mediaUid;};
  users.users._media = {
    isSystemUser = true;
    group = "_media";
    createHome = false;
    uid = facts.mediaUid;
  };
  programs.fish.enable = true;
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = ["networkmanager" "wheel" "_media"]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [firefox vscode.fhs];
    shell = pkgs.fish;
  };
  # home-manager.useGlobalPkgs = true;
  # home-manager.users.${user} = import ./home.nix;

  users.users.calibre-web.extraGroups = ["_media"];
  # Host-specific library path for the calibre-web program module.
  services.calibre-web.options.calibreLibrary = "${facts.mediaRoot}/books";

  # Unprivileged user for running AI agents read-only; see
  # docs/superpowers/specs/2026-07-22-agent-user-sandbox-design.md.
  services.aiAgentSandbox = {
    enable = true;
    docker.enable = true;
    k3s.enable = true;
  };
  # }}}

  # {{{ Networking, TZ, Locale
  networking.hostName = "globalhawk"; # Define your hostname.
  # Pick only one of the below networking options.
  networking.networkmanager.enable = false; # Easiest to use and most distros use this by default.
  networking.wireless = {
    enable = true; # Enables wireless support via wpa_supplicant.
    networks = {
      pokestop.psk = secrets.pokestop_psk;
    };
  };

  # Static LAN IP on the wired interface. The Fiber router only leases .100+, so
  # a static below the pool cannot collide — no DHCP reservation needed. AdGuard
  # answers the ingress wildcard with facts.lanIp, so it must be stable. Host
  # resolution uses public resolvers directly (independent of AdGuard, so the
  # host still resolves if AdGuard restarts). wlo1 stays on DHCP as a fallback.
  networking.interfaces.${facts.lanInterface} = {
    useDHCP = false;
    ipv4.addresses = [
      {
        address = facts.lanIp;
        prefixLength = 24;
      }
    ];
  };
  networking.defaultGateway = facts.lanGateway;
  networking.nameservers = ["1.1.1.1" "9.9.9.9"];

  # Firewall re-enabled after the torrent/arr migration (was wide open). Traefik
  # fronts every migrated app on 80/443, so the old per-service high ports are
  # gone. ssh/Samba/xrdp/avahi(mDNS) open their own ports via their modules;
  # Tailscale manages its interface. k3s's internal ports (6443 API, 10250
  # kubelet, flannel 8472) are not exposed on the LAN — only the CNI interfaces
  # are trusted so in-cluster pod/service/DNS traffic keeps flowing.
  networking.firewall = {
    enable = true;
    # k3s/flannel traffic can trip strict reverse-path filtering; loosen it so
    # pod/service packets aren't silently dropped.
    checkReversePath = "loose";
    trustedInterfaces = ["cni0" "flannel.1" "tailscale0"];
    allowedTCPPorts = [
      80 # Traefik ingress (HTTP -> HTTPS)
      443 # Traefik ingress (HTTPS)
      8083 # calibre-web (still native, deferred)
      config.services.immich-custom.port # immich (still on docker, deferred)
    ];
    allowedUDPPorts = [
      8472 # flannel VXLAN (k3s CNI)
    ];
  };

  # AdGuard Home DNS: reachable on the LAN interface and (implicitly, via the
  # trusted tailscale0 interface) the tailnet. The web UI (:3000) is deliberately
  # NOT opened here, so it stays off the LAN.
  networking.firewall.interfaces.${facts.lanInterface} = {
    allowedTCPPorts = [53];
    allowedUDPPorts = [53];
  };

  # Set your time zone.
  time.timeZone = facts.timezone;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };
  # }}}

  # {{{ Hardware
  # enable hardware video acceleration
  nixpkgs.config.packageOverrides = pkgs: {
    intel-vaapi-driver = pkgs.intel-vaapi-driver.override {enableHybridCodec = true;};
  };

  # Enable sound.
  # sound.enable = true;
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # add the external hard drives
  fileSystems = {
    "/data/OldMedia" = mkMediaFs "dc06f3b9-1f0e-4f63-9917-adcc11f2bb7f" "ext4";
    # "/data/disk2" = mkMediaFs "2104153c-21fa-3549-b27e-d9e9eb6944ff" "hfsplus";
  };

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      libva-vdpau-driver
      libvdpau-va-gl
    ];
  };
  # }}}

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    git-crypt
    tmux
    calibre
    vlc
    # xterm-ghostty terminfo, system-wide so SSH sessions resolve it for every
    # user (abe, the sandbox agent, root) without a per-user `tic` copy.
    ghostty.terminfo
  ];

  # {{{ Services
  # List services that you want to enable:

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  programs.nix-ld.enable = true;

  services.flatpak.enable = true;
  services.immich-custom = {
    enable = true;
    immichVersion = "v1.124.2";
    uploadDir = "${facts.mediaRoot}/immich/photos";
    backupDir = "${facts.mediaRoot}/immich/backups";
    dbPassword = secrets.immich_pass;
  };

  # services.homebridge = {
  #   enable = true;
  #   cfgDir = "/data/Media/docker-services/homebridge";
  #   # user = "${toString config.users.users._media.uid}:${toString config.users.groups._media.gid}";
  # };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the X11 windowing system.
  services.desktopManager.plasma6.enable = true;
  services.xserver = {
    enable = true;
    # Enable touchpad support (enabled default in most desktopManager).
    # libinput.enable = true;
    # Configure keymap in X11
    xkb.layout = "us";
    xkb.options = "caps:swapescape";
  };
  services.displayManager.sddm.enable = true;

  # Remote Desktop
  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startplasma-x11";
  services.xrdp.openFirewall = true;

  # stuck behind a double-NAT with no router control. useRoutingFeatures
  # "server" enables IP forwarding so globalhawk can advertise the LAN subnet as
  # a Tailscale route (operator runs `tailscale set --advertise-routes` once), so
  # the ingress names resolve+connect the same on the LAN and over the tailnet.
  #
  # accept-dns=false: MagicDNS otherwise hijacks /etc/resolv.conf (points it at
  # 100.100.100.100), overriding networking.nameservers and routing the host's —
  # and thus CoreDNS's and cert-manager's — lookups through the tailnet/AdGuard.
  # That broke cert-manager's DNS-01 self-check, which must resolve the PUBLIC
  # _acme-challenge TXT, not the split-horizon view (and would break once
  # AdGuard's *.h wildcard, which also matches _acme-challenge.h, goes live).
  # Declining tailnet DNS keeps the host on its declared public resolvers; a
  # server needn't resolve peers' *.ts.net names, and clients' split-DNS is
  # unaffected (that's a tailnet-wide admin-console setting, not consumed here).
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
    extraSetFlags = ["--accept-dns=false"];
  };

  # make sure mdns/.local addresses are working
  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns4 = true;
    # Only advertise/resolve mDNS on the LAN interface. Without this, avahi
    # publishes across the docker bridges and dozens of k3s veths, so even
    # globalhawk.local resolves to link-local/bridge junk instead of the LAN IP.
    allowInterfaces = [facts.lanInterface];
    publish = {
      enable = true;
      userServices = true;
      workstation = true;
      hinfo = true;
    };
    extraServiceFiles.ssh = "${pkgs.avahi}/etc/avahi/services/ssh.service";
  };

  services.samba-wsdd.enable = true;

  services.samba = {
    enable = true;
    openFirewall = true;
    nsswins = true;
    settings = {
      global = {
        workgroup = "WORKGROUP";
        "server string" = config.networking.hostName;
        "netbios name" = config.networking.hostName;
        security = "user";
        # "hosts allow" = ["192.168.1." "192.168.0." "10.0.0." "127.0.0.1" "localhost"];
        # "hosts deny" = ["0.0.0.0/0"];
        "guest account" = "nobody";
        "map to guest" = "bad user";
        "follow symlinks" = "yes";
        "wide links" = "yes";
        "unix extensions" = "no";
      };
      Media = {
        path = facts.mediaRoot;
        browseable = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "_media";
        "force group" = "_media";
        "write list" = user;
        "read list" = "${user}, guest, nobody";
      };
    };
  };

  # The `torrent` docker network + its init unit are gone with the torrent stack's
  # move to k3s. The docker `torrent` network may be orphaned on the host: remove
  # it once with `docker network rm torrent`.

  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [user];
  # podman is having issues resolving containers by name
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers = {
    # The torrent stack (vpn/gluetun + qbittorrent + prowlarr/radarr/sonarr)
    # migrated to k3s: the arr apps as Deployments and qbittorrent+gluetun as the
    # shared-netns torrent-vpn pod (k8s/apps/{arr,torrent}.nix). Only immich
    # remains on docker, pending its own migration.
    # minecraft-tina = {
    #   image = "itzg/minecraft-server";
    #   ports = [ "25565:25565" ];
    #   volumes = [
    #     "/data/Media/docker-services/minecraft-tina/data:/data"
    #   ];
    #   environment = {
    #     TZ = config.time.timeZone;
    #     UID = (toString config.users.users._media.uid);
    #     GID = (toString config.users.groups._media.gid);
    #     EULA = "TRUE";
    #     VERSION = "1.20.1";
    #     TYPE = "NEOFORGE";
    #     # WHITELIST_FILE: /extras/whitelist.json
    #     # MODS_FILE: /extras/mods.txt
    #     # OPS_FILE: /extras/ops.json
    #     # REMOVE_OLD_MODS: "true"
    #     ENABLE_RCON = "true";
    #     # password isn't a secret since we don't publicly
    #     # expose this server
    #     RCON_PASSWORD = "mc2022!awhite";

    #     MAX_TICK_TIME = "120000";
    #     MEMORY = "";
    #     JVM_XX_OPTS = "-XX:MaxRAMPercentage=75";

    #    EXAMPLE_COMMAND_BLOCK = "true";
    #    DIFFICULTY = "normal";
    #    VIEW_DISTANCE = "18";
    #    SIMULATION_DISTANCE = "7";
    #    LOG_TIMESTAMP = "true";
    #    SNOOPER_ENABLED = "false";
    #    SPAWN_PROTECTION = "0";
    #    ALLOW_FLIGHT = "TRUE";
    #  };
    #  };
  };

  # }}}

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
# vim:ft=nix foldmethod=marker foldlevel=0

