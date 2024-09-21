# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, myUserName, ... }:

let
  lib = pkgs.lib;
  secrets = import ./secrets.nix;
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
  linuxPkgs = pkgs.linuxKernel.packages.linux_6_7;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # <home-manager/nixos>
      ../../program/plex
      ../../program/calibre-web
      ../../program/immich
      ./disks.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.allowUnfree = true;

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

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
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
  users.groups._media = { gid = 994; };
  users.users._media = {
    isSystemUser = true;
    group = "_media";
    createHome = false;
    uid = 994;
  };
  programs.fish.enable = true;
  users.users.${myUserName} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "_media" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [ firefox vscode.fhs ];
    shell = pkgs.fish;
  };
  # home-manager.useGlobalPkgs = true;
  # home-manager.users.${myUserName} = import ./home.nix;

  users.users.calibre-web.extraGroups = ["_media"];
  # }}}

  # {{{ Networking, TZ, Locale
  networking.hostName = "globalhawk"; # Define your hostname.
  # Pick only one of the below networking options.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = false;  # Easiest to use and most distros use this by default.
  networking.wireless.networks = {
    Sorensen = {
      psk = secrets.sorensen_psk;
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # Set your time zone.
  time.timeZone = "America/Denver";

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
  # Enable sound.
  # sound.enable = true;
  hardware.pulseaudio.enable = false;
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

  # enable hardware video acceleration
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
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
  ];

  # {{{ Services
  # List services that you want to enable:

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  programs.nix-ld.enable = true;

  services.immich = {
    enable = true;
    immichVersion = "v1.115.0";
    uploadDir = "/data/Media/immich/photos";
    backupDir = "/data/Media/immich/backups";
    dbPassword = secrets.immich_pass;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
    # Enable touchpad support (enabled default in most desktopManager).
    # libinput.enable = true;
    # Configure keymap in X11
    layout = "us";
    xkbOptions = "caps:swapescape";
  };

  # Remote Desktop
  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startplasma-x11";
  services.xrdp.openFirewall = true;

  # stuck behind a double-NAT with no router control. This helps
  services.tailscale.enable = true;

  # make sure mdns/.local addresses are working
  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
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
    openFirewall =  true;
    nsswins = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = ${config.networking.hostName}
      netbios name = ${config.networking.hostName}
      security = user
      hosts allow = 192.168.1. 192.168.0. 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
      follow symlinks = yes
      wide links = yes
      unix extensions = no
    '';
    shares = {
      Media = {
        path = "/data/Media";
        browseable = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "samba";
        "force group" = "media";
        "write list" = myUserName;
        "read list" = "${myUserName}, guest, nobody";
      };
    };
  };

  # ensure that the torrent network is created
  systemd.services.init-torrent-network = {
    description = "Create the network bridge for torrent.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      check=$(${pkgs.docker}/bin/docker network ls | grep "torrent" || true)
      if [ -z "$check" ]; then
        ${pkgs.docker}/bin/docker network create torrent
      else
        echo "torrent network already exists in docker"
      fi
    '';
  };

  # TODO: ensure our folders exist for these containers
  # using systemd.tmpfiles.rules

  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [ myUserName ];
  # podman is having issues resolving containers by name
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers = {
    # https://gist.github.com/1player/dbdafdd197e1623f5831108fc0cc973a
    vpn = {
      image = "qmcgaw/gluetun";
      extraOptions = [
        "--network=torrent"
        "--cap-add=NET_ADMIN"
        "--device=/dev/net/tun"
        "--label=autoheal=true"
        # "--sysctl=net.ipv6.conf.all.disable_ipv6=0"
      ];
      environment = {
        TZ = config.time.timeZone;
        VPN_TYPE = "wireguard";
        VPN_SERVICE_PROVIDER = "mullvad";
        WIREGUARD_PRIVATE_KEY = secrets.wireguard_private_key;
        WIREGUARD_ADDRESSES = "10.67.246.222/32";
        SERVER_CITIES = "stockholm,amsterdam";
      };
      ports = [
        "8000:8000" # vpn control server
        "51413:51413/udp" # transmission listening port
        "51413:51413/tcp" # transmission listening port
        "9091:9091" # transmission web-ui
        "9117:9117" # Jackett
      ];
    };
    jackett = {
      image = "linuxserver/jackett:latest";
      extraOptions = [
        "--network=container:vpn"
        # "--health-cmd" "curl https://am.i.mullvad.net/connected | grep -q 'You are connected'"
        # "--health-start-period="
        # "--health-startup-cmd" "curl localhost:8000/v1/publicip/ip"
        "--label=autoheal=true"
      ];
      environment = {
        TZ = config.time.timeZone;
        PUID = (toString config.users.users._media.uid);
        PGID = (toString config.users.groups._media.gid);
      };
      volumes = [
        "/data/Media/docker-services/torrent-config/jackett:/config"
        "/data/Media/torrents/queue:/downloads"
      ];
      dependsOn = ["vpn"];
    };
    transmission = {
      image = "linuxserver/transmission:latest";
      extraOptions = [
        # "--restart=unless-stopped"
        "--network=container:vpn"
        # "--health-cmd" "curl https://am.i.mullvad.net/connected | grep -q 'You are connected'"
        "--label=autoheal=true"
      ];
      environment = {
        TZ = config.time.timeZone;
        PUID = (toString config.users.users._media.uid);
        PGID = (toString config.users.groups._media.gid);
      };
      volumes = [
        "/data/Media/docker-services/torrent-config/transmission:/config"
        "/data/Media:/data"
      ];
      dependsOn = ["vpn"];
    };
    radarr = {
      image = "linuxserver/radarr:latest";
      extraOptions = [
        "--network=torrent"
        "--label=autoheal=true"
      ];
      environment = {
        TZ = config.time.timeZone;
        PUID = (toString config.users.users._media.uid);
        PGID = (toString config.users.groups._media.gid);
      };
      volumes = [
        "/data/Media/docker-services/torrent-config/radarr:/config"
        "/data/Media:/data"
      ];
      ports = [ "7878:7878" ];
    };
    sonarr = {
      image = "linuxserver/sonarr:latest";
      extraOptions = [
        "--network=torrent"
        "--label=autoheal=true"
      ];
      environment = {
        TZ = config.time.timeZone;
        PUID = (toString config.users.users._media.uid);
        PGID = (toString config.users.groups._media.gid);
      };
      volumes = [
        "/data/Media/docker-services/torrent-config/sonarr:/config"
        "/data/Media:/data"
      ];
      ports = [ "8989:8989" ];
    };
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

  networking.firewall.allowedTCPPorts = [
    7878
    8989
    9091
    51413
    9117
    config.services.photoprism.port
    8083
  ];
  networking.firewall.allowedUDPPorts = [ 51413 ];

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
