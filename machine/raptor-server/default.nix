# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      <nixos-hardware/dell/xps/15-9550/nvidia>
      <home-manager/nixos>
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../role/nixos.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "raptor-server"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  time.timeZone = "America/Denver";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups.media = { gid = 994; };
  users.users.abe = {
    isNormalUser = true;
    extraGroups = [ "wheel" "media" ]; # Enable ‘sudo’ for the user.
  };
  home-manager.useGlobalPkgs = true;
  home-manager.users.abe = import ./home.nix;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # add the external hard drives
  fileSystems = let
    mkMediaFs = uuid: {
      device = "/dev/disk/by-uuid/" + uuid;
      fsType = "ntfs";
      options = [
        "nofail"
        "allow_other"
        "x-systemd.device-timeout=1ms"
        "x-systemd.automount"
        "gid=${toString config.users.groups.media.gid}"
      ];
    };
  in {
    "/data/disk1" = mkMediaFs "AC7208AE72087EF8";
    "/data/disk2" = mkMediaFs "0A92598F92597FDD";
  };

  services.plex = {
    enable = true;
    openFirewall = true;
    extraScanners = [
      (pkgs.fetchFromGitHub {
        owner = "ZeroQI";
        repo = "Absolute-Series-Scanner";
        rev = "4ef18a738c6263a8b96ab6f83ae391d4550b9cc9";
        sha256 = "2bdp0e5XES/phLLUP2mngwITUWdZIE6Y6ness86xSNI=";
      })
    ];
    extraPlugins = [
      (builtins.path {
        name = "Hama.bundle";
        path = pkgs.fetchFromGitHub {
          owner = "ZeroQI";
          repo = "Hama.bundle";
          rev = "16c8a40a7b004ed14e46cd457d8c393672a09c5a";
          sha256 = "2aLMfjc/qMImWx3nSBpRIiJJM66PUbACwUmtkKW7QCE=";
        };
      })
      # an audiobook library organizer
      # (builtins.path {
      #   name = "Audnexus.bundle";
      #   path = pkgs.fetchFromGitHub {
      #     owner = "djdembeck";
      #     repo = "Audnexus.bundle";
      #     rev = "v0.2.8";
      #     sha256 = "sha256-IWOSz3vYL7zhdHan468xNc6C/eQ2C2BukQlaJNLXh7E=";
      #   };
      # })
    ];
  };

  services.calibre-web = {
    enable = true;
    listen.ip = "0.0.0.0";
    options = {
      enableBookUploading = true;
      calibreLibrary = /data/Media/books;
    };
  };

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
    extraServiceFiles.ssh = "''${pkgs.avahi}/etc/avahi/services/ssh.service";
  };

  services.samba-wsdd.enable = true;
  users.users.samba = {
    createHome = false;
    isSystemUser = true;
    group = "media";
  };
  services.samba = {
    enable = true;
    openFirewall =  true;
    nsswins = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = raptor-server
      netbios name = raptor-server
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
        "write list" = "abe";
        "read list" = "abe, guest, nobody";
      };
    };
  };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
