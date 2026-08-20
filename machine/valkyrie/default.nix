# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix.settings = {
    # Prebuilt coding agents from numtide/llm-agents.nix (pkgs.llm-agents.*).
    # The Determinate NixOS module writes these settings to nix.custom.conf,
    # which its managed nix.conf includes.
    extra-substituters = ["https://cache.numtide.com"];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };

  networking.networkmanager.enable = true;
  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      userServices = true;
      workstation = true;
      hinfo = true;
    };
  };
  services.samba-wsdd.enable = true;
  users.users.${config.meta.user} = {
    isNormalUser = true;
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
  };
  services.openssh.settings.PermitRootLogin = "yes";
  services.openssh.enable = true;
}
