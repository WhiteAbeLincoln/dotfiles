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
  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;

  programs.ghostty.enable = true;
  programs.firefox.enable = true;

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;
  users.users.${config.dotfiles.host.user} = {
    isNormalUser = true;
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
  };
  services.openssh.enable = true;
}
