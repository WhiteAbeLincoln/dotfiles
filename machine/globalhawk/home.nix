{ config, pkgs, lib, ... }:

{
  imports = [
    ../../role/dev.nix
  ];

  # home.packages = [ unstable.prismlauncher ];

  programs.home-manager.enable = true;
  home.homeDirectory = "/home/abe";

  home.stateVersion = "23.11";
}
