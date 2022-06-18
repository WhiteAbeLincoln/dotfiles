{ config, pkgs, lib, ... }:

{
  imports = [
    ../../role/dev.nix
  ];

  programs.home-manager.enable = true;
  home.homeDirectory = "/home/abe";

  home.stateVersion = "22.05";
}
