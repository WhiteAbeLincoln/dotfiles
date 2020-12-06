{ pkgs, ... }:

{
  imports = [
    ./module.nix
  ];
  programs.tmux-custom = {
    enable = true;
  } // ((import ./settings.nix) pkgs);
}
