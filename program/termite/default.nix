{ pkgs, ... }:

{
  programs.termite = {
    enable = true;
  } // ((import ./settings.nix) pkgs);
}
