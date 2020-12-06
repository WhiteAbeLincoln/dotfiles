{ pkgs, ... }:

{
  programs.zathura = {
    enable = true;
  } // (import ./settings.nix);
}
