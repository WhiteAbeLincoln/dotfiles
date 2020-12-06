{ pkgs, ... }:

{
  programs.vim = {
    enable = true;
  } // ((import ./settings.nix) pkgs) ;
}
