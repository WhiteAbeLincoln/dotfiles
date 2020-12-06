{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
  } // (import ./settings.nix);
  home.sessionVariables = {
    DIRENV_INSTDIR = "${pkgs.direnv}";
  };
}
