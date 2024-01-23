{ config, pkgs, ... }:

{
  imports = [
    # ../../packages/nur
    ../../role/dev.nix
    ../../role/devgui.nix
    # ../../program/emacs
  ];
  # # Let Home Manager install and manage itself.
  # programs.home-manager.enable = true;

  home.packages = [
    # pkgs.git
    pkgs.rnix-lsp
    pkgs.nil
    pkgs.xclip
    pkgs.hostname
  ];
  # programs.texlive.enable = true;
  # programs.texlive.extraPackages = tpkgs: { inherit (tpkgs) scheme-full; };
  # # starship causes problems with Windows Terminal
  # programs.starship.enable = pkgs.lib.mkForce false;

  # # Home Manager needs a bit of information about you and the
  # # paths it should manage.
  home.username = "awhite";
  home.homeDirectory = "/home/awhite";

  # programs.git = {
  #   userEmail = pkgs.lib.mkForce "awhite@campbellsci.com";
  #   ignoreFiles = [
  #     # we don't want to check in nix things for campbell projects
  #     ../../program/git/ignores/nixshell.ignore
  #     ../../program/git/ignores/visualstudio.ignore
  #   ];
  # };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.

  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";
}
