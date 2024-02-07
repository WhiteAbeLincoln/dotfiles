{ config, pkgs, ... }:

{
  imports = [
    ../../program/git
    ../../program/vim
    ../../program/fish
    ../../program/direnv
    ../../program/starship
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.

  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "awhite";
  home.homeDirectory = "/home/${config.home.username}";

  home.packages = [
    pkgs.nil
    pkgs.xclip
    pkgs.hostname
    pkgs.git
    pkgs.openssh
  ];

  programs.ssh.enable = true;
  programs.keychain.enable = pkgs.stdenv.isLinux;
  programs.keychain.keys = [ "id_ed25519" ];

  programs.git = {
    userEmail = pkgs.lib.mkForce "awhite@campbellsci.com";
    ignoreFiles = [
      # we don't want to check in nix things for campbell projects
      ../../program/git/ignores/nixshell.ignore
      ../../program/git/ignores/visualstudio.ignore
    ];
  };
}
