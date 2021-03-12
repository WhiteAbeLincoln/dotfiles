{ pkgs, lib, ...  }:

with lib;

{
  imports = [
    ../program/git
    ../program/timew
    ../program/vscode
    ../program/vim
    ../program/firefox
    ../program/zsh
    ../program/emacs
    ../program/tmux
    ../program/direnv
  ];

  home.packages = with pkgs; [
    chromium
    haskellPackages.ShellCheck
    trash-cli
  ];
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.extraConfig = mkIf pkgs.stdenv.isDarwin ''
    IgnoreUnknown AddKeysToAgent,UseKeychain
    AddKeysToAgent yes
    UseKeychain yes
  '';
  programs.htop = {
    enable = true;
    vimMode = true;
  };
  programs.neovim.enable = true;
  programs.keychain.enable = pkgs.stdenv.isLinux;
  programs.opam.enable = true;
  programs.tmux-custom = lib.attrsets.recursiveUpdate ((import ../program/tmux/settings.nix) pkgs) {
    shell = "${pkgs.zsh}/bin/zsh";
  };
  programs.command-not-found.enable = true;
}
