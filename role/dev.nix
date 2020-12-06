{ pkgs, lib, ...  }:

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
    ../program/termite
  ];

  home.packages = with pkgs; [
    google-chrome
    haskellPackages.ShellCheck
    nur.repos.bb010g.pkgs.xcolor
    moreutils
  ];
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.htop = {
    enable = true;
    vimMode = true;
  };
  programs.neovim.enable = true;
  programs.keychain.enable = true;
  programs.opam.enable = true;
  programs.tmux-custom = lib.attrsets.recursiveUpdate ((import ../program/tmux/settings.nix) pkgs) {
    shell = "${pkgs.zsh}/bin/zsh";
  };
}
