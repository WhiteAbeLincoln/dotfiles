{ pkgs, lib, ...  }:

with lib;

{
  imports = [
    ../program/git
    ../program/timew
    ../program/vim
    ../program/zsh
    ../program/direnv
  ];

  home.packages = with pkgs; [
    haskellPackages.ShellCheck
  ];
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.extraConfig = mkIf pkgs.stdenv.isDarwin ''
    IgnoreUnknown AddKeysToAgent,UseKeychain
    AddKeysToAgent yes
    UseKeychain yes
  '';
  programs.neovim.enable = true;
  programs.keychain.enable = pkgs.stdenv.isLinux;
}
