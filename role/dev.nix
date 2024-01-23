{ pkgs, lib, ...  }:

with lib;

{
  imports = [
    ../program/git
    ../program/vim
    ../program/fish
    ../program/direnv
    ../program/tmux
    ../program/starship
  ];

  home.packages = [
    pkgs.haskellPackages.ShellCheck
  ];
  nix = {
    settings.experimental-features = ["nix-command" "flakes"];
  };
  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.extraConfig = mkIf pkgs.stdenv.isDarwin ''
    IgnoreUnknown AddKeysToAgent,UseKeychain
    AddKeysToAgent yes
    UseKeychain yes
  '';
  programs.keychain.enable = pkgs.stdenv.isLinux;
}
