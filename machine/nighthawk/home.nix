{ pkgs, ... }:

{
  imports = [
    ../../program/git
    ../../program/vim
    ../../program/fish
    ../../program/starship
    ../../program/wezterm
  ];

  home.packages = [
    pkgs.haskellPackages.ShellCheck
  ];

  programs.jq.enable = true;
  programs.ssh.enable = true;
  programs.ssh.extraConfig = ''
    IgnoreUnknown AddKeysToAgent,UseKeychain
    AddKeysToAgent yes
    UseKeychain yes
  '';
  programs.keychain.enable = pkgs.stdenv.isLinux;

  programs.nix-index.enable = true;

  home.stateVersion = "24.05";
}
