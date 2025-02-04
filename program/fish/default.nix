# ENVIRONMENTS: nix-darwin, home-manager
{ pkgs, lib, isHM, config, myUserName, ... }:

let
  # fish doesn't add these paths correctly in darwin
  # they're added to the end of the path instead of the beginning
  # fish_add_path won't add them if they don't exist so this is safe
  # on other systems
  homeBin = if isHM
    then "${config.home.homeDirectory}/.nix-profile/bin"
    else "";
  perUserBin = "/etc/profiles/per-user/${myUserName}/bin";
  systemBin = "/run/current-system/sw/bin";
in
{
  programs.fish = {
    enable = true;
    shellAliases = if pkgs.stdenv.isLinux then {
      pbcopy = "${pkgs.xclip}/bin/xclip -i -selection clipboard";
      pbpaste = "${pkgs.xclip}/bin/xclip -o -selection clipboard";
    } else {};
    interactiveShellInit = ''
      set -g fish_key_bindings fish_vi_key_bindings # use vim-style keys
    '';
  } // (lib.optionalAttrs isHM {
    plugins = [
      {
        name = "fish-history-merge";
        src = pkgs.fetchFromGitHub {
          owner = "2m";
          repo = "fish-history-merge";
          rev = "7e415b8ab843a64313708273cf659efbf471ad39";
          sha256 = "sha256-oy32I92sYgEbeVX41Oic8653eJY5bCE/b7EjZuETjMI=";
        };
      }
    ];
  });
}
