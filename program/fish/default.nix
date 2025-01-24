# ENVIRONMENTS: nix-darwin, home-manager
{ pkgs, lib, isHM, ... }:

{
  programs.fish = {
    enable = true;
    shellAliases = if pkgs.stdenv.isLinux then {
      pbcopy = "${pkgs.xclip}/bin/xclip -i -selection clipboard";
      pbpaste = "${pkgs.xclip}/bin/xclip -i -selection clipboard";
    } else {};
    interactiveShellInit = ''
      set -g fish_key_bindings fish_vi_key_bindings # use vim-style keys
    '';
    shellInit = ''
      # Nix
      if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
        source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
      end
      # End Nix
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
