{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellAliases = if pkgs.stdenv.isLinux then {
      pbcopy = "${pkgs.xclip}/bin/xclip -i -selection clipboard";
      pbpaste = "${pkgs.xclip}/bin/xclip -i -selection clipboard";
    } else {};
    shellInit = ''
      # Nix
      if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
        source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
      end
      # End Nix
    '';
  };
}
