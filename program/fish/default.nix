{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellAliases = if pkgs.stdenv.isLinux then {
      pbcopy = "xclip -i -selection clipboard";
      pbpaste = "xclip -i -selection clipboard";
    } else {};
  };
}
