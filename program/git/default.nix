{ pkgs, ...  }:

{
  programs.git = {
    enable = true;
  } // (import ./settings.nix);
}
