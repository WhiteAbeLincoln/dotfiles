{pkgs, ...}: {
  programs.abes-xmonad =
    {
      enable = true;
    }
    // (import ./settings.nix pkgs);
}
