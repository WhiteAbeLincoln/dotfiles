{pkgs, ...}: {
  programs.rofi =
    {
      enable = true;
    }
    // (import ./settings.nix);
}
