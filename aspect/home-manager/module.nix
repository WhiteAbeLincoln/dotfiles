{lib, ...}: {
  manual.manpages.enable = lib.mkDefault false;
  programs.home-manager.enable = lib.mkDefault true;
}
