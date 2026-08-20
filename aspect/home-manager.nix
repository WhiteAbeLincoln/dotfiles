{lib, ...}: {
  homeManager = {
    programs.home-manager.enable = lib.mkDefault true;
  };
}
