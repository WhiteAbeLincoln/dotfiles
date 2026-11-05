{lib, ...}: {
  nixos = {pkgs, ...}: {
    environment.systemPackages = [pkgs.wl-clipboard];

    services.desktopManager.plasma6.enable = lib.mkDefault true;
    services.displayManager = {
      sddm.enable = lib.mkDefault true;
      sddm.wayland.enable = lib.mkDefault true;
      defaultSession = lib.mkDefault "plasma"; # Plasma Wayland
    };
  };
}
