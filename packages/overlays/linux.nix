[
  (self: super: {
    abes-xmonad = import ../xmonad {pkgs = super;};
    adguard-exporter-image = super.callPackage ../adguard-exporter {};
    plex-exporter-image = super.callPackage ../plex-exporter {};
  })
]
