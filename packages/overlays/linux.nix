[
  (self: super: {
    abes-xmonad = import ../xmonad {pkgs = super;};
    adguard-exporter-image = super.callPackage ../adguard-exporter.nix {};
    plex-exporter-image = super.callPackage ../plex-exporter.nix {};
  })
]
