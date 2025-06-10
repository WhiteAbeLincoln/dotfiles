self: super: {
  plex = super.callPackage ./default.nix {};
  plexRaw = super.callPackage ./raw.nix {};
}
