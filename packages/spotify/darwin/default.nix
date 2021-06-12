{ stdenv, fetchurl, undmg, unzip }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip;
  name = "Spotify";
  version = "1.1.52.687.gf5565fe5";
  src = fetchurl {
    name = "Spotify-${version}.dmg";
    url = "https://download.spotify.com/Spotify.dmg";
    sha256 = "10xa2rnhswma8b50v4ynsz0x3x2hv0w203p5l7l0ynw31wrwc8b5";
  };
  installPhase = path: ''
    cp -pR Spotify.app/* ${path}
  '';
  description = "The Spotify music player";
  homepage = "https://www.spotify.com";
}
