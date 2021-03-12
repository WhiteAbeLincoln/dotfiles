{ stdenv, fetchurl, undmg, unzip }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip;
  name = "Spotify";
  version = "1.1.52.687.gf5565fe5";
  src = fetchurl {
    name = "Spotify-${version}.dmg";
    url = "https://download.spotify.com/Spotify.dmg";
    sha256 = "1rhr5ng5ydy2rdd9sq4sjccfgy4v3wdpsczlkg5ir2jz0q3nrkpy";
  };
  installPhase = path: ''
    cp -pR Spotify.app/* ${path}
  '';
  description = "The Spotify music player";
  homepage = "https://www.spotify.com";
}
