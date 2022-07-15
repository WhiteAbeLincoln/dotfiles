{ stdenv, fetchurl, undmg, unzip, lib }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip lib;
  name = "Spotify";
  version = "1.1.52.687.gf5565fe5";
  src = fetchurl {
    name = "Spotify-${version}.dmg";
    url = "https://download.spotify.com/Spotify.dmg";
    sha256 = "01v1nn3msaihmcjp851yqkzn7gxw3z13v37zgby1m0c597szx5l5";
  };
  installPhase = path: ''
    cp -pR Spotify.app/* ${path}
  '';
  description = "The Spotify music player";
  homepage = "https://www.spotify.com";
}
