{ stdenv, fetchurl, undmg, unzip, lib }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip lib;
  name = "Spotify";
  version = "1.1.52.687.gf5565fe5";
  src = fetchurl {
    name = "Spotify-${version}.dmg";
    url = "https://download.spotify.com/Spotify.dmg";
    sha256 = "1vrshxpxvnrlx4fjrwb6q8ffmx54sclni4y16g7hivg39wdw2bn2";
  };
  installPhase = path: ''
    cp -pR Spotify.app/* ${path}
  '';
  description = "The Spotify music player";
  homepage = "https://www.spotify.com";
}
