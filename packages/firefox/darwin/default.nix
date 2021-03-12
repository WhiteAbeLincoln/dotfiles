
{ stdenv, fetchurl, undmg, unzip }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip;
  name = "Firefox";
  version = "86.0.1";
  src = fetchurl {
    name = "Firefox-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/Firefox%20${version}.dmg";
    sha256 = "1cd55z11wpkgi1lnidwg8kdxy8b6p00arz07sizrbyiiqxzrmvx3";

  };
  installPhase = path: ''
    cp -pR Firefox.app/* ${path}
  '';
  description = "The Firefox web browser";
  homepage = "https://www.mozilla.org/en-US/firefox";
}
