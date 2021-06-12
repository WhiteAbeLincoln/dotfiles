
{ stdenv, fetchurl, undmg, unzip }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip;
  name = "Firefox";
  version = "89.0";
  src = fetchurl {
    name = "Firefox-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/Firefox%20${version}.dmg";
    sha256 = "0z86q1hlwmhfwrddhapwiy8qrn3v03d7nbsnzhnkr3fc9vz58ga3";
  };
  installPhase = path: ''
    cp -pR Firefox.app/* ${path}
  '';
  description = "The Firefox web browser";
  homepage = "https://www.mozilla.org/en-US/firefox";
}
