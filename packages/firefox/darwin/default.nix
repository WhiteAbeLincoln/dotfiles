
{ stdenv, fetchurl, undmg, unzip, lib }:

(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip lib;
  name = "Firefox";
  version = "92.0";
  src = fetchurl {
    name = "Firefox-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/Firefox%20${version}.dmg";
    sha256 = "0kln28330jhmpdvsdsnrqnl0fkpb18i9vi1n98v99aq61ncqr5v8";
  };
  installPhase = path: ''
    cp -pR Firefox.app/* ${path}
  '';
  description = "The Firefox web browser";
  homepage = "https://www.mozilla.org/en-US/firefox";
}
