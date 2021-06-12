{ stdenv, fetchurl, undmg, unzip }:

(import ../../../lib/funcs.nix).mkApplication rec {
  # updating - go to https://chromiumdash.appspot.com/releases?platform=Mac
  # find latest version number for stable
  # paste in version lookup section on https://omahaproxy.appspot.com
  # Branch Base position is the new version number
  inherit stdenv undmg unzip;
  name = "Chromium";
  version = "857950";
  src = fetchurl {
    url = "https://commondatastorage.googleapis.com/chromium-browser-snapshots/Mac/${version}/chrome-mac.zip";
    sha256 = "12vg0g75v7xzl5kgn85nmq8xvk2p6i1sh5bwgjavrharx1dvq2rs";
  };
  installPhase = path: ''
    cp -pR chrome-mac/Chromium.app/* ${path}
  '';
  description = "Chromium is an open-source browser project that aims to build a safer, faster, and more stable way for all Internet users to experience the web.";
  homepage = "https://chromium.org/Home";
}
