{
  stdenv,
  fetchurl,
  undmg,
  unzip,
  lib,
  writeScript,
}:
(import ../../../lib/funcs.nix).mkApplication rec {
  inherit stdenv undmg unzip lib;
  pname = "Plex Media Server";
  version = "1.32.6.7557-1cf77d501";
  src = fetchurl {
    name = "PlexMediaServer-${version}.zip";
    # https://downloads.plex.tv/plex-media-server-new/1.32.6.7557-1cf77d501/macos/PlexMediaServer-1.32.6.7557-1cf77d501-universal.zip
    url = "https://downloads.plex.tv/plex-media-server-new/${version}/macos/PlexMediaServer-${version}-universal.zip";
    sha256 = "b3e70cfb3c1d64d9bc9bea0b25c56200ee17476f6e6e736d473fd500edfe6205";
  };
  installPhase = path: ''
    cp -pR "Plex Media Server.app"/* "${path}"
  '';
  description = "Plex Media Server";
  homepage = "https://www.plex.tv/media-server-downloads/#plex-media-server";
}
