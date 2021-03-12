{ stdenv, fetchFromGithub, xcodebuild }:

stdenv.mkDerivation rec {
  pname = "choose-gui";
  version = "1.2.1";
  buildInputs = [ xcodebuild ];
}
