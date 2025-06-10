{
  stdenv,
  fetchFromGitHub,
  Carbon,
  Cocoa,
  ScriptingBridge,
  xxd,
  lib,
}:
stdenv.mkDerivation rec {
  pname = "yabai";
  version = "3.3.7";

  src = fetchFromGitHub {
    owner = "koekeishiya";
    repo = pname;
    rev = "v${version}";
    sha256 = "1yx4qp4rwk3ncw57yqy9m0nsg1rb62x4y2mj009lbzx0syfvh84s";
    name = "${pname}-${version}";
  };

  buildInputs = [Carbon Cocoa ScriptingBridge xxd];

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/man/man1/
    cp ./bin/yabai $out/bin/yabai
    cp ./doc/yabai.1 $out/share/man/man1/yabai.1
  '';

  meta = with lib; {
    description = ''
      A tiling window manager for macOS based on binary space partitioning
    '';
    homepage = "https://github.com/koekeishiya/yabai";
    platforms = platforms.darwin;
    maintainers = ["Abraham White <abelincoln.white@gmail.com>"];
    license = licenses.mit;
  };
}
