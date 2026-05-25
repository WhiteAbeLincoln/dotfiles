{
  stdenv,
  lib,
  fetchurl,
  autoPatchelfHook,
}: let
  version = "0.1.5";
  base = "https://github.com/chenhunghan/mdadf/releases/download/mdadf-v${version}";
  sources = {
    "aarch64-darwin" = fetchurl {
      url = "${base}/mdadf-darwin-arm64.tar.gz";
      sha256 = "ace85428fe822bbb51262bd9acac3ae3fe9a3e2ebbfc71b4ff981ce5fe995642";
    };
    "x86_64-darwin" = fetchurl {
      url = "${base}/mdadf-darwin-x64.tar.gz";
      sha256 = "d23b0f4b596e9f21f08d051600b1383ab01936192a796613d0ea023a0cde5461";
    };
    "aarch64-linux" = fetchurl {
      url = "${base}/mdadf-linux-arm64.tar.gz";
      sha256 = "e153ba59ef1fe23314812b5b82b862560b80ad3588470ab09a881853d995f2fd";
    };
    "x86_64-linux" = fetchurl {
      url = "${base}/mdadf-linux-x64.tar.gz";
      sha256 = "92c97826ad2479ad5240c2ad3b4681e6715fea05de05fe14d4e8a1b3c69c3d22";
    };
  };
in
  stdenv.mkDerivation {
    pname = "mdadf";
    inherit version;

    src = sources.${stdenv.hostPlatform.system} or (throw "mdadf: unsupported platform ${stdenv.hostPlatform.system}");

    # The release tarball is just the bare `mdadf` binary, no top-level dir.
    sourceRoot = ".";

    nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [autoPatchelfHook];

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall
      install -Dm755 mdadf $out/bin/mdadf
      runHook postInstall
    '';

    meta = {
      description = "Convert Markdown to Atlassian Document Format (ADF) JSON";
      homepage = "https://github.com/chenhunghan/mdadf";
      license = lib.licenses.mit;
      platforms = builtins.attrNames sources;
      mainProgram = "mdadf";
    };
  }
