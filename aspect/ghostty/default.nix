{...}: {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.ghostty.package = lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin
      then pkgs.ghostty-bin
      else pkgs.ghostty
    );
  };
}
