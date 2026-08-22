{
  inputs,
  pkgs,
  ...
}: {
  home.packages = [
    inputs.sidra.packages.${pkgs.stdenv.hostPlatform.system}.default
    pkgs.unstable.zed-editor
  ];

  programs.ghostty = {
    enable = true;
    settings.window-decoration = "client";
  };
}
