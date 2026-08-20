{pkgs, ...}: {
  # home.sessionVariables = {
  #   EDITOR = "vim";
  # };
  home.packages = [
    pkgs.unstable.zed-editor
  ];
}
