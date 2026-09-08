{pkgs, ...}: {
  # TODO: move these deps to flakes/dev shells in project directories
  home.packages = [
    pkgs.unstable.rustup
    pkgs.unstable.nodejs_24
    pkgs.unstable.go
    pkgs.unstable.terraform
    pkgs.unstable.fastly
    pkgs.unstable.gh
  ];
  programs.ghostty.enable = true;
}
