{pkgs, ...}: {
  # TODO: move these deps to flakes/dev shells in project directories
  home.packages = [
    pkgs.unstable.rustup
    pkgs.unstable.nodejs_24
    pkgs.unstable.go
    pkgs.unstable.terraform
    pkgs.unstable.fastly
    pkgs.unstable.gh
    pkgs.unstable.awscli2
  ];
  programs.ghostty.enable = true;
  # use fastly email for all repos not covered
  # by aspect/git/home.nix includes
  programs.git.settings.user.email = "abe.white@fastly.com";
}
