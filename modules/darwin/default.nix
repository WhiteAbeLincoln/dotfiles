{pkgs, ...}: {
  imports = [
    ./system-defaults/defaults-writer.nix
  ];

  # I'm using determinate nix, so we can't have nix-darwin manage
  # /etc/nix/nix.conf
  # https://github.com/DeterminateSystems/determinate?tab=readme-ov-file#nix-darwin
  nix.enable = pkgs.lib.mkForce false;
}
