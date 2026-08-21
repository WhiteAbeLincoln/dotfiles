{inputs, ...}: {
  darwin = {
    config,
    pkgs,
    ...
  }: {
    imports = [
      inputs.determinate.darwinModules.default
    ];
    # I'm using determinate nix, so we can't have nix-darwin manage
    # /etc/nix/nix.conf
    # https://github.com/DeterminateSystems/determinate?tab=readme-ov-file#nix-darwin
    nix.enable = pkgs.lib.mkForce false;

    determinateNix.customSettings = {
      extra-trusted-users = "@admin ${config.dotfiles.host.user}";
    };
  };
}
