{self, ...}: {
  nixos = {
    config,
    lib,
    ...
  }: {
    nixpkgs.hostPlatform = config.dotfiles.host.system;
    networking.hostName = config.dotfiles.host.hostName;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
  };

  darwin = {
    config,
    lib,
    ...
  }: {
    nixpkgs.hostPlatform = config.dotfiles.host.system;
    networking.hostName = config.dotfiles.host.hostName;
    networking.localHostName = config.dotfiles.host.hostName;
    system.primaryUser = config.dotfiles.host.user;
    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
    users.users.${config.dotfiles.host.user}.home = lib.mkDefault "/Users/${config.dotfiles.host.user}";
  };

  homeManager = {
    config,
    lib,
    pkgs,
    ...
  }: {
    home.username = config.dotfiles.host.user;
    home.homeDirectory = lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin
      then "/Users/${config.dotfiles.host.user}"
      else "/home/${config.dotfiles.host.user}"
    );
  };
}
