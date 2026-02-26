{
  pkgs,
  myUserName,
  ...
}: {
  imports = [
    ./system-defaults/defaults-writer.nix
    ./linux-builder
  ];

  # I'm using determinate nix, so we can't have nix-darwin manage
  # /etc/nix/nix.conf
  # https://github.com/DeterminateSystems/determinate?tab=readme-ov-file#nix-darwin
  nix.enable = pkgs.lib.mkForce false;
  # virby didn't work - I kept on getting cache misses whenever setting additional configuration
  # which triggered https://github.com/quinneden/virby-nix-darwin/issues/8 and https://github.com/quinneden/virby-nix-darwin/issues/2
  # even with the default image, I was unable to ssh into the machine or run a build
  # services.virby = {
  #   enable = false;
  #   onDemand = {
  #     enable = true;
  #     ttl = 180; # timeout in minutes
  #   };
  #   debug = true;
  #   allowUserSsh = true;
  #   # rosetta = true;
  # };

  # instead, I've copied the linux-builder from nix-darwin and modified it to work with determinate-nix
  determinate-nix.linux-builder = {
    enable = false;
    ephemeral = true;
    config = {
      virtualisation = {
        darwin-builder = {
          diskSize = 40 * 1024;
          memorySize = 8 * 1024;
        };
        cores = 6;
      };
    };
  };
  determinate-nix.customSettings = {
    # extra-substituters = ["https://virby-nix-darwin.cachix.org"];
    # extra-trusted-public-keys = [
    #   "virby-nix-darwin.cachix.org-1:z9GiEZeBU5bEeoDQjyfHPMGPBaIQJOOvYOOjGMKIlLo="
    # ];
    extra-trusted-users = "@admin ${myUserName}";
  };
}
