{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./system-defaults/defaults-writer.nix
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

  determinateNix.customSettings = {
    # extra-substituters = ["https://virby-nix-darwin.cachix.org"];
    # extra-trusted-public-keys = [
    #   "virby-nix-darwin.cachix.org-1:z9GiEZeBU5bEeoDQjyfHPMGPBaIQJOOvYOOjGMKIlLo="
    # ];
    # Prebuilt coding agents from numtide/llm-agents.nix (pkgs.llm-agents.*).
    extra-substituters = ["https://cache.numtide.com"];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
    extra-trusted-users = "@admin ${config.meta.user}";
  };
}
