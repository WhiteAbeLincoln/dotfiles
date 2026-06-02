{...}: {
  imports = [
    ./defaults.nix
    ./docker-rootless/module.nix
    ./ai-agents/module.nix
  ];
}
