{...}: {
  imports = [
    ./stack.nix
    ./kubernetes-logs.nix
    ./host.nix
    ./services.nix
    ./sso.nix
    ./alerts.nix
    ./dashboards.nix
  ];
}
