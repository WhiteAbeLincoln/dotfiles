{...}: {
  imports = [
    ./nixpkgs
    ./home-manager
    ./common-cli.nix
    ./ghostty
    ./mdns.nix
    ./rbw.nix
    ./ssh
    ./tmux
    ./zellij
  ];
}
