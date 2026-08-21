{inputs, ...}: {
  imports = [
    ../../aspect/plasma-desktop.nix
    ../../aspect/ai-agents
  ];

  nixos = {
    imports = [
      inputs.determinate.nixosModules.default
      ./nixos.nix
    ];
    system.stateVersion = "26.05";
  };

  homeManager = {
    imports = [./home.nix];
    home.stateVersion = "26.05";
  };
}
