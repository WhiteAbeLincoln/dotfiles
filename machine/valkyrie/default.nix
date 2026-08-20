{inputs, ...}: {
  imports = [
    ../../aspect/ai-agents
    ../../aspect/plasma-desktop.nix
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
