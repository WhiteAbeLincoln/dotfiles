{...}: {
  imports = [
    ../../aspect/darwin-system.nix
    ../../aspect/ai-agents
  ];

  darwin = {
    imports = [
      ./darwin.nix
    ];
    system.stateVersion = 6;
  };

  homeManager = {
    imports = [./home.nix];
    home.stateVersion = "26.05";
  };
}
