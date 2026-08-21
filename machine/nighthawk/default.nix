{...}: {
  imports = [
    ../../aspect/darwin-desktop
    ../../aspect/ai-agents
    ../../aspect/userscripts
  ];

  darwin = {
    imports = [
      ./darwin.nix
    ];
    system.stateVersion = 5;
  };

  homeManager = {
    imports = [./home.nix];
    home.stateVersion = "24.05";
  };
}
