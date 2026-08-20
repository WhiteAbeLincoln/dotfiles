{inputs, ...}: {
  imports = [
    ../../aspect/darwin-system.nix
    ../../aspect/darwin-desktop
    ../../aspect/shell-utilities.nix
    ../../aspect/development.nix
    ../../aspect/userscripts
  ];

  darwin = {
    imports = [
      inputs.determinate.darwinModules.default
      ./darwin.nix
    ];
    system.stateVersion = 5;
  };

  homeManager = {
    imports = [./home.nix];
    home.stateVersion = "24.05";
  };
}
