{...}: {
  nixpkgs.overlays = [
    (_final: prev: {
      aspectConstructorMarker = prev.writeText "aspect-constructor-marker" "overlay-applied";
    })
  ];

  homeManager = {
    config,
    lib,
    pkgs,
    ...
  }: {
    options.programs.aspect-constructor.enable =
      lib.mkEnableOption "the aspect constructor fixture";
    config = {
      programs.aspect-constructor.enable = lib.mkDefault true;
      home.stateVersion = "26.05";
      home.file.".aspect-constructor-test".text =
        if config.programs.aspect-constructor.enable
        then "enabled"
        else "overridden";
      home.file.".aspect-nixpkgs-test".text = builtins.readFile pkgs.aspectConstructorMarker;
    };
  };
}
