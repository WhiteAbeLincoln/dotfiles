{...}: {
  homeManager = {
    config,
    lib,
    ...
  }: {
    options.programs.aspect-constructor.enable =
      lib.mkEnableOption "the aspect constructor fixture";
    config = {
      programs.aspect-constructor.enable = lib.mkDefault true;
      home.file.".aspect-constructor-test".text =
        if config.programs.aspect-constructor.enable
        then "enabled"
        else "overridden";
    };
  };
}
