{lib, ...}: {
  programs.starship = {
    enable = lib.mkDefault true;
    settings = {
      format = "$all";
      command_timeout = 1250;
    };
  };
}
