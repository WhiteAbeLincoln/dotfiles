{lib, ...}: {
  programs.starship = {
    enable = lib.mkDefault true;
    settings = {
      format = "$all";
      right_format = "$time";
      command_timeout = 1250;
      time.disabled = false;
    };
  };
}
