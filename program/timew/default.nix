{ pkgs, ... }:

{
  home.packages = [
    pkgs.tasksh
    pkgs.timewarrior
  ];

  programs.taskwarrior.enable = true;

  home.file = {
    ".local/share/task/hooks/on-modify.timewarrior" = {
      source = "${pkgs.timewarrior}/share/doc/timew/ext/on-modify.timewarrior";
      executable = true;
    };
  };
}
