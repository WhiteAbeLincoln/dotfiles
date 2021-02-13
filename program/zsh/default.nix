{ config, pkgs, ... }:

let

cfg = config.programs.zsh;

in

{
  home.packages = with pkgs; [
    zplug
    zsh
    xclip # required by pbcopy alias in zaliases
  ];
  home.sessionVariables = {
    ZPLUG_INSTDIR="${pkgs.zplug}";
  };
  home.file = {
    ".zaliases".source = ./files/zaliases;
    ".zprofile".source = ./files/zprofile;
    ".zshenv".source = ./files/zshenv;
    ".zshrc".source = ./files/zshrc;
    ".zshrc.local".source = ./files/zshrc.local;
    ".zshrc.pre".source = ./files/zshrc.pre;
    ".zshrc.post".text = "${cfg.initExtra}";
  };
}
