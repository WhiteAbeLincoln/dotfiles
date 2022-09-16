{ config, lib, pkgs, ... }:

with lib;

let

cfg = config.programs.zsh;
relToDotDir = file: (optionalString (cfg.dotDir != null) (cfg.dotDir + "/")) + file;
figInitText = ''
# Fig pre block. Keep at the top of this file.
export PATH="${PATH}:${HOME}/.local/bin"
eval "$(fig init zsh pre)"
'';
figExitText = ''
# Fig post block. Keep at the bottom of this file.
eval "$(fig init zsh post)"
'';
withFig = text: if cfg.fig then (figInitText + text + figExitText) else text;

in

{
  options = {
    programs.zsh.fig = mkOption {
      default = false;
      type = types.bool;
      description = ''
        Enable Fig autocomplete for darwin systems.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      zplug
      xclip # required by pbcopy alias in zaliases
      hostname # some systems require this (rhel based)
    ];
    home.sessionVariables = {
      ZPLUG_INSTDIR="${pkgs.zplug}";
    };
    home.file = {
      "${relToDotDir ".zaliases"}".source = ./files/zaliases;
      "${relToDotDir ".zprofile"}".source = ./files/zprofile;
      "${relToDotDir ".zshenv"}".source = ./files/zshenv;
      "${relToDotDir ".zshrc"}".text = lib.mkForce (withFig (builtins.readFile ./files/zshrc));
      "${relToDotDir ".zshrc.local"}".source = ./files/zshrc.local;
      "${relToDotDir ".zshrc.pre"}".source = ./files/zshrc.pre;
      "${relToDotDir ".zshrc.post"}".text = "${cfg.initExtra}";
    };
  };

}
