{ pkgs, lib, ... }:

{
  programs.wezterm = {
    enable = true;
    extraConfig = ''
    local wezterm = require 'wezterm'
    local config = wezterm.config_builder()
    local my_shell <const> = '${pkgs.fish}/bin/fish'
    '' + (builtins.readFile ./config.lua);
  };
}
