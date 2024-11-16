{ pkgs, ... }:

{
  programs.wezterm = {
    enable = true;
    extraConfig = ''
    local wezterm = require 'wezterm'

    function get_appearance()
      if wezterm.gui then
        return wezterm.gui.get_appearance()
      end
      return 'Dark'
    end

    function scheme_for_appearance(appearance)
      if appearance:find 'Dark' then
        return 'Brush Trees Dark (base16)'
        -- return 'Builtin Solarized Dark'
      else
        return 'Brush Trees (base16)'
        -- return 'Builtin Solarized Light'
      end
    end

    local config = wezterm.config_builder()

    config.color_scheme = scheme_for_appearance(get_appearance())

    -- config.hide_tab_bar_if_only_one_tab = true
    config.tab_bar_at_bottom = true
    config.use_fancy_tab_bar = false

    config.default_prog = { '${pkgs.fish}/bin/fish', '-l' }

    return config
    '';
  };
}
