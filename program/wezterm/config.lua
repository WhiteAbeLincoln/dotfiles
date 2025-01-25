-- locals `my_shell`, `wezterm`, and `config` defined in the nix file
local act = wezterm.action

local is_darwin <const> = wezterm.target_triple:find("darwin") ~= nil
local mod_key <const> = is_darwin and 'SUPER' or 'CTRL'

function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Firewatch'
  else
    return 'dayfox'
  end
end

config.color_scheme = scheme_for_appearance(get_appearance())

-- config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = true

config.leader = { key = 'f', mods = 'CTRL' }
config.keys = {
  { key = '\\', mods = 'LEADER', action = act.SplitHorizontal{ domain = 'CurrentPaneDomain' }, },
  { key = '-', mods = 'LEADER', action = act.SplitVertical{ domain = 'CurrentPaneDomain' }, },
  { key = 'x', mods = 'LEADER', action = act.CloseCurrentPane{ confirm = false } },
  { key = 'z', mods = 'LEADER', action = act.TogglePaneZoomState, },
  { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection 'Left', },
  { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection 'Down', },
  { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection 'Up', },
  { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection 'Right', },
  -- send 'CTRL-F' when pressing CTRL-F, CTRL-F
  { key = 'f', mods = 'LEADER|CTRL', action = act.SendKey { key = 'f', mods = 'CTRL' } },
  { key = 'Escape', mods = 'LEADER', action = act.ActivateCopyMode },
}

config.ssh_domains = {
  {
    name = 'globalhawk',
    remote_address = 'globalhawk.local',
    username = 'abe'
  }
}

config.default_prog = { my_shell, '-l' }

-- nix automatically installs the wezterm terminfo
config.term = 'wezterm'

return config
