{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.abes-xmonad;
  keybindingOptions = mkOption {
    default = {};
    type = types.attrsOf (types.submodule {
      options = {
        name = mkOption {
          type = types.str;
        };
        value = mkOption {
          type = types.str;
        };
      };
    });
  };
in
  {
    options = {
      programs.abes-xmonad = {
        enable = mkEnableOption "abes-xmonad";
        enableKde = mkOption {
          default = false;
          type = types.bool;
        };
        options = mkOption {
          default = null;
          type = types.nullOr (types.submodule {
            options = {
              keybindings = keybindingOptions;
              dmenu = mkOption {
                default = null;
                type = types.nullOr types.str;
              };
              dmenuArgs = mkOption {
                default = null;
                type = types.nullOr (types.listOf types.str);
              };
              normalBorderColor = mkOption {
                default = null;
                type = types.nullOr types.str;
              };
              focusedBorderColor = mkOption {
                default = null;
                type = types.nullOr types.str;
              };
              borderWidth = mkOption {
                default = null;
                type = types.nullOr types.ints.unsigned;
              };
              terminal = mkOption {
                default = null;
                type = types.nullOr (types.submodule {
                  options = {
                    keyMap = mkOption {
                      type = types.str;
                    };
                    name = mkOption {
                      type = types.str;
                    };
                    value = mkOption {
                      type = types.str;
                    };
                  };
                });
              };
            };
          });
        };
      };
    };

    config = mkIf cfg.enable (
      let
        omitNull = filterAttrsRecursive (n: v: v != null);
        mapKeybindings = mapAttrsToList (key: value: { inherit key; inherit (value) name value; });
        mapConfig = opts:
          let
            filtered = omitNull opts;
            termHandled = if filtered ? terminal
              then filtered // { terminal = { key = filtered.terminal.keyMap; value = filtered.terminal.value; name = filtered.terminal.value; }; }
              else filtered;
            final = if termHandled ? keybindings then termHandled // { keybindings = mapKeybindings (filtered.keybindings); } else termHandled;
          in
            final;
        abesXmonadConfig = if cfg.options != null then mapConfig cfg.options else cfg.options;
        package = pkgs.abes-xmonad.override {
          inherit abesXmonadConfig;
        };
      in
        {
          home.packages = [
            package
          ];

          xdg.configFile."plasma-workspace/env/set_window_manager.sh" = mkIf cfg.enableKde {
            text=''
            export KDEWM="${package}/bin/abes-xmonad"
            export _JAVA_AWT_WM_NONREPARENTING=1
            '';
            executable = true;
          };
        }
    );
  }
