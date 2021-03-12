{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.yabai-custom;

  toYabaiConfig = opts:
    concatStringsSep "\n" (mapAttrsToList
      (p: v: "yabai -m config ${p} ${toString v}") opts);

  configFile = mkIf (cfg.config != {} || cfg.extraConfig != "" || cfg.bigSurScriptingAddition)
    "${pkgs.writeScript "yabairc" (
      (optionalString cfg.bigSurScriptingAddition ''
        sudo yabai --load-sa
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
      '')
      + (if (cfg.config != {})
       then "${toYabaiConfig cfg.config}"
       else "")
      + optionalString (cfg.extraConfig != "") ("\n" + cfg.extraConfig + "\n"))}";
in

{
  options = with types; {
    services.yabai-custom.enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to enable the yabai window manager.";
    };

    services.yabai-custom.package = mkOption {
      type = path;
      description = "The yabai package to use.";
      default = pkgs.yabai;
    };

    services.yabai-custom.bigSurScriptingAddition = mkOption {
      type = bool;
      default = false;
      description = ''
        Whether to enable yabai's scripting-addition on Big Sur.
      '';
    };

    services.yabai-custom.enableScriptingAddition = mkOption {
      type = bool;
      default = false;
      description = ''
        Whether to install yabai's scripting-addition.
      '';
    };

    services.yabai-custom.config = mkOption {
      type = attrs;
      default = {};
      example = literalExample ''
        {
          focus_follows_mouse = "autoraise";
          mouse_follows_focus = "off";
          window_placement    = "second_child";
          window_opacity      = "off";
          top_padding         = 36;
          bottom_padding      = 10;
          left_padding        = 10;
          right_padding       = 10;
          window_gap          = 10;
        }
      '';
      description = ''
        Key/Value pairs to pass to yabai's 'config' domain, via the configuration file.
      '';
    };

    services.yabai-custom.extraConfig = mkOption {
      type = str;
      default = "";
      example = literalExample ''
        yabai -m rule --add app='System Preferences' manage=off
      '';
      description = "Extra arbitrary configuration to append to the configuration file";
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      environment.systemPackages = [ cfg.package ];

      launchd.user.agents.yabai = {
        serviceConfig.ProgramArguments = [ "${cfg.package}/bin/yabai" ]
                                         ++ optionals (cfg.config != {} || cfg.extraConfig != "") [ "-c" configFile ];
        serviceConfig.StandardOutPath = "/tmp/yabai.out.log";
        serviceConfig.StandardErrorPath = "/tmp/yabai.err.log";
        serviceConfig.KeepAlive = true;
        serviceConfig.RunAtLoad = true;
        serviceConfig.EnvironmentVariables = {
          PATH = "${cfg.package}/bin:${config.environment.systemPath}";
        };
        serviceConfig.ProcessType = "Interactive";
        serviceConfig.Nice = -20;
      };
    })

    # TODO: Handle removal of yabai scripting additions
    (mkIf (cfg.bigSurScriptingAddition) {
      environment.etc = {
        "sudoers.d/10-yabai-sa".text = ''
        %staff ALL = (root) NOPASSWD: ${cfg.package}/bin/yabai --load-sa
        '';
      };
    })

    (mkIf (cfg.enableScriptingAddition || cfg.bigSurScriptingAddition) {
      launchd.daemons.yabai-sa = {
        script = ''
          if ! ${cfg.package}/bin/yabai --check-sa; then
            ${cfg.package}/bin/yabai --install-sa
          fi
        '';

        serviceConfig.RunAtLoad = true;
        serviceConfig.KeepAlive.SuccessfulExit = false;
      };
    })
  ];
}
