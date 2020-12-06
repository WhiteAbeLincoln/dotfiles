{ config, options, lib, pkgs, ... }:

with lib;

let
  cfg = config.home-theme;
  opts = options;
  forceSet = builtins.mapAttrs (name: value: mkForce value);
  mkThemeOverride = attr: mkIf (builtins.hasAttr attr cfg) (forceSet (builtins.getAttr attr cfg));
  # set here in case I want to map over these attributes and force/mkAfter later
  termiteAttrs = ["font" "geometry" "iconName" "cursorBlink" "cursorShape" "scrollbar" "backgroundColor" "cursorColor" "cursorForegroundColor" "foregroundColor" "foregroundBoldColor" "highlightColor" "hintsActiveBackgroundColor" "hintsForegroundColor" "hintsBorderColor" "hintsBorderWidth" "hintsFont" "hintsPadding" "hintsRoundness" "colorsExtra"];
  zathuraAttrs = ["options" "extraConfig"];
  vscodeAttrs = ["extensions"];
  vimAttrs = ["settings" "extraConfig"];

  mapColorScheme = value: if value == "light" then 0 else if value == "dark" then 1 else 2;
  mapDensity = value: if value == "normal" then 0 else if value == "compact" then 1 else 2;
  mapTheme = value: if value == "light" then "firefox-compact-light@mozilla.org" else if value == "dark" then "firefox-compact-dark@mozilla.org" else "default-theme@mozilla.org";
  programOverridesDefault = {
    rofi = "rofi";
    termite = "termite";
    zathura = "zathura";
    vscode = "vscode";
    vim = "vim";
    firefox = "firefox";
  };
in
{
  options.home-theme = {
    enable = mkEnableOption "home-theme";
    programOverrides = mkOption {
      default = programOverridesDefault;
      type = types.submodule {
        options = {
          rofi = mkOption {
            default = "rofi";
            type = types.str;
          };
          termite = mkOption {
            default = "termite";
            type = types.str;
          };
          zathura = mkOption {
            default = "zathura";
            type = types.str;
          };
          vscode = mkOption {
            default = "vscode";
            type = types.str;
          };
          vim = mkOption {
            default = "vim";
            type = types.str;
          };
          firefox = mkOption {
            default = "firefox";
            type = types.str;
          };
        };
      };
    };
    rofi = builtins.removeAttrs opts.programs.rofi [ "enable" "package" "terminal" "configPath" ];
    termite = getAttrs termiteAttrs opts.programs.termite;
    zathura = getAttrs zathuraAttrs opts.programs.zathura;
    vscode = mkOption {
      default = null;
      type = types.nullOr (types.submodule {
        options = {
          themeExtension = mkOption {
            default = null;
            type = types.nullOr (types.submodule {
              options = {
                name = mkOption { type = types.str; };
                publisher = mkOption { type = types.str; };
                version = mkOption { type = types.str; };
                sha256 = mkOption { type = types.str; };
              };
            });
          };
          theme = mkOption {
            default = "";
            type = types.str;
          };
        };
      });
    };
    vim = {
      themePlugin = mkOption {
        description = "function taking pkgs and returning a package";
        default = null;
      };
    } // (getAttrs vimAttrs opts.programs.vim);
    firefox = mkOption {
      default = {};
      type = types.attrsOf (types.submodule {
        options = {
          density = mkOption {
            default = "normal";
            type = types.enum ["compact" "normal" "touch"];
          };
          color-scheme = mkOption {
            default = "no-preference";
            type = types.enum ["no-preference" "light" "dark"];
          };
          userChrome = mkOption {
            default = "";
            type = types.lines;
            description = "Custom Firefox CSS";
          };
        };
      });
    };
  };

  config = let
    overrides = programOverridesDefault // cfg.programOverrides;
  in
    mkIf cfg.enable (mkMerge [
      {
        home.packages = [
          # we depend on rofi in the switcher
          # and jq for firefox theme switching
          pkgs.rofi
          # pkgs.jq
          # pkgs.moreutils
        ];
        home.file.".local/bin/theme-switcher" = {
          source = ./theme-switcher.sh;
          executable = true;
        };

        programs.${overrides.rofi} = mkThemeOverride "rofi";
        programs.${overrides.zathura} = mkIf (cfg ? zathura) {
          options = forceSet cfg.zathura.options;
          extraConfig = mkAfter cfg.zathura.extraConfig;
        };
        programs.${overrides.termite} = mkThemeOverride "termite";
        programs.${overrides.vscode} = mkIf (cfg.vscode != null) {
          extensions = mkIf (cfg.vscode.themeExtension != null) [ (pkgs.vscode-utils.extensionFromVscodeMarketplace cfg.vscode.themeExtension) ];
          userSettings = mkIf (cfg.vscode.theme != "") {
            "workbench.colorTheme" = cfg.vscode.theme;
          };
        };
        programs.${overrides.vim} = mkIf (cfg ? vim) {
          plugins = mkIf (cfg.vim ? themePlugin && cfg.vim.themePlugin != null) [ (cfg.vim.themePlugin pkgs) ];
          settings = forceSet cfg.vim.settings;
          extraConfig = mkAfter cfg.vim.extraConfig;
        };
        programs.${overrides.firefox}.profiles = builtins.mapAttrs (name: value: {
          settings = forceSet {
            "devtools.theme" = if value.color-scheme == "no-preference" then "light" else value.color-scheme;
            "extensions.activeThemeID" = mapTheme value.color-scheme;
            "ui.systemUsesDarkTheme" = mapColorScheme value.color-scheme;
            "browser.in-content.dark-mode" = value.color-scheme == "dark";
            "browser.uidensity" = mapDensity value.density;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = value.userChrome != "";
          };
          userChrome = mkIf (value.userChrome != "") (mkForce value.userChrome);
        }) cfg.firefox;
      }
      # (mkIf (cfg.firefox != {}) (
      #   let
      #     inherit (pkgs.stdenv.hostPlatform) isDarwin;

      #     mozillaConfigPath =
      #       if isDarwin
      #       then "Library/Application Support/Mozilla"
      #       else ".mozilla";

      #     firefoxConfigPath =
      #       if isDarwin
      #       then "Library/Application Support/Firefox"
      #       else "${mozillaConfigPath}/firefox";

      #     profilesPath =
      #       if isDarwin
      #       then "${firefoxConfigPath}/Profiles"
      #       else firefoxConfigPath;

      #     profiles = lib.mapAttrsToList (name: value: {
      #       file = "$HOME/${profilesPath}/${config.programs.${overrides.firefox}.profiles.${name}.path}/extensions.json";
      #       activeThemeId = mapTheme value.color-scheme;
      #     }) cfg.firefox;
      #     setActive = builtins.toJSON { active = true; userDisabled = false; };
      #     setInactive = builtins.toJSON { active = false; userDisabled = true; };
      #     jqScript = activeThemeId: ''(.addons[] | select(.type == "theme")) |= . + (if .id == "${activeThemeId}" then ${setActive} else ${setInactive} end)'';
      #     action = {file, activeThemeId}: ''
      #       if [ -f "${file}" ]; then
      #         jq '${jqScript activeThemeId}' "${file}" | sponge "${file}"
      #       fi'';
      #   in
      #     {
      #       home.activation.changeFFTheme = hm.dag.entryAfter ["writeBoundary"] ''
      #         ${builtins.concatStringsSep "\n" (map action profiles)}
      #       '';
      #     }
      # ))
    ]);
}
