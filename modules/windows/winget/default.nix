{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.winget;

  cfgObj = types.submodule {
    options = {
      text = mkOption {
        default = null;
        type = types.nullOr types.lines;
        description = ''
          Raw text containing yaml configuration for the <command>winget configure</command> command.
        '';
      };
      source = mkOption {
        default = null;
        type = types.nullOr types.path;
        description = ''
          A path to a file in the nix store containing yaml configuration for the <command>winget configure</command> command.
        '';
      };
    };
  };
  cfgSet = types.attrsOf (types.submodule {
    options = {
      configuration = mkOption {
        type = cfgObj;
        description = ''
          The configuration for <command>winget configure</command>.
          See: https://learn.microsoft.com/en-us/windows/package-manager/configuration
        '';
      };

      schema = mkOption {
        type = types.str;
        default = "https://aka.ms/configuration-dsc-schema/0.2";
        description = ''
          The schema url to use for the winget configuration file.
        '';
      };
    };
  });
in {
  options.winget = {
    enable = mkEnableOption ''
      Configures windows using <command>winget</command>.

      Note that enabling this option does not install winget. See the Microsoft
      website for installation instructions: https://docs.microsoft.com/en-us/windows/package-manager/.
    '';
  };

  config = let
    exec = ''
    '';
  in
    mkMerge [
      {home.activation.addWinEnv = hm.dag.entryAfter ["writeBoundary"] exec;}
      (mkIf cfg.enable (let
        # schema = lib.optionalString (cfg.config-schema != "") "# yaml-language-server: $schema=${cfg.config-schema}\n";
        # cfgText = lib.toYAML pkgs.configuration;
        # cfgFile = if cfgText == "" then null else pkgs.writeText "hm-config.dsc.yaml" (schema + cfgText);
      in {
        home.extraBuilderCommands = lib.optionalString (cfgText != "") ''ln -s ${cfgFile} $out/hm-config.dsc.yaml'';
      }))
    ];
}
