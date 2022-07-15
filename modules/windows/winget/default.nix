{ config, lib, pkgs, ... }:

with lib;

let
    cfg = config.winget;
    sourceDef = types.submodule {
        options = {
            Identifier = mkOption {
                type = types.str;
                description = "Identifier for the source";
            };
            Argument = mkOption {
                type = types.str;
                description = "Argument used to install the source";
            };
            Type = mkOption {
                type = types.str;
                description = "Type of the source";
            };
        };
    };
    toSource = s: { Name = s; };
    pkgDef = types.submodule {
        options = {
            PackageIdentifier = mkOption {
                type = types.str;
                example = "Microsoft.Powertoys";
                description = ''
                    The unique identifier for a package. Use <command>winget search</command> to find package ids.
                '';
            };
            Version = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Package version";
                example = "0.15.2";
            };
            Channel = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Package channel";
            };
            Scope = mkOption {
                type = types.enum ["user" "machine"];
                default = "user";
                description = "Package installation scope";
            };
            Source = mkOption {
                type = types.str;
                default = "winget";
                description = "The source name from which to install the package";
            };
        };
    };
    toPkg = s: { PackageIdentifier = s; };
    fixPkg = filterAttrs (n: v: v != null && n != "Source");

    pkgsBySource = mapAttrs (n: v: map fixPkg v) (groupBy (x: x.Source) cfg.packages);
    addSourceDetails = (n: v:
        if hasAttr n cfg.sources then
            { SourceDetails = { Name = n; } // (getAttr n cfg.sources); Packages = v; }
        else
            { SourceName = n; Packages = v; }
    );
    pkgsWithSourceDetails = attrValues (mapAttrs addSourceDetails pkgsBySource);
in
{
    options.winget = {
        enable = mkEnableOption ''
            Allows installing windows applications using <command>winget</command>.

            Note that enabling this option does not install winget. See the Microsoft
            website for installation instructions: https://docs.microsoft.com/en-us/windows/package-manager/.
        '';

        # cleanup = mkOption {
        #     type = types.enum ["none" "uninstall"];
        #     default = "none";
        #     example = "uninstall";
        #     description = ''
        #         This option configures what should happen to Windows applications which are not managed
        #         by this nix configuration.

        #         When set to <literal>"none"</literal> (the default), applications not present in this
        #         configuration are left installed.

        #         When set to <literal>"uninstall"</literal>, winget will uninstall any applications not
        #         listed here. This is not limited to applications installed by winget.
        #     '';
        # };

        packages = mkOption {
            type = with types; listOf (coercedTo str toPkg pkgDef);
            default = [];
            example = [ "Microsoft.VisualStudioCode" { PackageIdentifier = "Microsoft.Powertoys"; Version = "0.15.2"; } ];
            description = "Definitions of packages to install as an id or attrset. Use <command>winget search</command> to find package ids.";
        };

        sources = mkOption {
            type = types.attrsOf sourceDef;
            default = [];
            example = [
                {
                    Name = "winget";
                    Type = "Microsoft.PreIndexed.Package";
                    Argument = "https://winget.azureedge.net/cache";
                    Identifier = "Microsoft.Winget.Source_8wekyb3d8bbwe";
                }
            ];
            description = "Package installation sources";
        };
    };

    config = {
        home.file."test-winget.json" = mkIf cfg.enable { text = (builtins.toJSON { Sources = pkgsWithSourceDetails; }); };
    };
}
