# Writes a shared set of userscripts (inline text or files) into one or more
# directory "targets". On darwin a target for the Userscripts Safari extension
# (https://github.com/quoid/userscripts) is registered automatically.
#
# Scripts are copied as real files rather than symlinked: the Userscripts Safari
# extension is sandboxed to its selected directory via a security-scoped bookmark
# and cannot follow a symlink into the nix store.
#
# Out of scope: Tampermonkey/Violentmonkey store scripts in extension storage, not
# a directory, so they cannot be driven this way. The `targets` option is shaped to
# admit a future `kind = "http"` (a localhost static-file service) if ever needed.
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.userscripts;

  # A single script entry: exactly one of `text` / `source` (enforced by assertion).
  scriptType = types.submodule {
    options = {
      text = mkOption {
        type = types.nullOr types.lines;
        default = null;
        description = "Inline script body. Mutually exclusive with `source`.";
      };
      source = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "File whose contents are copied. Mutually exclusive with `text`.";
      };
    };
  };

  targetType = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to write the shared scripts to this target.";
      };
      directory = mkOption {
        type = types.str;
        example = "\${config.home.homeDirectory}/userscripts";
        description = "Directory that managed scripts are written into.";
      };
    };
  };

  # Default scripts directory for the Userscripts Safari extension (darwin only).
  # NOT the app's sandbox container (~/Library/Containers/…): macOS protects that
  # tree, so darwin-rebuild activation (and even sudo) cannot write into it. This is
  # an unprotected location instead, which you point the Userscripts app at once via
  # its "change location" button. The username is parameterized via home.homeDirectory.
  safariDir = "${config.home.homeDirectory}/Library/Application Support/Userscripts/scripts";

  enabledTargets = filterAttrs (_: t: t.enable) cfg.targets;

  # Attribute names are the destination filenames verbatim.
  scriptNames = attrNames cfg.scripts;

  # All scripts realised once in the store; each link-farm entry's name is the
  # destination filename, its target the file or generated text.
  scriptsStore = pkgs.linkFarm "userscripts" (
    mapAttrsToList (fname: spec: {
      name = fname;
      path =
        if spec.source != null
        then spec.source
        else pkgs.writeText "userscript" spec.text;
    })
    cfg.scripts
  );

  # The manifest written after each run: the filenames we installed. Identical for
  # every target, so realise it once.
  manifestStore = pkgs.writeText "userscripts-manifest" (
    concatMapStrings (n: n + "\n") scriptNames
  );

  # Manifests live under xdg.stateHome so target directories stay free of bookkeeping.
  manifestFor = tname: "${config.xdg.stateHome}/nix-userscripts/${tname}";

  # bash associative-array literal of the currently-managed filenames.
  managedDecl = concatMapStringsSep " " (n: "[${escapeShellArg n}]=1") scriptNames;

  # Activation commands for one target: prune scripts we wrote last run but no longer
  # manage, install the current set as real files, then rewrite the manifest. Files
  # added manually (never recorded in the manifest) are left untouched.
  targetSection = tname: tcfg: let
    dir = tcfg.directory;
    manifest = manifestFor tname;
  in ''
    # target: ${tname}
    $DRY_RUN_CMD mkdir -p ${escapeShellArg dir}
    $DRY_RUN_CMD mkdir -p ${escapeShellArg (dirOf manifest)}
    if [ -f ${escapeShellArg manifest} ]; then
      while IFS= read -r _us_f; do
        [ -z "$_us_f" ] && continue
        if [ -z "''${_us_managed[$_us_f]:-}" ]; then
          $DRY_RUN_CMD rm -f ${escapeShellArg dir}/"$_us_f"
        fi
      done < ${escapeShellArg manifest}
    fi
    ${concatMapStringsSep "\n" (n: ''
        $DRY_RUN_CMD cp -fL ${escapeShellArg "${scriptsStore}/${n}"} ${escapeShellArg "${dir}/${n}"}
        $DRY_RUN_CMD chmod 644 ${escapeShellArg "${dir}/${n}"}
      '')
      scriptNames}
    $DRY_RUN_CMD cp -f ${escapeShellArg "${manifestStore}"} ${escapeShellArg manifest}
  '';
in {
  options.programs.userscripts = {
    enable = mkEnableOption "userscript management for directory-based managers";

    scripts = mkOption {
      type = types.attrsOf scriptType;
      default = {};
      example = literalExpression ''
        {
          "dark-mode.user.js".text = "// ==UserScript== ...";
          "from-file.user.js".source = ./scripts/from-file.user.js;
        }
      '';
      description = ''
        Userscripts written to every enabled target. The attribute name is the
        destination filename verbatim (include the `.user.js` suffix). Each entry
        sets exactly one of `text` (inline) or `source` (a file).
      '';
    };

    targets = mkOption {
      type = types.attrsOf targetType;
      default = {};
      example = literalExpression ''
        {
          firefox-sync.directory = "''${config.home.homeDirectory}/userscripts";
        }
      '';
      description = ''
        Named directory targets. The shared `scripts` set is written into each
        enabled target's `directory`. On darwin a `userscripts-safari` target is
        registered automatically; disable it with
        `targets.userscripts-safari.enable = false`.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # macOS default: the Userscripts Safari extension's scripts directory.
    (mkIf pkgs.stdenv.hostPlatform.isDarwin {
      programs.userscripts.targets.userscripts-safari.directory = mkDefault safariDir;
    })

    {
      assertions =
        (mapAttrsToList (fname: spec: {
            assertion = (spec.text != null) != (spec.source != null);
            message = "programs.userscripts.scripts.\"${fname}\": set exactly one of `text` or `source`.";
          })
          cfg.scripts)
        ++ (mapAttrsToList (fname: _: {
            assertion = !(hasInfix "/" fname);
            message = "programs.userscripts.scripts.\"${fname}\": filename must not contain '/'.";
          })
          cfg.scripts);

      # One activation entry handles every enabled target. The managed-name set is
      # declared once and shared across target sections.
      home.activation.userscripts = hm.dag.entryAfter ["writeBoundary"] ''
        declare -A _us_managed=( ${managedDecl} )
        ${concatStringsSep "\n" (mapAttrsToList targetSection enabledTargets)}
        unset _us_managed
      '';
    }
  ]);
}
