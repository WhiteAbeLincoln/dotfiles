{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.custom.programs.spacemacs;

in {
  options.custom.programs.spacemacs = {
    enable = mkEnableOption "spacemacs";
    repo = mkOption {
      type = types.str;
      default = "https://github.com/syl20bnr/spacemacs";
    };
    branch = mkOption {
      type = types.str;
      default = "develop";
    };
    rev = mkOption {
      type = types.str;
    };
    rcfile = mkOption {
      type = types.nullOr ((import ../../lib/types.nix { inherit lib; }).file);
      default = null;
    };
    package = mkOption {
      type = types.package;
      default = pkgs.emacs;
      description = "The Emacs package to use.";
    };
  };

  config = mkIf cfg.enable(
    mkMerge ([
      {
        programs.emacs = {
          enable = true;
          package = cfg.package;
        };
        home.activation.spacemacs-setup =
          lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if ! [ -e ~/.emacs.d ]; then
            $DRY_RUN_CMD git \
              clone $VERBOSE_ARG \
              -b ${escapeShellArg cfg.branch} \
              ${escapeShellArg cfg.repo} \
              ~/.emacs.d
          fi
          if ! [ -e ~/.emacs.d/.git ]; then
            if [ -e ~/.emacs.d/private ]; then
              temp=$(mktemp -d)
              mv ~/.emacs.d/private "$temp"
            fi

            $DRY_RUN_CMD git \
              clone $VERBOSE_ARG \
              -b ${escapeShellArg cfg.branch} \
              ${escapeShellArg cfg.repo} \
              ~/.emacs.d

            rm -r ~/.emacs.d/private
            if [ -n "''${temp+set}" ] && [ -e "$temp/private" ]; then
              mv "$temp"/private ~/.emacs.d
              rm -r "$temp"
            fi
          fi
          $DRY_RUN_CMD git \
            --git-dir ~/.emacs.d/.git \
            --work-tree ~/.emacs.d \
            fetch $VERBOSE_ARG \
            origin \
            ${escapeShellArg cfg.branch}
          $DRY_RUN_CMD git \
            --git-dir ~/.emacs.d/.git \
            --work-tree ~/.emacs.d \
            update-ref \
            refs/heads/${escapeShellArg cfg.branch} \
            ${escapeShellArg cfg.rev}
          $DRY_RUN_CMD git \
            --git-dir ~/.emacs.d/.git \
            --work-tree ~/.emacs.d \
            checkout \
            ${escapeShellArg cfg.branch}
          '';
      }
      (mkIf (cfg.rcfile != null) {
        home.file.".spacemacs" = cfg.rcfile;
      })
    ])
  ) ;
}
