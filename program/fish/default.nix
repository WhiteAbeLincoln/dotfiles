# ENVIRONMENTS: nix-darwin, home-manager
{
  pkgs,
  lib,
  isHM,
  config,
  myUserName,
  ...
}: {
  programs.fish =
    {
      enable = true;
      shellAliases =
        if pkgs.stdenv.isLinux
        then {
          pbcopy = "${pkgs.xclip}/bin/xclip -i -selection clipboard";
          pbpaste = "${pkgs.xclip}/bin/xclip -o -selection clipboard";
        }
        else {};
      generateCompletions = true;
      shellInit = ''
        # Nix
        if ! type -q nix && test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
          source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
          set --export __ETC_PROFILE_NIX_SOURCED 1
        end
        # End Nix
        fish_add_path -m ~/.local/bin
      '';
      loginShellInit = ''
        set PPID (echo (ps --pid %self -o ppid --no-headers) | xargs)
        if ps --pid $PPID | grep ssh
            tmux has-session -t remote; and tmux attach-session -t remote; or tmux new-session -s remote; and kill %self
            echo "tmux failed to start; using plain fish shell"
        end
      '';

      interactiveShellInit = ''
        set -g fish_key_bindings fish_vi_key_bindings # use vim-style keys
      '';
    }
    // (lib.optionalAttrs isHM {
      plugins = [
        # {
        #   name = "fish-history-merge";
        #   src = pkgs.fetchFromGitHub {
        #     owner = "2m";
        #     repo = "fish-history-merge";
        #     rev = "7e415b8ab843a64313708273cf659efbf471ad39";
        #     sha256 = "sha256-oy32I92sYgEbeVX41Oic8653eJY5bCE/b7EjZuETjMI=";
        #   };
        # }
      ];
    });
}
