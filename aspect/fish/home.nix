# ENVIRONMENTS: nix-darwin, home-manager
{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [
    ./module.nix
  ];
  programs.fish = {
    enable = lib.mkDefault true;
    # fish 4.8 stopped shipping share/fish/tools/ and instead embeds
    # create_manpage_completions.py in the binary (`status get-file`), but
    # home-manager's release-26.05 generateCompletions still expects the
    # on-disk script (fixed only on HM master). Re-materialise it at the old
    # path until the fix reaches a 0.2605 release, then drop this wrapper.
    package = pkgs.symlinkJoin {
      name = "${pkgs.unstable.fish.name}-manpage-tools";
      paths = [pkgs.unstable.fish];
      postBuild = ''
        mkdir -p $out/share/fish/tools
        ${pkgs.unstable.fish}/bin/fish --no-config \
          -c 'status get-file tools/create_manpage_completions.py' \
          > $out/share/fish/tools/create_manpage_completions.py
      '';
    };
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
      # add homebrew if it exists, but don't error if it doesn't since it's only on darwin
      if test -d /opt/homebrew/bin
        fish_add_path -m /opt/homebrew/bin
      end
    '';
    loginShellInit = lib.optionalString (config.programs.fish.sshMultiplexer == "tmux") ''
      # Attach to (or create) a shared tmux session on SSH login, then close the
      # shell once it detaches so exiting the multiplexer ends the SSH session
      # rather than dropping into a bare fish. Detect SSH via $SSH_CONNECTION
      # (set by sshd) instead of walking the process tree, and skip when already
      # inside tmux to avoid nesting.
      if set -q SSH_CONNECTION; and not set -q TMUX
          if tmux attach-session -t remote 2>/dev/null; or tmux new-session -s remote
              kill $fish_pid
          end
          echo "tmux failed to start; using plain fish shell"
      end
    '';

    interactiveShellInit = ''
      set -g fish_key_bindings fish_vi_key_bindings # use vim-style keys
    '';

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
  };
}
