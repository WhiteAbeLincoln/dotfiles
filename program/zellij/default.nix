# ENVIRONMENTS: home-manager
{...}: {
  programs.zellij = {
    enable = true;
    # globalhawk is headless and only ever reached over SSH, so every
    # interactive shell is a remote one. Let zellij's own fish integration
    # auto-start a session on login instead of gating on SSH detection
    # ourselves (which is what the tmux path in program/fish tries to do).
    enableFishIntegration = true;
    # Close the shell once zellij exits so detaching/quitting ends the SSH
    # session instead of leaving a bare fish behind (sets ZELLIJ_AUTO_EXIT).
    exitShellOnExit = true;
    # Leave attachExistingSession (ZELLIJ_AUTO_ATTACH) off on purpose: that
    # would make the integration run `zellij attach -c`, which ignores the
    # session_name below and spawns a fresh random session each time. With it
    # off the integration runs a bare `zellij`, which honours the config and
    # joins-or-creates the single "remote" session.
    settings = {
      session_name = "remote";
      attach_to_session = true;
    };
  };
}
