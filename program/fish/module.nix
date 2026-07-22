# Adds a knob for whether fish attaches to a terminal multiplexer on SSH login
# (see the `loginShellInit` in ./default.nix). Kept as an option so a host can
# opt in without forking the shared fish config. Defaults to "none": the old
# hard-coded tmux path detected SSH via `ps -p %self` (which inspects fish's
# own command line, never the sshd parent), so it never actually fired — "none"
# preserves that real-world behaviour while the "tmux" path is now correct.
# globalhawk uses zellij's native fish integration instead (see program/zellij).
{lib, ...}:
with lib; {
  options.programs.fish.sshMultiplexer = mkOption {
    type = types.enum ["tmux" "none"];
    default = "none";
    description = ''
      When "tmux", fish attaches to (or creates) a shared tmux session named
      "remote" on interactive SSH logins. "none" disables the behaviour.
    '';
  };
}
