# Expose nixpkgs-unstable as `pkgs.unstable.<pkg>` in system configs.
# Standalone home-manager configs construct pkgs themselves and apply the
# same overlay inline in flake.nix — they can't set nixpkgs.overlays here.
{inputs, ...}: {
  nixpkgs.overlays = [
    (final: prev: {
      unstable = import inputs.nixpkgs-unstable {
        inherit (prev.stdenv.hostPlatform) system;
        config.allowUnfree = true;
      };
    })
    (import ../../packages/mdadf/overlay.nix)
    # `pkgs.llm-agents.<name>` — coding agents from numtide/llm-agents.nix.
    # Lazy: hosts that don't reference it (globalhawk) pay nothing.
    inputs.llm-agents.overlays.default
  ];
  nixpkgs.config.allowUnfree = true;
}
