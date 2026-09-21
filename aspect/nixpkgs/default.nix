{inputs, ...}: let
  # Both registries point at the flake inputs themselves rather than an
  # equivalent ref, so bumping an input moves every consumer at once and the
  # source resolves offline from the store.
  registry = {
    nixpkgs.flake = inputs.nixpkgs;
    # `pkgs.unstable.<name>` comes from this input; keep the ref consistent.
    nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
  };
  nixPath = "nixpkgs=${inputs.nixpkgs}";
  # Determinate Nix's compiled-in default, restored below.
  hostedFlakeRegistry = "https://install.determinate.systems/flake-registry/stable/flake-registry.json";
in {
  nixpkgs = {
    overlays = [
      # Expose nixpkgs-unstable as `pkgs.unstable.<pkg>` in system configs.
      # Standalone home-manager configs construct pkgs themselves and apply the
      # same overlay in their constructor — they can't set nixpkgs.overlays here.
      (final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit (prev.stdenv.hostPlatform) system;
          config.allowUnfree = true;
        };
      })
      (import ../../packages/mdadf/overlay.nix)
      # `pkgs.llm-agents.<name>` — coding agents from numtide/llm-agents.nix.
      # Lazy: hosts that don't reference it (globalhawk) pay nothing.
      # Deliberately NOT upstream's `overlays.shared-nixpkgs`: that builds the
      # packages against our (stable) nixpkgs, rebuilding from source and missing
      # cache.numtide.com. `packages` are prebuilt against their pinned unstable.
      (final: _prev: {
        llm-agents = inputs.llm-agents.packages.${final.stdenv.hostPlatform.system};
      })
    ];
    config.allowUnfree = true;
  };

  # Pin the flake registry and `<nixpkgs>` to the same inputs these
  # configurations are built from, so `nix run nixpkgs#foo`, `nix-shell -p foo`
  # and `pkgs.foo` here all land on one store path. A bare `nixpkgs` ref
  # otherwise falls through to whichever global registry the running nix ships
  # (Determinate Nix points it at FlakeHub's nixpkgs-weekly, upstream nix at the
  # nixpkgs-unstable channel tarball), each drifting from this flake on its own
  # schedule. Pinned at system scope rather than per-user: that also covers root
  # and the `nix` comma execs from its own closure.
  darwin = {lib, ...}: {
    determinateNix = {
      inherit registry;
      customSettings = {
        # Determinate owns /etc/nix/nix.conf and forbids `extra-nix-path` here,
        # but nix.custom.conf is `!include`d after its `extra-nix-path` line, so
        # this plain assignment replaces it.
        nix-path = nixPath;
        # Whenever `registry` is non-empty the determinate module repoints
        # `flake-registry` at /etc/nix/registry.json, which would drop the ~39
        # ids its hosted registry provides — `home-manager`, and the non-exact
        # `nixpkgs` entry that makes `nixpkgs/nixos-26.05` resolve. Our entries
        # are unaffected either way: the system registry outranks the global
        # one. Drop this line to make bare ids resolve only from this repo.
        flake-registry = lib.mkForce hostedFlakeRegistry;
      };
    };
  };

  nixos.nix = {
    inherit registry;
    # `nix.nixPath` would export this as the NIX_PATH *environment variable*,
    # which flips comma onto an impure `-f <nixpkgs>` code path that ignores
    # COMMA_NIXPKGS_FLAKE. Setting nix.conf directly keeps the env var unset.
    settings.nix-path = nixPath;
  };
}
