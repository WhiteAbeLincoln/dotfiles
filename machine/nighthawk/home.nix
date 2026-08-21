{pkgs, ...}: {
  home.packages = [
    # Lima-based Docker daemon for macOS — works with Tilt + kind where
    # podman 5's Docker API compat layer falls down (BuildKit gRPC, kind
    # load docker-image both fail on podman). Docker CLI is a separate
    # package since pkgs.colima only provides the daemon manager.
    pkgs.unstable.colima
    pkgs.unstable.docker-client
    pkgs.unstable.ollama
    pkgs.unstable.lmstudio
    pkgs.tmux
    pkgs.imagemagick
    # Work stuff
    pkgs.glab
  ];

  # programs.texlive = {
  #   enable = true;
  #   extraPackages = tpkgs: {
  #     inherit (tpkgs) scheme-full;
  #   };
  # };
  programs.ssh.enableDefaultConfig = false;
}
