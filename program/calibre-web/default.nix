{pkgs, ...}: {
  # importing this conditionally causes recursion issues
  # instead, macos machines will be required to import the module statically
  # imports = [./macos-module.nix];

  # Create the calibre-web user even when the service is disabled, so that
  # per-host modules (e.g. machine/globalhawk) can reference it.
  # Values mirror what the nixpkgs services.calibre-web module sets (it gates
  # its own user/group behind enable), so re-enabling merges cleanly — keep in sync.
  users.users.calibre-web = {
    isSystemUser = true;
    group = "calibre-web";
  };
  users.groups.calibre-web = {};

  services.calibre-web = {
    # Cutover to k3s Calibre-Web-Automated (books.h.…), which shares this same
    # ${mediaRoot}/books library + metadata.db — so the two must not both run as
    # writers. Disabled, not removed: `enable = true;` + switch is the one-line
    # rollback (library format is unchanged). Full decommission (module, user,
    # ACL, port 8083) is a deferred follow-up once CWA is validated.
    enable = false;
    listen.ip = "0.0.0.0";
    options = {
      enableBookUploading = true;
      # calibreLibrary is a host-specific path, set by the importing machine
      # (globalhawk derives it from facts.mediaRoot).
    };
  };
}
