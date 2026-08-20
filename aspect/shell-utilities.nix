{lib, ...}: {
  homeManager = {pkgs, ...}: {
    home.packages = [pkgs.haskellPackages.ShellCheck];
    programs.jq.enable = lib.mkDefault true;
    programs.keychain.enable = lib.mkDefault pkgs.stdenv.isLinux;
    programs.ssh = {
      enable = lib.mkDefault true;
      # Default host block. As of home-manager 26.05, matchBlocks/extraOptions are
      # deprecated in favour of `settings` keyed by host, with OpenSSH directive
      # names used directly. IgnoreUnknown keeps older ssh from choking on the
      # macOS-only UseKeychain directive.
      settings."*" = {
        IgnoreUnknown = lib.mkDefault "AddKeysToAgent,UseKeychain";
        AddKeysToAgent = lib.mkDefault "yes";
        UseKeychain = lib.mkDefault "yes";
      };
    };
  };
}
