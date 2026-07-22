# Stands up an unprivileged, read-only AI-agent sandbox user and drives both the
# operator's and the agent's home-manager configs from ONE shared ai-agents module,
# so nothing has to be kept in sync by hand. The operator (already an admin) gets
# sudo wrappers that run the harnesses as the sandbox user, plus suffixed escape-hatch
# binaries; the sandbox user gets the full harness setup and can read/debug the box
# but write/control nothing (DAC + polkit + group hygiene — no kernel sandbox).
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit
    (lib)
    mkDefault
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    types
    ;
  cfg = config.services.aiAgentSandbox;
in {
  options.services.aiAgentSandbox = {
    enable = mkEnableOption "a sandboxed, read-only AI-agent user driven from a shared ai-agents config";

    operator = mkOption {
      type = types.str;
      default = config.meta.user;
      defaultText = lib.literalExpression "config.meta.user";
      description = ''
        Trusted user (already an admin) who gets the sudo wrappers that run the
        harnesses as {option}`services.aiAgentSandbox.user`, plus the suffixed
        escape-hatch commands. Defaults to the machine's {option}`meta.user`.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "agent";
      description = "The unprivileged sandbox user the harnesses run as.";
    };

    binSuffix = mkOption {
      type = types.str;
      default = "-local";
      description = ''
        Suffix for the operator's unsandboxed escape-hatch commands (e.g.
        `claude-local`). Passed straight to {option}`programs.ai-agents.binSuffix`.
      '';
    };

    sharedModules = mkOption {
      type = types.listOf types.deferredModule;
      default = [../../program/ai-agents];
      defaultText = lib.literalExpression "[ ../../program/ai-agents ]";
      description = ''
        Home-manager module(s) carrying the opinionated ai-agents config. Imported
        verbatim into BOTH the operator and sandbox homes, so there is a single
        source of truth. Not hardcoded to any particular file — pass an inline
        module if you like.
      '';
    };

    docker = {
      enable = mkEnableOption "read-only Docker access for the sandbox user via docker-socket-proxy";

      listenAddress = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "Host address the proxy binds (localhost by default — no firewall change).";
      };

      port = mkOption {
        type = types.port;
        default = 2375;
        description = "Host port the proxy binds; also the sandbox user's DOCKER_HOST port.";
      };

      image = mkOption {
        type = types.str;
        default = "ghcr.io/tecnativa/docker-socket-proxy:latest";
        description = "The docker-socket-proxy image.";
      };

      settings = mkOption {
        type = types.attrsOf (types.either types.int types.str);
        default = {};
        description = ''
          docker-socket-proxy environment (which API areas are exposed). The
          read-only debugging defaults (read areas on, `POST` off) are applied with
          `mkDefault`, so individual areas can be overridden per key from outside.
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Unprivileged sandbox user. systemd-journal (read-only logs) is the ONLY
      # extra group — never wheel / docker / _media. Locked password + no SSH keys
      # => reachable only via `sudo -u` from the operator.
      users.users.${cfg.user} = {
        isNormalUser = true;
        description = mkDefault "AI agent sandbox user";
        hashedPassword = mkDefault "!";
        # No explicit `shell`: `isNormalUser` already turns on `useDefaultShell`
        # (itself `mkDefault`), which resolves to `pkgs.bashInteractive` via the
        # base bash module. Setting it again here would be a second `mkDefault`
        # at the same priority for a non-mergeable option -> "defined multiple
        # times", even though both sides agree on the value.
        extraGroups = ["systemd-journal"];
      };

      # One-directional privilege drop. Permissive on purpose: the operator is
      # already root, so scoping commands buys no security; SETENV lets the wrapper
      # hand the agent a clean, deterministic environment. Matches only when the
      # CALLER is the operator, so the agent cannot use it.
      security.sudo.extraRules = [
        {
          users = [cfg.operator];
          runAs = cfg.user;
          commands = [
            {
              command = "ALL";
              options = ["NOPASSWD" "SETENV"];
            }
          ];
        }
      ];

      # Sandbox user's home: the full local ai-agents setup from the shared
      # module(s). runAs/binSuffix stay at their defaults => normal setup. Needs the
      # same base HM modules the flake gives the operator (meta option + hm modules).
      home-manager.users.${cfg.user} = {
        imports = [../common-hm ../hm] ++ cfg.sharedModules;
        meta.user = cfg.user;
        home.stateVersion = mkDefault config.system.stateVersion;
      };

      # Operator's home: the SAME shared module(s) + the wrapper / escape-hatch
      # knobs. Merges onto the operator's HM user already defined in flake.nix.
      home-manager.users.${cfg.operator} = {
        imports = cfg.sharedModules;
        programs.ai-agents.runAs = cfg.user;
        programs.ai-agents.binSuffix = cfg.binSuffix;
      };
    }

    (mkIf cfg.docker.enable {
      # Read-only debugging defaults: read areas on, all mutations (POST) off.
      # mkDefault so any single area can be flipped from outside without mkForce.
      services.aiAgentSandbox.docker.settings = {
        CONTAINERS = mkDefault 1;
        IMAGES = mkDefault 1;
        NETWORKS = mkDefault 1;
        VOLUMES = mkDefault 1;
        INFO = mkDefault 1;
        VERSION = mkDefault 1;
        PING = mkDefault 1;
        EVENTS = mkDefault 1;
        POST = mkDefault 0;
      };

      # Point the sandbox user at the proxy and give it the read-only debugging
      # toolset. Never add it to the `docker` group; it reaches Docker only here.
      home-manager.users.${cfg.user} = {
        home.sessionVariables.DOCKER_HOST = "tcp://${cfg.docker.listenAddress}:${toString cfg.docker.port}";
        home.packages = [pkgs.docker-client pkgs.jq pkgs.ripgrep];
      };

      # docker-socket-proxy: bind the real socket, expose only whitelisted GET
      # endpoints on localhost. POST=0 => run/exec/stop/rm/build return 403.
      #
      # KNOWN LIMITATION (accepted; may restrict later): loopback TCP has no
      # per-UID access control, so the read-only API here is reachable by ANY
      # local account (plex/immich/calibre-web/…), not just `user`. Those can
      # read container env (incl. secrets) via `inspect`. Acceptable on this
      # single-operator box; to tighten, front it with a group-owned unix socket
      # or an nftables `owner --uid-owner` rule for this port.
      virtualisation.oci-containers.containers.docker-proxy = {
        image = cfg.docker.image;
        volumes = ["/var/run/docker.sock:/var/run/docker.sock:ro"];
        ports = ["${cfg.docker.listenAddress}:${toString cfg.docker.port}:2375"];
        environment = lib.mapAttrs (_: toString) cfg.docker.settings;
      };
    })
  ]);
}
