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

    k3s = {
      enable = mkEnableOption "read-only kubectl access for the sandbox user via a view-bound ServiceAccount";

      clusterRole = mkOption {
        type = types.str;
        default = "view";
        description = ''
          The ClusterRole the sandbox user's ServiceAccount is bound to
          cluster-wide. Defaults to the built-in read-only `view` role, which
          excludes Secrets and every write/exec/port-forward verb.
        '';
      };

      kubeconfig = mkOption {
        type = types.str;
        default = "/etc/rancher/k3s/agent-readonly.kubeconfig";
        description = ''
          Where the generated read-only kubeconfig is written. Owned by the sandbox
          user, mode 0400 — the token is not world-readable (unlike the docker proxy
          port), so other local accounts cannot use it.
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
      #
      # DOCKER_HOST is also set as a session variable (honoured by docker-aware
      # tools running under the wrapper, which sources this user's HM session
      # vars), but this user has NO shell integration enabled, so session vars are
      # not sourced for a bare `sudo -u ${cfg.user} docker ...`. So we ship a
      # `docker` that pins the proxy itself — working in every context (bare,
      # login, and under the wrapper) rather than depending on shell sourcing.
      home-manager.users.${cfg.user} = {
        home.sessionVariables.DOCKER_HOST = "tcp://${cfg.docker.listenAddress}:${toString cfg.docker.port}";
        home.packages = [
          (pkgs.writeShellScriptBin "docker" ''
            export DOCKER_HOST="tcp://${cfg.docker.listenAddress}:${toString cfg.docker.port}"
            exec ${pkgs.docker-client}/bin/docker "$@"
          '')
          pkgs.jq
          pkgs.ripgrep
        ];
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

    (mkIf cfg.k3s.enable {
      # In-cluster RBAC delivered through the same services.k3s.manifests lane as
      # every other workload: a ServiceAccount bound cluster-wide to the read-only
      # `view` ClusterRole (no Secrets, no write/exec/port-forward), plus a
      # long-lived token Secret (k8s >=1.24 no longer auto-mints SA tokens) and a
      # small supplementary role for the cluster-scoped resources `view` omits.
      services.k3s.manifests.agent-readonly-rbac.content = [
        {
          apiVersion = "v1";
          kind = "ServiceAccount";
          metadata = {
            name = "agent-readonly";
            namespace = "kube-system";
          };
        }
        {
          apiVersion = "rbac.authorization.k8s.io/v1";
          kind = "ClusterRoleBinding";
          metadata.name = "agent-readonly-view";
          roleRef = {
            apiGroup = "rbac.authorization.k8s.io";
            kind = "ClusterRole";
            name = cfg.k3s.clusterRole;
          };
          subjects = [
            {
              kind = "ServiceAccount";
              name = "agent-readonly";
              namespace = "kube-system";
            }
          ];
        }
        {
          apiVersion = "v1";
          kind = "Secret";
          metadata = {
            name = "agent-readonly-token";
            namespace = "kube-system";
            annotations."kubernetes.io/service-account.name" = "agent-readonly";
          };
          type = "kubernetes.io/service-account-token";
        }
        {
          apiVersion = "rbac.authorization.k8s.io/v1";
          kind = "ClusterRole";
          metadata.name = "agent-readonly-cluster";
          rules = [
            {
              apiGroups = [""];
              resources = ["nodes" "namespaces" "persistentvolumes"];
              verbs = ["get" "list" "watch"];
            }
            # cluster-scoped CRs the built-in `view` role does not aggregate, but
            # that are useful read-only for debugging cert issuance.
            {
              apiGroups = ["cert-manager.io"];
              resources = ["clusterissuers"];
              verbs = ["get" "list" "watch"];
            }
          ];
        }
        {
          apiVersion = "rbac.authorization.k8s.io/v1";
          kind = "ClusterRoleBinding";
          metadata.name = "agent-readonly-cluster";
          roleRef = {
            apiGroup = "rbac.authorization.k8s.io";
            kind = "ClusterRole";
            name = "agent-readonly-cluster";
          };
          subjects = [
            {
              kind = "ServiceAccount";
              name = "agent-readonly";
              namespace = "kube-system";
            }
          ];
        }
      ];

      # Root oneshot: read the SA token+CA (via the admin kubeconfig) once k3s has
      # populated them, and assemble an agent-owned 0400 read-only kubeconfig.
      systemd.services.agent-readonly-kubeconfig = {
        description = "Generate the read-only kubeconfig for the ${cfg.user} sandbox user";
        after = ["k3s.service"];
        wants = ["k3s.service"];
        wantedBy = ["multi-user.target"];
        path = [pkgs.kubectl pkgs.coreutils];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          set -euo pipefail
          admin=/etc/rancher/k3s/k3s.yaml
          token=""
          for _ in $(seq 1 60); do
            token=$(kubectl --kubeconfig "$admin" -n kube-system get secret agent-readonly-token -o jsonpath='{.data.token}' 2>/dev/null | base64 -d || true)
            [ -n "$token" ] && break
            sleep 2
          done
          if [ -z "$token" ]; then
            echo "agent-readonly ServiceAccount token was not populated in time" >&2
            exit 1
          fi
          cafile=$(mktemp)
          kubectl --kubeconfig "$admin" -n kube-system get secret agent-readonly-token -o jsonpath='{.data.ca\.crt}' | base64 -d > "$cafile"

          out=${cfg.k3s.kubeconfig}
          umask 077
          rm -f "$out"
          KUBECONFIG="$out" kubectl config set-cluster default --server=https://127.0.0.1:6443 --certificate-authority="$cafile" --embed-certs=true
          KUBECONFIG="$out" kubectl config set-credentials agent-readonly --token="$token"
          KUBECONFIG="$out" kubectl config set-context default --cluster=default --user=agent-readonly
          KUBECONFIG="$out" kubectl config use-context default
          rm -f "$cafile"

          chown ${cfg.user}:users "$out"
          chmod 0400 "$out"
          install -d -o ${cfg.user} -g users -m 0700 /home/${cfg.user}/.kube
          install -o ${cfg.user} -g users -m 0400 "$out" /home/${cfg.user}/.kube/config
        '';
      };

      # Ship a `kubectl` that pins the read-only kubeconfig, robust in every
      # context (bare `sudo -u`, login, under the wrapper) — mirrors the `docker`
      # wrapper above rather than relying on session-var sourcing.
      home-manager.users.${cfg.user} = {
        home.sessionVariables.KUBECONFIG = cfg.k3s.kubeconfig;
        home.packages = [
          (pkgs.writeShellScriptBin "kubectl" ''
            export KUBECONFIG="${cfg.k3s.kubeconfig}"
            exec ${pkgs.kubectl}/bin/kubectl "$@"
          '')
        ];
      };
    })
  ]);
}
