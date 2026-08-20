# Installs and configures coding harnesses (claude-code, codex, pi-coding-agent)
# around a shared ~/.agents tree. AGENTS.md is authored once with @ctx/ references
# and rewritten into each enabled agent's config dir; skills live canonically under
# ~/.agents/skills (which codex and pi read directly) and are additionally linked
# into ~/.claude/skills for claude. Defers to the upstream programs.claude-code /
# programs.codex modules where they exist.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.ai-agents;

  inherit
    (lib)
    filterAttrs
    isDerivation
    literalExpression
    mapAttrs'
    mkDefault
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    mkPackageOption
    nameValuePair
    optional
    optionalAttrs
    types
    ;

  jsonFormat = pkgs.formats.json {};

  homeDir = config.home.homeDirectory;
  contextRoot = "${homeDir}/.agents/context";
  skillsRoot = "${homeDir}/.agents/skills";
  # Honour a custom programs.claude-code.configDir; defaults to ~/.claude.
  claudeSkillsRoot = "${config.programs.claude-code.configDir}/skills";

  hasContext = cfg.contextDir != null;
  hasSkills = cfg.skills != {};

  # AGENTS.md with `@ctx/<rel>` references rewritten to the absolute deployed path
  # under ~/.agents/context. Absolute (not `~`) so it resolves regardless of whether
  # an agent expands `~` in @-references; homeDir is evaluated per build, so it stays
  # correct per-machine. Lazily evaluated: only forced when hasContext is true.
  rewrittenAgents =
    builtins.replaceStrings ["@ctx/"] ["@${contextRoot}/"]
    (builtins.readFile (cfg.contextDir + "/AGENTS.md"));
  agentsFile = pkgs.writeText "AGENTS.md" rewrittenAgents;

  # Everything in contextDir except AGENTS.md -> ~/.agents/context/<name> (files and
  # subdirectories alike).
  contextDocs =
    mapAttrs' (
      name: _type:
        nameValuePair "${contextRoot}/${name}" {
          source = cfg.contextDir + "/${name}";
        }
    )
    (filterAttrs (name: _: name != "AGENTS.md") (builtins.readDir cfg.contextDir));

  # A fetcher's output is a derivation; coerce it to its store-path string for use as
  # a home.file source. Local paths and store-path strings pass through unchanged.
  norm = v:
    if isDerivation v
    then "${v}"
    else v;

  # One single (non-recursive) symlink per skill under `root`. Each link points at the
  # same store path, so a skill is realised once and every link just references it.
  mkSkillLinks = root:
    mapAttrs' (
      name: src:
        nameValuePair "${root}/${name}" {source = norm src;}
    )
    cfg.skills;

  # Expose <pkg>'s primary command <cmd> under the name <cmd><suffix>, preserving
  # the rest of the package tree; update mainProgram so lib.getExe still resolves.
  # A no-op when suffix is empty or the package is null.
  # NOTE: safe here because programs.claude-code only re-wraps its package's bin/claude
  # when mcpServers/lspServers/plugins are set (none are). If you ever configure those
  # alongside a binSuffix, upstream's `mv $out/bin/claude` would miss the renamed binary
  # — switch that harness to package = null plus an own writeShellScriptBin instead.
  suffixPackage = suffix: cmd: pkg:
    if suffix == "" || pkg == null
    then pkg
    else
      pkgs.symlinkJoin {
        name = "${pkg.pname or cmd}${suffix}";
        paths = [pkg];
        postBuild = ''
          if [ -e "$out/bin/${cmd}" ]; then
            target=$(readlink -f "$out/bin/${cmd}")
            rm "$out/bin/${cmd}"
            ln -s "$target" "$out/bin/${cmd}${suffix}"
          fi
        '';
        meta = (pkg.meta or {}) // {mainProgram = "${cmd}${suffix}";};
      };

  # localSetup: whether THIS user hosts the harnesses (~/.agents, ~/.claude) locally.
  # A bare runAs (binSuffix == "") emits wrappers only and provisions nothing here.
  localSetup = cfg.runAs == null || cfg.binSuffix != "";
  emitWrappers = cfg.runAs != null;

  # sudo-wrapper: run <cmd> as cfg.runAs, in the caller's cwd, with the target
  # user's per-user profile on PATH and its HM session vars (e.g. DOCKER_HOST)
  # sourced. HOME is set from the target user's passwd entry via `sudo -H`.
  runAsWrapper = cmd:
    pkgs.writeShellScriptBin cmd ''
      exec /run/wrappers/bin/sudo -H -u ${cfg.runAs} \
        PATH=/etc/profiles/per-user/${cfg.runAs}/bin:/run/current-system/sw/bin \
        ${pkgs.bash}/bin/bash -c '
          vars="/etc/profiles/per-user/${cfg.runAs}/etc/profile.d/hm-session-vars.sh"
          [ -r "$vars" ] && . "$vars"
          cd "$1" || exit 1
          shift
          exec ${cmd} "$@"
        ' ${cmd}-runas "$PWD" "$@"
    '';

  enabledHarnesses =
    optional cfg.claude-code.enable "claude"
    ++ optional cfg.codex.enable "codex"
    ++ optional cfg.pi.enable "pi";
in {
  options.programs.ai-agents = {
    enable = mkEnableOption "AI coding-agent harnesses and the shared ~/.agents tree";

    contextDir = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = literalExpression "./agents";
      description = ''
        Directory containing an {file}`AGENTS.md` plus the context documents it
        references. Every entry other than {file}`AGENTS.md` is placed under
        {file}`~/.agents/context/`. References of the form `@ctx/<rel>` in
        {file}`AGENTS.md` are rewritten to the absolute deployed path
        (`@~/.agents/context/<rel>`) and the result is linked into each enabled
        agent's config directory.
      '';
    };

    skills = mkOption {
      type = types.attrsOf (types.oneOf [types.package types.path types.str]);
      default = {};
      example = literalExpression ''
        {
          foo = ./skills/foo;
          bar = pkgs.fetchgit {
            url = "https://example.com/skill.git";
            rev = "0000000000000000000000000000000000000000";
            hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          };
          sub = "''${repo}/skills/sub";
        }
      '';
      description = ''
        Skills to place under {file}`~/.agents/skills/<name>`. Each value is a local
        path, a fetcher's output (for example {command}`pkgs.fetchgit { ... }`), or a
        store-path string — which may point at a subdirectory of a fetched repository
        (for example `"''${repo}/skills/foo"`). Codex and pi read
        {file}`~/.agents/skills` directly; for claude the same skills are additionally
        linked into {file}`~/.claude/skills/<name>`.
      '';
    };

    runAs = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "agent";
      description = ''
        If set, this user does NOT get the harnesses configured in its own home.
        Instead, each enabled harness is exposed as a `sudo` wrapper that runs the
        real harness as the named user, in the caller's working directory, using
        that user's home-manager profile and session variables. Provisions nothing
        for the target user — configure the target user's home separately (e.g. via
        {option}`services.aiAgentSandbox`). Combine with {option}`binSuffix` to also
        keep a full, local (unsandboxed) setup for this user under suffixed names.
      '';
    };

    binSuffix = mkOption {
      type = types.str;
      default = "";
      example = "-local";
      description = ''
        If non-empty, do the full local setup for this user (context, skills,
        harness config) and expose each enabled harness command under its suffixed
        name (for example `binSuffix = "-local"` yields `claude-local`). The
        unsuffixed command name is left free so a {option}`runAs` wrapper can claim
        it.
      '';
    };

    claude-code = {
      enable = mkEnableOption "the claude-code coding harness";
      package = mkPackageOption pkgs "claude-code" {nullable = true;};
      settings = mkOption {
        type = types.attrsOf types.anything;
        default = {};
        example = literalExpression ''
          {
            settings = { theme = "dark"; includeCoAuthoredBy = false; };
            enableMcpIntegration = true;
          }
        '';
        description = ''
          Passed through verbatim to {option}`programs.claude-code` (its `enable` and
          `package` are supplied by the options above and stripped from this set).
          Any upstream option is therefore reachable. Note that the keys here are the
          full {option}`programs.claude-code` options, so the settings.json content
          itself lives under `settings.settings`.
        '';
      };
    };

    codex = {
      enable = mkEnableOption "the codex coding harness";
      package = mkPackageOption pkgs "codex" {nullable = true;};
      settings = mkOption {
        type = types.attrsOf types.anything;
        default = {};
        example = literalExpression ''
          {
            settings = { model = "gpt-5"; };
          }
        '';
        description = ''
          Passed through verbatim to {option}`programs.codex` (its `enable` and
          `package` are supplied by the options above and stripped from this set).
        '';
      };
    };

    pi = {
      enable = mkEnableOption "the pi-coding-agent coding harness";
      package = mkPackageOption pkgs "pi-coding-agent" {nullable = true;};
      settings = mkOption {
        type = jsonFormat.type;
        default = {};
        description = ''
          Raw JSON written to {file}`~/.pi/agent/settings.json`. pi has no upstream
          home-manager module, so unlike the other agents this is not a passthrough.
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # Operator wrappers: run each enabled harness as cfg.runAs. Emitted whenever
    # runAs is set, regardless of whether a local (suffixed) setup also exists.
    (mkIf emitWrappers {
      home.packages = map runAsWrapper enabledHarnesses;
    })

    # ---- Local setup (this user hosts the harnesses). Skipped for a pure runAs
    #      (wrappers-only) user; that user's config lives in the target home. ----

    # Canonical ~/.agents tree (read by codex and pi directly).
    (mkIf (localSetup && hasContext) {
      home.file =
        contextDocs
        // {
          "${contextRoot}/AGENTS.md".source = agentsFile;
        };
    })
    # Skills are realised once in the store and linked under ~/.agents/skills (which
    # codex and pi read directly). claude reads ~/.claude/skills instead, so the same
    # store paths are linked there too. We deliberately avoid
    # programs.claude-code.skills: its recursive per-file linking turns the skill into
    # many individually-managed paths that collide with the single ~/.agents/skills
    # directory symlink during activation. Single symlinks to a shared store path let
    # both locations be treated as the same.
    (mkIf (localSetup && hasSkills) {
      home.file = mkMerge [
        (mkSkillLinks skillsRoot)
        (mkIf cfg.claude-code.enable (mkSkillLinks claudeSkillsRoot))
      ];
    })

    # claude-code: defer to the upstream module for context and settings; feed it the
    # (optionally renamed) package.
    (mkIf (localSetup && cfg.claude-code.enable) {
      programs.claude-code = mkMerge [
        (removeAttrs cfg.claude-code.settings ["enable" "package"])
        ({
            enable = true;
            package = suffixPackage cfg.binSuffix "claude" cfg.claude-code.package;
          }
          // optionalAttrs hasContext {context = mkDefault rewrittenAgents;})
      ];
    })

    # codex: defer to the upstream module. Reads ~/.agents/skills directly.
    (mkIf (localSetup && cfg.codex.enable) {
      programs.codex = mkMerge [
        (removeAttrs cfg.codex.settings ["enable" "package"])
        ({
            enable = true;
            package = suffixPackage cfg.binSuffix "codex" cfg.codex.package;
          }
          // optionalAttrs hasContext {context = mkDefault rewrittenAgents;})
      ];
    })

    # pi-coding-agent: no upstream module. Reads ~/.agents/skills directly.
    (mkIf (localSetup && cfg.pi.enable) (mkMerge [
      {home.packages = mkIf (cfg.pi.package != null) [(suffixPackage cfg.binSuffix "pi" cfg.pi.package)];}
      (mkIf hasContext {
        home.file."${homeDir}/.pi/agent/AGENTS.md".source = agentsFile;
      })
      (mkIf (cfg.pi.settings != {}) {
        home.file."${homeDir}/.pi/agent/settings.json".source =
          jsonFormat.generate "pi-settings.json" cfg.pi.settings;
      })
    ]))
  ]);
}
