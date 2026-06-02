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
    # Canonical ~/.agents tree (read by codex and pi directly).
    (mkIf hasContext {
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
    (mkIf hasSkills {
      home.file = mkMerge [
        (mkSkillLinks skillsRoot)
        (mkIf cfg.claude-code.enable (mkSkillLinks claudeSkillsRoot))
      ];
    })

    # claude-code: defer to the upstream module for context, settings, and packaging.
    (mkIf cfg.claude-code.enable {
      programs.claude-code = mkMerge [
        (removeAttrs cfg.claude-code.settings ["enable" "package"])
        ({
            enable = true;
            package = cfg.claude-code.package;
          }
          // optionalAttrs hasContext {context = mkDefault rewrittenAgents;})
      ];
    })

    # codex: defer to the upstream module. Reads ~/.agents/skills directly.
    (mkIf cfg.codex.enable {
      programs.codex = mkMerge [
        (removeAttrs cfg.codex.settings ["enable" "package"])
        ({
            enable = true;
            package = cfg.codex.package;
          }
          // optionalAttrs hasContext {context = mkDefault rewrittenAgents;})
      ];
    })

    # pi-coding-agent: no upstream module. Reads ~/.agents/skills directly.
    (mkIf cfg.pi.enable (mkMerge [
      {home.packages = mkIf (cfg.pi.package != null) [cfg.pi.package];}
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
