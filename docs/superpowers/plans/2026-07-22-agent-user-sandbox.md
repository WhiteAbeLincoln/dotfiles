# Sandboxed AI-agent user on globalhawk — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run the AI coding harnesses on globalhawk as an unprivileged `agent` user that can read/debug everything but write and control nothing, driven from a single ai-agents config.

**Architecture:** Two per-user knobs on the `programs.ai-agents` home-manager module (`runAs` → sudo-wrappers, `binSuffix` → renamed escape-hatch binaries), plus a NixOS orchestrator (`services.aiAgentSandbox`) that stands up the `agent` user, a one-directional `abe → agent` sudoers rule, a read-only `docker-socket-proxy`, and imports one shared ai-agents module into both the operator's and the agent's homes. Enforcement is DAC + polkit + group hygiene (Model A) — no kernel sandbox.

**Tech Stack:** Nix (flake-parts), NixOS module system, home-manager (as a NixOS module), `virtualisation.oci-containers` (docker backend), `security.sudo`, `docker-socket-proxy`.

**Spec:** `docs/superpowers/specs/2026-07-22-agent-user-sandbox-design.md`

## Global Constraints

- **Enforcement = Model A only** — DAC + polkit + group hygiene. No bubblewrap / `systemd-run` kernel sandbox.
- **Hard invariant:** the sandbox user is in `systemd-journal` and its own primary group ONLY — never `wheel`, `docker`, or `_media`.
- **The audit script is NEVER wired into `nix flake check`** — it must not block `nixos-rebuild`.
- **Format before every commit that touches `.nix`:** run `nix fmt` (alejandra is the flake formatter).
- **Use the Edit tool, not `sed`,** for file edits. This environment uses GNU sed if sed is unavoidable (`sed -i`, not `sed -i ''`).
- **Commit messages document the *why*,** not a file list. End every commit message body with:
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`
- **Do not remove existing (human-written) comments** unless the code they describe is deleted.
- **Verification runs from this darwin host via `nix eval` / `nix build`** (evaluation cross-compiles; it does not build the linux system). Activation (`nixos-rebuild switch`) happens ON globalhawk and is operator-run (Task 5) — never run it from here.
- **Work happens on branch `agent-user-sandbox`** (already created; the spec is already committed there).
- All `nix eval` commands below append `2>/dev/null` in examples to hide the "uncommitted changes" git warning; drop it if you need to see errors.

**Confirmed facts (do not re-verify):**
- Harness command names: claude-code→`claude`, codex→`codex`, pi→`pi` (all have `meta.mainProgram`).
- Upstream `programs.claude-code` installs `cfg.package` directly when no plugins are set (our case); config is gated on `enable`, independent of `package`. `programs.codex` installs `cfg.package` directly. So a suffix-renamed package fed as `package` yields a renamed binary + normal config. No `getExe` usage.
- `pkgs.docker-client` exists (`meta.mainProgram = "docker"`).
- `config.system.stateVersion` on globalhawk is `"23.11"`.
- home-manager is wired as a NixOS module in `flake.nix`; `useGlobalPkgs`/`useUserPackages`/`extraSpecialArgs` are global, so a new `home-manager.users.agent` inherits them.

---

## Task 1: Add `runAs` + `binSuffix` to the `programs.ai-agents` module

**Files:**
- Modify: `modules/hm/ai-agents/module.nix`

**Interfaces:**
- Produces (new options on `programs.ai-agents`):
  - `runAs :: nullOr str` (default `null`) — when set, this user gets sudo-wrappers for each enabled harness (named `claude`/`codex`/`pi`) that run the real harness as the named user; **no** local harness/context/skills setup unless `binSuffix` is also set.
  - `binSuffix :: str` (default `""`) — when non-empty, do the full local setup and expose each enabled harness under its suffixed name (`claude${binSuffix}`).
- Behavior matrix (per enabled harness command `C`):
  - `runAs=null, binSuffix=""` → unchanged: full local setup, `C` on PATH.
  - `runAs="u", binSuffix=""` → wrapper `C` → runs as `u`; nothing else locally.
  - `runAs=null, binSuffix="S"` → full local setup; command is `C+S`.
  - `runAs="u", binSuffix="S"` → wrapper `C` (→ `u`) **and** full local setup as `C+S`.

- [ ] **Step 1: Write the failing check (option does not exist yet)**

Run:
```bash
nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.ai-agents.binSuffix 2>/dev/null; echo "exit=$?"
```
Expected: no value printed, `exit=1` (the option `binSuffix` does not exist yet).

- [ ] **Step 2: Add the `optional` builtin to the `inherit (lib)` list**

In `modules/hm/ai-agents/module.nix`, the `inherit (lib) …` block currently lists helpers alphabetically-ish. Add `optional` (used to build the wrapper list). Change:

```nix
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
```
to add `optional` (keep the rest as-is):
```nix
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
```

- [ ] **Step 3: Add the `let`-bindings (helper + mode flags + wrapper) after `mkSkillLinks`**

Find the end of the `mkSkillLinks` binding (just before `in {`):

```nix
  mkSkillLinks = root:
    mapAttrs' (
      name: src:
        nameValuePair "${root}/${name}" {source = norm src;}
    )
    cfg.skills;
in {
```

Insert these bindings between `cfg.skills;` and `in {`:

```nix
  mkSkillLinks = root:
    mapAttrs' (
      name: src:
        nameValuePair "${root}/${name}" {source = norm src;}
    )
    cfg.skills;

  # Expose <pkg>'s primary command <cmd> under the name <cmd><suffix>, preserving
  # the rest of the package tree; update mainProgram so lib.getExe still resolves.
  # A no-op when suffix is empty or the package is null.
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

  # Provision the harnesses + ~/.agents/~/.claude in THIS user's home? runAs alone
  # (binSuffix == "") means "emit wrappers only, provision nothing here".
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
```

- [ ] **Step 4: Add the `runAs` and `binSuffix` options**

Find the `skills` option block and its closing `};`, immediately followed by `claude-code = {`:

```nix
    skills = mkOption {
      …
    };

    claude-code = {
```

Insert the two new options between them:

```nix
    skills = mkOption {
      …
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
```

- [ ] **Step 5: Rewrite the `config` block to honour `localSetup` / `emitWrappers`**

Replace the entire existing `config = mkIf cfg.enable (mkMerge [ … ]);` block (from `config = mkIf cfg.enable (mkMerge [` through its closing `]);`) with:

```nix
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
    # Skills realised once in the store and linked under ~/.agents/skills (codex/pi)
    # and additionally under ~/.claude/skills for claude.
    (mkIf (localSetup && hasSkills) {
      home.file = mkMerge [
        (mkSkillLinks skillsRoot)
        (mkIf cfg.claude-code.enable (mkSkillLinks claudeSkillsRoot))
      ];
    })

    # claude-code: defer to the upstream module; feed it the (optionally renamed) package.
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
```

- [ ] **Step 6: Format**

Run:
```bash
nix fmt 2>/dev/null
```
Expected: exits 0; `modules/hm/ai-agents/module.nix` reformatted if needed.

- [ ] **Step 7: Verify the options now exist and existing configs are unaffected**

Run:
```bash
echo -n "binSuffix default: "; nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.ai-agents.binSuffix 2>/dev/null; echo
echo -n "runAs default: "; nix eval .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.ai-agents.runAs 2>/dev/null; echo
echo -n "nighthawk claude finalPackage (unchanged): "; nix eval --raw .#darwinConfigurations.nighthawk.config.home-manager.users.abe.programs.claude-code.finalPackage.name 2>/dev/null; echo
echo -n "globalhawk composes: "; nix eval --raw .#nixosConfigurations.globalhawk.config.home-manager.users.abe.home.homeDirectory 2>/dev/null; echo
```
Expected:
```
binSuffix default: ""
runAs default: null
nighthawk claude finalPackage (unchanged): claude-code-<version>
globalhawk composes: /home/abe
```

- [ ] **Step 8: Commit**

```bash
git add modules/hm/ai-agents/module.nix
git commit -m "$(cat <<'EOF'
feat(ai-agents): add runAs + binSuffix knobs

Let a trusted user drop the harnesses into another (sandbox) user via sudo
wrappers, and/or expose an unsandboxed escape hatch under suffixed binary
names, without configuring the harnesses in the wrapping user's own home.
These are the per-user primitives the NixOS orchestrator composes; a bare
runAs is the "wrappers only, provision nothing" opt-out.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: Orchestrator module — user, sudoers, both homes (no docker yet)

**Files:**
- Create: `modules/nixos/ai-agent-sandbox.nix`
- Modify: `machine/globalhawk/default.nix` (import the module + enable it)
- Modify: `machine/globalhawk/home.nix` (drop the direct `program/ai-agents` import)

**Interfaces:**
- Consumes: `programs.ai-agents.runAs` / `.binSuffix` (Task 1); `config.meta.user`; `config.system.stateVersion`.
- Produces (new options): `services.aiAgentSandbox.{enable, operator, user, binSuffix, sharedModules, docker.*}`. The `docker.*` options are declared here but only wired up in Task 3.
- Wires: `users.users.<user>` (unprivileged, locked, `systemd-journal` only); `security.sudo.extraRules` (`operator → user`, `NOPASSWD SETENV`); `home-manager.users.<user>` (full local ai-agents from `sharedModules`) and `home-manager.users.<operator>` (same `sharedModules` + `runAs`/`binSuffix`).

- [ ] **Step 1: Write the failing check (agent user does not exist)**

Run:
```bash
nix eval --raw .#nixosConfigurations.globalhawk.config.users.users.agent.name 2>/dev/null; echo "exit=$?"
```
Expected: no value, `exit=1` (no `agent` user).

- [ ] **Step 2: Create `modules/nixos/ai-agent-sandbox.nix`**

Create the file with this exact content (the `docker` options are declared now; their `config` wiring is added in Task 3):

```nix
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
        shell = mkDefault pkgs.bashInteractive;
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
  ]);
}
```

- [ ] **Step 3: Import the module and enable it in `machine/globalhawk/default.nix`**

Add the module to the `imports` list. Find:

```nix
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # <home-manager/nixos>
    ../../program/plex
    ../../program/calibre-web
    ../../program/immich
    # TODO: homebridge has been added to nixos
    # remove custom module
    # ../../program/homebridge
    ./disks.nix
    # ./backup.nix
  ];
```
Add `../../modules/nixos/ai-agent-sandbox.nix` to the list:
```nix
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # <home-manager/nixos>
    ../../program/plex
    ../../program/calibre-web
    ../../program/immich
    # TODO: homebridge has been added to nixos
    # remove custom module
    # ../../program/homebridge
    ./disks.nix
    # ./backup.nix
    ../../modules/nixos/ai-agent-sandbox.nix
  ];
```

Then enable it. The `# {{{ Users` fold ends with `# }}}` after the `calibre-web` line. Immediately after that `# }}}` closing the Users section, add a new block (defaults cover operator=`abe` via `meta.user` and user=`agent`; docker is turned on in Task 3):

Find:
```nix
  users.users.calibre-web.extraGroups = ["_media"];
  # }}}
```
Replace with:
```nix
  users.users.calibre-web.extraGroups = ["_media"];

  # Unprivileged user for running AI agents read-only; see
  # docs/superpowers/specs/2026-07-22-agent-user-sandbox-design.md.
  services.aiAgentSandbox.enable = true;
  # }}}
```

- [ ] **Step 4: Drop the direct `program/ai-agents` import from `machine/globalhawk/home.nix`**

The orchestrator now drives abe's ai-agents (it imports `sharedModules` into abe's home). Remove the duplicate direct import. Change:

```nix
  imports = [
    ../../role/dev.nix
    ../../program/ai-agents
  ];
```
to:
```nix
  imports = [
    ../../role/dev.nix
  ];
```

- [ ] **Step 5: Format**

Run:
```bash
nix fmt 2>/dev/null
```
Expected: exits 0.

- [ ] **Step 6: Verify the user, sudoers, and both homes**

Run:
```bash
G=.#nixosConfigurations.globalhawk.config
echo -n "agent isNormalUser: "; nix eval "$G.users.users.agent.isNormalUser" 2>/dev/null; echo
echo -n "agent extraGroups: "; nix eval --json "$G.users.users.agent.extraGroups" 2>/dev/null; echo
echo -n "agent home: "; nix eval --raw "$G.home-manager.users.agent.home.homeDirectory" 2>/dev/null; echo
echo -n "agent real claude: "; nix eval --raw "$G.home-manager.users.agent.programs.claude-code.finalPackage.name" 2>/dev/null; echo
echo -n "abe claude renamed: "; nix eval --raw "$G.home-manager.users.abe.programs.claude-code.finalPackage.name" 2>/dev/null; echo
echo -n "sudo rule runAs: "; nix eval --json "$G.security.sudo.extraRules" --apply 'rs: map (r: r.runAs) rs' 2>/dev/null; echo
echo -n "abe wrapper bins: "; nix eval --json "$G.home-manager.users.abe.home.packages" --apply 'ps: builtins.filter (n: n == "claude" || n == "codex" || n == "pi") (map (p: p.name or "") ps)' 2>/dev/null; echo
```
Expected (versions may differ):
```
agent isNormalUser: true
agent extraGroups: ["systemd-journal"]
agent home: /home/agent
agent real claude: claude-code-<version>
abe claude renamed: claude-code-<version>-local
sudo rule runAs: ["agent"]
abe wrapper bins: ["claude","codex","pi"]
```

- [ ] **Step 7: Verify the whole config still composes (forces full evaluation)**

Run:
```bash
nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath 2>/dev/null; echo " <- exit=$?"
```
Expected: a `/nix/store/…-nixos-system-globalhawk-….drv` path and `exit=0`. (This is evaluation only; it does not build the linux system.)

- [ ] **Step 8: Commit**

```bash
git add modules/nixos/ai-agent-sandbox.nix machine/globalhawk/default.nix machine/globalhawk/home.nix
git commit -m "$(cat <<'EOF'
feat: orchestrate the globalhawk AI-agent sandbox user

Provision the unprivileged `agent` user, the one-directional abe->agent sudo
rule, and both home-manager homes from a single shared ai-agents module, so
the operator gets sandbox wrappers + a suffixed escape hatch while the agent
gets the real harnesses. Cross-user provisioning must live at the NixOS layer
because a home-manager module cannot reach a sibling user's home.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 3: Read-only Docker (docker-socket-proxy + DOCKER_HOST + tools)

**Files:**
- Modify: `modules/nixos/ai-agent-sandbox.nix` (add the `docker.enable` config branch)
- Modify: `machine/globalhawk/default.nix` (set `services.aiAgentSandbox.docker.enable = true`)

**Interfaces:**
- Consumes: `services.aiAgentSandbox.docker.{listenAddress,port,image,settings}` (declared in Task 2).
- Produces: `virtualisation.oci-containers.containers.docker-proxy`; `DOCKER_HOST` + `docker`/`jq`/`ripgrep` in the sandbox user's home.

- [ ] **Step 1: Write the failing check (proxy not defined)**

Run:
```bash
nix eval --raw .#nixosConfigurations.globalhawk.config.virtualisation.oci-containers.containers.docker-proxy.image 2>/dev/null; echo "exit=$?"
```
Expected: no value, `exit=1` (`docker-proxy` container is not defined — `docker.enable` defaults to false).

- [ ] **Step 2: Add the docker branch to the orchestrator's `config` `mkMerge`**

In `modules/nixos/ai-agent-sandbox.nix`, the `config` is `mkIf cfg.enable (mkMerge [ { … } ])`. Add a second list element after the base `{ … }` block (i.e. between the base block's closing `}` and the `]);`):

```nix
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
      virtualisation.oci-containers.containers.docker-proxy = {
        image = cfg.docker.image;
        volumes = ["/var/run/docker.sock:/var/run/docker.sock:ro"];
        ports = ["${cfg.docker.listenAddress}:${toString cfg.docker.port}:2375"];
        environment = lib.mapAttrs (_: toString) cfg.docker.settings;
      };
    })
  ]);
```

- [ ] **Step 3: Turn docker on in `machine/globalhawk/default.nix`**

Change the block added in Task 2:
```nix
  # Unprivileged user for running AI agents read-only; see
  # docs/superpowers/specs/2026-07-22-agent-user-sandbox-design.md.
  services.aiAgentSandbox.enable = true;
```
to:
```nix
  # Unprivileged user for running AI agents read-only; see
  # docs/superpowers/specs/2026-07-22-agent-user-sandbox-design.md.
  services.aiAgentSandbox = {
    enable = true;
    docker.enable = true;
  };
```

- [ ] **Step 4: Format**

Run:
```bash
nix fmt 2>/dev/null
```
Expected: exits 0.

- [ ] **Step 5: Verify the proxy, env, and tools**

Run:
```bash
G=.#nixosConfigurations.globalhawk.config
echo -n "proxy image: "; nix eval --raw "$G.virtualisation.oci-containers.containers.docker-proxy.image" 2>/dev/null; echo
echo -n "proxy ports: "; nix eval --json "$G.virtualisation.oci-containers.containers.docker-proxy.ports" 2>/dev/null; echo
echo -n "proxy env POST/CONTAINERS: "; nix eval --json "$G.virtualisation.oci-containers.containers.docker-proxy.environment" --apply 'e: { POST = e.POST or "?"; CONTAINERS = e.CONTAINERS or "?"; }' 2>/dev/null; echo
echo -n "agent DOCKER_HOST: "; nix eval --raw "$G.home-manager.users.agent.home.sessionVariables.DOCKER_HOST" 2>/dev/null; echo
echo -n "agent has docker client: "; nix eval "$G.home-manager.users.agent.home.packages" --apply 'ps: builtins.any (p: (p.pname or "") == "docker-client") ps' 2>/dev/null; echo
echo -n "agent NOT in docker group: "; nix eval "$G.users.users.agent.extraGroups" --apply 'gs: !(builtins.elem "docker" gs)' 2>/dev/null; echo
```
Expected:
```
proxy image: ghcr.io/tecnativa/docker-socket-proxy:latest
proxy ports: ["127.0.0.1:2375:2375"]
proxy env POST/CONTAINERS: {"CONTAINERS":"1","POST":"0"}
agent DOCKER_HOST: tcp://127.0.0.1:2375
agent has docker client: true
agent NOT in docker group: true
```

- [ ] **Step 6: Verify the whole config still composes**

Run:
```bash
nix eval --raw .#nixosConfigurations.globalhawk.config.system.build.toplevel.drvPath 2>/dev/null; echo " <- exit=$?"
```
Expected: a `.drv` path and `exit=0`.

- [ ] **Step 7: Commit**

```bash
git add modules/nixos/ai-agent-sandbox.nix machine/globalhawk/default.nix
git commit -m "$(cat <<'EOF'
feat: give the sandbox user read-only Docker via socket-proxy

Debugging the container stack needs Docker visibility, but the docker group is
root-equivalent. Expose only whitelisted GET endpoints through
docker-socket-proxy on localhost (POST off => no run/exec/stop/rm) and point the
agent at it via DOCKER_HOST, so it can inspect containers and read logs without
ever touching the real socket or joining the docker group.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: Standalone audit script (`nix run .#audit-agent-access`)

**Files:**
- Create: `packages/audit-agent-access.sh` (the shellcheck-clean body)
- Create: `packages/audit-agent-access.nix` (the package)
- Modify: `flake.nix` (expose it in `perSystem.packages`)

**Interfaces:**
- Consumes: nothing from earlier tasks (it inspects the live, activated system at runtime).
- Produces: a flake package `audit-agent-access`. It is deliberately NOT referenced by `nix flake check`.

- [ ] **Step 1: Write the failing check (package does not exist)**

Run:
```bash
nix build .#audit-agent-access 2>&1 | tail -1; echo "exit=${PIPESTATUS[0]}"
```
Expected: an error that the flake does not provide `audit-agent-access`, non-zero exit.

- [ ] **Step 2: Create `packages/audit-agent-access.sh`**

Body only (no shebang — `writeShellApplication` adds the shebang and `set -euo pipefail`). Must be shellcheck-clean, since `writeShellApplication` runs shellcheck at build time:

```bash
# Audit that the AI-agent sandbox user is confined as designed. Read-only; run on
# globalhawk (needs sudo to inspect /data and drop to the agent). Reports findings
# and exits non-zero if any check fails. NOT wired into `nix flake check`.

user="${1:-agent}"
proxy="${2:-http://127.0.0.1:2375}"
fail=0

note() { printf '  %s\n' "$1"; }

echo "== group hygiene for '${user}' =="
if ! id "${user}" >/dev/null 2>&1; then
  note "user '${user}' does not exist here (not globalhawk?) — nothing to audit"
  exit 0
fi
mapfile -t groups < <(id -nG "${user}" | tr ' ' '\n')
for bad in wheel docker _media; do
  for g in "${groups[@]}"; do
    if [ "${g}" = "${bad}" ]; then
      note "FAIL: '${user}' is in forbidden group '${bad}'"
      fail=1
    fi
  done
done
if [ "${fail}" -eq 0 ]; then
  note "ok: groups = ${groups[*]}"
fi

echo "== /data has no world-writable paths =="
writable=$(find /data -xdev -perm -0002 2>/dev/null || true)
if [ -n "${writable}" ]; then
  note "FAIL: world-writable paths under /data (agent could write these):"
  printf '%s\n' "${writable}" | while IFS= read -r p; do note "    ${p}"; done
  fail=1
else
  note "ok: no world-writable paths under /data"
fi

echo "== real docker socket is not accessible to '${user}' =="
if sudo -u "${user}" test -r /var/run/docker.sock 2>/dev/null; then
  note "FAIL: '${user}' can read the real /var/run/docker.sock"
  fail=1
else
  note "ok: '${user}' cannot read the real docker socket"
fi

echo "== docker-socket-proxy is read-only =="
get_code=$(curl -s -o /dev/null -w '%{http_code}' "${proxy}/version" || echo 000)
post_code=$(curl -s -o /dev/null -w '%{http_code}' -X POST "${proxy}/containers/create" || echo 000)
if [ "${get_code}" = "200" ]; then
  note "ok: GET /version -> 200"
else
  note "FAIL: GET /version -> ${get_code} (proxy up?)"
  fail=1
fi
if [ "${post_code}" = "403" ]; then
  note "ok: POST /containers/create -> 403"
else
  note "FAIL: POST /containers/create -> ${post_code} (expected 403)"
  fail=1
fi

echo
if [ "${fail}" -eq 0 ]; then
  echo "PASS: sandbox user is confined as designed."
else
  echo "FAIL: one or more checks failed (see above)."
fi
exit "${fail}"
```

- [ ] **Step 3: Create `packages/audit-agent-access.nix`**

```nix
{
  writeShellApplication,
  coreutils,
  findutils,
  curl,
}:
writeShellApplication {
  name = "audit-agent-access";
  runtimeInputs = [coreutils findutils curl];
  # `sudo` is a setuid system wrapper (not a nix package) and is reached via the
  # inherited PATH; this script is meant to run on globalhawk as the operator.
  text = builtins.readFile ./audit-agent-access.sh;
}
```

- [ ] **Step 4: Expose it in `flake.nix`**

Find the `packages` attr in `perSystem` (it currently defines `decrypt-secrets`):

```nix
        packages =
          {
            decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
              ${pkgs.gnupg}/bin/gpg --decrypt ${./local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
            '';
          }
          // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
```
Add `audit-agent-access` alongside `decrypt-secrets`:
```nix
        packages =
          {
            decrypt-secrets = pkgs.writeShellScriptBin "decrypt-secrets" ''
              ${pkgs.gnupg}/bin/gpg --decrypt ${./local.key.asc} | ${pkgs.git-crypt}/bin/git-crypt unlock -
            '';
            # Read-only audit of the globalhawk AI-agent sandbox. Deliberately NOT
            # part of `nix flake check` — it must never block activation.
            audit-agent-access = pkgs.callPackage ./packages/audit-agent-access.nix {};
          }
          // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
```

- [ ] **Step 5: Format**

Run:
```bash
nix fmt 2>/dev/null
```
Expected: exits 0.

- [ ] **Step 6: Verify the package builds (shellcheck passes) and check-graph is clean**

Run:
```bash
nix build .#audit-agent-access 2>/dev/null && echo "built ok"
head -1 ./result/bin/audit-agent-access
```
Expected: `built ok`, and the first line is a shebang (`#!/nix/store/…-bash …` or `#!/bin/sh`). A shellcheck failure would have failed the build.

Confirm it is not a flake check:
```bash
nix flake show 2>/dev/null | grep -A3 'checks' || echo "no dedicated checks output (expected)"
```
Expected: `audit-agent-access` appears only under `packages`, never under `checks`.

- [ ] **Step 7: Commit**

```bash
git add packages/audit-agent-access.sh packages/audit-agent-access.nix flake.nix
git commit -m "$(cat <<'EOF'
feat: add audit-agent-access sandbox self-check

Model A leans on file ownership, so a future stray chmod could silently open a
write hole. Ship a standalone auditor (group hygiene, no world-writable /data,
real docker socket unreachable, proxy refuses POST) runnable on demand. Kept out
of `nix flake check` on purpose so a current misconfiguration can never block a
rebuild.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 5: Activate on globalhawk and verify runtime behavior (operator-run)

**Files:** none — this is operational verification. It MUST run on globalhawk (the linux host); it cannot be done from the darwin dev host, and activation is the operator's call.

**Interfaces:**
- Consumes: everything from Tasks 1–4, on branch `agent-user-sandbox`.

- [ ] **Step 1: Build the system on globalhawk (no activation)**

On globalhawk, from the repo root on branch `agent-user-sandbox`:
```bash
sudo nixos-rebuild build --flake .#globalhawk
```
Expected: builds to `./result` with no evaluation errors.

- [ ] **Step 2: Activate (operator decision)**

```bash
sudo nixos-rebuild switch --flake .#globalhawk
```
Expected: activation succeeds; the `agent` user, the `docker-proxy` container, and abe's wrapper/`-local` commands now exist.

- [ ] **Step 3: Verify group hygiene and read-only filesystem**

```bash
id agent                                   # groups: agent systemd-journal ONLY
sudo -u agent touch /data/Media/__probe     # expect: Permission denied
sudo -u agent cat /data/Media/<some-file>   # expect: succeeds (read works)
```
Expected: `id agent` shows no `wheel`/`docker`/`_media`; the `touch` fails with `Permission denied`; the read succeeds.

- [ ] **Step 4: Verify the wrappers and escape hatch**

```bash
which claude claude-local                   # claude = wrapper in abe's profile; claude-local = real
sudo -u agent id                            # confirms passwordless drop works
claude --version                            # runs as `agent` via the wrapper (check with: pgrep -u agent -a node/claude while it runs)
claude-local --version                      # runs as abe (escape hatch)
```
Expected: `claude` and `claude-local` both resolve; the drop to `agent` works without a password prompt.

- [ ] **Step 5: Verify read-only Docker + systemd debugging**

```bash
sudo -u agent env | grep DOCKER_HOST        # tcp://127.0.0.1:2375
sudo -u agent docker ps                     # works (read)
sudo -u agent docker rm -f prowlarr         # expect: 403 / permission denied from proxy
sudo -u agent journalctl -u docker-radarr -n 5   # works (systemd-journal group)
sudo -u agent systemctl restart docker-radarr    # expect: denied (polkit, not an admin)
```
Expected: `docker ps` and `journalctl` succeed; `docker rm` is refused (403) and `systemctl restart` is denied.

- [ ] **Step 6: Run the audit script**

```bash
nix run .#audit-agent-access
```
Expected: every line `ok:`, final line `PASS: sandbox user is confined as designed.`, exit 0. If it reports a world-writable path under `/data`, fix that path's mode (operator task) — it is a real finding, not a script bug.

- [ ] **Step 7: (No commit — verification only.)** If any check fails, stop and diagnose with superpowers:systematic-debugging before considering the feature done.

---

## Self-Review

**1. Spec coverage:**
- Part 1 (module `runAs`/`binSuffix`, wrapper, behavior matrix, `suffixPackage`) → Task 1. ✓
- Part 2 (why orchestrator) → captured in Task 2 rationale + commit. ✓
- Part 3 (orchestrator: user, sudoers, both homes, `systemd-journal`, mkDefault/merge extensibility, `sharedModules`) → Task 2. ✓
- Part 4 (docker-socket-proxy, `docker.settings`, DOCKER_HOST) → Task 3. ✓
- Part 5 (systemd/journal debugging; polkit-blocked control) → verified in Task 5 Step 5; `systemd-journal` granted in Task 2. ✓
- Part 6 (standalone audit script, not a flake-check gate) → Task 4. ✓
- Validation plan (eval + post-activation checks) → Task 5. ✓
- globalhawk wiring (enable orchestrator, drop `program/ai-agents` from home.nix) → Task 2 Steps 3–4. ✓

**2. Placeholder scan:** No TBD/TODO/"appropriate error handling"/"similar to Task N". Every code step shows full content; every check shows exact command + expected output. ✓

**3. Type/name consistency:** Option names (`runAs`, `binSuffix`, `services.aiAgentSandbox.{operator,user,binSuffix,sharedModules,docker.*}`), the `suffixPackage`/`runAsWrapper`/`enabledHarnesses`/`localSetup`/`emitWrappers` bindings, the container name `docker-proxy`, and the package `audit-agent-access` are used identically across tasks. Harness command names `claude`/`codex`/`pi` match confirmed `mainProgram`s. ✓

**Note for the executor on `sharedModules` typing:** it is `types.listOf types.deferredModule`. If evaluation in Task 2 Step 6/7 errors on the deferredModule coercion of the `../../program/ai-agents` path, fall back to `types.listOf types.raw` (imports accept raw modules) — the semantics are unchanged. Everything else in the plan is verified against the pinned inputs.
