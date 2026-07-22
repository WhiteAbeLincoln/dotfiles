# Sandboxed AI-agent user on globalhawk — design

**Status:** design, awaiting review
**Date:** 2026-07-22

## Goal

Run AI coding harnesses (claude-code, codex, pi) on **globalhawk** as a dedicated
unprivileged Unix user (`agent`) that can **read broadly** — including `/data/Media`
and the disks — but can **write nothing that matters** and **control nothing**, so an
agent can investigate and debug the box without any risk of data loss or service
disruption. Any change that needs write access is applied by the operator (`abe`), not
the agent.

Two capabilities are added:

1. A reusable extension to the `programs.ai-agents` home-manager module: knobs to run
   the harnesses as another user (sandbox) and/or expose them under renamed binaries
   (escape hatch).
2. A NixOS orchestrator (`services.aiAgentSandbox`) that stands up the `agent` user and
   wires both the operator's and the agent's home-manager configs from a **single
   settings module**, so nothing has to be kept in sync by hand.

Non-goals: defending against `abe` (already root — the privilege drop is a *safety*
mechanism, not a boundary against the operator); giving the agent elevated *read* access
it wouldn't otherwise have (no extra ACLs/groups to widen reads); sandboxing on
nighthawk/campbell (untouched).

## Threat model / where the boundary is

`abe` is in `wheel` and already has full root. The `abe → agent` transition therefore
grants `abe` nothing new; it exists so the agent's *own* actions are confined. The real
boundary is drawn around **what `agent` can do**, enforced by three existing kernel/OS
mechanisms — no bespoke policy engine:

- **Unix DAC** — `agent` owns only its own home and is in no write-granting group, so it
  falls into the "other" class everywhere else.
- **polkit** — privileged systemd actions (start/stop/restart/edit units) default to
  `auth_admin`; `agent` is not a polkit admin (not `wheel`), has no password, and runs
  with no interactive auth agent, so those actions simply fail.
- **Group hygiene** — `agent` is deliberately excluded from `wheel`, `docker`, and
  `_media`; membership in those is what would grant escalation, socket-root, or media
  write respectively.

This is "Model A" (DAC + hardened login). We explicitly rejected a kernel-enforced
sandbox (bubblewrap / `systemd-run` read-only bind mounts): the marginal gain on this
box — protection against a *misowned world-writable path* under `/data` — is small and
auditable, while the cost (fragile interactive PTY + subprocess/`docker`-CLI plumbing
through a namespace) is high and would fight the read-only-docker goal. Instead, the
residual DAC gap is covered by a standalone audit script (below), not by a jail.

### What `agent` can and cannot do

- **Read:** everything the `other` class can already read — all of `/data`
  (Media/OldMedia), `/nix`, world-readable system state, the full systemd journal, and
  the read-only Docker API. (This can include secrets that services log or expose in
  container env — consistent with the accepted "read-only to everything" posture and no
  worse than the world-readable store paths that already carry those secrets.)
- **Write:** only `/home/agent` and the usual world-writable scratch (`/tmp`,
  `/var/tmp`, `/dev/shm`). Nothing under `/data`, no service state, no unit files.
- **Control:** nothing — no `sudo`, no Docker mutation (POST blocked at the proxy), no
  systemd unit start/stop/restart/edit (polkit-denied).

### `/data` is read-only by existing permissions

`/data/Media` is ZFS `pool/media`, mode `0775 _media:_media` with a default ACL
`group:_media:rwx` (`machine/globalhawk/disks.nix`). A process **not** in `_media` hits
the "other" bits → `r-x` → read + traverse, no create/modify/delete. So keeping
`agent ∉ _media` gives read-only media for free; no bind mount required.

## Part 1 — `programs.ai-agents` module extension (per-user primitives)

`modules/hm/ai-agents/module.nix` gains two options. Each affects **only the current
home-manager user** — an HM module evaluated under `home-manager.users.<name>` has no
write path to a sibling user (see "Why the orchestrator exists"), so provisioning the
*sandbox* user is not this layer's job.

```nix
runAs = mkOption {
  type = types.nullOr types.str;
  default = null;
  description = ''
    If set, this user does NOT get the harnesses configured in its own home.
    Instead, each enabled harness is exposed as a sudo-wrapper that runs the real
    harness as the named user. Provisions nothing for that target user — the target
    user's home is expected to be configured separately (e.g. by services.aiAgentSandbox).
  '';
};

binSuffix = mkOption {
  type = types.str;
  default = "";
  description = ''
    If non-empty, do the full local setup for this user (context, skills, harness
    config) and expose each enabled harness command under its suffixed name
    (e.g. binSuffix = "-local" → `claude-local`). The unsuffixed name is left free
    so a runAs wrapper can claim it.
  '';
};
```

### Behavior matrix (per enabled harness command `C`, e.g. `claude`)

| `runAs` | `binSuffix` | Effect for this user |
|---|---|---|
| `null` | `""`  | **Today's behavior**, unchanged. Full local setup; `C` on PATH via the upstream module. |
| `"u"`  | `""`  | No local harness config, no `~/.agents`/`~/.claude/skills`. Wrapper `C` → runs the harness as `u`. (This is the opt-out: wrappers only, provision nothing.) |
| `null` | `"S"` | Full local setup; command exposed as `C+S`; plain `C` not installed. |
| `"u"`  | `"S"` | Wrapper `C` → runs as `u`, **and** full local setup exposed as `C+S` (runs as this user). The globalhawk operator case. |

`runAs = null, binSuffix = ""` on every other machine means nighthawk/campbell are
untouched.

### Wrapper mechanism

For each enabled harness, when `runAs != null`, install a `pkgs.writeShellScriptBin`
named for the harness command (`claude-code → claude`, `codex → codex`, `pi → pi`) that
drops to the target user while preserving the interactive session and the caller's
working directory:

```sh
# claude (installed on the operator's PATH)
exec /run/wrappers/bin/sudo -u <runAs> \
  --preserve-env=TERM,COLORTERM \
  HOME=/home/<runAs> \
  PATH=/etc/profiles/per-user/<runAs>/bin:/run/current-system/sw/bin \
  -- bash -c '
      # pick up the target user'\''s HM session vars (DOCKER_HOST, etc.)
      f="$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"; [ -r "$f" ] && . "$f"
      cd "$1" || exit 1; shift
      exec claude "$@"
  ' _ "$PWD" "$@"
```

Design points:
- **Deterministic PATH.** With `home-manager.useUserPackages = true` (set in `flake.nix`),
  the target user's packages live at `/etc/profiles/per-user/<runAs>/bin`. The wrapper
  sets PATH explicitly rather than relying on login-shell sourcing, so behavior does not
  depend on the target user's login shell (fish vs bash).
- **`HOME` + cwd.** `HOME=/home/<runAs>` makes the harness read the sandbox user's
  `~/.agents` / `~/.claude`; the original `$PWD` (readable by `agent`) is preserved so the
  agent operates on the directory the operator launched from.
- **Session vars.** `DOCKER_HOST` and any other HM `sessionVariables` are sourced from the
  target user's `hm-session-vars.sh`.
- **TTY.** `sudo` preserves the controlling tty by default, so interactive harnesses work.

Exact env plumbing (which vars to `--preserve-env`, whether sourcing hm-session-vars is
needed given the explicit PATH) is verified during implementation; the semantics above
are fixed.

### `binSuffix` and the upstream modules

The module defers to upstream `programs.claude-code` / `programs.codex`, which install a
**plain-named** binary on PATH. To expose a *renamed* binary (`claude-local`) without
colliding with a `runAs` wrapper's plain `claude`, we hand the upstream module a package
whose **entrypoint is renamed**, and let it install + configure normally — config
discovery is by `$HOME`, independent of the binary name, so `~/.claude` / `~/.codex` are
still written:

```nix
# Expose <pkg>'s primary command <cmd> as <cmd><suffix>, preserving the rest of the
# package tree; update mainProgram so lib.getExe still resolves.
suffixPackage = suffix: cmd: pkg:
  if suffix == "" then pkg
  else (pkgs.symlinkJoin {
    name = "${pkg.pname or "harness"}${suffix}";
    paths = [ pkg ];
    postBuild = ''
      if [ -e "$out/bin/${cmd}" ]; then
        t=$(readlink -f "$out/bin/${cmd}"); rm "$out/bin/${cmd}"
        ln -s "$t" "$out/bin/${cmd}${suffix}"
      fi
    '';
  }) // { meta = (pkg.meta or {}) // { mainProgram = "${cmd}${suffix}"; }; };
# programs.claude-code.package = suffixPackage cfg.binSuffix "claude" cfg.claude-code.package;
```

Upstream then installs `claude-local`; there is no plain `claude` from upstream to collide
with the `runAs` wrapper. The same helper covers codex and pi (pi's renamed package just
goes into `home.packages`). Chosen over `package = null` + a hand-built
`writeShellScriptBin` because it reuses upstream's install/config path wholesale and
removes the "does upstream support config-without-binary?" uncertainty.

Residual (low risk) to confirm during the plan: whether the upstream module references the
binary by a **literal** `bin/<cmd>` path rather than `lib.getExe`. `meta.mainProgram`
covers `getExe`; if a module hardcodes the name, the fallback is `package = null` + own
`writeShellScriptBin`.

## Part 2 — Why the orchestrator exists (the HM constraint)

Home-manager, running as a NixOS module, evaluates each `home-manager.users.<name>` as an
**isolated module tree**. Inside `abe`'s evaluation, `config` is abe's home only. HM
injects the NixOS config as `osConfig`, but **read-only** — an HM module cannot *define*
`home-manager.users.agent`. So `programs.ai-agents.runAs` on its own can *never*
provision the sandbox user's home; that capability lives one layer up, at the NixOS layer
that owns `home-manager.users.*`. The orchestrator is that layer.

## Part 3 — `services.aiAgentSandbox` NixOS orchestrator

New NixOS module (reusable; only globalhawk uses it for now). It is the batteries-included
path: one `enable`, one settings source, both homes provisioned + all security wired.

```nix
services.aiAgentSandbox = {
  enable    = true;
  operator  = "abe";                     # trusted user; gets the sandbox wrappers (+ escape hatch)
  user      = "agent";                   # sandbox user; gets the full local setup
  binSuffix = "-local";                  # operator's escape-hatch suffix
  sharedModules = [ ../../program/ai-agents ];  # SINGLE source of the opinionated ai-agents config
  docker.enable = true;                  # stand up docker-socket-proxy + point agent at it
  # docker.settings = { CONTAINERS = 1; POST = 0; ... };  # tune the proxy's API surface (mkDefault)
};
```

What it sets:

1. **Sandbox user** — `users.users.${user}`:
   - `isNormalUser = true` (real `/home/${user}`, real shell), **locked password**
     (`hashedPassword = mkDefault "!"`), no SSH authorized keys → not independently
     loginable. Optionally hidden from the SDDM greeter (`mkDefault`).
   - `extraGroups = [ "systemd-journal" ]` (read-only journal access) and **nothing
     else** — explicitly not `wheel`, `docker`, or `_media`. This is a plain list, so
     further read-only groups merge additively from outside the module.
   - No sudoers entry *for* this user.
2. **sudoers `operator → user`**:
   ```nix
   security.sudo.extraRules = [{
     users = [ operator ];
     runAs = user;
     commands = [{ command = "ALL"; options = [ "NOPASSWD" "SETENV" ]; }];
   }];
   ```
   Permissive on purpose: `operator` is already root, so scoping the command list buys no
   security; `ALL` + `SETENV` lets the wrapper hand the agent a clean, deterministic
   environment without a password prompt. The rule matches only when the *caller* is
   `operator`, so `agent` cannot use it.
3. **Both homes from one source** — imports `cfg.sharedModules` into *both*
   `home-manager.users.${user}` and `home-manager.users.${operator}`:
   - `home-manager.users.${user}` = `{ imports = cfg.sharedModules; meta.user = user; }` →
     full local setup, `runAs` defaults to `null`. This writes `/home/${user}/.agents/…`
     and `/home/${user}/.claude/skills/…` — byte-identical to what the operator would get,
     because the AGENTS.md `@ctx/` rewrite keys off `config.home.homeDirectory` per user.
   - `home-manager.users.${operator}.programs.ai-agents = { runAs = user; inherit (cfg) binSuffix; }`
     merged onto the operator's existing ai-agents config (from the same `sharedModules`).
   - Also sets `home-manager.users.${user}.home.sessionVariables.DOCKER_HOST` and adds the
     `docker` client + a lean debugging toolset (git/jq/ripgrep) to the agent's packages
     when `docker.enable`. (`journalctl` is system-wide.)

   The imports also include `../../modules/common-hm` and `../../modules/hm` for the agent
   user, mirroring how `flake.nix` composes the operator's HM user. `useGlobalPkgs`,
   `useUserPackages`, and `extraSpecialArgs` are already global in `flake.nix`, so the
   agent user inherits them.
4. **Read-only Docker** (`docker.enable`) — a `docker-socket-proxy` container (Part 4).

Because `sharedModules` is a *parameter* (any HM module, defaulting to
`../../program/ai-agents`, even an inline module), the feature is not hardcoded to that
file, and there is exactly one definition of the opinionated config, imported into two
homes. Nothing to keep in sync.

### Extending the orchestrator (merge, not passthrough options)

Both the generated user and the generated home are ordinary options the module
contributes to, so extension is done by **Nix merge** from `machine/globalhawk/`, without
editing the module or re-exposing every setting:

```nix
users.users.agent.extraGroups = [ "video" ];            # additive list merge
home-manager.users.agent.programs.htop.enable = true;   # merges into the agent's HM eval
```

To keep this painless the module (a) sets every scalar it owns — `hashedPassword`, shell,
greeter visibility, docker-proxy port — with `mkDefault`, so a plain assignment wins
without `mkForce`, and (b) keeps group contributions as additive lists. Deliberately **no**
redundant `extraGroups`/`agentModules` passthrough options; they would only duplicate
mergeable behavior. The one exception is `docker.settings` (below), where merging into a
large env attrset is clumsy enough to justify a first-class knob.

### globalhawk wiring changes

- `machine/globalhawk/default.nix` — enable `services.aiAgentSandbox` as above (via the
  new module import).
- `machine/globalhawk/home.nix` (operator/abe) — **drop** the direct
  `../../program/ai-agents` import; the orchestrator now drives abe's ai-agents (imports
  the shared module + adds `runAs`/`binSuffix`). `role/dev.nix` import stays.
- `program/ai-agents` — unchanged; still the single settings module, still imported
  directly by nighthawk/campbell (whose `runAs`/`binSuffix` stay at their defaults).

## Part 4 — Read-only Docker via `docker-socket-proxy`

Add one `virtualisation.oci-containers.containers.docker-proxy` (Tecnativa
`docker-socket-proxy`), bind-mounting the real `/var/run/docker.sock`, listening on
`127.0.0.1:2375` (localhost only → no firewall change). The proxy's env comes from
`services.aiAgentSandbox.docker.settings`, whose defaults (set with `mkDefault`, so they
can be overridden per-key from outside) are the standard read set on, writes off:

```
CONTAINERS=1  IMAGES=1  NETWORKS=1  INFO=1  VERSION=1
POST=0  EXEC=0  VOLUMES=0  BUILD=0  COMMIT=0  ...   # everything mutating disabled
```

The agent reaches it only through `DOCKER_HOST=tcp://127.0.0.1:2375`; it is never in the
`docker` group and never sees the real socket. So `docker ps`, `docker logs`,
`docker inspect`, `docker stats` work; `run/exec/stop/rm/build` return 403 at the proxy.
`inspect` exposes container env (incl. secrets like `WIREGUARD_PRIVATE_KEY`) — kept on
because it is essential for debugging and consistent with the read-only-everything
posture.

## Part 5 — Debugging the `oci-containers` through systemd

Each `virtualisation.oci-containers.containers.<name>` becomes a `docker-<name>.service`
unit, run attached, so container stdout/stderr lands in the journal. The agent gets two
complementary, read-only debug paths:

- **Free (no grant):** `systemctl status/show/list-units/cat/is-active/is-failed` — all
  read-only systemd introspection is open to any user over the system D-Bus.
- **Via `systemd-journal` group (read-only):** `journalctl -u docker-radarr.service`,
  `journalctl -u plex`, kernel/ZFS logs, and the log tail in `systemctl status`.

Blocked and **structural**, not convention: `systemctl start/stop/restart/reload`,
`systemctl edit`, `daemon-reload` are polkit `manage-units` actions defaulting to
`auth_admin`; `agent` is not an admin and cannot authenticate → denied. Unit files are
read-only symlinks into `/nix/store`; editing the real definition means editing this repo
(operator territory).

## Part 6 — Standalone audit script (not a `flake check` gate)

A flake package, e.g. `nix run .#audit-agent-access`. **Never** wired into
`nix flake check` — a current misconfiguration must not block `nixos-rebuild`. It reports
findings and exits non-zero if any are present (so it is scriptable), checking:

- `agent`'s group list contains none of `wheel` / `docker` / `_media`;
- no group- or other-writable paths under `/data` that `agent` could modify (catches a
  future stray `chmod`);
- `agent` cannot `stat`/open the real `/var/run/docker.sock`;
- the proxy answers read verbs and refuses a `POST` (e.g. container create) with 403.

Lives under `packages/` and is exposed in `flake.nix`'s `perSystem.packages` next to
`decrypt-secrets`.

## Files touched

| File | Change |
|---|---|
| `modules/hm/ai-agents/module.nix` | add `runAs` + `binSuffix` options and their behavior (`suffixPackage` helper for renamed binaries, sudo-wrappers, local-setup suppression). |
| `modules/nixos/ai-agent-sandbox.nix` *(new)* | `services.aiAgentSandbox` orchestrator: user, sudoers, journal group, both HM homes from `sharedModules`, docker-socket-proxy, `DOCKER_HOST` + toolset. |
| `modules/nixos/default.nix` *(new, if absent)* | aggregate import so the module is available to NixOS hosts. |
| `machine/globalhawk/default.nix` | import the new NixOS module dir; set `services.aiAgentSandbox`. |
| `machine/globalhawk/home.nix` | drop the direct `program/ai-agents` import (orchestrator drives it). |
| `packages/…` + `flake.nix` | `audit-agent-access` script package. |

`program/ai-agents/` and nighthawk/campbell configs are unchanged.

## Validation plan

The unit of work is a Nix evaluation plus runtime behavior.

- **Eval:** `nixos-rebuild build --flake .#globalhawk` (build, not switch — no activation)
  proves the config composes. `nix flake check` still passes (audit script is not wired
  in).
- **Post-activation manual checks** (documented; run by the operator after a `switch`):
  - `sudo -u agent id` → groups are exactly `agent` + `systemd-journal`.
  - `sudo -u agent claude` (via the wrapper) launches with the agent's config and cwd.
  - `claude-local` launches as `abe` with abe's config (escape hatch).
  - `sudo -u agent touch /data/Media/x` → `EACCES`.
  - `sudo -u agent docker ps` works; `sudo -u agent docker rm <id>` → 403.
  - `sudo -u agent systemctl restart docker-radarr` → denied; `journalctl -u docker-radarr`
    works.
  - `nix run .#audit-agent-access` → clean report.

## Decisions log

- **Enforcement = Model A** (DAC + polkit + group hygiene), not a kernel sandbox.
  Reliability of an agent that shells out heavily outweighs a marginal, auditable DAC gap;
  the gap is covered by the audit script.
- **Provisioning lives in a NixOS orchestrator, not in `runAs`.** HM's per-user isolated
  evaluation makes sibling-user provisioning impossible from an HM module.
- **Single source of truth = one HM module imported into both homes** (`sharedModules`),
  because the opinionated setup is genuinely a module (it adds `home.packages` via
  `jira.nix`), not a plain value.
- **`runAs` alone = opt-out** (wrappers only, provision nothing); the orchestrator is the
  batteries-included wrapper around it.
- **sudoers is permissive** (`operator → agent: ALL NOPASSWD SETENV`) because the operator
  is already root; the boundary is around `agent`, not the transition.
- **Read-only Docker via `docker-socket-proxy`**, not sudo-whitelisted `docker`
  subcommands (argv matching is a poor boundary) and not deferral (would gut the debugging
  goal). `inspect` kept enabled despite env-secret exposure.
- **`systemd-journal` is the only extra group** — read-only; unit control stays
  polkit-denied.
- **`binSuffix` renames the harness's entrypoint in a wrapped package** (`suffixPackage`)
  fed to the upstream module, rather than `package = null` + a separate binary install.
  Reuses upstream's install/config path and drops the config-without-binary uncertainty.
- **Extension is by Nix merge, not passthrough options** — the module uses `mkDefault` on
  the scalars it owns and additive lists for groups, so users extend `users.users.<user>`
  / `home-manager.users.<user>` directly. Only `docker.settings` is a first-class knob.
- **Audit script is standalone**, never a `flake check` gate, so it can't block activation.

## Open items to confirm during planning

- Low-risk `binSuffix` residual: does the upstream `programs.claude-code` /
  `programs.codex` reference the binary by a **literal** `bin/<cmd>` path rather than
  `lib.getExe`? `meta.mainProgram` covers `getExe`; if a name is hardcoded, fall back to
  `package = null` + own `writeShellScriptBin`.
- Exact wrapper env set (`--preserve-env` list; necessity of sourcing `hm-session-vars.sh`
  given the explicit PATH).
- Module home: `modules/nixos/` (new tree) vs a machine-scoped `machine/globalhawk/`
  module. Spec assumes reusable `modules/nixos/`.
