# `programs.userscripts` home-manager module — design

Date: 2026-06-17

## Goal

A self-contained home-manager module (`program/userscripts/module.nix`) that takes a
shared set of userscripts — supplied either inline (`text`) or from a file (`source`) —
and writes them as real files into one or more **directory targets**. On macOS a target
for the [Userscripts Safari extension](https://github.com/quoid/userscripts) is
registered automatically; additional directory targets can be declared for any other
manager that loads scripts from a watched folder.

The module follows the self-contained `program/<name>/` convention (as in
`program/git/`): a `module.nix` holding the option definition and `config`, plus a
`default.nix` that `imports = [ ./module.nix ]` and provides a documented, enabled
usage. A machine activates it by adding `../../program/userscripts` to its home imports.

## Prior art / why a custom module

There is no existing home-manager module for userscripts (the `programs.script-directory`
module is for the unrelated `sd` shell-script launcher). A custom module is the right call.

The "write scripts into a directory" mechanism only works for managers that actually read
a directory:

- **Userscripts (Safari, quoid/userscripts)** — directory-based. Writing files into its
  selected scripts folder works. This is the macOS default target.
- **Tampermonkey (Firefox/Chrome)** — stores scripts in extension storage, *not* a folder.
  It cannot load or watch a local directory and cannot update via `file://`. The only
  programmatic bridge is a local HTTP server with `@require` / `@downloadURL` /
  `@updateURL` pointing at `http://localhost:PORT/…`.
- **Violentmonkey (Firefox)** — extension storage; sync via ZIP import/export or cloud.
  No directory watch.

Consequently Tampermonkey/Violentmonkey are **out of scope** for this module: they cannot
be driven by writing to a directory. The `targets` option is shaped so a future `http`
target kind (a localhost static-file service) could be added without breaking the
interface, but that is explicitly not built now (YAGNI).

## Why real copies instead of symlinks

home-manager normally materializes files as symlinks into `/nix/store`. The Userscripts
Safari extension is sandboxed and granted access only to the user-selected scripts
directory via a security-scoped bookmark; it may be unable to read a symlink whose target
lives outside that grant (in `/nix/store`). The module therefore copies real file contents
into each target directory during activation, rather than linking.

## Why not the app's sandbox container

The extension's *built-in* default directory lives inside its sandbox container
(`~/Library/Containers/com.userscripts.macos.Userscripts-Extension/Data/Documents/scripts`).
On macOS Sequoia that container tree is protected by the system: a normal process —
including `darwin-rebuild` activation, and even `sudo` — gets `Operation not permitted`
(EPERM) writing into it. Only an entitled app (Finder) or a process with Full Disk Access
can. Rather than require a broad, fragile Full Disk Access grant on whatever terminal runs
the rebuild, the darwin default points at an *unprotected* directory
(`~/Library/Application Support/Userscripts/scripts`); the user points the Userscripts app
at it once via its "change location" button — the extension's intended mechanism for an
external scripts location.

## Option surface (`programs.userscripts`)

```nix
programs.userscripts = {
  # Master switch. mkEnableOption; default false.
  enable = true;

  # Shared userscripts, written to every enabled target. The attribute name is the
  # destination filename verbatim — include the `.user.js` suffix yourself. Each entry
  # sets exactly one of `text` (inline) or `source` (a file).
  scripts = {                          # type: attrsOf (submodule { text; source; }); default {}
    "dark-mode.user.js".text = ''
      // ==UserScript==
      // ...
      // ==/UserScript==
    '';
    "from-file.user.js".source = ./scripts/from-file.user.js;
  };

  # Named directory targets. Each writes the shared `scripts` set into `directory`.
  # Shape is intentionally extensible (room for a future `kind = "http"`).
  targets = {                          # type: attrsOf (submodule { enable; directory; }); default {}
    # Auto-registered on darwin (shown for reference); override directory or disable:
    #   userscripts-safari.enable = false;
    userscripts-safari.directory =
      "${config.home.homeDirectory}/Library/Application Support/Userscripts/scripts";

    firefox-sync.directory = "${config.home.homeDirectory}/userscripts";
  };
};
```

### Option details

- `scripts.<filename>.text` — type `nullOr lines`, default `null`. Inline script body.
- `scripts.<filename>.source` — type `nullOr path`, default `null`. A file whose contents
  are copied.
- `targets.<name>.enable` — type `bool`, default `true`.
- `targets.<name>.directory` — type `str`, required. Destination directory.

### Assertions

- For each script, exactly one of `text` / `source` is set (XOR).
- Each script filename contains no `/`.

## Behavior

### darwin default target

When `enable` is true and the platform is darwin, `config` defines
`programs.userscripts.targets.userscripts-safari.directory` with `mkDefault` set to:

```
${config.home.homeDirectory}/Library/Application Support/Userscripts/scripts
```

(An unprotected location — see "Why not the app's sandbox container" above.)

The username is parameterized via `home.homeDirectory`. The directory is overridable, and
the whole target is disable-able via `targets.userscripts-safari.enable = false`. On
non-darwin platforms no target is registered by default.

### Materialization

1. The shared `scripts` set is assembled into a single `pkgs.linkFarm` (`name ->
   store path`, where the store path is the `source` file or `pkgs.writeText` of `text`).
2. `home.activation.userscripts` (`lib.hm.dag.entryAfter ["writeBoundary"]`, every real
   operation behind `$DRY_RUN_CMD`) iterates the **enabled** targets. For each:
   1. `mkdir -p` the target directory.
   2. **Prune**: read the per-target manifest at
      `${config.xdg.stateHome}/nix-userscripts/<target>` (a newline-separated list of the
      filenames written last run) and `rm -f` any entry not in the current managed set.
      Scripts added manually (e.g. via the extension UI) are never in the manifest, so
      they are left alone.
   3. **Install**: `cp -fL` each managed script from the link farm (dereferencing the
      store symlink to write a real file) into the target directory, then `chmod 644`.
   4. Rewrite the manifest with the current filenames.
3. All filenames are passed through `lib.escapeShellArg`, and managed-name membership is
   checked via a bash associative array, so spaces / special characters are safe.

Manifests live under `xdg.stateHome` rather than beside the scripts, keeping each target
directory free of bookkeeping files.

## Edge cases / notes

- First run: no manifest exists, so the prune step is skipped and scripts are installed.
- A manually-added script that shares a filename with a previously-managed (now removed)
  script will be pruned, since that name is still in the manifest. Filename collisions
  between managed and manual scripts are the user's responsibility.
- The user must point the Userscripts app at the target directory once, via its "change
  location" button. That one-time step is outside nix's scope. Writing into the app's
  default sandbox container instead is intentionally avoided (see above).
- Enabling the module with an empty `scripts` set is a no-op beyond creating each enabled
  target directory and an empty manifest.

## Files

- `program/userscripts/module.nix` — option definition + `config` (assertions, darwin
  default target, link farm, activation script).
- `program/userscripts/default.nix` — `imports = [ ./module.nix ]`; `enable = true`; empty
  `scripts`; inline-comment examples of both script forms and of adding/disabling targets.

`program/default.nix` (a stale aggregator not on the active machine import path) is left
untouched. Wiring into a specific machine is out of scope for this change.
