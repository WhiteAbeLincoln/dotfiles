# Libation Operator Scripts Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace bespoke Python Kubernetes orchestration with two short Nix-packaged Bash commands: Kubernetes manual reconciliation and host-local Docker authentication.

**Architecture:** Scheduled reconciliation remains the existing Kubernetes CronJob. `libation-reconcile` derives a one-off Job from that live CronJob with `kubectl`, removes jitter with `jq`, and applies it; `libation-auth` invokes the exact pinned Libation image through globalhawk's existing Docker daemon against the persistent host config directory.

**Tech Stack:** Nix `writeShellApplication`, Bash, `kubectl`, `jq`, Docker.

## Global Constraints

- Do not change the scheduled CronJob, storage, persistence wrapper, jitter, naming templates, or backups.
- Use `rmcrackan/libation:13.5.1@sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0`.
- Authentication runs only on globalhawk as UID/GID 994 with `/data/Media/apps/libation/config:/config`.
- Manual reconciliation derives its container, mounts, settings, and image from `cronjob/libation-reconcile`; do not duplicate them.
- The scripts must support `--help`, reject unexpected arguments, propagate command failures, and never print credentials.
- Do not add fake-`kubectl`, fake-Docker, or source-shape unit tests. Verify package builds, help output, shell syntax, and the eventual real operator smoke run.
- Preserve existing comments unless they become inaccurate.

---

### Task 1: Replace Python orchestration with Bash commands

**Files:**

- Modify: `packages/libation-reconcile.nix`
- Modify: `packages/libation-auth.nix`
- Modify: `flake.nix`
- Delete: `packages/libation-ops.py`
- Delete: `packages/tests/test_libation_ops.py`

**Interfaces:**

- Consumes: live `CronJob/library/libation-reconcile`, host Docker daemon, and `/data/Media/apps/libation/config`.
- Produces: `nix run .#libation-reconcile` and `nix run .#libation-auth -- ACCOUNT LOCALE`.

- [ ] **Step 1: Replace manual reconciliation wrapper**

Use `writeShellApplication` with `runtimeInputs = [kubectl jq coreutils]`. Its Bash body:

```bash
usage() {
  echo "Usage: libation-reconcile"
}

case "''${1-}" in
  -h|--help) usage; exit 0 ;;
  "") ;;
  *) usage >&2; exit 2 ;;
esac

job_name="libation-reconcile-manual-$(date -u +%Y%m%d%H%M%S)-$RANDOM"

kubectl create job \
  --from=cronjob/libation-reconcile \
  "$job_name" \
  --namespace library \
  --dry-run=client \
  --output=json \
  | jq 'del(.spec.template.spec.initContainers)' \
  | kubectl apply --filename=-

echo "kubectl logs --namespace library --follow job/$job_name"
echo "kubectl wait --namespace library --for=condition=complete --timeout=48h job/$job_name"
```

`writeShellApplication` supplies Bash strict mode, so failure in any pipeline stage is
returned to the caller.

- [ ] **Step 2: Replace authentication wrapper**

Use `writeShellApplication` with `runtimeInputs = [docker-client]`. Parse exactly two
positional arguments or `--help`; reject all other shapes with exit 2. Use constants:

```bash
image='rmcrackan/libation:13.5.1@sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0'
config='/data/Media/apps/libation/config'
```

Run:

```bash
docker run --rm --interactive --tty \
  --user 994:994 \
  --volume "$config:/config" \
  --entrypoint /libation/LibationCli \
  "$image" \
  login-external \
  --libationFiles /config \
  --account "$account" \
  --locale "$locale"

docker run --rm \
  --user 994:994 \
  --volume "$config:/config" \
  --entrypoint /libation/LibationCli \
  "$image" \
  list-accounts \
  --libationFiles /config
```

The second invocation runs only after successful login because the generated Bash uses
strict error handling.

- [ ] **Step 3: Remove obsolete Python and check wiring**

Delete `packages/libation-ops.py` and `packages/tests/test_libation_ops.py`. Remove only
`checks.libation-ops` from `flake.nix`; retain both package outputs.

- [ ] **Step 4: Verify the commands without mutating Docker or Kubernetes**

```bash
nix fmt packages/libation-reconcile.nix packages/libation-auth.nix flake.nix
nix build --no-link .#libation-reconcile .#libation-auth
nix run .#libation-reconcile -- --help
nix run .#libation-auth -- --help
git diff --check
```

Expected: both packages build; help commands exit 0 without invoking external
runtimes; diff check passes.

- [ ] **Step 5: Verify the unchanged system**

```bash
nix flake check
nix build --no-link '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
nix build --no-link '.#nixosConfigurations.globalhawk.config.system.build.toplevel'
```

Expected: all three commands exit 0. Only pre-existing Nix warnings are acceptable.

- [ ] **Step 6: Commit**

```bash
git add flake.nix packages/libation-auth.nix packages/libation-reconcile.nix
git add -u packages/libation-ops.py packages/tests/test_libation_ops.py
git commit -m "refactor(libation): keep operator recovery transparent"
```

### Task 2: Operator smoke acceptance

**Files:** None unless the real commands reveal a defect.

**Interfaces:**

- Consumes: deployed CronJob and globalhawk host Docker/config state.
- Produces: authenticated persistent account and one successful manual reconciliation.

- [ ] **Step 1: [OPERATOR] Activate the branch on globalhawk**

```bash
sudo nixos-rebuild switch --flake .#globalhawk
```

- [ ] **Step 2: [OPERATOR] Authenticate**

```bash
nix run .#libation-auth -- 'audible-account@example.com' us
```

Complete the printed Amazon login flow. Expected: `list-accounts` reports the account
authenticated and `/data/Media/apps/libation/config/AccountsSettings.json` persists.

- [ ] **Step 3: [OPERATOR] Start manual reconciliation**

```bash
nix run .#libation-reconcile
```

Run the exact log and wait commands it prints. Expected: the Job has no jitter init
container, downloads unarchived titles, and completes.

- [ ] **Step 4: [OPERATOR] Confirm Audiobookshelf ingestion**

Confirm a completed M4B with Audible ID, chapters, and cover appears under
`/data/Media/audiobooks` and Audiobookshelf discovers it. Run manual reconciliation a
second time and confirm no duplicate is created.
