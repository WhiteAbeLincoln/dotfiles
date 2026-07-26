# Audible Libation Reconciliation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **Operator-command revision:** Tasks 2–3's Python/Kubernetes operator tooling was
> superseded by
> `docs/superpowers/plans/2026-07-25-libation-operator-scripts-refactor.md`.
> The scheduled workload, storage, and rollout sections remain historical context.

**Goal:** Add a twice-monthly, jittered, headless Libation job that archives the operator's Audible library into the existing Audiobookshelf library, plus immediate reconciliation and CLI authentication commands.

**Architecture:** A nixidy-authored Kubernetes CronJob runs Libation in the `library` namespace with persistent hostPath state and a random-delay init container. Two Nix-packaged operator commands derive temporary Jobs from that live CronJob, ensuring manual reconciliation and account bootstrap use the exact deployed image, mounts, and security context rather than duplicating them.

**Tech Stack:** Nix flakes, nixidy, Kubernetes CronJob/Job, Libation Docker CLI, Python 3 standard library, `kubectl`, `jq`, NixOS tmpfiles, restic.

## Global Constraints

- Run scheduled reconciliation at `04:00` America/Denver on the 1st and 15th with a uniformly selected delay in `[0, 7200]` seconds.
- Set `concurrencyPolicy = "Forbid"`, `backoffLimit = 2`, and `activeDeadlineSeconds = 172800`.
- Run Libation as `_media` UID/GID `994`; all hostPath directories must already be `_media:_media`.
- Persist config, database, and incomplete work under `/data/Media/apps/libation`; write completed books under `/data/Media/audiobooks`.
- Disable podcast/episode import and download.
- Local audiobook storage is append-only: reconciliation never deletes completed files based on remote account state.
- Both workload images must use concrete release tags and immutable `sha256` digests; floating tags are prohibited.
- Authentication is CLI-only. Never commit Audible credentials, passwords, callback URLs, or saved tokens.
- Never remove existing human-written comments. Update comments affected by these changes.
- Production Rust/Go panic APIs are prohibited; this plan adds no Rust or Go.
- Tests assert rendered or command behavior, not the presence of source attributes or strings.
- Do not run `nixos-rebuild switch` or mutate the live cluster during implementation. Live deployment steps are explicitly marked **[OPERATOR]**.

---

## File map

- `k8s/apps/libation.nix` — owns the scheduled reconciliation CronJob and its four hostPath mounts.
- `k8s/default.nix` — imports the Libation workload.
- `packages/libation-ops.py` — derives safe manual and authentication Jobs from the live CronJob JSON.
- `packages/libation-reconcile.nix` — exposes `nix run .#libation-reconcile`.
- `packages/libation-auth.nix` — exposes `nix run .#libation-auth -- ACCOUNT LOCALE`.
- `packages/tests/test_libation_ops.py` — behavior tests for Job derivation and cleanup-sensitive metadata.
- `flake.nix` — exports both operator packages and a Python behavior check.
- `machine/globalhawk/disks.nix` — pre-creates Libation hostPath directories.
- `machine/globalhawk/backup.nix` — backs up Libation state.

### Task 1: Scheduled Libation workload

**Files:**

- Create: `k8s/apps/libation.nix`
- Modify: `k8s/default.nix`

**Interfaces:**

- Consumes: nixidy `_module.args` values `mediaRoot : string`, `mediaUid : int`, and `timezone : string`; `k8s/lib.nix` function `appLabels : string -> attrs`.
- Produces: `CronJob/library/libation-reconcile`, whose pod template is the sole source used by both operator commands.

- [ ] **Step 1: Verify the locked immutable images still resolve**

Use `skopeo` through Nix rather than installing it:

```bash
nix run nixpkgs#skopeo -- inspect docker://docker.io/rmcrackan/libation:13.5.1 |
  jq -r '"libation tag=\(.Labels["org.opencontainers.image.version"]) digest=\(.Digest)"'
nix run nixpkgs#skopeo -- inspect docker://docker.io/library/busybox:1.37.0 |
  jq -r '"busybox digest=\(.Digest)"'
```

Expected:

```text
libation tag=13.5.1 digest=sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0
busybox digest=sha256:9532d8c39891ca2ecde4d30d7710e01fb739c87a8b9299685c63704296b16028
```

If either immutable reference no longer resolves, stop; do not silently substitute a
new version.

- [ ] **Step 2: Write the failing rendered-manifest check**

Before importing the new module, render the current environment and confirm the
CronJob does not exist:

```bash
OUT="$(nix build --no-link --print-out-paths \
  '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')"
find "$OUT" -type f -print0 |
  xargs -0 grep -l 'name: libation-reconcile'
```

Expected: no output and exit status `123` from `xargs`/`grep`, demonstrating the
behavior is absent.

- [ ] **Step 3: Implement the CronJob**

Create `k8s/apps/libation.nix` with this structure:

```nix
{
  lib,
  mediaRoot,
  mediaUid,
  timezone,
  ...
}: let
  l = import ../lib.nix {inherit lib;};
  labels = l.appLabels "libation-reconcile";
in {
  applications.libation = {
    namespace = "library";
    createNamespace = false;
    resources = {
      configMaps.libation-settings.data."Settings.json" = builtins.toJSON {
        ImportEpisodes = false;
        DownloadEpisodes = false;
        AutoDownloadEpisodes = false;
      };
      cronJobs.libation-reconcile.spec = {
        schedule = "0 4 1,15 * *";
        timeZone = timezone;
        concurrencyPolicy = "Forbid";
        successfulJobsHistoryLimit = 2;
        failedJobsHistoryLimit = 3;
        jobTemplate.spec = {
          backoffLimit = 2;
          activeDeadlineSeconds = 172800;
          template = {
            metadata.labels = labels;
            spec = {
              restartPolicy = "Never";
              securityContext = {
                runAsUser = mediaUid;
                runAsGroup = mediaUid;
                fsGroup = mediaUid;
              };
              initContainers.jitter = {
                image = "busybox:1.37.0@sha256:9532d8c39891ca2ecde4d30d7710e01fb739c87a8b9299685c63704296b16028";
                command = [
                  "/bin/sh"
                  "-eu"
                  "-c"
                  ''
                    value="$(od -An -N4 -tu4 /dev/urandom)"
                    case "$value" in
                      *[!0-9]*|'') exit 1 ;;
                    esac
                    delay=$((value % 7201))
                    test "$delay" -ge 0
                    test "$delay" -le 7200
                    echo "scheduled jitter: ''${delay}s"
                    sleep "$delay"
                  ''
                ];
              };
              containers.libation = {
                image = "rmcrackan/libation:13.5.1@sha256:71b9db4bbda7d7e14bb9f5efcdcfe980915c90867599bc0d512d958069fb3da0";
                env = [
                  {name = "TZ"; value = timezone;}
                  {name = "SLEEP_TIME"; value = "-1";}
                  {name = "LIBATION_BOOKS_DIR"; value = "/data";}
                  {name = "LIBATION_CONFIG_DIR"; value = "/config";}
                  {name = "LIBATION_DB_DIR"; value = "/db";}
                ];
                volumeMounts = [
                  {name = "books"; mountPath = "/data";}
                  {name = "config"; mountPath = "/config";}
                  {name = "db"; mountPath = "/db";}
                  # Libation 13.5.1's container forces Settings.InProgress to /tmp.
                  {name = "in-progress"; mountPath = "/tmp";}
                  {name = "settings"; mountPath = "/config/Settings.json"; subPath = "Settings.json";}
                ];
              };
              volumes = [
                {name = "books"; hostPath = {path = "${mediaRoot}/audiobooks"; type = "Directory";};}
                {name = "config"; hostPath = {path = "${mediaRoot}/apps/libation/config"; type = "Directory";};}
                {name = "db"; hostPath = {path = "${mediaRoot}/apps/libation/db"; type = "Directory";};}
                {name = "in-progress"; hostPath = {path = "${mediaRoot}/apps/libation/in-progress"; type = "Directory";};}
                {name = "settings"; configMap.name = "libation-settings";}
              ];
            };
          };
        };
      };
    };
  };
}
```

Add `./apps/libation.nix` immediately after `./apps/audiobookshelf.nix` in
`k8s/default.nix`.

- [ ] **Step 4: Verify rendered behavior**

```bash
nix fmt
OUT="$(nix build --no-link --print-out-paths \
  '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage')"
MANIFEST="$(find "$OUT" -type f -print0 |
  xargs -0 grep -l 'name: libation-reconcile' | head -1)"
yq 'select(.kind == "CronJob") | {
  schedule: .spec.schedule,
  timeZone: .spec.timeZone,
  concurrency: .spec.concurrencyPolicy,
  backoff: .spec.jobTemplate.spec.backoffLimit,
  deadline: .spec.jobTemplate.spec.activeDeadlineSeconds,
  security: .spec.jobTemplate.spec.template.spec.securityContext,
  images: ([.spec.jobTemplate.spec.template.spec.initContainers[].image] +
           [.spec.jobTemplate.spec.template.spec.containers[].image]),
  paths: [.spec.jobTemplate.spec.template.spec.volumes[].hostPath.path]
}' "$MANIFEST"
```

Expected: schedule `0 4 1,15 * *`, timezone `America/Denver`, `Forbid`, backoff `2`,
deadline `172800`, UID/GID/fsGroup `994`, two digest-pinned images, and exactly the four
specified host paths. Inspect the rendered `Settings.json` or equivalent configuration
and confirm all three episode settings are false and that the `in-progress` hostPath
is mounted at `/tmp`, matching Libation 13.5.1's container entrypoint.

- [ ] **Step 5: Commit the workload**

```bash
git add k8s/apps/libation.nix k8s/default.nix
git commit -m "feat(globalhawk): archive Audible purchases without continuous polling"
```

### Task 2: Safe manual and authentication Job derivation

**Files:**

- Create: `packages/libation-ops.py`
- Create: `packages/tests/test_libation_ops.py`

**Interfaces:**

- Consumes: JSON emitted by `kubectl create job --from=cronjob/libation-reconcile`.
- Produces:
  - `manual_job(source: dict, name: str) -> dict`, with no jitter init container.
  - `auth_job(source: dict, name: str) -> dict`, with no jitter and a sleeping Libation container.
  - CLI subcommands `reconcile` and `auth ACCOUNT LOCALE`.

- [ ] **Step 1: Write failing behavior tests**

Create `packages/tests/test_libation_ops.py` using `unittest`. Define a fixture with
one jitter init container, one Libation container, volumes, generated labels, and
owner references. Tests must assert:

```python
def test_manual_job_preserves_workload_but_removes_jitter(self):
    result = libation_ops.manual_job(self.source, "manual-123")
    self.assertEqual(result["metadata"]["name"], "manual-123")
    self.assertNotIn("initContainers", result["spec"]["template"]["spec"])
    self.assertEqual(
        result["spec"]["template"]["spec"]["containers"],
        self.source["spec"]["template"]["spec"]["containers"],
    )
    self.assertEqual(
        result["spec"]["template"]["spec"]["volumes"],
        self.source["spec"]["template"]["spec"]["volumes"],
    )
    self.assertNotIn("ownerReferences", result["metadata"])

def test_auth_job_sleeps_without_running_libation_entrypoint(self):
    result = libation_ops.auth_job(self.source, "auth-123")
    container = result["spec"]["template"]["spec"]["containers"][0]
    self.assertEqual(container["command"], ["/bin/sh", "-eu", "-c"])
    self.assertEqual(container["args"], ["sleep 86400"])
    self.assertEqual(container["name"], "libation")
    self.assertNotIn("initContainers", result["spec"]["template"]["spec"])

def test_job_derivation_rejects_an_unexpected_container(self):
    self.source["spec"]["template"]["spec"]["containers"][0]["name"] = "other"
    with self.assertRaisesRegex(ValueError, "libation container"):
        libation_ops.auth_job(self.source, "auth-123")
```

Load `packages/libation-ops.py` with `importlib.util.spec_from_file_location` because
the filename contains a hyphen.

- [ ] **Step 2: Run the tests and verify failure**

```bash
python3 -m unittest -v packages/tests/test_libation_ops.py
```

Expected: FAIL because `packages/libation-ops.py` does not exist.

- [ ] **Step 3: Implement pure Job transformation**

In `packages/libation-ops.py`, use `copy.deepcopy`. Both transformation functions must:

- validate `source.kind == "Job"`;
- validate exactly one container named `libation`;
- replace `metadata` with `{"name": name, "namespace": "library"}` so generated
  owner references and CronJob labels do not survive;
- remove `spec.template.spec.initContainers`;
- set `spec.template.metadata.labels["app.kubernetes.io/name"]` to the temporary
  Job name.

`manual_job` otherwise preserves the source pod template. `auth_job` additionally sets:

```python
container["command"] = ["/bin/sh", "-eu", "-c"]
container["args"] = ["sleep 86400"]
```

Raise `ValueError` with a precise message on an unexpected source shape; do not
silently choose the first container.

- [ ] **Step 4: Run tests and verify success**

```bash
python3 -m unittest -v packages/tests/test_libation_ops.py
```

Expected: all three tests PASS.

- [ ] **Step 5: Commit the transformation boundary**

```bash
git add packages/libation-ops.py packages/tests/test_libation_ops.py
git commit -m "feat: keep one deployed source of truth for Libation jobs"
```

### Task 3: Operator-facing Nix commands

**Files:**

- Modify: `packages/libation-ops.py`
- Create: `packages/libation-reconcile.nix`
- Create: `packages/libation-auth.nix`
- Modify: `packages/tests/test_libation_ops.py`
- Modify: `flake.nix`

**Interfaces:**

- Consumes: pure transformation functions from Task 2; executables `kubectl`.
- Produces: flake packages/apps runnable as `nix run .#libation-reconcile` and
  `nix run .#libation-auth -- ACCOUNT LOCALE`.

- [ ] **Step 1: Add failing CLI tests with a fake command runner**

Refactor the planned CLI entry functions to accept
`run_command: Callable[..., subprocess.CompletedProcess]`. Add tests that record calls
and return fixture JSON for:

```text
kubectl create job --from=cronjob/libation-reconcile SOURCE_NAME
  --namespace library --dry-run=client --output=json
kubectl apply --filename=-
```

Assert reconciliation applies JSON from `manual_job` and prints both:

```text
kubectl logs --namespace library --follow job/MANUAL_NAME
kubectl wait --namespace library --for=condition=complete --timeout=48h job/MANUAL_NAME
```

For authentication, assert the CLI:

1. applies `auth_job`;
2. waits for its pod to become Ready;
3. discovers the pod with label `job-name=AUTH_NAME`;
4. executes `/libation/LibationCli login-external --libationFiles /config --account ACCOUNT --locale LOCALE`;
5. executes `/libation/LibationCli list-accounts --libationFiles /config`;
6. deletes only `job/AUTH_NAME` in a `finally` block.

Add a failure-path test in which `login-external` returns nonzero and assert cleanup
still runs.

- [ ] **Step 2: Verify the new tests fail**

```bash
python3 -m unittest -v packages/tests/test_libation_ops.py
```

Expected: FAIL because the CLI orchestration functions are absent.

- [ ] **Step 3: Implement CLI orchestration**

Use `argparse`, `json`, `secrets.token_hex(4)`, `subprocess.run(check=True)`, and
`datetime.datetime.now(datetime.UTC)`. Names must match:

```python
def unique_name(prefix: str) -> str:
    timestamp = datetime.datetime.now(datetime.UTC).strftime("%Y%m%d%H%M%S")
    return f"{prefix}-{timestamp}-{secrets.token_hex(4)}"
```

Pass transformed JSON to `kubectl apply --filename=-` through standard input. For
interactive login, call `kubectl exec --stdin --tty`; do not capture its stdout.
Reject missing/extra CLI arguments through `argparse`. Catch
`subprocess.CalledProcessError` only to add context, then return its nonzero status.
Never print account tokens or callback URLs.

- [ ] **Step 4: Add Nix wrappers and flake outputs**

Both Nix files use `writeShellApplication` with runtime inputs `[python3 kubectl]`.
`packages/libation-reconcile.nix` executes:

```nix
text = ''
  exec python3 ${./libation-ops.py} reconcile "$@"
'';
```

`packages/libation-auth.nix` executes:

```nix
text = ''
  exec python3 ${./libation-ops.py} auth "$@"
'';
```

Add both packages to the existing `perSystem.packages` attrset in `flake.nix`.
Also add:

```nix
checks.libation-ops = pkgs.runCommand "libation-ops-tests" {
  nativeBuildInputs = [pkgs.python3];
} ''
  python3 -m unittest -v ${./packages/tests/test_libation_ops.py}
  touch "$out"
'';
```

If the test imports the production script by a relative path, pass its immutable store
path through an environment variable in the check instead of assuming the repository
working directory.

- [ ] **Step 5: Verify tests, builds, and help output**

```bash
python3 -m unittest -v packages/tests/test_libation_ops.py
nix build --no-link .#checks.x86_64-linux.libation-ops
nix build --no-link .#libation-reconcile .#libation-auth
nix run .#libation-reconcile -- --help
nix run .#libation-auth -- --help
```

Expected: tests and builds pass; reconcile help accepts no account options; auth help
requires positional `account` and `locale`. Do not run either command without
`--help`, because that would mutate the live cluster.

- [ ] **Step 6: Commit the operator commands**

```bash
git add flake.nix packages/libation-ops.py packages/libation-reconcile.nix \
  packages/libation-auth.nix packages/tests/test_libation_ops.py
git commit -m "feat: make Libation recovery and reconciliation repeatable"
```

### Task 4: Persistent directories and backup coverage

**Files:**

- Modify: `machine/globalhawk/disks.nix`
- Modify: `machine/globalhawk/backup.nix`

**Interfaces:**

- Consumes: `${facts.mediaRoot}` and the workload's exact hostPath suffixes.
- Produces: writable `_media` directories and restic coverage for all mutable Libation state.

- [ ] **Step 1: Demonstrate missing evaluated behavior**

```bash
nix eval --json \
  '.#nixosConfigurations.globalhawk.config.systemd.tmpfiles.rules' |
  jq -e 'map(select(contains("/apps/libation"))) | length == 4'
nix eval --json \
  '.#nixosConfigurations.globalhawk.config.services.restic.backups.media.paths' |
  jq -e 'index("/data/Media/apps/libation") != null'
```

Expected: both commands exit nonzero before the changes.

- [ ] **Step 2: Add exact tmpfiles rules**

Under the existing audiobook application-state comment in
`machine/globalhawk/disks.nix`, add:

```nix
"d ${facts.mediaRoot}/apps/libation 0775 _media _media -"
"d ${facts.mediaRoot}/apps/libation/config 0770 _media _media -"
"d ${facts.mediaRoot}/apps/libation/db 0770 _media _media -"
"d ${facts.mediaRoot}/apps/libation/in-progress 0770 _media _media -"
```

The narrower child permissions protect stored Audible tokens. Do not recursively
change ownership or permissions of existing audiobooks.

- [ ] **Step 3: Add restic state coverage**

Append `"${facts.mediaRoot}/apps/libation"` to the media backup paths beside the
Audiobookshelf application-state path. Update the nearby comment to mention Libation's
database and refreshable account material. Do not add the audiobook library again; it
is already covered.

- [ ] **Step 4: Verify evaluated behavior**

```bash
nix fmt
nix eval --json \
  '.#nixosConfigurations.globalhawk.config.systemd.tmpfiles.rules' |
  jq -e 'map(select(contains("/apps/libation"))) | length == 4'
nix eval --json \
  '.#nixosConfigurations.globalhawk.config.services.restic.backups.media.paths' |
  jq -e 'index("/data/Media/apps/libation") != null'
nix build --no-link '.#nixosConfigurations.globalhawk.config.system.build.toplevel'
```

Expected: both `jq` checks return `true`, and the NixOS system builds.

- [ ] **Step 5: Commit storage lifecycle**

```bash
git add machine/globalhawk/disks.nix machine/globalhawk/backup.nix
git commit -m "feat(globalhawk): preserve Libation state across jobs and failures"
```

### Task 5: Whole-system verification and operator rollout

**Files:**

- Modify if findings require corrections: files from Tasks 1–4

**Interfaces:**

- Consumes: all preceding deliverables.
- Produces: verified desired state and a live, authenticated reconciliation path.

- [ ] **Step 1: Run repository-wide static verification**

```bash
nix fmt
git diff --check
python3 -m unittest -v packages/tests/test_libation_ops.py
nix flake check
nix build --no-link \
  '.#nixidyEnvs.x86_64-linux.globalhawk.environmentPackage'
nix build --no-link \
  '.#nixosConfigurations.globalhawk.config.system.build.toplevel'
```

Expected: every command succeeds.

- [ ] **Step 2: Review the final diff for safety boundaries**

```bash
git diff master...HEAD -- \
  k8s/apps/libation.nix k8s/default.nix \
  packages/libation-ops.py packages/libation-reconcile.nix \
  packages/libation-auth.nix packages/tests/test_libation_ops.py \
  flake.nix machine/globalhawk/disks.nix machine/globalhawk/backup.nix
```

Confirm no secret values, floating image tags, delete operations against audiobook
paths, public ingress, or unbounded retry loops are present.

- [ ] **Step 3: Commit any verification corrections**

If verification required code corrections, rerun Step 1 and stage only the corrected
paths from the File map before committing:

```bash
git commit -m "fix(globalhawk): keep Audible reconciliation bounded and recoverable"
```

If no corrections were needed, do not create an empty commit.

- [ ] **Step 4: [OPERATOR] Activate desired state**

On globalhawk:

```bash
cd /srv/dotfiles
sudo nixos-rebuild switch --flake .#globalhawk
sudo kubectl get cronjob -n library libation-reconcile
sudo kubectl get cronjob -n library libation-reconcile \
  -o jsonpath='{.spec.schedule}{" "}{.spec.timeZone}{"\n"}'
```

Expected: the CronJob exists and prints `0 4 1,15 * * America/Denver`.

- [ ] **Step 5: [OPERATOR] Bootstrap Audible authentication**

From the repository checkout with cluster credentials:

```bash
AUDIBLE_LOGIN='your-login@example.com'
nix run .#libation-auth -- "$AUDIBLE_LOGIN" us
```

Open the printed Amazon URL, complete login/CAPTCHA/2FA, and paste the resulting
callback URL into the terminal when Libation asks. This is account authentication,
not a configuration or book-selection GUI. Confirm `list-accounts` reports the account
authenticated before the temporary Job is removed.

- [ ] **Step 6: [OPERATOR] Run and observe immediate reconciliation**

```bash
nix run .#libation-reconcile
sudo kubectl get jobs -n library --sort-by=.metadata.creationTimestamp
MANUAL_JOB='use-the-job-name-printed-by-libation-reconcile'
sudo kubectl logs -n library --follow "job/$MANUAL_JOB"
```

Use the exact Job name printed by the helper. Expected: the Job completes, completed
M4B files appear beneath `/data/Media/audiobooks`, and no partial files appear there.

- [ ] **Step 7: [OPERATOR] Verify idempotence and Audiobookshelf discovery**

Run `nix run .#libation-reconcile` a second time. Expected: existing Audible IDs are
recognized and completed books are not duplicated or downloaded again. Trigger or
wait for Audiobookshelf's normal library scan, then confirm one imported title,
embedded chapters, cover art, and playback through Audiobookshelf.

- [ ] **Step 8: [OPERATOR] Verify failure preserves the archive**

Record a representative completed file's path and checksum. Temporarily invalidate
authentication by moving the Libation account settings file within the protected
config directory, trigger a manual run, then restore the file immediately. Confirm
the Job fails with finite retries and the recorded file still exists with the same
checksum. Re-run authentication only if restoring the file does not restore the
session.

- [ ] **Step 9: Final status**

```bash
git status --short
git log --oneline master..HEAD
```

Expected: clean working tree and the design plus implementation commits on
`feat/audible-libation-reconciliation`.
