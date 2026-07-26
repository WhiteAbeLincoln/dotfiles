# Audible → Audiobookshelf reconciliation with Libation

**Status:** Revised and approved (2026-07-25)
**Date:** 2026-07-25
**Scope:** Add a headless, scheduled Audible ingestion path to the existing
Audiobookshelf library on globalhawk.

## Goal

Automatically archive the operator's Audible library as DRM-free audiobooks under
`${mediaRoot}/audiobooks`, where the existing Audiobookshelf deployment can discover
them. Reconcile twice per month with bounded start-time jitter, and provide
scriptable commands for immediate reconciliation and initial account authentication.
No GUI may be required for account configuration or book selection.

The operator buys books with Audible Premium Plus credits and does not use Plus
Catalog titles. The reconciler may therefore process every downloadable audiobook in
the account library. Podcast episodes are excluded.

## Decisions

- Use **Libation**. Its CLI can authenticate an Audible account, scan the account
  library, and download/decrypt books to M4B without interactive book selection.
- Run reconciliation as a Kubernetes `CronJob` in the existing `library` namespace,
  rather than as an always-running pod or a host systemd timer. This follows the
  existing k3s application boundary, leaves no idle workload, and exposes native Job
  history and logs.
- Schedule the CronJob for 04:00 in the configured local timezone on the 1st and 15th
  of each month. Delay scheduled runs by a uniformly selected 0–2 hours before making
  Audible requests.
- Set `concurrencyPolicy = "Forbid"` so a slow initial import cannot overlap the next
  scheduled run.
- Provide a repository command that creates an immediate Job from the same workload
  definition while disabling the scheduled jitter. Manual reconciliation must begin
  without an artificial delay.
- Implement the two operator commands as short Bash programs packaged by Nix. Manual
  reconciliation uses `kubectl` plus `jq`; authentication uses the host's existing
  Docker daemon. Python orchestration is unnecessary for these transparent workflows.
- Treat the local archive as append-only. A scan may add or replace an explicitly
  re-requested book, but it must never delete a local audiobook merely because the
  title is absent or inaccessible in a later Audible response.
- Do not implement a purchased-versus-Plus filter. The account does not use Plus
  titles, and Libation's documented CLI does not provide a reliable headless filter
  for that distinction. If account usage changes, eligibility filtering must be
  designed before the reconciler is allowed to process Plus titles.

## Kubernetes workload

Add a Libation application definition alongside
`k8s/apps/audiobookshelf.nix`. Its primary resource is a CronJob:

- Namespace: `library`
- Nominal schedule: `0 4 1,15 * *`
- Timezone: the same IANA timezone supplied to the other homelab workloads
- Concurrency policy: `Forbid`
- Successful Job history: 2
- Failed Job history: 3
- Retry policy: a small finite backoff; authentication failures must not cause an
  unbounded retry loop
- Active deadline: 48 hours, covering the two-hour jitter and allowing a large initial
  library download to run overnight
- Security context: `runAsUser`, `runAsGroup`, and `fsGroup` set to `mediaUid` (994)
- Image: pin a concrete Libation container release and immutable digest when the
  implementation is authored; floating tags such as `latest` are prohibited

The scheduled pod has two phases:

1. A small pinned init container reads randomness from the kernel and sleeps for a
   value in `[0, 7200]` seconds.
2. The Libation container scans all configured accounts and liberates every
   not-yet-downloaded audiobook. Podcast and episode import/download are disabled.

The random-delay implementation must fail closed: if it cannot generate a valid
bounded delay, the Job fails before contacting Audible instead of sleeping for an
unbounded duration.

## Storage and file flow

Use hostPath storage already established for media applications:

- `${mediaRoot}/audiobooks` → Libation's books output directory
- `${mediaRoot}/apps/libation/config` → Libation configuration and account tokens
- `${mediaRoot}/apps/libation/db` → Libation's persistent library database
- `${mediaRoot}/apps/libation/in-progress` → downloads and decryptions in progress

All directories are pre-created by NixOS tmpfiles as `_media:_media`. Persistent
configuration, database, and in-progress state are included in globalhawk's restic
paths. The audiobook output is already backed up.

Temporary and final output live on the same host filesystem so Libation can finish a
book without exposing a cross-filesystem partial move. The output naming template
retains the Audible ID in the book directory or filename because Libation uses that
identifier to recognize an existing download. The preferred final form is one M4B per
book with embedded chapters and cover art; PDF supplements may be stored beside the
book.

Audiobookshelf continues mounting the same library directory. No API coupling is
introduced: completed files become visible through Audiobookshelf's normal library
scan. Libation does not write Audiobookshelf's config or metadata directories.

## Headless account bootstrap

Authentication is a one-time, operator-initiated, host-local CLI workflow run on
globalhawk:

1. `nix run .#libation-auth -- ACCOUNT LOCALE` invokes the same digest-pinned Libation
   image used by the CronJob through globalhawk's existing Docker daemon.
2. Docker runs the container interactively as UID/GID 994, mounting only
   `${mediaRoot}/apps/libation/config` at `/config`.
3. The container stages an internal Libation files directory from the persistent
   `Settings.json` and `AccountsSettings.json`, substituting an empty JSON object for
   either file when bootstrapping a new installation. This mirrors the initialization
   normally performed by the image entrypoint without starting a library scan.
4. The command runs Libation's external-login flow against the staged directory. It
   prints an Amazon/Audible URL;
   the operator opens it, completes Amazon authentication and any CAPTCHA/2FA, then
   supplies the resulting callback URL to the CLI.
5. After successful authentication, the command atomically copies only the resulting
   `AccountsSettings.json` to `/config`, then runs `list-accounts` against the staged
   directory to verify the persisted account.

The workflow is terminal-driven and does not expose a web GUI or require selecting
books. Audible passwords are never committed to Nix, Git, a Kubernetes manifest, or
sops. The persistent Libation state contains sensitive tokens, is readable only by
the media identity, and is backed up with the other application state. The container
uses `--rm`; its temporary database and other initialization state disappear on exit,
and no long-running Docker container, Kubernetes Job, or Pod is created.

This command intentionally runs only on globalhawk. The trusted operator is already in
the Docker group and the config path is host-local. A future dashboard running as that
same trusted user may invoke the packaged command, but Docker socket access remains
effectively root-equivalent and must not be exposed to an untrusted service account.

If Audible invalidates the saved session, scheduled runs fail visibly without
replacing the stored account state. The operator reruns the bootstrap command to
reauthenticate.

## Manual reconciliation

Provide `nix run .#libation-reconcile`, which:

- uses `kubectl create job --from=cronjob/libation-reconcile --dry-run=client
  --output=json` to derive a one-off Job from the live CronJob;
- pipes that JSON through `jq` to remove the jitter init container;
- applies the resulting Job through `kubectl apply --filename=-`;
- gives the Job a collision-resistant name;
- prints commands for following logs and inspecting final Job status; and
- does not alter the CronJob schedule or persistent state.

The helper is non-interactive after invocation. Repeated invocations are safe because
Libation's database and Audible IDs identify books already downloaded. The bootstrap
workflow is exposed separately as `nix run .#libation-auth`. JSON is used throughout;
`yq` is not required.

## Failure handling

- A scan or download error fails the Job and remains visible in Kubernetes Job status
  and container logs.
- Partial artifacts remain in the dedicated in-progress directory for Libation's next
  run; they are not placed in the final Audiobookshelf path as complete books.
- Finite Kubernetes backoff protects the Audible account from a tight authentication
  or API retry loop.
- A missing remote title never causes local deletion.
- A failed Audiobookshelf scan does not affect Libation state; the two systems
  communicate only through completed files.
- Initial bulk import may exceed ordinary run duration. The active deadline must cover
  that case, and manual reruns resume from persistent state rather than starting the
  entire library again.

## Verification

The long-lived workload retains behavioral tests and deployment checks:

- Rendered manifests contain the intended schedule, timezone, forbidden concurrency,
  finite retry/deadline settings, security context, and exact hostPath mounts.
- The jitter helper is tested at its lower and upper bounds and never emits a delay
  outside `[0, 7200]`.
- The pinned-image runtime contract proves refreshed account state is persisted only
  after a successful run and that failures preserve the last known-good state.

The two short operator scripts do not have fake-`kubectl` or fake-Docker unit tests.
Their behavior is reviewed directly from the Bash source, checked by the Nix package
build and shell syntax validation, and proven by the first real operator execution.
This avoids maintaining test doubles that restate straightforward command pipelines.

Live acceptance checks:

- In a live smoke test, authenticate the account, trigger a manual reconciliation,
  observe a completed M4B under `${mediaRoot}/audiobooks`, and confirm Audiobookshelf
  discovers it.
- Trigger reconciliation again and confirm it does not redownload or duplicate the
  completed title.
- Simulate a failed scan and confirm existing audiobook files remain untouched.

No test should merely assert that a Nix attribute or source string exists.

## Operational and legal boundary

Libation removes Audible DRM so Audiobookshelf can play the resulting files. Audible's
current terms prohibit bypassing DRM even for purchased content. The operator has
explicitly accepted this requirement for personal use. The system does not share,
serve publicly, sell, or otherwise distribute the resulting files; Audiobookshelf
remains LAN-private behind the homelab's existing access controls.

## Out of scope

- Audible Plus Catalog entitlement tracking or local deletion
- Playback-progress synchronization between Audible and Audiobookshelf
- A Libation web interface
- Automatic purchase or credit redemption
- Public ingress for Libation
- Changes to Audiobookshelf authentication or library layout
