# Globalhawk Immich 3.0.3 Upgrade Design

## Goal

Upgrade Immich from 3.0.0 to the latest stable patch release, 3.0.3, so
account linking works with OIDC providers that do not include a `sid` claim.

## Compatibility

The 3.0.1 through 3.0.3 release notes identify bug fixes and small
enhancements, with no breaking configuration, storage-layout, or operator
migration requirements. Version 3.0.2 contains the upstream OAuth account
linking fix. Version 3.0.3 adds further fixes, including Live Photo background
upload handling.

## Change

Update the pinned Immich server and machine-learning images together to 3.0.3,
using immutable registry digests. Leave Postgres, Valkey, storage, networking,
and OIDC configuration unchanged.

## Verification

- Build the rendered nixidy environment and the complete Globalhawk system.
- After the operator switches, verify the Immich workloads complete their
  rollout and report healthy.
- Confirm the original local Immich account can link the Authelia identity.
- Confirm the original account retains its existing assets and local-password
  login.

## Rollback

The previous 3.0.0 image pins remain available in Git history. Rollback is a
revert and operator switch, subject to the usual caveat that an application
database migration may not be backward-compatible even when release notes do
not call out a migration.
