# populate-sops fixed-value prompts

## Goal

Replace the `populate-sops` editor handoff with direct terminal prompts for
selected fixed-mode fields. The existing `--field` and `--tag` filters define
the operator's intended population scope, so every selected fixed field is
prompted on every run, whether or not it already has a value.

## Behavior

After generated and derived fields are processed, `populate-sops` prompts for
each selected fixed field in schema order. Each prompt identifies the field by
its schema `name` and includes its `description` when present.

Secret input is read without terminal echo. Empty input is rejected and the
same field is prompted again. Once a non-empty value is entered, it is written
directly with `sops set`. Secret values are never printed.

Generated and derived field behavior remains unchanged. `--force` continues to
control only regeneration and re-derivation; fixed fields are already
explicitly selected through the command-line filtering system and therefore do
not depend on `--force`.

## Interface changes

The editor integration and `SecretStore.edit` operation are removed.
`--no-edit` is removed because there is no longer an editor step to suppress.
Operators who do not want to repopulate fixed values should exclude them with
the existing `--field` or `--tag` filters.

## Error handling

An empty response does not modify the sops file and causes an immediate retry
of that field. End-of-input or an interrupted prompt follows normal Python
command-line behavior and terminates without pretending the remaining fields
were populated. Values accepted before an interruption may already have been
written, consistent with the script's existing incremental update model.

## Validation

Behavioral unit tests will inject a prompt function and verify that:

- every selected fixed field is prompted in schema order;
- prompts contain both the schema name and description;
- existing values are replaced;
- empty responses retry without being written; and
- accepted fixed values are not printed.

The existing population tests will continue to cover generated and derived
values. The package's focused Python test suite and the repository formatter
will be run after implementation.
