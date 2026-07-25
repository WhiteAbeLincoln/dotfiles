# populate-sops

`populate-sops` populates fields in an existing sops file from a reusable TOML
schema. It preserves existing values by default and never prints secret values.

```sh
nix run .#populate-sops -- schema.toml secrets/example.sops.yaml
```

Schemas declare `version = 1` followed by `[[fields]]` tables. Supported modes:

- `generated`: create a random URL-safe value. Requires
  `generator = "token_urlsafe"` and `bytes`.
- `derived`: derive a value from another field. Currently supports
  `derivation = "authelia_argon2"` and requires `source`.
- `fixed`: add an empty key when absent, then open sops for the operator to fill
  it. An empty fixed value makes the command fail.

Every field may have `tags`; fixed fields may also have a `description`.

```toml
version = 1

[[fields]]
name = "example_client_secret"
mode = "generated"
generator = "token_urlsafe"
bytes = 48
tags = ["example", "oidc"]

[[fields]]
name = "example_client_secret_hash"
mode = "derived"
source = "example_client_secret"
derivation = "authelia_argon2"
tags = ["example", "oidc"]

[[fields]]
name = "example_api_key"
mode = "fixed"
description = "API key created in the example application's admin UI"
tags = ["example", "operator"]
```

Use repeatable `--field GLOB` and `--tag TAG` filters to populate a subset.
Selecting either side of a generated/derived pair automatically selects the
other side. `--force` rotates selected generated values and recomputes their
derived values. `--no-edit` is useful when selected fixed fields are already
populated; it still fails if one is missing or empty.
