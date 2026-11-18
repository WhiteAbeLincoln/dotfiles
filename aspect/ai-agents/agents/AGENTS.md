# Agent Instruction

## Commit Messages

Commit messages should document the "Why" of the change,
NOT a list of what was changed - git diff is sufficient for telling what.

## Code Comments

DO NOT remove existing comments unless directed to do so,
especially if they appear to be human written.
Do ensure that comments are up to date with the code.

## CLI Tools
- This environment uses **GNU sed** (via nix), not BSD sed. Use `sed -i` for in-place edits, NOT `sed -i ''` (that's BSD syntax and treats `''` as an input file).
- Prefer the Edit tool over sed for file modifications anyway — it's more reliable and auditable.

I use nix for developer environment management.
If you need a one-off tool, prefer using `nix run` instead of installing through other means.

### GitLab

Use the `glab` CLI for all GitLab operations (creating merge requests, reviewing comments, managing issues, etc.). The GitLab MCP connector does not work — do not attempt to use it. `glab` should be pre-authenticated via the `GITLAB_TOKEN` env var.

## General Coding Conventions

### Testing

See @ctx/testing.md

### Error Handling

In Go production code, never write `panic` unless I have explicitly directed
you to. Return an error and let the caller decide, widening function signatures
if necessary. This also applies to constructors that panic on programmer errors.

In Rust production code, use `Result` / `Option` for realistic runtime failures.
`unwrap`, `expect`, and `panic!` are allowed when established invariants make
failure impossible in practice, provided the reasoning and invariants are
documented in a nearby comment or relevant API documentation. Do not introduce
unnecessary error types, signature changes, or defensive branches for states
that those invariants rule out. "This should never happen" alone is not a
justification; explain why it cannot happen. See @ctx/lang/rust.md for details,
including mutex poisoning.

Test code (`t.Fatal*` in Go tests, `panic!` inside `#[test]` Rust tests) is
exempt; that's the testing framework's intended failure path.

## Language Guidelines

- Rust :: see @ctx/lang/rust.md
- Python :: see @ctx/lang/python.md
- Golang :: see @ctx/lang/go.md
