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

Never write `panic` (Go) or `unwrap` / `expect` / `panic!` (Rust) in production
code unless I have explicitly directed you to. Return an error and let the
caller decide. If a function's signature needs to widen to accommodate this
(adding `error` to the return values, or threading `Result` through callers),
do so. The same rule applies inside constructors that previously panicked on
"this is a programmer error" failures — return the error.

Test code (`t.Fatal*` in Go tests, `panic!` inside `#[test]` Rust tests) is
exempt; that's the testing framework's intended failure path.

## Language Guidelines

- Rust :: see @ctx/lang/rust.md
- Python :: see @ctx/lang/python.md
- Golang :: see @ctx/lang/go.md
