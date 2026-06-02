# Test Discipline

Tests MUST assert behaviour and logic, not the expected shape of the code. A test whose only job is to restate a structural fact the compiler already guarantees (the const exists, the struct has this field, the file is non-empty, the function is exported) has negative value: it fails on every rename / refactor without catching any real regression, and the fact that it passes tells you nothing about correctness. Do not write these.

## Concrete red flags

If a test matches one of these, delete it or rewrite it to exercise behaviour:

- Asserts a compiled-in const, enum variant, or type exists.
- Asserts a string / slice is non-empty when `include_str!`, `const`, or a known-non-empty literal source produces the value.
- Re-states a struct's field layout (`assert_eq!(std::mem::size_of...)` on a type you own, "has this field" checks via reflection, etc.).
- Mirrors the production implementation one-to-one in the test body, so any real behaviour change must be updated in two places.
- Asserts a specific log line, span name, or tracing field on a code path that doesn't affect observable behaviour.
- Asserts a `derive`d trait works (`Copy`, `Clone`, `Hash`, `Send + Sync`, etc.) — the compiler proves this, and the test fails to refactor noise instead of catching real bugs.
- Compile-only "trait is object-safe" / "type implements `Foo`" assertions when the same property is already exercised by any code path that actually uses the trait/type.

## Instead

Drive the system through its real interface (HTTP handler, engine method, SQL query) and assert on the observable outcome (returned value, DB row, emitted event, error variant). If a piece of code has no observable behaviour worth asserting, it probably doesn't need a test at all.

## When you encounter a violation

When you encounter a failing test that violates this rule (a brittle assertion on shape rather than behaviour), STOP and ask before silently updating the brittle assertion to match the new shape — preserving the negative-value test hides the opportunity to fix it.
