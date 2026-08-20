# Rust Language Guidelines

- Always use `cargo-nextest` over `cargo test` where the project has nextest configured.
- Avoid using `unwrap()` and `expect()` in production code. Instead, use proper error handling with `Result` and `Option`.

## Async Rust

### Consolidate await points across branches
Instead of awaiting in each match arm, compute the value first, then await once. Reduces generated state machine complexity.
```rust
 // avoid: duplicates the state machine for each arm
 match cmd {
     A => send(123).await,
     B => send(456).await,
 }
 // prefer: one await, one state
 let val = match cmd { A => 123, B => 456 };
 send(val).await;
```

### Don't mark functions async if they don't call `.await`
A function that always returns a ready value still generates a full state machine.
Just return the value directly (or return a Ready future if you need the trait signature).
```rust
// Don't
async fn get_value() -> i32 {
  42
}
async fn deferred() -> i32 {
  get_value().await
}

// Do
fn get_value() -> Future<Output = i32> {
  future::ready(42)
}
// or just fn get_value () -> i32 { 42 } if the caller allows

fn deferred() -> Future<Output = i32> {
  get_value()
}
```

### No `.await` inside `select!` branch handlers when other branches borrow futures

If any branch uses `&mut future`, awaiting in another branch's handler stops
polling that future. If it holds a shared resource (mutex, channel), you get
"futurelock" — deadlock without contention. Either spawn the long-lived future
as a task, or restructure so the handler doesn't await.

```rust
// BAD: future1 holds a lock, stops being polled while op2 awaits
tokio::select! {
    _ = &mut future1 => { ... }
    _ = sleep(timeout) => {
        do_thing(lock.clone()).await; // futurelock
    }
}

// GOOD: future1 runs in its own task, keeps making progress
let mut task1 = tokio::spawn(future1);
tokio::select! {
    _ = &mut task1 => { ... }
    _ = sleep(timeout) => {
        do_thing(lock.clone()).await; // safe — task1 still polled by runtime
    }
}
```

### Prefer `JoinSet` over `FuturesUnordered` / `FuturesOrdered`

`JoinSet` runs each future in a separate task, so polling one doesn't block
others. `FuturesUnordered` runs all futures in one task — awaiting anything
else in the poll loop stops all of them. If you must use `FuturesUnordered`,
never await anything outside the stream consumption loop.

### Don't await other futures inside stream poll loops

After `futs.next().await` returns one result, the remaining futures in the set
are not being polled. Awaiting something that shares a resource with those
futures → futurelock. Add work to the set/`JoinSet` instead of awaiting inline.

```rust
// BAD: remaining futures in futs stop being polled during do_thing().await
while let Some(result) = futs.next().await {
    do_thing(lock.clone()).await; // futurelock risk
}

// GOOD: spawn follow-up work back into the set
while let Some(result) = futs.next().await {
    join_set.spawn(do_thing(lock.clone()));
}
```

### Bounded channels: prefer `try_send()` over `send().await` with small capacity

`send().await` on a full channel creates an unbounded wait queue. If the
sender's task is in a futurelock, the timeout can't fire because the sender
isn't being polled. `try_send()` fails immediately, letting you handle
backpressure without blocking the task. Choose capacity empirically — too small
causes spurious failures, too large masks problems with latency and memory.

### `select!` has dual risks depending on ownership

- **Borrowed futures** (`&mut f`): risk futurelock — future stops being polled
  while holding resources
- **Owned futures**: risk cancel-safety violations — future dropped mid-operation,
  partial state lost

Neither is locally obvious from reading the code. When in doubt, spawn as a
separate task.

### Dropping `&mut future` in `select!` does not cancel the future

Only the reference is dropped. The future itself persists, still holds
resources, and stays in wait queues. To actually cancel, drop the owned future
or abort the spawned task.

### Never leave shared state invalid across an await point

In sync code you can temporarily violate an invariant inside a `&mut` block —
the borrow checker guarantees you'll restore it before the reference is
released. In async, cancellation at any await leaves that invalid state
permanent. There's no unwinding, no poisoning, just silently corrupted state.

```rust
// BAD: if cancelled at the await, lock drops with state == None
let mut guard = state.lock().await;
let old = guard.take(); // temporarily None
let new = transform(old).await; // cancellation point!
*guard = Some(new);

// GOOD: compute first, then update atomically (no invalid intermediate state)
let old = state.lock().await.clone();
let new = transform(old).await;
*state.lock().await = Some(new);
```

### Use the reserve() pattern for channel sends in `select!`

`sender.send(value)` in a select branch is cancel-unsafe — the value is owned
by the future and lost if another branch wins. Split it: `sender.reserve().await`
to get a permit (cancel-safe — only loses queue position), then
`permit.send(value)` synchronously.

```rust
// BAD: value is lost if timeout fires first
tokio::select! {
    res = sender.send(value) => { ... }
    _ = sleep(timeout) => { ... }
}

// GOOD: reserve is cancel-safe, send is synchronous
tokio::select! {
    permit = sender.reserve() => {
        permit.unwrap().send(value); // synchronous, cannot be cancelled
    }
    _ = sleep(timeout) => { ... }
}
```

### Pin and resume cancel-unsafe futures across `select!` iterations

Don't recreate cancel-unsafe futures each loop iteration — that discards
partial progress. Create once with `std::pin::pin!()` outside the loop,
reference with `&mut` inside.

```rust
let mut response = std::pin::pin!(client.request(req));
loop {
    tokio::select! {
        res = &mut response => return res,
        _ = interval.tick() => { log_progress(); }
    }
}
```

### Avoid `tokio::sync::Mutex`

Prefer `std::sync::Mutex` (never hold across await) or actor/message-passing.
`tokio::sync::Mutex` held across an await + cancellation = silent state
corruption. Unlike `std::sync::Mutex`, it doesn't poison on cancel/panic, so
other tasks see the corrupted state without warning.

If you need mutual exclusion across await points, use the actor pattern: a
single task owns the state and receives requests via an mpsc channel.

### Never use `JoinHandle::abort()` for normal control flow

`abort()` cancels at an arbitrary await point — the task has no control over
where it stops. Use cooperative cancellation (`CancellationToken`, channels) so
the task chooses where to stop. Treat `abort()` like `panic!` — a last resort.

Note: dropping a `JoinHandle` does NOT cancel the task. The task runs to
completion unless explicitly aborted or the runtime shuts down.

### Use `join!` not `try_join!` for side-effectful futures

`try_join!` cancels remaining futures on first error. If those futures have
side effects (flushes, writes, external operations), use `join!` and check
errors after all complete.

```rust
// BAD: if write1 fails, write2 is cancelled mid-flush
tokio::try_join!(write1.flush(), write2.flush())?;

// GOOD: both complete, then check errors
let (r1, r2) = tokio::join!(write1.flush(), write2.flush());
r1?;
r2?;
```

### Use Drop guards to restore invariants on cancellation

`AsyncDrop` doesn't exist. Use `scopeguard::guard` or manual `Drop` impls to
ensure invariants are restored if a future is cancelled at an await point.
The guard runs synchronously when the future is dropped — the only reliable
cleanup mechanism for cancelled async code.

### Document cancel safety on public async methods

No compiler checking exists for cancellation safety. Add a "Cancel Safety"
section to public async methods documenting what happens if the returned future
is dropped before completion: is data lost? Are invariants preserved? Can the
caller retry?

## External References (only read if asked)

- [Oxide RFD 397 — Async cancellation](https://rfd.shared.oxide.computer/rfd/0397) — initial survey of cancellation risks in the Oxide control plane
- [Oxide RFD 400 — Cancel safety guidelines](https://rfd.shared.oxide.computer/rfd/0400) — comprehensive cancel-safety patterns, API design, and mitigation strategies
- [Oxide RFD 609 — Futurelock](https://rfd.shared.oxide.computer/rfd/0609) — deadlock-without-contention caused by stopped polling in select!/FuturesUnordered
- [Async Rust Never Left the MVP State](https://tweedegolf.nl/en/blog/237/async-rust-never-left-the-mvp-state) — state machine codegen overhead and await-point consolidation
