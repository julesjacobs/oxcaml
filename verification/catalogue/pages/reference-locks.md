title: Reference and unique-payload locks
blurb: Two spin locks whose acquire and release are checked to hand out and take back exactly the ownership of the guarded cell; one guards a nonnegative integer, the other a unique payload.
status: review-pending
date: 27 September 2026
sources:
  - verification/library/reference_lock.mli — Lock over a nonnegative integer: interface
  - verification/library/reference_lock.ml — Its implementation
  - verification/library/unique_lock.mli — Lock over a unique payload: interface
  - verification/library/unique_lock.ml — Its implementation
  - verification/library/spin_lock.mli — The flag protocol both locks instantiate: interface
  - verification/library/spin_lock.ml — The protocol and its proof
  - verification/library/verified_atomic.mli — Trusted atomic contract
  - verification/library/unique_cell.mli — Trusted cell moves
  - verification/library/concurrency-boundary.md — Claims, assumptions and reading order
  - testsuite/tests/vox/atomic_lock.ml — Reference-lock client
  - testsuite/tests/vox/unique_lock_demo.ml — Unique-lock client
  - testsuite/tests/vox/unique_lock_buffer_client.ml — Unique lock over a raw byte buffer
  - testsuite/tests/vox/reference_lock_parallel.ml — Four domains incrementing the reference lock
  - testsuite/tests/vox/unique_lock_parallel.ml — Four domains incrementing the unique lock
  - testsuite/tests/vox/concurrency_boundary.ml — Public-only compile, rejections (including the one shown below) and erasure check
---
`Reference_lock` and `Unique_lock.Make` are spin locks built on one atomic flag: `try_acquire` makes one compare-and-set from 0 to 1, and `release` one from 1 back to 0. Both are instances of one functor, `Spin_lock.Make`, which proves the protocol once for any cell with a ghost location and a predicate saying when its payload is full. Holding the lock means holding a ghost permission, erased at run time, for the cell the lock guards. The interfaces prove that a successful `try_acquire` returns a permission that owns exactly that cell and nothing else, that a failed one returns a permission that owns nothing, and that `release` requires and consumes exactly such a permission. The proof also shows that the flag is 1 when `release`'s compare-and-set runs, so that step always succeeds. For `Reference_lock` the cell holds an integer that must be nonnegative whenever the lock is released; `read_owned` returns it. For `Unique_lock` the cell holds a unique payload: `take` moves it out and `put` moves one back, and `release` requires the cell to be full again.

Nothing is proved about the sequence of events: no mutual-exclusion theorem over executions, no linearizability, deadlock freedom, progress or fairness. Exclusion follows only from the ownership model, under which two live permissions never own the same cell. `make` records no relation between its argument and what a later holder observes, and `Reference_lock.try_increment` has no contract at all. Contracts describe normal return: a holder that raises or drops its permission leaves the lock held forever. The concurrency argument is not mechanized. It rests on the `Verified_atomic` and `Unique_cell` contracts listed on the shared page and on the assumptions under Trusted base.

## Client example

From `atomic_lock.ml`, which uses only `reference_lock.mli`. `{n : int | 0 <= n}` is `int` restricted to nonnegative values. `r.P.value` says whether the lock was acquired and `r.P.state` is the ghost permission; `P.own t` is the finite map of cells it owns. Inside `if r.P.value` the checker proves `owned a (P.own t)` from `try_acquire`'s contract, which `read_owned` and `release` require. `borrow_ t` lends the permission to the read without consuming it. `try_increment`'s body is checked like any other code, but it exports no contract, so the `assert`s about its effect are checked only at run time. The test runs in bytecode and native code.

@code testsuite/tests/vox/atomic_lock.ml "let () =" "end;"

`unique_lock_demo.ml` writes an increment against `Unique_lock`, without the overflow guard: acquire, `take`, add one, `put`, release. With the ghost steps that re-establish `owned` before `release`, it takes 28 lines.

## A rejected program

Releasing a unique lock after taking its payload out, without putting it back, is a type error. `Unique_lock_demo.Data` is an `int` payload whose snapshot is the value itself. `ghost_ (...)` is proof code, checked and then erased; here it applies `owned_def`, the lemma that states the definition of `owned`, so that `take`'s precondition can be proved.

The test `concurrency_boundary.ml` checks it against the public `.cmi` files, with `module L = Unique_lock.Make(Unique_lock_demo.Data)`, and requires this error:

@code testsuite/tests/vox/concurrency_boundary.ml "(* unique-release-empty *)" "|}]"

The same test requires ten more rejected lock programs to fail, each with its exact error: releasing with an empty permission or with another lock's permission, reading with a permission that `release` has consumed, reading after a failed acquire, claiming a negative value, taking with an empty permission, taking twice with one permission, passing one payload to `make` twice, and reaching the hidden atomic of either lock.

## Interface

@code verification/library/reference_lock.mli

@code verification/library/unique_lock.mli

`Unique_cell.Payload` supplies the payload type `V.t`, which must be `value mod portable contended` (shareable between domains without a lock), and a ghost `snapshot` of it. A permission gives the cell the contents `Some m` while it holds a payload with snapshot `m`, and `None` after `take`; `Ghost_pref.Heap.at` wraps these in one more `Some`. `Ghost_pref.Heap.put h p x` is `h` with location `p` set to `x`, and `===` is logical equality. `@ unique ghost` marks a permission that is consumed and erased.

## Trusted base

- `Unique_cell.Make`'s `create`, `take`, `put` and `replace` are C externals (`caml_unique_cell_*` in `runtime/pref.c`), although `unique_cell.mli` declares them with `val` and gives `Make` no comment saying so.
- `Ghost_pref.alloc`, which `Reference_lock.make` uses to allocate the integer cell, is an external (`caml_pref_alloc_step`), although `ghost_pref.mli` declares it with `val`.
- The guarded cell is read and written with ordinary, non-atomic accesses. The proof assumes that the sequentially consistent compare-and-set on the flag orders them between domains, as `concurrency-boundary.md` states. This is not derived from a memory model.
- The buffer client relies on `Raw_memory`'s externals (`malloc`, `read`, `write`, `free` and others, in `runtime/pref.c`) and on its `location_law` axiom.
- The multi-domain tests and `concurrency_boundary.ml` disable the `do_not_spawn_domains` alert.

## Scope

- `Reference_lock`: `make`, `location`, `try_acquire`, `release`, `read_owned`, `try_increment`. There is no `write_owned`; a holder writes through `Ghost_pref.write` at `location a`. The invariant `0 <= x` is fixed, not a parameter.
- `try_increment` acquires, adds one unless that overflows, and releases. Its type is `t -> bool`; its behavior is checked only by runtime assertions in `atomic_lock.ml` and `reference_lock_parallel.ml`, and the overflow case is not exercised.
- `Unique_lock.Make`: `make`, `location`, `try_acquire`, `release`, `take`, `put`. There is no blocking acquire; clients loop on `try_acquire`.
- The buffer client's snapshot is the constant 0, so the lock's contract says nothing about the byte's contents across release and reacquisition; the payload's refinement carries ownership of the byte and of the permission to free the buffer.
- Contracts describe normal return. A holder that raises loses its permission and the lock stays held; nothing restores it.
- The parallel tests run four domains of 1,000 increments each, with forced collections, and check the total of 4,000 at run time. They are tests, not proofs of progress or of the runtime.

## Reproduce

```
./dev test vox/atomic_lock.ml vox/unique_lock_demo.ml vox/unique_lock_buffer_client.ml vox/reference_lock_parallel.ml vox/unique_lock_parallel.ml vox/concurrency_boundary.ml
```

The two parallel tests are skipped unless the compiler was configured with `--enable-multidomain` and `Domain.recommended_domain_count ()` is at least 2. `concurrency_boundary.ml` compiles the channel and lock libraries with both compilers, compiles the public clients against the libraries' `.cmi` files only (and, for native code, their `.cmx` files), links them and runs those that do not spawn domains, requires each of 15 rejected programs to fail with its exact error, and fails if one of a fixed list of ghost primitives (heap operations, token split and join, an atomic's invariant key, a cell's location) appears in the Lambda or Cmm of the five channel and lock libraries (including `spin_lock`). It links the three clients that spawn domains without running them; their own tests run them.
