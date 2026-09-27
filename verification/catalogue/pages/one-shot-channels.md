title: One-shot channels
blurb: A single-use channel between domains, checked so that the receiver takes the payload out of the shared cell only after the sender has put it there; the payload keeps its type and refinements.
status: owner-review
date: 27 September 2026
sources:
  - verification/library/one_shot.mli — Public interface
  - verification/library/one_shot.ml — Implementation: the flag invariant and the two compare-and-set steps
  - verification/library/verified_atomic.mli — Trusted atomic contract
  - verification/library/unique_cell.mli — Trusted cell moves (module `Slot`)
  - verification/library/channel_buffer.mli — Byte-buffer protocol built on channels
  - verification/library/channel_buffer.ml — Its implementation
  - verification/library/raw_memory.mli — Trusted byte buffers used by the protocol
  - verification/library/concurrency-boundary.md — Claims, assumptions and reading order
  - testsuite/tests/vox/one_shot_public_client.ml — Public client
  - testsuite/tests/vox/one_shot_rejected.ml — Rejected programs
  - testsuite/tests/vox/one_shot_parallel.ml — Clients on two domains, including nested endpoints
  - testsuite/tests/vox/channel_buffer_demo.ml — Buffer-protocol client
  - testsuite/tests/vox/concurrency_boundary.ml — Public-only compile, rejections and erasure check
---
`One_shot` is a channel for one message. `create` returns a sender and a receiver that share a cell and an atomic flag. `send` puts the payload in the cell and sets the flag from 0 to 1; `recv` spins until it can set the flag from 1 back to 0, then takes the payload out. The implementation is checked against the trusted contracts of the atomic flag and the cell. The flag's invariant states that at 0 the flag owns nothing (an endpoint holds the cell) and at 1 it owns the full cell. So `recv` takes from the cell only with the ownership its successful compare-and-set hands it, and `send` fills the cell only while it owns it empty. The proof also shows that the flag is 0 when `send`'s compare-and-set runs, so that step always succeeds. Endpoints are unique values, so each can be used at most once.

A client learns what the payload type says: `recv` returns a value of that type, and when the type is a refinement type the receiver also gets the fact it states. `Channel_buffer` uses channels to send ownership of one byte of a raw buffer to a worker domain and back. The receipt it returns records the byte the worker wrote.

The interface itself has no refinements and records no history. For an unrefined type such as `int` it does not say that the value received is the value sent; a singleton refinement type recovers that, as in the example below. `recv` may spin forever. Nothing is proved about progress, fairness or linearizability. Contracts describe normal return: a dropped endpoint or an exception can leave the payload undelivered, and nothing is proved about reclaiming it. The steps at the flag are checked against the `Verified_atomic` and `Unique_cell` contracts listed on the shared page. That those contracts hold for concurrent execution on the OCaml runtime is assumed, together with the items under Trusted base; it is not derived from a memory model.

## Client example

The public client, which uses only `one_shot.mli`; `concurrency_boundary.ml` also compiles it with only the libraries' `.cmi` (and `.cmx`) files present. `(n : int) -> {r : int | r = n}` names the argument and refines the result: `{r : int | p}` is `int` restricted to values satisfying `p`. The channel's payload type is the singleton `{v : int | v = n}`. The checker proves `n` has that type at `send`, and the result of `recv` has it, which proves the result type of `roundtrip`. The test runs in bytecode and native code.

@code testsuite/tests/vox/one_shot_public_client.ml "let roundtrip" "One_shot.recv rx"

## A rejected program

Sending a value that does not satisfy the payload refinement is a type error. `C` is `One_shot`, and the `[%%expect]` block holds the compiler's message as the test records it.

@code testsuite/tests/vox/one_shot_rejected.ml "let bad_value () =" "|}]"

The same test rejects nine more programs: sending or receiving twice on one endpoint, using a receiver as a sender, claiming `n = 42` for a receive at plain `int`, using an endpoint after sending it as a payload, sending an `int ref`, reading the hidden `cell` field, and taking from an empty `Unique_cell.Slot` or filling one twice. `concurrency_boundary.ml` compiles 15 more programs against the public `.cmi` files only and requires each to fail with its exact error; four are about channels (a reused sender, a false payload, a false buffer receipt and a hidden module), the rest about locks.

## Interface

@code verification/library/one_shot.mli

`('a : value mod portable contended)` is a kind: the payload may be shared between domains without a lock, which excludes `int ref`. Mutable state travels as a `Pref` cell or `Raw_memory` buffer together with its ghost permission, which the compiler erases. `@ unique` means the caller gives up the value.

The buffer protocol's interface follows. `P.token` is a ghost permission and `P.own t` the finite map of locations it owns. `H.at h l` is `Some c` when `h` owns location `l` with contents `c`; a byte's contents are `None` until it is written. `===` is logical equality. The types are concrete. `fill` is the worker's step: it writes `value` at byte `index`. `dispatch` starts a domain, sends it the slot and its permission on one channel, and returns the receiver of a second channel on which the worker sends the filled slot back. `read_receipt` reads the byte, proved equal to `expected`, and returns it with the slot.

@code verification/library/channel_buffer.mli

The buffer client splits the permission for a two-byte buffer into one token per byte, hands each byte to its own worker, and reads both receipts. The checker proves that the results are the bytes the workers were asked to write, 79 and 75 (the client prints `OK`). The client then joins the permissions and frees the buffer.

@code testsuite/tests/vox/channel_buffer_demo.ml 25-32

## Trusted base

- `Unique_cell.Slot.empty`, `put` and `take` are C externals (`caml_unique_cell_create`, `caml_unique_cell_put` and `caml_unique_cell_take` in `runtime/pref.c`), although `unique_cell.mli` declares them with `val`. The Boolean they record for the cell is the only link between the ghost state and what the cell holds: `take` returns whatever the cell contains at type `'a`, and an empty slot contains `()`. The proof shows `take` is called only when that Boolean records a full cell.
- The payload is stored and loaded with ordinary, non-atomic accesses (`caml_modify` and a field load). The proof assumes that the flag's sequentially consistent compare-and-set orders them for the other domain; `concurrency-boundary.md` states this assumption about the runtime's atomics in general. It is not derived from a memory model.
- Buffer protocol only: `Raw_memory`'s `length`, `location`, `malloc`, `read`, `write` and `free` are externals implemented in `runtime/pref.c`, and `location_law` (distinct buffers or indices give distinct locations) is an axiom. A finalizer frees a buffer once its descriptor is unreachable.
- `Domain.Safe.spawn`, `Domain.join` and `Domain.cpu_relax` from the standard library. The multi-domain tests and `concurrency_boundary.ml` disable the `do_not_spawn_domains` alert.

## Scope

- Operations: `create`, `send`, `recv`. One message per channel; there is no close, timeout, non-blocking receive or selection among channels.
- `recv` spins with `Domain.cpu_relax` until the payload is published and does not return if it never is.
- Payload types must be `value mod portable contended`.
- For a payload type without a refinement, the interface does not relate the received value to the sent one.
- `Channel_buffer.fill` and `dispatch` accept a permission that may own more than the selected byte, but return ownership of that byte only; the rest is lost to the caller, although the implementation keeps it. `read_receipt` returns the slot without the fact about its value. The client splits out single bytes first.
- Contracts describe normal return. Dropped endpoints, exceptions and cancellation can prevent delivery; there is no recovery or leak-freedom guarantee.
- `one_shot_parallel.ml` runs 200 round trips between two domains with forced collections, passes nested endpoints and a `Pref` cell with its permission, and checks the values with runtime assertions. It and `channel_buffer_demo.ml` run in bytecode only. These runs are tests, not proofs of progress or of the runtime.

## Reproduce

```
./dev test vox/one_shot_public_client.ml vox/one_shot_demo.ml vox/one_shot_rejected.ml vox/one_shot_parallel.ml vox/channel_buffer_demo.ml vox/concurrency_boundary.ml
```

`one_shot_parallel.ml` and `channel_buffer_demo.ml` are skipped unless the compiler was configured with `--enable-multidomain` and `Domain.recommended_domain_count ()` is at least 2. `concurrency_boundary.ml` compiles the channel and lock libraries with both compilers, compiles the public clients against the libraries' `.cmi` files only (and, for native code, their `.cmx` files), links them and runs those that do not spawn domains, requires each of 15 rejected programs to fail with its exact error, and fails if one of a fixed list of ghost primitives (heap operations, token split and join, an atomic's invariant key, a cell's location) appears in the Lambda or Cmm of the five channel and lock libraries (including the shared `Spin_lock` functor). It links the three clients that spawn domains without running them; their own tests run them.
