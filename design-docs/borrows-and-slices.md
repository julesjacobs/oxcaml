# Borrows and slices

Implemented in `verification/library/borrow.mli` and `borrow.ml`. The compiler
checks the wrappers and demos against a small storage boundary implemented in
`runtime/borrow.c` and `verification/vox_vc.ml`.

## Ownership and models

Use scoped exclusive loans. Every operation consumes its input handle; an
operation that leaves the loan open returns a successor handle. OxCaml's
uniqueness and locality checks prevent reuse of the consumed handle, access to
a suspended parent, and escape of a local loan.

| Type | Meaning | Runtime representation |
| --- | --- | --- |
| `'a Owned_array.t` | Exclusive ownership of a complete array | Heap descriptor containing array, offset, length |
| `'a Slice.t` | Exclusive loan of a contiguous range | Same descriptor layout |
| `'a Model.t` | Immutable sequence for specifications | `'a list` when explicitly executed |

Elements have kind `immutable_data`, whose layout excludes float elements.
The storage primitive tests separately cover flat float arrays. Reads share
elements; mutation replaces
slots. Mutation inside an element and element borrows require a later design.
For composite concrete types under `-principal`, give the type an explicit
kind, such as `type snapshot : immutable_data = int iarray`.

The ghost projections are:

- `Owned_array.contents a`: contents of the state represented by owner `a`.
- `Slice.current s`: contents of the state represented by loan `s`.
- `Slice.final s`: contents when that loan ends, including nested borrows.

`current s` remains the old model after `set s` returns a successor `s1`.
`current s1` describes the write, and `final s1 === final s`. These are stable
logical observations even though both runtime handles may have the same
pointer. Model facts do not grant runtime access to a consumed handle.

Write observations used by proof code as:

```ocaml
let before = ghost_ (Slice.current (borrow_ s)) in
let eventual = ghost_ (Slice.final (borrow_ s)) in
```

`ghost_` erases evaluation. An `@ ghost` result annotation alone does not erase
a call. Projections can appear directly in static predicates. Runtime code,
including `assume_`, cannot observe them. `===` is available in ghost code and
refinement predicates; executable `assume_` predicates use its checked runtime
implementation when their operands are real.

## Public operations

The complete checked signatures are in `verification/library/borrow.mli`.
The table uses `C = current s`, `F = final s`, and bigint model indices.

| Operation | Result and contract |
| --- | --- |
| `Owned_array.of_iarray xs` | Fresh copy with model `Model.of_iarray xs` |
| `Owned_array.into_iarray a` | Consume ownership; freeze the underlying array without copying |
| `Owned_array.with_mut a post body` | Root loan; return the callback result and owner satisfying `post` |
| `Slice.length s` | Length and successor, with unchanged current/final models |
| `Slice.get s i` | Shared element and successor; `Some value === Model.at C i` |
| `Slice.set s i x` | Successor with `current = Model.set C i x` and unchanged final |
| `Slice.swap s i j` | Successor with `current = Model.swap C i j` and unchanged final |
| `Slice.snapshot s` | Real immutable-array copy and unchanged successor |
| `Slice.finish s` | Consume loan; refined unit establishing `F === C` |
| `Slice.split_at s k post body` | Lend adjacent disjoint children; reconstruct parent from their finals |
| `Slice.split3 s lo hi post body` | Lend prefix, `[lo, hi)`, and suffix |
| `Slice.with_range s lo hi post body` | Lend `[lo, hi)`; preserve the surrounding contents exactly |
| `Slice.parallel spawn left right lp rp f g` | Run two consuming callbacks and establish both final postconditions |

All indices have checked bounds. Empty/full splits and empty ranges are
allowed. Loan length is invariant; resizing and arbitrary joining are absent.
`swap`, `split3`, and `with_range` are verified library implementations.

Operations returning a value and a handle use:

```ocaml
type ('value, 'state) step = {
  value : 'value @@ global;
  state : 'state;
}
```

The `value` field is global and shared; the `state` field retains the result's
local/unique modes. This prevents a read from granting unique ownership of an
element still present in the array. Unpack results with `let refine_ result =
...` followed by `let {value; state} = result in ...`.

## Exporting a callback proof

`with_mut` has this contract, with `Slice` and `Owned_array` opened as needed:

```ocaml
val with_mut : ('a : immutable_data) ('r : immutable_data).
  (a : 'a Owned_array.t) @ unique ->
  (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost))
    @ ghost ->
  ((s : {s : 'a Slice.t | Slice.current s === Owned_array.contents a})
      @ local unique ->
    {r : 'r | let refine_ s = s in post r (Slice.final s)}) @ local once ->
  {r : ('r, 'a Owned_array.t) step |
    post r.value (Owned_array.contents r.state)
    && Model.length (Owned_array.contents r.state)
       === Model.length (Owned_array.contents a)} @ unique
```

The caller supplies a named total `[@def]` postcondition with ghost Boolean
result and passes `ghost_ post`. Inside the callback, name the final model,
perform the writes, call `finish`, expose `post_def`, and return a refined
result. After `with_mut`, expose `post_def` again to use the concrete theorem.
`borrow_demo.ml` is the smallest complete mutation example.

A root frame links the callback's final prophecy to the restored owner's
contents. A split frame links each child's final prophecy to its part of the
restored parent. The parent retains its original final prophecy: a child
ending is not the parent ending. `with_range` reconstructs the current model as
`append prefix (append middle_final suffix)`. Later writes may change that
reconstructed parent before its own `finish`.

There is no public frame type, prophecy allocator, prophecy resolver, storage
transfer, or unrestricted cast. The library's `.mli` seals these operations.

## Parallel callbacks

`Slice.parallel` accepts two local unique loans and two global portable once
callbacks. With `spawn = true`, it transfers the left descriptor to a new
domain, runs the right callback on the calling domain, and joins the left.
The private transfer relies on descriptors always being heap allocated. It
does not extend the lifetime of arbitrary local closures.

Both callbacks must establish their supplied final postconditions. The wrapper
returns their conjunction. If either callback raises, the spawned domain is
joined before propagation. If both raise, the right exception takes priority,
with its original backtrace. Borrow contracts describe normal returns;
exceptions consume the owner rather than returning a partially modified one.

The sequential path uses the same callbacks and contracts. Direct spawning
requires a multicore runtime. Quicksort bounds its domain budget by
`Domain.recommended_domain_count`, so it also runs on a single-domain runtime.
There is no shared worker pool in this version.

## Sequence model and verification boundary

`Borrow_model` defines total `at`, `set`, `take`, `drop`, `sub`, `append`, and
`swap`, with checked equations and lemmas. Length and model indices use
`Bigint.t`; runtime indices use `int`, bridged by `Bigint.of_int`.
`at` returns an option. `of_iarray` is a checked total traversal; its length
and indexed-element lemmas connect immutable-array observations to the list
model.

The compiler supplies the list-length constructor equation and nonnegativity,
plus ground equations for the following storage operations:

- `contents`, `current`, and `final` have the handle's extent.
- Opening/restoring a root links its contents, final prophecy, and extent.
- Splitting creates child extents `k` and `n-k`, links child finals to the
  split frame, and preserves the parent's final prophecy.
- Recombining preserves the parent extent and final prophecy.
- Finishing equates current and final; transferring preserves both.
- Reading length returns the extent and an unchanged successor.

A frame's left/right final projections have the child lengths, not the parent
length. They therefore do not receive the ordinary handle-extent equation.
The rejected regression checks this distinction directly.

The C implementation and its external contracts are the trusted storage
boundary: copying/freezing, slot reads/writes, snapshots, disjoint splitting,
and reconstruction. Bounds checks also execute in C. Child offsets remain
inside the original array, so offset arithmetic stays within machine bounds.
The descriptor allocation, GC rooting, and disjoint storage implementation
are reviewed and runtime-tested, not proved by Vox. The verified wrappers and
sequence proofs require no `assume_unchecked_`.

Erased local observations use a total, stateless, portable ghost context that
permits local captures. The ordinary runtime locality rules remain in force.
The regression tests cover local ghost observations, rejected local escapes,
mutable reads in ghost code, and partial proof computations.

## Demos and guarantees

| Demo | Guarantee |
| --- | --- |
| `borrow_demo.ml` | Exact end swap, including empty and singleton arrays; shared input remains unchanged |
| `borrow_ranges.ml` | Exact range replacement, preserved frame, later parent write, stable snapshot |
| `borrow_validation.ml` | A runtime `assume_` check on a snapshot establishes sortedness of the unchanged final slice |
| `borrow_parallel.ml` | Separate domains, sequential fallback, and joining before exception propagation |
| `quicksort_client.ml` | Sequential and parallel sorting, with sortedness and preservation of every multiplicity |
| `borrow_rejected.ml` | Ownership, bounds, ghost/runtime separation, stale models, and split consistency |
| `borrow_runtime.ml` | Storage primitives, snapshots, empty slices, and float arrays |

Quicksort uses a middle-position pivot and distributes equal values using
index parity. Partition maintains a lower prefix and upper middle region,
then places the pivot and splits into left/pivot/right loans. Recursive
callbacks sort the disjoint children. Checked lemmas reconstruct sortedness
and permutation of the original model. Permutation is equality of canonical
insertion-sorted models; additional checked lemmas establish equality of every
element count.

Parallel quicksort divides its domain budget between the two children and
spawns only when both children meet the cutoff. Sequential children retain
the full budget for later splits. The public API accepts
`?max_domains` and `?cutoff`. Tests exercise duplicates, integer extrema,
ordered/reverse-ordered inputs, and deterministic random inputs.

Pure model functions and lemmas are total. Partition also has a checked
numeric decreases measure. The effectful recursive sort exposes normal-return
correctness: Vox does not yet prove termination of recursive calls through
scoped callbacks. Effect-aware termination, element borrows, shared read loans,
and a reusable parallel worker pool remain separate extensions.

## Building

Configure with `--enable-poll-insertion --enable-multidomain` to run the actual
parallel-domain demos. Z3 must be on `PATH`.

```sh
make vox-library
./dev init
./dev test vox/
```

`vox-library` first installs the final compiler, then verifies bytecode and
native versions of the library into `_build/vox-library` and installs them
under the configured prefix's `lib/ocaml/vox`. It is an explicit target so
bootstrap and ordinary compiler installation do not acquire a Z3 requirement.
See `verification/library/README.md` for client commands.
