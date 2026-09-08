# Borrows and slices

Implemented in `verification/library/borrow.mli` and `borrow.ml`. The compiler
checks the wrappers and demos against a small storage boundary implemented in
`runtime/borrow.c` and `verification/vox_vc.ml`.

## Ownership and models

Use scoped exclusive loans. Reads temporarily borrow the handle. Writes,
splits, and completion consume it; a consuming operation that leaves the loan
open returns a successor handle. OxCaml's uniqueness and locality checks
prevent reuse of a consumed handle, mutation during a read borrow, access to a
suspended parent, and escape of a local loan.

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
| `Slice.length (borrow_ s)` | Length of the current model |
| `Slice.get (borrow_ s) i` | Shared element; `Some value === Model.at C i` |
| `Slice.set s i x` | Successor with `current = Model.set C i x` and unchanged final |
| `Slice.swap s i j` | Successor with `current = Model.swap C i j` and unchanged final |
| `Slice.snapshot (borrow_ s)` | Real immutable-array copy of the current model |
| `Slice.finish s` | Consume loan; refined unit establishing `F === C` |
| `Slice.split_at s k post body` | Lend adjacent disjoint children; reconstruct parent from their finals |
| `Slice.split3 s lo hi post body` | Lend prefix, `[lo, hi)`, and suffix |
| `Slice.with_range s lo hi post body` | Lend `[lo, hi)`; preserve the surrounding contents exactly |
| `Slice.parallel spawn left right lp rp f g` | Run two consuming callbacks and establish both final postconditions |

All indices have checked bounds. Empty/full splits and empty ranges are
allowed. Loan length is invariant; resizing and arbitrary joining are absent.
`swap`, `split3`, and `with_range` are verified library implementations.

Scoped operations returning a callback result and a restored handle use:

```ocaml
type ('value, 'state) step = {
  value : 'value @@ global;
  state : 'state;
}
```

The `value` field is global and shared; the `state` field retains the result's
local/unique modes. Unpack these results with `let refine_ result = ...` followed
by `let {value; state} = result in ...`. Reads return their refined result
directly. Elements read from a slice are shared, including composite elements
that remain in the array. A borrowed variable can serve as a dependent function
argument; its contract refers to the same stable binding. Computed expressions
and mutable bindings still require a separate stable binding.

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

The caller supplies a ghost lambda describing the postcondition. Inside the
callback, perform the writes, call `finish`, and return a refined result. The
verifier substitutes the predicate at its applications, so neither side needs
`post_def` calls. `borrow_demo.ml` is the smallest complete mutation example.
Named predicates with explicit `[@def]` lemmas remain supported.

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
numeric decreases measure. Sequential sorting also checks its size decrease through scoped callbacks
and exposes a total contract. Parallel sorting exposes normal-return
correctness: domain joining remains partial. Element borrows, shared read loans,
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

## Design experiments

Baseline: `a7bd9bf43b` on `jujacobs/vox/borrows-slices-20260907`.
The experiments preserve separate branches. An experiment is complete only
when its implementation, representative clients, and evaluation are recorded.

| Task | Branch | Status |
| --- | --- | --- |
| Shared sequence model | `jujacobs/vox/shared-sequence-20260908` | Evaluated: revise |
| Callback predicate arguments | `jujacobs/vox/callback-predicates-20260908` | Evaluated: adopt |
| Reusable collection mathematics | `jujacobs/vox/collection-theory-20260908` | Evaluated: adopt |
| Borrow interface and transitions | `jujacobs/vox/borrow-interface-20260908` | Evaluated: adopt |
| Scoped callback termination | `jujacobs/vox/scoped-termination-20260908` | Evaluated: adopt |

### Shared sequence model: verdict

The checked list theory now belongs to `Vox_sequence`. Slices use that module,
and its immutable-array readers expose both native array observations and
sequence observations in their result refinements. The abstract sorted-array
API exposes the same sequence model, with checked length and element bridges.
The model still exposes its list representation, preserving existing induction
proofs and finite extensional equality. This prototype does not replace the
compiler's immutable-array observation encoding.

**Revise before adopting the whole proposal.** Extracting the common theory is
a useful foundation, but adding a second observation interface to sorted arrays
adds proof obligations and does not yet remove their existing observation
interface. The subsequent collection-theory experiment must demonstrate actual
reuse before this becomes a clear overall improvement. Avoid claiming that a
module extraction alone unifies the SMT encodings.

`verification/benchmarks/sequence_models.py` compares symbolic read-after-write,
unchanged-index framing, and split reconstruction, using ground read equations,
native arrays, and native sequences. Each valid equation also has an incorrect
variant checked for satisfiability. With Z3 4.16.0, three repetitions, and a
2-second query timeout, the tested ground/array queries took about 5--8 ms
including solver startup. Native sequences proved last-write reads and split
reconstruction, but timed out on unchanged-index framing for both one and eight
writes. A diagnostic with an explicit prefix/suffix case split still timed
out in the prefix case; using `seq.at` instead of `seq.nth` also timed out.
Z3 4.16.0 rejected the tested `seq.update a i (seq.unit x)` operation. This argues against selecting the
tested native sequence encoding as the default. These are observation VCs, not complete sorting proofs or a comparison
of induction strategies; arrays and ground equations remain viable candidates.

`verification/benchmarks/sequence_demos.py` uses the compiler from `make install`
to compile complete demo sources with `-principal`. The baseline's length
primitive is renamed to match the candidate compiler; its semantics is
unchanged. Three repetitions on this machine gave:

| Sources compiled | Baseline median | Candidate median |
| --- | --- | --- |
| Sequence library, borrows, end swap | 433 ms | 439 ms |
| Sorted-array implementation and client, including new library dependency | 525 ms | 681 ms |
| Sequence library, borrows, quicksort and client | 1068 ms | 1094 ms |

The sorted-array difference includes compiling the newly used sequence library
on every run. This is an uncached source-build comparison, not incremental
client compilation or a runtime benchmark. Native array integration remains
unevaluated at the complete-demo level.

Verification: `make -s test-one DIR=vox` passed all 61 files, including the
borrow runtime, sequential/parallel quicksort, abstract sorted-array clients,
and rejection tests. The focused clients passed bytecode/native compilation
with and without `-principal`. The installed library also compiled and verified
in both backends with `-principal`. `make -s fmt` passed.

## Callback predicate experiment

Branch: `jujacobs/vox/callback-predicates-20260908`. The semantic baseline is
`a7bd9bf43b`; `68b3b6a07d` separately formats the existing library and is the
benchmark baseline. The shared-sequence experiment remains on
`jujacobs/vox/shared-sequence-20260908` (`316979b039`), with a revise verdict.
The experiment index above records the subsequent branches and their status.

**Verdict: adopt explicit ghost lambdas for local callback predicates.** Vox
already accepts predicate arguments. Retaining and substituting their local
logical expressions removes the manual definition-lemma transport without
requiring new syntax, interface metadata, or higher-order SMT. The change adds
139 compiler lines after formatting, and removes 22 postcondition-unfolding
calls and 11 generated definition lemmas from the migrated library and demos.

`split3`, `with_range`, end swap, range editing, runtime validation, and
sequential/parallel quicksort use the same public contracts as before. Ordinary
named definitions remain opaque. Recursive definitions retain explicit
unfolding. Exported predicate definitions still need contracts or definition
lemmas: local lambda bodies are not exported through compiled interfaces.

The retained expression is instantiated only for fully applied, unlabelled
arguments with matching SMT sorts. Parameter patterns are supported when the
body has a logical encoding. Unsupported expressions and sort-changing
polymorphic applications retain their opaque interpretation. This restriction
keeps the prototype small; the successful polymorphic borrow clients use
predicates with the local function's fixed abstract element sort. The change
does not infer postconditions.

The implementation retains only captured symbols and parameters as free
variables. Body-local definitions form a DAG; substitution preserves sharing.
An unmodelled fresh result prevents transparency. Body proof obligations remain
in their original verification batch rather than becoming assumptions of an
application. Recursive bindings discard transparency.

`verification/benchmarks/callback_demos.py` compiles baseline and candidate
sources using the same compiler from `make install`, with `-principal`. Three
repetitions gave these medians; query and byte counts come from a separate
`-dsmtlib` run to exclude dump overhead from timings:

| Complete sources | Baseline | Candidate | Queries before/after | SMT bytes before/after |
| --- | --- | --- | --- | --- |
| Library and end swap | 453 ms | 462 ms | 29 / 29 | 523961 / 512004 |
| Unchanged sorted-array control | 579 ms | 598 ms | 46 / 46 | 635572 / 635572 |
| Library and quicksort | 1190 ms | 1093 ms | 81 / 81 | 1693195 / 1672612 |

The unchanged control also varied, so these timings do not establish a general
speedup. The reduced SMT input and unchanged query counts show that the simpler
proof source does not require additional solver queries on these examples.
Library plus swap shrinks from 1135 to 1087 source lines; library plus quicksort
shrinks from 2152 to 2090, against the equally formatted baseline.

Validation: all 62 Vox files pass. Ghost-predicate tests cover callbacks,
lexical captures, different arguments, matches, tuple parameters, conditional
predicate selection, and polymorphic specialization. VC unit tests check both
valid and invalid applications, preservation of body obligations, ordinary
function opacity through ghost aliases, and linear substitution size. The
installed library verifies in bytecode/native modes with `-principal`, and a
separately compiled end-swap client runs successfully with both archives.
Its Lambda output contains the erased-value marker for the predicate; its
native assembly contains no predicate closure or model-projection calls.

## Collection theory experiment

Branch: `jujacobs/vox/collection-theory-20260908`. Baseline `7809359026` combines
the sequence and callback prototypes. **Verdict: adopt the shared mathematics
and the integer multiset specification.** This experiment adds no compiler code.

`Vox_sequence` gains polymorphic take, subrange, and decomposition laws.
`Vox_int_sequence` contains integer bounds, sortedness, and permutation laws.
Quicksort's model shrinks from 757 to 96 lines, containing only
`swap_partition`, `partition_bounds`, and `glue_partition`. Its public `Spec` is
a module alias to the shared library. Runtime validation now obtains sortedness
from the library, and a separate rotation client reuses the permutation laws.

The multiset representation is abstract. `bag` maps a sequence to a multiset;
`multiplicity` observes that multiset, while `count` directly counts a sequence.
The checked `bag_multiplicity` law equates those observations.
`permutation_count` derives equal counts from permutation; `count_extensional`
establishes permutation from a total proof function supplying equal counts at
any integer. Thus the public laws characterize permutation by multiplicity in
both directions. Bounds, sortedness, and count definitions are exported as
checked definition lemmas. Canonical insertion remains private.

Internally, bags use canonical sorted lists. The new proof establishes sortedness
of that representation and uniqueness from equal counts, using structural
induction. This requires no quantifier axioms or higher-order SMT. Executing
`bag` can take quadratic time; specification clients enclose model computations
and proof calls in `ghost_`, so they are erased. This is an integer multiset
specification; a polymorphic multiset implementation remains future work.

The complete quicksort source set grows by 455 lines, including the library's
209-line interface and the new count-extensionality proofs. That is a reasonable
cost for a public mathematical interface and stronger specification laws. It
moves reusable proof work into an installed library and provides actual reuse
across pure rotation, slice sorting, and runtime validation. The additional
sorted-array observation interface from the sequence experiment still needs
revision; this branch does not change the compiler's native-array encoding.

`verification/benchmarks/collection_demos.py` uses the compiler from
`make install`, with `-principal`, against the same baseline compiler. Three
repetitions gave these medians. Complete-source timings recompile dependencies;
client timings compile only the final client after those dependencies exist.
SMT dumps are collected separately from timed runs.

| Workload | Complete sources, before/after | Client, before/after | Source queries, before/after |
| --- | --- | --- | --- |
| Library and end swap | 389 / 416 ms | 32 / 32 ms | 31 / 35 |
| Library and sorted array | 633 / 642 ms | 56 / 54 ms | 67 / 71 |
| Library and quicksort | 962 / 1049 ms | 22 / 23 ms | 83 / 95 |

The complete quicksort build costs 87 ms more, with 12 additional proof queries;
its SMT input grows from 1698664 to 1878914 bytes. Final-client query counts and
SMT input are unchanged for all three workloads. These measurements support
adopting the abstraction and proof reuse, without claiming a verification
speedup.

Verification covers all 64 Vox tests: 63 passed in the full `test-one DIR=vox`
run, and the remaining runtime-validation dependency was corrected and passed
separately. The final collection library, rotation, quicksort, runtime
validation, and rejection clients passed focused checks. Rotation runs 153
empty/full/interior splits with duplicates. The installed library verifies in
bytecode and native modes with `-principal`. Representation construction,
unproved count callbacks, and using permutation-count without its premise are
rejected. Separately compiled installed-library rotation clients run with both
archives; Lambda output erases the three proof calls in `rotate_at` to ghost
markers. `make fmt` passes.

An existing refinement-symbol scope limitation prevented re-exporting the
library with `include`; an ordinary module alias preserves the intended public
specification and its symbol identities. This experiment does not fix that
compiler limitation.

## Borrowed read experiment

Branch: `jujacobs/vox/borrow-interface-20260908`, baseline `d9d6a7f6d3`.
**Verdict: adopt borrowed reads.** `length`, `get`, and `snapshot` accept a
read borrow and return only their refined result. Writes, splits, and completion
retain their consuming interfaces. Reads do not create logical successor states.
The public specification continues to describe immutable sequence observations
of each state, while loan transitions carry the final prophecy between states.

A dependent argument may now be `borrow_ variable`. Substitution uses the same
stable binding as an ordinary variable; the ordinary borrow context still
controls its lifetime and access permissions. Computed expressions and mutable
bindings remain rejected. This is one additional frontend pattern, imported
into Merlin. The VC generator handles the length observation separately from
loan transitions; it no longer invents a successor for that read.

The public library loses 27 lines, including repeated index conversions in
`swap`. Its two reads share the initial handle, followed by two consuming
writes. End swap, range editing, runtime validation, parallel callbacks, and
quicksort use the new interface. Elements returned by `get` remain shared;
the `immutable_data` element restriction is unchanged. Reads, snapshots, and
callback results preserve the same useful output permissions as the previous
`step.value` field. Quicksort retains explicit `int` annotations for the pivot
and size under `-principal`, now as ordinary bindings after refinement
elimination. Explicit refinement elimination remains part of the idiom.

The runtime removes one three-word result pair per read.
`verification/benchmarks/borrow_reads.py` compiles the baseline C source with
renamed symbols and measures both versions in one executable built by the
installed native compiler. It alternates their order over five repetitions and
checks equal results. The array has 16 integers; these are primitive
microbenchmarks, without a claim about whole-quicksort runtime speedup.

| Operation | Iterations | Baseline/candidate median CPU time | Baseline/candidate bytes per operation |
| --- | --- | --- | --- |
| Length | 2000000 | 11.905 / 4.335 ms | 24 / 0 |
| Get | 2000000 | 17.660 / 9.435 ms | 24 / 0 |
| Snapshot | 200000 | 3.203 / 2.606 ms | 160 / 136 |

`collection_demos.py --current-only` measures verification using the installed
compiler. Complete source builds take 412 ms for library plus end swap, 656 ms
for sorted arrays, and 1069 ms for quicksort; final-client medians are 30, 55,
and 22 ms. Against the preceding branch's recorded output, query counts remain
35, 71, and 95. The end-swap SMT input shrinks from 617500 to 603565 bytes, and
quicksort from 1878914 to 1851429 bytes; sorted arrays are unchanged. Those
baseline timings were collected in a separate run, so the evidence supports
simpler VCs and comparable verification cost rather than a timing speedup.

Validation covers all 64 Vox tests: 63 passed in the refreshed full run, then
quicksort passed after restoring its size annotation. The 42 refinement-typing
tests, the existing borrowing-mode test, and `make merlin-test` pass. Rejection
tests cover borrowing a consumed handle, using the owner during a read borrow,
claiming unique ownership of an element, incorrect bounds, and unproved
postconditions. The runtime tests include split offsets, snapshots, empty
arrays, and flat float storage. The installed library verifies in both backends
with `-principal`, and separately linked end-swap clients run with both archives.
`make fmt` passes.

This branch keeps scoped operations and parallel joining partial. Their
termination contracts are evaluated separately in the scoped-termination
experiment.


## Callback termination experiment

Branch: `jujacobs/vox/scoped-termination-20260908`, baseline `13c52c9fe7`.
**Verdict: adopt direct callback recursion and sequential totality.**
The recursion-use checker now traverses directly supplied callback bodies with
its current permission to call the recursive function. Numeric recursion still
proves the existing nonnegative, strictly decreasing measure at each recursive
call; structural recursion still requires a strict descendant. A callback
inside an already delayed body does not regain that permission. Bare recursive
function values, aliases, and let-bound recursive closures remain rejected.

This is a direct-callback rule. It does not prove that an arbitrary callee
invokes its callback immediately, once, or before returning. The decrease
obligations establish the recursive call relation; the existing totality modes
independently constrain the callee and its captures. Neither `local` nor `once`
is used as evidence that the callee terminates. No callee whitelist, new effect,
SMT axiom, or callback-specific termination measure is introduced.

Finite borrow primitives receive total declarations after inspecting their
runtime implementations. Checked wrappers propagate totality through callbacks
using the existing modes. Runtime array bounds are justified by their refinement
preconditions. `Slice.parallel` and domain joining retain partial contracts;
passing a blocking callback also prevents a total contract. Marking finite
mutators total does not allow ghost code to consume a real unique handle.

Quicksort takes a private runner with the existing two-child postcondition.
Its sequential runner calls both children; its parallel runner is
`Slice.parallel`. The same partition and recursive sort prove sortedness,
permutation, and size decrease. Public sequential sorting exposes `total`;
public parallel sorting retains normal-return correctness. Checked division
by two supplies explicit midpoint bounds. A shift-based prototype failed those
bounds because shifts lack the required arithmetic encoding; it was discarded.
Only the finite `land` primitive gains a total declaration in Stdlib.

The compiler change adds ten lines, plus its two generated Merlin copies.
Quicksort adds roughly forty net lines for the private runner, checked midpoint,
and total contracts, without duplicating its sorting algorithm. This is a
reasonable cost for distinct sequential and parallel termination contracts.
The runner contract does repeat the parallel interface; it should stay private
until another algorithm needs this abstraction.

A concrete well-founded recursion combinator also verifies. Its step function
receives a callback restricted to smaller integers. The returned function is
`stateful`, reflecting its captured step function; this does not implement a
stronger interface promising a stateless result. This example and structural
list recursion exercise the rule independently of the borrow implementation.


`collection_demos.py --baseline 13c52c9fe7` compares both source versions using
the same installed compiler, with `-principal` and three repetitions:

| Workload | Complete sources, before/after | Client, before/after | Source queries, before/after |
| --- | --- | --- | --- |
| Library and end swap | 506 / 506 ms | 38 / 37 ms | 35 / 35 |
| Library and sorted array | 795 / 809 ms | 68 / 70 ms | 71 / 71 |
| Library and quicksort | 1289 / 1344 ms | 27 / 27 ms | 95 / 99 |

Runner, arithmetic, and termination obligations add four quicksort queries. Its source SMT input grows from 1852085 to 1987241 bytes. The
sorted-array source is unchanged and provides a timing control. The additional
55 ms is about 4% of the complete quicksort source build in this run.

`quicksort_runtime.py` builds both versions with the installed native compiler
and checks their outputs against `List.sort`. Five runs alternate version
order. Each workload sorts a 4096-element input 20 times; the table reports the
median per-sort CPU time and allocation across those runs. Measurement includes
copying the input into an owned array, sorting, and returning an immutable array.
Correctness comparisons and explicit pre-run collections are outside the timed
interval. These measurements cover sequential sorting only.

| Input | CPU time, before/after | Bytes per sort, before/after |
| --- | --- | --- |
| Ordered | 1.014 / 1.013 ms | 622760 / 655528 |
| Reverse ordered | 1.129 / 1.138 ms | 767912 / 808744 |
| Five repeated values | 1.200 / 1.196 ms | 717800 / 755848 |
| Deterministic random | 1.527 / 1.533 ms | 821768 / 865592 |

Timings are close; allocations rise by roughly 5%. The stronger totality contract
and shared sequential/parallel implementation justify this measured cost, but
this is not a runtime optimization.

All 66 Vox tests pass, including bytecode/native quicksort with and without
`-principal`, the new callback examples, and rejection of unchanged recursive
arguments, blocking callees, and ghost consumption of real unique handles.
The existing structural-recursion test, all 42 refinement-typing tests, Merlin,
and the actual-Z3 VC and session tests pass. A clean rebuild was needed to
refresh compiler-library and test-library artifacts after the Stdlib interface
change; the initial failures were interface-digest mismatches. The installed
library verifies in both backends with `-principal`; independently linked
end-swap clients run with both archives. `make fmt` passes.
