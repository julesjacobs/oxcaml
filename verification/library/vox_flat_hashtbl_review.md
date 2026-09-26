# Flat hash table review boundary

Baseline: `d143961f17`, the inspected remote head of PR #193. This boundary
change does not alter the typed-heap migration or trusted storage contracts.
`vox_table_implementation.ml` skips redundant lookup after rebuilding, and the
vacancy scanner erases its proved-impossible exhaustion check. The combined
insertion experiment was rejected; see `vox_flat_hashtbl_simplification.md`. Explicit refinement introductions use
implicit checking; ownership and normal-return guarantees remain unchanged.

## Ordered review files

Read these files in order, relative to the repository root. The indicated
parts of shared interfaces are the transitive semantic dependencies; their
other collection operations are unused by this contract.

1. `stdlib/bigint.mli`: signed unbounded integers, `of_int`, addition and
   subtraction. Ordinary `int` operations retain OCaml's signed 63-bit wrapping
   and bitwise semantics on the supported 64-bit runtime.
2. `verification/library/pref.mli`: abstract locations, affine typed ownership,
   finite heaps, and `Heap` operations and laws. The complete finite-heap
   observation equations assumed by these trusted primitives are below.
3. `verification/library/ghost_pref.mli`: erased ownership, allocation,
   split/join, borrowed observations and consuming updates.
4. `verification/library/vox_sequence.mli`: `at`/`at_def` and `set`/`set_def`.
   These recursive equations completely define the list operations used in
   the primitive storage model. No sequence proof body is needed.
5. `verification/library/vox_table_model.ml`: the entire primitive storage
   model and its equations, including control bytes, vacant slots, wrapping
   clones, counters and the exact sixteen-lane mask.
6. `verification/library/vox_table_bits.ml`: `count_trailing_zeros` and `first`.
   The trailing-zero primitive uses all 63 bits of an OCaml integer and returns
   63 for zero. `first` exposes the checked selected-lane characterization.
   `clear` and `clear_lane` are internal bit-manipulation proofs.
7. `verification/library/vox_table_storage.mli`: the entire trusted allocation,
   storage, SIMD, clearing and backing-storage replacement interface.
8. `verification/library/vox_verified_flat_hashtbl.mli`: the entire public
   interface. `Key` gives the equality/hash assumptions; `Map` gives an
   abstract finite map and its laws; the remaining declarations specify
   actual execution.

This document supplies the trust and resource conventions accompanying that
list. The semantic surface contains no probe invariant or opaque correctness
predicate. `Map.t` is abstract; clients reason only with its laws
(`lookup_empty`, `put_get`, `erase_get`, `count_put`, `count_erase`) and
the type of `empty`, which has count zero.
Each law is proved in `vox_table_bindings.ml`, where the map is an
association list with at most one binding per `Key.equal` class. That
invariant is what makes `count` the number of bindings.

`view` and `state` are abstract observations, not caller obligations. A view
pairs a valid storage snapshot with a ghost `Map.t`, its `bindings`; the
view's invariant says that map has the same bindings as the live slots, in
any order. Mutations therefore state exact equations
(`bindings r.#view === Map.put (bindings before) key value`), and a client
computes the length after any update from `count_put` or `count_erase`.
Physical empty slots are absent from the public model. Checked ghost
compaction bridges preserve lookup, key equivalence, distinctness, map
updates and cardinality.
`model` identifies the exact owned storage version associated with a snapshot.
The snapshot has void layout and is immutable. A saved snapshot grants no
access without the matching current ownership token.

Creation and clearing return exactly `Map.empty`. Replacement specifies `Map.put`, removal
specifies `Map.erase`, and reads specify actual returned values. Every mutation
updates precisely the original handle's location in the owned heap; all other
locations retain their values. Rebuilding can change backing blocks without
changing that location. Creation extends the heap at a fresh location. These
are public operation contracts, not conclusions inferred from runtime asserts.

## Trusted semantics and limits

A heap is a finite map from stable, distinct allocation identities to values
of its payload type. Write `at(h,p)` for its optional value. The trusted
observation equations are:

- `at(empty,p) = None`; `mem(h,p)` iff `at(h,p)` is `Some`.
- `at(put(h,p,x),q) = if p = q then Some x else at(h,q)`.
- `at(union(a,b),p) = if mem(a,p) then at(a,p) else at(b,p)`.
- `at(restrict(a,b),p) = if mem(b,p) then at(a,p) else None`.
- `at(exclude(a,b),p) = if mem(b,p) then None else at(a,p)`.
- `disjoint(a,b)` means no location is present in both; `same_domain(a,b)`
  means their membership predicates agree everywhere. Heap equality is
  extensional equality of these observations.

`own` observes the heap at a particular affine-token occurrence. Borrowing
permits observation; consuming operations replace authority rather than
copying it. The equations are implemented by `pref_observe`, `pref_disjoint`
and the Pref primitive handling in `verification/vox_vc.ml`; they are part of
Vox's trusted ownership encoding, not a new table axiom. `caml_vox_int_ctz` is
encoded in `verification/vox_encoding.ml`. The ordinary integer, datatype,
logical-equality and refinement checking of Vox are also trusted.

The primitive interfaces assume their runtime implementations and native
lowering satisfy their declared contracts. Implementation audit anchors are
`runtime/pref.c` (table storage, stable identities, allocation and barriers),
`runtime/vox_control.c` (SIMD/scalar masks and trailing zeros), and
`backend/cmm_builtins.ml` (native lowering). These are trust implementation
anchors, not hidden definitions needed to understand the public map claim.
No proof of the C implementation or backend is claimed here.

Keys and values have `immutable_data` kind. Equality is a pure, total, stable
equivalence relation and equal keys have equal hashes; hashes may collide and
may be any signed machine integer. The four law functions must verify.
The public table starts at capacity 16; capacity is bounded above by 2^30.
An insertion that needs growth beyond that limit can raise `Invalid_argument
"Vox_verified_flat_hashtbl: capacity exhausted"`. `find` raises `Not_found` for an absent
key. Allocation, runtime identity exhaustion, stack/resource failures and
runtime implementation correctness are not proved away.

The operations have normal-return contracts. General termination, allocation
success, exception-safe reclamation, concurrency and a complexity bound are
not claimed. An exception after a consuming mutation does not restore its
input token. Split unrelated ownership before fallible operations when that
frame must remain available on an exceptional path. Clearing removes retained
payloads through the trusted bulk-clear primitive and preserves capacity;
there is no theorem about when GC runs or physical memory is returned.
There is no cost model or credit convention beyond this explicit absence of a
complexity claim.

## Proof boundary and evidence

`vox_table_invariant`, all other algorithm/proof modules, and
`vox_table_implementation` are implementation dependencies. The public CMI
exposes none of their records, probe plans, validity predicates or auxiliary
proofs. The installed aggregate archive still includes these modules for
internal regression clients; they are excluded from the public-only client's
include directory. The storage-level `table_model.ml` regression deliberately
uses the internal driver to retain its detailed representation checks.

Reproduce the boundary check after `make install` and
`bash verification/library/build.sh _install`:

```
verification/clients/check_flat_hashtbl_public.sh
```

The script copies only `Pref`, `Ghost_pref` and `Vox_verified_flat_hashtbl`
CMIs into its public include directory. The independently compiled generic
client derives empty lookup, arbitrary-query replacement/removal/clearing,
the length after replacement and removal, and an unrelated owned table's
unchanged lookup. It supplies no invariant or
probe certificate. Runtime cases exercise 300 colliding keys, resizing,
aliased handles, GC compaction and subsequent lookup. Additional cases update
a key beyond a tombstone without duplication and replace/remove logically
different keys in the same `Key.equal` equivalence class.

Both bytecode and native clients pass. Thirteen rejection cases pass in both modes:
false reflexivity, inconsistent hashing, hidden invariant/implementation/proof/
representation/compaction/list-model access, constructing a `Map.t` from a
list, stale views, missing ownership, token reuse and a false lookup result. The erasure check inspects both emitted Lambda files: generic
execution contains no calls to the map/ownership observations or semantic
lemmas. Native snapshots and tokens have zero layout, and mutation results
contain only zero-layout fields. There is no runtime certificate accumulation
or final semantic checker. Runtime asserts in regression clients are tests.
`_build/flat-hashtbl-public/erasure.json` records the checked properties.

The implementation also passed `table_model.ml` in bytecode and native:
10,000 differential operations, 65,536-entry growth, GC/pointer payload and
rebuild regressions. The existing `table_ownership_rejected.ml` passed both
expect engines, including the principal-mode follow-up, with byte-identical
expected output (exit 3 is the expect harness convention, not a test failure). The migrated benchmark
client compiles with the installed native compiler. The simplification report
records the limited benchmark comparisons and rejected combined traversal. All checks used this worktree's compiler built by `make install`.
