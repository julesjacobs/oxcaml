# Flat hash table specification

Read `stdlib/map.mli` (`EquatableType` and `MakeLogical`), then
`verification/library/vox_verified_flat_hashtbl.mli`.

The public interface has an aliasable table handle and a permission carrying
its current bindings. `owner p` identifies the table; `bindings p` is its pure
finite map. Reads borrow the permission. Mutations consume it and return a
replacement whose bindings are the corresponding map update. A saved pure map
remains a historical snapshot. Capacity is an implementation detail.

`Model` is `Map.MakeLogical(Key)`. Its map equations are built into the verifier,
so clients need no map-law calls. The model compares keys with `Key.equal` and
uses extensional equality: insertion order and equivalent key representatives
do not affect equality. Cardinality uses mathematical integers.

## Private proof

A permission contains a valid storage snapshot and the ghost heap token that
owns the table's storage. Its refinement ties the snapshot, token and table
together. This representation and all heap-update equations are private.
Each table has its own permission; modifying one table leaves other tables'
permissions usable. A permission cannot be fabricated or reused after mutation.

`vox_table_bindings_bridge.ml` folds storage slots into `Model` and proves the
lookup and update equations. The public facade proves cardinality agrees with
the live-slot count. There is no separately maintained map of update history.
Probe order, tombstones, growth, invariants and proof helpers are hidden by the
public interface.

## Trusted base

- The Vox checker, its ownership encoding and the SMT solver.
- The built-in logical-map semantics and `Model.Proof.difference`, the
  distinguishing-key contract used by the private bridge.
- `pref.mli` and `ghost_pref.mli`: typed locations, heap ownership and allocation.
- `vox_table_storage.mli`: storage, backing replacement, clearing and SIMD mask
  contracts. Their model is checked OCaml in `vox_table_model.ml`.
- Count-trailing-zeros in `vox_table_bits.ml`, given its meaning by the checker.

The primitive implementations are in `runtime/pref.c`, `runtime/vox_control.c`
and `backend/cmm_builtins.ml`. Their compliance with the declared contracts is
assumed; this development does not prove the C implementation or native lowering.

Keys have kind `logical_data`; values have kind `immutable_data`. Equality and
hashing are total and stateless, equality is an equivalence relation, and equal
keys hash equally. Hash collisions are allowed.

Contracts describe normal return. `find` raises `Not_found` for an absent key.
A mutation that raises does not restore the permission it consumed. General
termination, allocation success, concurrency, complexity and memory reclamation
are outside this specification.

## Checks

```
./dev test vox/flat_hashtbl_boundary.ml vox/table_ownership_rejected.ml
```

The boundary test compiles a client with only the public table CMI available.
It verifies arbitrary-key lookup/update/cardinality equations and independence
of a second table. Runtime cases cover aliases, collisions, equivalent keys,
resizing, tombstones and GC compaction.

Rejection cases cover false key laws, false lookup results, private
representations, consumed permissions and permissions for another table. The
ownership misuses are also rejected without enabling the refinement extension.

Lambda and Cmm checks establish that permissions have zero runtime layout and
that clients call no ownership primitive, logical-map primitive or proof helper.
The native client is checked at the default optimization level and at `-O3`;
at `-O3`, it calls the table's search directly. The vacancy scanner's existing
allocation and erasure checks remain in place.

Earlier implementation experiments and measurements are recorded separately in
`vox_flat_hashtbl_simplification.md`.
