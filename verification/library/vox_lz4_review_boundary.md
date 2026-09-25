# LZ4 review boundary

## Read in this order

Unprefixed file names below are relative to `verification/library/`; paths
starting with `runtime/`, `backend/`, `testsuite/` or `verification/` are relative
to the repository root. The public executable
interface is `vox_lz4.mli`. It contains the normal-return compressor and decoder
contracts, executable byte-identity contract, and total composition theorem.
It names no streaming implementation or proof module.

The complete semantic definitions are in these files:

1. `vox_lz4_spec.ml`: compression relation, decoder observation relation, error
   types, returned observations, and erased allocation witness.
2. `vox_lz4_spec_decode.ml`: byte conversion, token fields, extended lengths,
   literal writes, overlapping match writes, terminal-sequence restrictions,
   status classification, and the total decoder. This is the full parser,
   including malformed-input and output-limit decisions.
3. `vox_lz4_spec_bytes.ml`: bounded source access, length extension encoding,
   literal bytes, individual wire bytes, and output-prefix equality.
4. `vox_lz4_spec_wire.ml`: exact wire layout of each sequence and terminal run.
5. `vox_lz4_spec_token.ml`: token and little-endian offset encoding.
6. `vox_lz4_spec_plan.ml`: sequence/plan types and validity, including the final
   five literal bytes and twelve-byte last-match restriction.
7. `vox_lz4_spec_match.ml`: source-distance equality, maximal match search,
   candidate validation, and the four-byte hash. The exclusion below applies
   to its one embedded proof body.
8. `vox_lz4_spec_hashes.ml`: immutable most-recent-position table lookup.
9. `vox_lz4_spec_scan.ml`: exact visited positions, hash-table updates, match
   jumps, and termination measure of the compressor model.
10. `vox_lz4_spec_storage.ml`: initialized-prefix predicate used by the trusted
    final-copy contract and both mutable buffer representations.

These modules contain the definitions themselves, rather than opaque predicate
names. The old implementation/proof modules reuse these definitions; they do
not supply an alternative semantic model. In particular, compressor correctness
still fixes the actual scanner's wire output, rather than merely asserting the
existence of some decodable output.

## Explicit proof-body exclusion

In `vox_lz4_spec_match.ml`, the body of
`source_matches_distance_extend` (from `fun source index distance count` through
its `[@@decreases count]`) is outside the human-review surface. It is checked
proof-irrelevant ghost evidence. Its signature states a derived consequence of
the fully visible `source_matches_distance` equation; it introduces no assumption
or semantic choice. The compiler checks the body and termination. All other
computational definitions in the ten semantic modules remain in the review
surface, including `scan_match`, `match_length`, `choose_match`, `hash_bytes`,
`hash4`, and their exact source/length/position conditions. No scanner behavior is
hidden by this exclusion.

## Transitive semantic primitives and assumptions

Read the following interfaces as part of the review surface:

- `vox_string_view.mli`: trusted immutable string contents, length, and indexed
  byte observation. No runtime iarray is made by the contents selector.
- `vox_sequence.mli`: `iarray_get` is the total bounded primitive array read.
- `vox_iarray.mli`: `get`, `at` characterized by `at_get`/`at_outside`, and extensional
  array equality; `updated`/`updated_read` for the owned-array access contracts.
- `pref.mli`: location, heap and affine-token types, finite-map operations and
  laws in `Heap`, and ownership primitives.
- `ghost_pref.mli`: erased ownership adapters and the heap aliases.
- `raw_memory.mli`: allocation identity, length, byte range, `location_law`,
  complete `range_def`/`footprint_def` characterizations, memory access and free
  contracts. `footprint` initializes the decoder's mathematical heap.

The semantic code also uses standard integer arithmetic/comparison, iarray
length, lists/options/records, character primitives, and 32-bit word operations.
Their language/compiler meanings are assumed. Byte conversion assumptions are
spelled out in `vox_lz4_spec_decode.ml`; hash word primitives are listed in
`vox_lz4_spec_match.ml`. Refinement, totality and ghost-erasure checking and the
SMT backend are trusted.

For the executable refinement proof, additionally review the primitive contracts
in `borrow_iarray.mli` and `vox_lz4_string_copy.mli`. The implementation boundary
includes `runtime/borrow.c`, `runtime/pref.c` and the integer-array/raw-byte
lowerings in `backend/cmm_builtins.ml`. These are trusted implementations of the
primitive contracts, not additional codec semantics.

The decoder model currently uses raw-memory locations as mathematical indices.
Consequently its erased block witness and the finite-map/location semantics are
explicitly part of this review surface. The witness is returned by the API;
clients supply no allocation token, heap invariant, match plan, or proof object.
No mutable permission or intermediate heap is exposed in the decoded result.

## Domain and limitations

Blocks and decoded capacities are limited to 4 MiB. Source sizes above the limit
raise `Invalid_argument`; allocation can raise `Out_of_memory`. The total theorem
requires capacity at least the source length and at most 4 MiB. Ordinary mutable
entrypoints have normal-return contracts, not totality contracts.

Detailed diagnostic reasons/positions are not specified by the total model;
its three statuses are specified. Compression need not shrink every source.
There is one final copy from raw storage into a fresh GC string.

Allocation failure during compressor scanning or decoder sequence processing
consumes ownership authority. Unreachable raw carriers have a GC finalizer,
which releases abandoned storage without restoring that authority. Normal
returns and string-copy failures release explicitly; clearing the pointer
prevents a later finalizer from freeing it twice. External bytes are accounted
to the collector through a major-heap carrier. Review this trusted lifetime
mechanism in `runtime/pref.c` alongside the raw-memory interface.
Reclamation on exceptional exits is GC-dependent, with no prompt-cleanup
guarantee. No exception-safety theorem is claimed.

## Reproducible checks

- `make -s vox-library` checks all semantic definitions, existing proofs,
  implementations and the sealed public interface in bytecode and native modes.
- `python3 verification/benchmarks/lz4_boundary_check.py` copies only the public
  CMIs into an isolated directory, separately compiles `vox_lz4_public_client.ml`,
  derives byte identity from the two actual calls and exported total theorem,
  and links/runs it in bytecode and native modes. It checks Lambda output for
  erased semantic dependencies and rejects access to hidden implementation
  modules, runtime use of ghost contents, and a false compression identity.
- `python3 verification/benchmarks/lz4_finalizers.py` checks live-buffer GC
  safety, explicit release without double reclamation, and GC reclamation after
  simulated synchronous `Out_of_memory` exits following actual compressor and
  decoder processing, in bytecode and native modes. This is not allocator fault
  injection inside the public calls.
- Existing LZ4 differential, malformed-input, maximum-size and interoperability
  regressions continue to exercise the same public mutable entrypoints.
