# LZ4 review boundary

## Read in this order

Unprefixed file names below are relative to `verification/library/`; paths
starting with `runtime/`, `backend/`, `testsuite/` or `verification/` are relative
to the repository root. The public executable
interface is `vox_lz4.mli`. It contains the normal-return compressor and decoder
contracts, executable byte-identity contract, and total composition theorem.
It names no streaming implementation or proof module.

The complete allocation-independent semantic definitions are in these files:

1. `vox_lz4_spec.ml`: compression relation, decoder observation relation,
   error types and the ordinary string/error result.
2. `vox_lz4_spec_parse.ml`: byte conversion, token nibbles, extended lengths
   and parser status classification.
3. `vox_lz4_spec_decode_bytes.ml`: reverse-order decoded bytes, distance lookup,
   literal appends, overlapping match appends, exact parser/capacity/terminal
   decisions, the total decoder and its output-byte observation predicate.
4. `vox_lz4_spec_bytes.ml`: bounded source access, length-extension encoding,
   literal bytes and individual wire bytes.
5. `vox_lz4_spec_wire.ml`: exact wire layout of each sequence and terminal run.
6. `vox_lz4_spec_token.ml`: token and little-endian offset encoding.
7. `vox_lz4_spec_plan.ml`: sequence/plan types and validity, including the final
   five literal bytes and twelve-byte last-match restriction.
8. `vox_lz4_spec_match.ml`: source-distance equality, maximal match search,
   candidate validation and the four-byte hash. The exclusion below applies
   to its one embedded proof body.
9. `vox_lz4_spec_hashes.ml`: immutable most-recent-position table lookup.
10. `vox_lz4_spec_scan.ml`: exact visited positions, hash-table updates, match
    jumps and termination measure of the compressor model.

The definitions fix the actual scanner's wire output and every successful
output byte. None of these modules uses a raw allocation, location or heap.
The result has one discriminator and contains no allocation witness.

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

The public semantic dependency closure additionally uses:

- `vox_string_view.mli`: trusted immutable string contents, length and indexed
  byte observation. No runtime iarray is made by the contents selector.
- `vox_sequence.mli`: `iarray_get`, the total bounded primitive array read.
- `vox_iarray.mli`: `get`, `at` characterized by `at_get`/`at_outside`, and
  extensional array equality.

Standard integer arithmetic/comparison, iarray length, lists/options/records,
character primitives and 32-bit word operations retain their language/compiler
meanings. Byte conversion assumptions are explicit in `vox_lz4_spec_parse.ml`;
hash word primitives are in `vox_lz4_spec_match.ml`. Refinement, totality and
proof-erasure checking and the SMT backend are trusted.

## Executable ownership and primitive boundary

The pure public model does not remove any ownership assumption from the
executable verification. Review these contracts and their complete meanings:

- `pref.mli`: location, heap and affine-token types, finite-map operations and
  laws in `Heap`, and ownership primitives. Heaps and tokens are typed by
  their payload; every LZ4 heap and token has payload `Raw_memory.contents`.
- `ghost_pref.mli`: erased ownership adapters and heap aliases.
- `raw_memory.mli`: allocation identity, length, byte range, `location_law`,
  complete `range_def`/`footprint_def`/`covers_def` characterizations, memory
  access and free contracts.
- `borrow_iarray.mli`: owned-array access, including integer reads/writes and
  the `vox_iarray.mli` update equations they use.
- `vox_lz4_string_copy.mli`: final-copy contract, with complete initialized
  storage and byte-prefix meanings in `vox_lz4_spec_storage.ml` and
  `vox_lz4_heap_bytes.ml`.

Trusted implementations remain `runtime/borrow.c`, `runtime/pref.c` and the
integer-array/raw-byte lowerings in `backend/cmm_builtins.ml`.

`vox_lz4_spec_decode.ml` is now an internal heap model. The checked
`vox_lz4_decode_bytes_proof.initial_observations` theorem equates status, count
and output-byte observations in both directions for every initial heap and
block at a valid capacity. Its prefix/copy induction and intermediate
`heap_matches` invariant are proof internals. Mutable decoder contracts still
preserve exact buffer identity, count and mathematical heap state.
`vox_lz4_decode_bytes_roundtrip.ml` proves the pure plan/wire composition
without requiring any allocation to exist. Cursor-progress lemmas justify
erasing the mutable decoder's fuel counters; the compressor counter is fixed
by its current position and is also erased. Input and capacity guards remain.
Neither proof module belongs to
the public semantic dependency closure.

## Domain and limitations

Blocks and decoded capacities are limited to 4 MiB. Source sizes above the limit
raise `Invalid_argument`; allocation can raise `Out_of_memory`. The total theorem
requires capacity at least the source length and at most 4 MiB. Ordinary mutable
entrypoints have normal-return contracts, not totality contracts.

Detailed diagnostic reasons/positions are not specified by the total model;
its three statuses and their result classification are specified. Compression need not shrink every source.
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
  implementations and the sealed public interface once, in the bytecode
  compilation; the native compilation of the same sources passes
  `-smt-assume-verified`.
- `python3 verification/benchmarks/lz4_boundary_check.py` copies only the public
  CMIs into an isolated directory, separately compiles `vox_lz4_public_client.ml`,
  derives byte identity from the two actual calls and exported total theorem,
  and links/runs it in bytecode and native modes. It checks Lambda output for
  erased semantic dependencies and rejects access to hidden implementation
  modules, runtime use of ghost contents, a false compression identity and
  a false decoder-status claim. The isolated client has no heap-model, raw-memory or ownership CMIs.
- `python3 verification/benchmarks/lz4_finalizers.py` checks live-buffer GC
  safety, explicit release without double reclamation, and GC reclamation after
  simulated synchronous `Out_of_memory` exits following actual compressor and
  decoder processing, in bytecode and native modes. This is not allocator fault
  injection inside the public calls.
- Existing LZ4 differential, malformed-input, maximum-size and interoperability
  regressions continue to exercise the same public mutable entrypoints.
