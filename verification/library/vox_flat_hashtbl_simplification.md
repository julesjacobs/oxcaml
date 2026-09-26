# Flat hash table simplification, 25 September 2026

Status: compiled and locally retained dirty changes above `ec71493f35057e3631604380cec782f2f9132345`.
Independent boundary acceptance remains with the catalogue coordinator. Nothing
has been staged, committed, pushed or published in this continuation.
The source review input is `research/demo-simplification-review-20260925/collections.json`,
entry `flat-hash-table` (the entry is not in `systems.json`).

## Public specification

`vox_verified_flat_hashtbl.mli` now defines a complete finite binding model:
`Map.t = (Key.t * value) list`, distinct modulo `Key.equal`. Lookup, distinctness,
agreement, extensional equality, erase, put and cardinality have visible checked
equations. Physical holes and the separate public absence predicate are gone.
Creation and clearing promise the exact empty binding list. Cardinality, key
equivalence, arbitrary-query lookup, replacement/removal, capacity and exact
heap frames remain specified. The physical model remains visible in the
separately identified trusted storage contracts; this change does not reduce
storage trust.

`vox_table_bindings.ml` implements the equations and derived elimination lemmas.
`vox_table_bindings_bridge.ml` proves compaction preserves arbitrary-key lookup,
distinctness, agreement/equality, erase and put. The facade proves that binding
count equals storage live count. Compaction is ghost-only. The public CMI hides
these bridges and storage invariants. See `vox_flat_hashtbl_review.md` for the
ordered transitive semantic review surface and all resource/trust conventions.

## Executable implementation and source readability

`vox_table_implementation.ml` separates existing-key replacement from
`insert_absent_hashed`. After rebuilding, the latter retries insertion directly;
the key is already proved absent, so it does not repeat key lookup. The private
`Proof` module names the absence-after-rebuild, put-after-rebuild and
equal-key replacement arguments. The runtime branches now contain compact calls
instead of repeated map congruence/distinctness/transitivity chains. Local
operation contracts and the heap update law remain near the code they justify.
This intentionally increases proof-source size while making execution easier
to follow.

`vox_table_vacancy.ml` uses the checked `Progress.not_exhausted` fact and a ghost
rank. Its runtime loop has no rank increment, rank bound check or impossible
exhaustion raise. The emitted Cmm returns two unboxed scalars, allocates no scan
result, and leaves only an unused ghost-rank argument placeholder. Emitted
driver functions also contain no calls to the extracted ghost proof helpers. This is an
execution check distinct from the source-readability assessment. The original
tombstone preference, capacity-16 insertion path, specialized key search,
load-factor checks and growth limit remain.

The current scanner retains short local route/mask/progress proof blocks; moving
all of those into another file would add parameter plumbing without simplifying
the execution. No global claim that every table proof module is minimal is made.

## Combined traversal: checked experiment, rejected

A combined existing-key/vacancy probe was implemented and verified, including
remembering the first tombstone while searching subsequent groups for an
existing equivalent key. Positive, rejection and erasure checks passed. Five
alternating benchmark repeats with the installed native compiler showed some
build/churn improvements, but existing-key replacement regressions of roughly
6–17% in several sampled workloads. The combined traversal is not retained.
Its source and measurements are preserved under
`_build/hashtable-simplification/rejected-combined/`.

The retained-driver comparison uses the committed driver with the current
finite-binding facade and current vacancy scanner as its baseline; it isolates
the retry/control-flow change, not the removal of the vacancy rank. Three
alternating repeats cover mixed-hash sizes 14, 4096 and 65536 and constant-hash
size 256, integer/string payloads, build/hit/miss/replace/churn. Raw samples,
medians, allocations and executable hashes are in `retained-measurements.json`.
Allocations were equal in every sampled case. An initial large-table integer
replacement ratio of 1.110 did not reproduce in a five-repeat follow-up
(`retained-large-followup.json`: integer 0.962, string 0.951). Large-table
integer churn varied from 1.016 to 1.067. These short local runs establish no
general speed or non-regression guarantee; the changes are retained for simpler
control flow and removal of demonstrably redundant execution.

## Checked evidence and remaining obligations

All checks use this worktree's installed compiler built at `d143961f17`; the
compiler was not reinstalled. The library was rebuilt in both backends.
The isolated public client passes both backends, all 24 rejection runs and
Lambda erasure. Runtime regressions include collisions, tombstone replacement,
distinct-but-equivalent keys, 10,000 differential operations, 65,536-entry growth,
GC/pointer payloads and rebuilds. The existing ownership expect test is
byte-identical in both engines with and without `-principal`.

`_build/hashtable-simplification/final-provenance.json` identifies the exact dirty
snapshot, compiler hashes, semantic review spans, implementation/proof sources
and evidence. `flat_hashtbl_vacancy_erasure.py` checks emitted native scan code.
No new axiom, opaque semantic predicate, runtime certificate or final checker
was added. Allocation success, general termination, exception-safe reclamation,
concurrency and complexity remain unproved. Independent semantic/trust review
and authorization for any commit/PR/catalogue update remain outstanding.
