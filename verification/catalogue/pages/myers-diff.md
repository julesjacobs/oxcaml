title: Myers diff
blurb: A generic diff with the right source and target and the minimum number of insertions and deletions.
status: owner-review
date: 4 October 2026
sources:
  - verification/library/vox_diff_spec.ml — Pure definitions: diffs, source, target, and cost
  - verification/library/vox_diff.mli — Public operations and their contracts
  - verification/library/vox_diff.ml — Myers frontier search and private proofs
  - testsuite/tests/vox/diff_public_client.ml — A generic verified client
  - testsuite/tests/vox/diff_rejected.ml — Rejected claims and comparisons
  - testsuite/tests/vox/diff.ml — Runtime tests against an independent distance oracle
  - testsuite/tests/vox/diff_boundary.ml — Public client compilation and proof erasure
  - verification/demos/diff_demo.ml — Command-line byte-string demo
---
`Vox_diff.diff equal old fresh` computes a minimum-cost diff between two lists. A diff is a list of `Keep x`, `Delete x`, and `Insert x`. Its source contains the kept and deleted elements; its target contains the kept and inserted elements. Each insertion or deletion costs one; keeping an element costs zero.

The supplied `equal` must decide exact equality of elements. This makes the source and target guarantees exact even for a user-defined element type. Any minimum-cost diff is allowed.

## Interface

These are the complete semantic definitions. Keeps cost zero; insertions and deletions cost one. A substitution therefore costs two.

@code verification/library/vox_diff_spec.ml

`[@def]` lets proofs unfold a definition. `===` is logical equality, and `0Z` and `1Z` are mathematical integers. `logical_data` permits elements to occur in these logical definitions.

The result contains `edits` and an erased `optimality` proof: no other diff with the same source and target costs less. The proof field refers to the earlier `edits` field. The operation's contract connects those edits to the input lists. Applying and inverting are specified through source, target, and cost.

@code verification/library/vox_diff.mli

## Trusted base

The pure model and public contracts are the review surface. The distance recurrence, reconstruction, frontier invariants, and optimality proofs are private in `vox_diff.ml`. No new axiom or external primitive is introduced; the trusted base is Vox's checker, SMT encoding and solver, and the shared list and integer primitives.

## Scope

`diff` raises `Invalid_argument` if either input exceeds 1,000,000 elements. This cap bounds machine-integer positions. Every successful return carries optimality. Time, memory, and stack usage are not specified. The command-line demo encodes bytes as integers; the library supports generic elements.

## A verified client

This generic client calls `diff` normally, then uses the result's `optimality` field to compare its edits against an arbitrary competing diff. It also checks that applying the diff works forwards and applying its inverse works backwards. All proof calls are erased.

@code testsuite/tests/vox/diff_public_client.ml "let verified_client" "  edits"

## A rejected claim

Deleting and reinserting the same element costs two; keeping it costs zero. The former cannot be claimed to cost no more than the latter.

@code testsuite/tests/vox/diff_rejected.ml "let nonminimal () =" "(() : {u : unit | cost edits <= cost other});;"

The same test rejects a wrong patched result, a cost comparison without matching source and target, a dishonest equality function, and access to private helpers and the distance recurrence.

## Reproduce

```
./dev test vox/diff.ml vox/diff_rejected.ml vox/diff_boundary.ml
scripts/run-diff-demo 'ABCABBA' 'CBABAC'
```

Runtime tests compare against an independent dynamic-programming oracle on all 3,969 pairs of binary words of length at most five, 200 random pairs, and boundary cases. A variant-valued example exercises the generic API. The boundary test compiles clients using only public interfaces and audits bytecode and native Lambda: the search calls the supplied comparison and algorithm helpers, with no surviving metric or proof computation.
