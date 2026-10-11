# Verified Myers diff

A separate [compact alternative](vox_compact_diff.md) stores keeps as counts
and retains single-element insertions and deletions.

Read `vox_diff_spec.ml` for the pure model, then `vox_diff.mli` for the
operations. The model is a concrete `'a diff`: a list of `Keep`, `Delete`,
and `Insert`. Direct definitions give its source, target, and cost.
`vox_diff.ml` hides the algorithm and its proofs.

`diff equal old fresh` returns a record containing `edits` with source `old`
and target `fresh`, and an erased `optimality` proof: every diff with those
endpoints costs at least as much. The proof field depends on the `edits`
field. `equal` must decide exact equality of elements. Any minimum-cost diff
is allowed. `apply` succeeds exactly
when its input is the diff's source; `invert` swaps source and target and
preserves cost.

`diff` raises `Invalid_argument` if either input exceeds 1,000,000 elements.
The cap bounds machine-integer positions. Its contract describes successful
returns; time, memory, and stack use are not bounded.

## Implementation

The Myers frontier stores one candidate per diagonal: an old-input position,
remaining input lists, and a reverse diff. Each step inserts or deletes one
element, then `snake` consumes equal heads. `choose` keeps the candidate
furthest along the old input; insertion wins ties. This tie rule is tested
but does not constrain the public spec.

A private distance recurrence supports proofs of reconstruction, suffix
bounds, frontier dominance, and preservation of minimum remaining cost.
A decreasing ghost distance proves termination. A lower bound on every diff proves optimality of the
result. The search executes no distance calculation or proof; reconstruction
reverses the selected diff once.

## Checks

After `./dev init`, run:

```sh
./dev test vox/diff.ml vox/diff_rejected.ml vox/diff_boundary.ml
scripts/run-diff-demo 'abab' 'baba'
```

`diff.ml` compares all 3,969 pairs of short binary words and 200 seeded
random pairs against an independent distance oracle. It also covers byte
values, extreme integers, generic variant elements, malformed patches,
inversion, repeated elements, ties, long common prefixes, and the size cap.

`diff_public_client.ml` checks the generic contracts using public interfaces.
`diff_rejected.ml` rejects false claims, a dishonest comparison, and private
helper access. `diff_boundary.ml` audits bytecode and native Lambda for
surviving metric or proof computation, checks that verified clients compile
to one `diff` call, and runs the byte-string demo.
