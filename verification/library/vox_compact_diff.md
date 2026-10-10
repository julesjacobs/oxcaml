# Compact diff alternative

This version sits alongside `Vox_diff`. Read `vox_compact_diff_spec.ml` for
its pure definitions and `vox_compact_diff.mli` for its operation contracts.

```ocaml
type 'a operation = Keep of int | Delete of 'a | Insert of 'a
type 'a diff = 'a operation list
```

For example, changing `[1; 2; 3]` to `[1; 4; 3]` gives:

```ocaml
[Keep 1; Delete 2; Insert 4; Keep 1]
```

`Keep n` copies the next `n` source elements. `Delete x` consumes a source
element equal to `x`. `Insert x` adds `x` to the target. All source elements
must be consumed. Negative counts, keeps beyond the end, and mismatching
deletions fail. Zero-length keeps are allowed in supplied diffs; generated
diffs use positive counts and merge consecutive keeps.

## Spec

`patch edits old` is the pure meaning of a patch: either `Some fresh` or
`None`. `relates edits old fresh` means `patch edits old = Some fresh`.
Cost counts insertions and deletions; keeps cost zero.

`diff equal old fresh` returns `edits` satisfying that relation and an erased
proof that every other diff relating those same inputs costs at least as
much. The result's `old` and `fresh` fields are ghost fields: they anchor the
proof to the inputs without retaining those lists at runtime. Counts alone
cannot reconstruct the original inputs, and a diff that is optimal for one
pair of inputs need not be optimal for every pair it can patch.

`apply` agrees exactly with `patch`. `invert` swaps insertions and deletions
and leaves keeps unchanged. Its cost is unchanged; `invert_correct` proves
that applying the inverse to the target recovers the source.

Counted keeps do not validate unchanged content. For example, `[Keep 2]`
works on any two-element source. Deleted content is still checked.

## Implementation and comparison

The implementation runs the existing verified Myers algorithm, then compresses
its element-bearing keeps into counts. Compression preserves the input/output
relation and edit cost. A private expansion proof transfers optimality to
arbitrary compact competitors. Expansion runs only in proofs.

The final representation omits unchanged elements. The algorithm still
constructs the original diff before compression, so this version does not
reduce the search's peak memory use. It retains the original 1,000,000-element
input limit and raises `Invalid_argument` beyond it.

Compared with the original, the compact version needs a source to apply a
patch and a relation to specify its endpoints. The original can reconstruct
both complete lists from the diff alone. Both retain deleted content, support
inversion, and guarantee minimum edit cost.

## Checks

```sh
./dev test vox/compact_diff.ml vox/compact_diff_rejected.ml
```

The tests exercise bytecode and native execution, 3,969 exhaustive input
pairs, 200 seeded random pairs against an independent distance oracle,
generic elements, malformed patches, inversion, million-element keeps,
input limits, ghost-field erasure, and client proofs using the public
interface. Rejection tests check that unsupported claims are rejected.
