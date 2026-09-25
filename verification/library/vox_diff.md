# Verified Myers diff

Run the byte-string demo from the worktree root after `./dev init`:

```sh
scripts/run-diff-demo
scripts/run-diff-demo 'abab' 'baba'
scripts/run-diff-demo '' 'hello'
./dev test vox/diff.ml
./dev test vox/diff_rejected.ml
scripts/check-diff-erasure
```

The library accepts integer lists; the executable maps bytes to integers.
`Keep x` consumes and emits `x`, `Delete x` consumes `x`, and `Insert x`
emits `x`. Application rejects mismatched consumed values and leftover input.
Substitution uses deletion plus insertion. Keep costs zero; both edits cost one.

## Public contract

`vox_diff.mli` exposes a total `diff old fresh`. Each input may contain at most
1,000,000 elements. Accepted inputs always return `Ok script`; the only error
is `Input_too_large`.

The result refinement proves:

- `source script === old` and `target script === fresh`;
- `apply old script === Some fresh`;
- `cost script = Vox_diff_metric.metric old fresh`;
- the input-size limits, nonnegative cost, and at most
  `size old + size fresh` operations.

`optimal_at old fresh computed other` proves that **every** script `other`
whose application produces `fresh` has cost at least that of `computed`.
There is no bound on the competing script. `apply_characterization` connects
application with the source/target specification. `inverse_patch` proves that
applying the inverted script to its target restores its source;
`invert_correct` also proves cost preservation.

`diff.ml` includes a separately compiled total client that uses only the public
interface to derive application, inversion, and comparison against an arbitrary
competing script. `diff_rejected.ml` rejects forged optimality evidence and a
wrong patch result, and omission of the competing-script validity premise.

## Executable algorithm and tie rule

At edit depth `d`, the frontier has `d + 1` slots for diagonals
`-d, -d + 2, ..., d`. A present slot stores the selected old-input position,
residual input lists, and a shared reverse script. An absent slot has no
retained candidate. Pruning preserves an optimal continuation; it does not
represent every path of that edit depth. An edit consumes one list head,
then `snake` consumes
all equal heads as Keeps. Reconstruction reverses the selected script once.
There are no list-index searches, copied full frontiers, or appended scripts.

For two legal predecessors on the same destination diagonal, compare their
old-input positions **after the edit and before the snake**. Choose deletion
only when its position is strictly greater; choose insertion on equality.
Use the available candidate when the other edit is impossible. Frontier scans
run from the lowest diagonal upward. These rules fix the returned script;
for example, `a` to `b` returns `Delete 'a'; Insert 'b'`.

## Direct proof

`vox_diff_spec.ml` defines operations, application, inversion, sequence size,
and suffix relations. `vox_diff_metric.ml` defines the usual insertion/deletion
distance recurrence independently of frontier search and proves:

1. Every script's cost bounds the distance between its source and target.
2. Removing equally many elements from both input prefixes cannot increase
   distance (`strip` and `crop`).
3. Distance is nonnegative, bounded by the combined input length, and zero
   only for equal sequences.

The runtime functions in `vox_diff.ml` maintain refined invariants directly.
`dominance` applies the suffix theorem to candidates on the same diagonal.
`snake` preserves remaining distance. `branches` shows that, for an unfinished
settled entry, some edit reduces the remaining distance budget by one.
`advance` preserves the existence of such an entry after pruning. `search`
therefore returns a script costing at most the initial mathematical distance.
The independent script lower bound proves equality and universal optimality.
The exhausted-search branch uses checked `unreachable_ ()`.

Every invocation of these model computations and proofs from the algorithm
is inside `ghost_`. Neither the distance recurrence nor an output checker
runs during diff. The reverse scripts contain reconstruction data, with no
proof certificates. Ghost arguments may leave dummy ABI arguments in bytecode;
they carry no model or proof computation. The erasure audit checks emitted
bytecode and native Lambda call targets, including reconstruction, and
rejects surviving proof,
metric, or bigint computations in the executable diff functions.

## Termination and resource bounds

Let `N` and `M` be input lengths, `S = N + M`, and `D` the minimum edit cost.
The checked invariants establish `S <= 2,000,000`, `0 <= x <= 1,000,000`,
nonnegative edit depth, frontier width `depth + 1`, and
`depth + remaining_fuel <= S`. Search starts with fuel `S` and strictly
decreases it. Snake decreases the mathematical length of its old-input tail.
Frontier traversal, reconstruction, and input validation use structural
recursion. All machine-integer increments and decrements have proved bounds.
No proof depends on wrapping arithmetic behaving like unbounded arithmetic.

The invariants and loop structure give these operational bounds:

- At most `D + 1 <= S + 1` frontier levels and slots in any one frontier.
- At most `(D + 1)(D + 2)/2` calls to snake, including the initial call.
- At most `N` Keep steps per snake and at most `S` reconstructed operations.
- Each length validation inspects at most 1,000,001 input cells.

The elementary sums above are derived from the checked width and descent
invariants; a separate allocation/time cost semantics is not mechanized.
The sharper amortized Myers time bound and a linear-space reconstruction
variant are not proved here. Shared reverse scripts can retain many candidate
paths. Totality follows Vox's resource-exhaustion convention: the accepted
length limit does not promise that every worst-case input fits available RAM
or the runtime stack.

Tests cover all 3,969 pairs of binary sequences of length at most five,
200 seeded random pairs, all byte values and extreme integer payloads,
empty inputs, repeated elements, deterministic ties,
a 10,000-byte common prefix, malformed patches, the exact million-element
limit, and rejection above the limit. Edit cost is compared with an independent
row-based dynamic-programming oracle. Bytecode, native, and both principal
variants run the same tests. No compiler changes or new trusted axioms are used.
