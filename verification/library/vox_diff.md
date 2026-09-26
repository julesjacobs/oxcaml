# Verified Myers diff

Run from the worktree root after `./dev init`:

```sh
scripts/run-diff-demo
scripts/run-diff-demo 'abab' 'baba'
scripts/run-diff-demo '' 'hello'
./dev test vox/diff.ml
./dev test vox/diff_rejected.ml
scripts/check-diff-erasure
```

## Exact human-review surface

Read these two public interfaces, in order:

1. **`verification/library/vox_diff_spec.mli`**: integer-sequence and edit
   types; complete checked equations for `source`, `target`, `cost`, `apply`,
   `invert`, `size`, `script_size`, and `minimum_cost`. These are all the
   observations used by the public claims. The minimum-cost recurrence
   decreases the sum of input lengths; it contains no fuel, frontier,
   certificate, suffix invariant, or opaque auxiliary predicate.
2. **`verification/library/vox_diff.mli`**: the actual total `diff` operation,
   accepted size limits, permitted error, deterministic tie policy, output
   bounds, arbitrary competing-script optimality, and inversion theorems.

Their transitive semantic dependencies are the standard finite inductive
list, option and result types, machine integers with ordinary value equality,
and mathematical signed integers. The shared primitive review anchors are
`stdlib/list.mli`'s inductive list declaration, `stdlib/stdlib.mli`'s result
sum, and `stdlib/bigint.mli`'s `t`, `add`, numeric equality/comparison and
integer-literal interpretation. `None`/`Some`, `unit`, Boolean connectives and
`===` use Vox's built-in algebraic types and logical value equality. Bigint
addition and comparisons in these contracts are exact, not wrapping machine
arithmetic. The compiler's refinement/totality checker, inductive-type rules,
SMT encoding and solver, and runtime primitive implementations are trusted;
no new axiom or external declaration is added. The resource-exhaustion exclusion is explicit in the introductory comment
of `vox_diff.mli`.

Those primitive meanings and the two public interfaces exhaust the semantic
review surface. No claim depends on an uninterpreted predicate in a private
module. Each equation is checked against an implementation; it is not an
assumed axiom. The recursive equations decrease finite inputs; `apply_characterization`
completely determines success, failure and output from `source` and `target`. The implementation of an equation
can change without changing the claim a reviewer is being asked to accept.

`vox_diff_spec.ml` implements those equations; its private fuel implementation
and the proof of the distance recurrence are hidden by `vox_diff_spec.mli`.
`vox_diff.ml` contains private `Proof` and `M` modules for suffix reasoning,
reconstruction, and optimality induction, together with the executable
frontier code and erased proof calls. Private `Entry_proof`, `Choice_proof`,
and `Frontier_proof` modules collect record-preservation, selection and
frontier-reconstruction lemmas. The executable bodies keep bounds before
arithmetic and call those lemmas after constructing entries or frontiers.
`vox_diff.mli` exposes none of those
modules, helpers, representations, or refinement invariants. There is no
separate importable metric/proof module and no module-type-of re-export.

## Public guarantees

`Keep x` consumes and emits `x`, `Delete x` consumes `x`, and `Insert x`
emits `x`. Application rejects mismatched consumed values and leftover input.
Keep costs zero; both edits cost one. Substitution uses deletion plus insertion.

For inputs of at most 1,000,000 elements each, `diff` always returns a script
whose application to the source produces the target. Its cost equals the
specified minimum cost and its length is at most the combined input length.
The only error is `Input_too_large`, exactly for an oversized input.

`optimal_at` derives the cost comparison against **any** script that applies
successfully to the same source and target. There is no size restriction on
that competing script. `inverse_patch` restores the source from the target;
`invert_correct` also proves source/target exchange and cost preservation.
These theorem calls can be erased with `ghost_`.

## Executable algorithm and proof boundary

At edit depth `d`, the frontier has `d + 1` slots for diagonals
`-d, -d + 2, ..., d`. A present slot stores an old-input position, residual
input lists, and a shared reverse script. An absent slot has no retained
candidate; pruning preserves an optimal continuation rather than every path.
An edit consumes one list head, then `snake` greedily consumes equal heads as
Keeps. Reconstruction reverses the selected script once. On equal post-edit
old-input positions, insertion wins. For example, `a` to `b` returns
`Delete 'a'; Insert 'b'`.

The private proof establishes an independent script lower bound, synchronous
suffix cropping, frontier dominance, preservation of remaining distance by
snake, and progress of a settled unfinished frontier. `search` returns a
script costing at most the mathematical distance; the lower bound proves
universal optimality. An unfinished frontier has strictly positive ghost
budget, which decreases at each recursive search call.

All calls from diff to model computations or proofs are inside `ghost_`.
Neither the distance recurrence nor an output checker runs during diff.
Reverse scripts contain reconstruction data, with no proof certificates.
Ghost arguments may leave dummy ABI arguments, but no proof computation.

## Resource bounds and limitations

For input lengths `N` and `M`, let `S = N + M` and minimum edit cost `D`.
Checked internal invariants establish `S <= 2,000,000`, bounded positions,
nonnegative depth, frontier width `depth + 1`, and
`depth + budget <= S`. Search decreases the ghost budget, initially `D`;
snake decreases its ghost old-input tail length. Search has no executable
fuel parameter, arithmetic, or exhausted-fuel branch. Other traversals use
structural recursion. Machine-integer increments have proved bounds.

The loop structure and checked invariants imply at most `D + 1` levels and
slots per frontier, at most `(D + 1)(D + 2)/2` snake calls, at most `N` Keep
steps per snake, and at most `S` output operations. Each size validation
inspects at most 1,000,001 cells. The aggregate sums are derived, not a
mechanized allocation/time cost semantics. The sharper amortized Myers bound
and linear-space reconstruction are not proved. Shared reverse scripts may
retain many candidate paths. The length limit is not a promise that every
worst-case input fits available RAM or stack.

## Boundary and execution evidence

`testsuite/tests/vox/diff_public_client.ml` imports only `Vox_diff_spec` and
`Vox_diff`. It derives patch correctness, inversion and arbitrary-script
optimality, and proves the error case impossible for accepted input sizes.
It also derives the former complete recursive `apply_def` equation from
`apply_characterization` and the public source/target equations, preserving
its semantic consequences (the public theorem name changes).
`scripts/check-diff-erasure` compiles it in a directory containing only the
two public `.cmi` files, in bytecode and native principal modes. The same
script audits executable Lambda call targets, including reconstruction, for
surviving model, proof, certificate or bigint computations. It additionally
checks that search has no integer arithmetic, comparison, or trap, and that
`diff` has no integer addition for initial fuel.

`diff_rejected.ml` rejects forged optimality, a wrong patch result, omission
of competing-script validity, access to private reconstruction, and access
to private metric fuel. Runtime tests cover all 3,969 short binary input
pairs, 200 seeded random pairs, all byte values, extreme integer payloads,
empty inputs, repeated elements, ties, a 10,000-byte common prefix, malformed
patches, the exact million-element limit and oversized-input rejection.
An independent dynamic-programming oracle checks edit cost. The tests run in
bytecode, native, and both principal variants.
