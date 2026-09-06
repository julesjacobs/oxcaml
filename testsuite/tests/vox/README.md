# Vox demos

An executable tour of the PR stack, not a replacement for detailed regression
tests. Each PR adds its demos; run every demo present at the current checkout:

```sh
./dev test vox/
```

When switching stack stages, rerun `./dev init` if requested: changes to the
serialized type representation invalidate installed interfaces.

Expect files show types, results, and nearby rejected programs. Definitions
whose modes matter live inside modules to avoid the interactive toplevel's
legacy-mode defaults.

| Stage | Files | What is established |
| --- | --- | --- |
| Dev loop | `smoke.ml` | The expect-test workflow runs. |
| Totality | `totality.ml` | Totality constrains function values, not effects producing integers. |
| Refinements | `refinements.ml`, `principal.ml` | Wrappers and scope checking work. |
| Dependent functions | `dependent.ml` | Results and recursive callback domains can depend on arguments. |
| Assume | `assume.ml`, `assume_runtime.ml` | Predicates are checked at runtime, including under `-noassert`. |
| SMT interface | `smt.ml`, `smt_solver.ml` | Queries serialize and the solver can prove or refute them. |
| VC generation | `verification.ml`, `unchecked.ml` | Branches and successful runtime checks prove introductions; false claims and wraparound are rejected. |
| Checked windows | `checked_windows.ml` | Runtime validation establishes ordered bounds; subtraction and a client budget are proved safe. |
| Logical equality | `equality.ml` | `===` is logical equality in predicates and a checked equality in `assume_`. |
| Definition lemmas | `definitions.ml` | Explicit unfolding proves calls; ignored lemmas do not expose equations. |
| Clamp laws | `clamp.ml` | Explicit equations prove interval bounds, identity, and idempotence. |
| Structural recursion | `structural.ml` | Checked inductive values support terminating recursive traversals. |
| Expression evaluation | `expressions.ml` | Structural recursion establishes termination; induction proves constant folding preserves wrapping-integer evaluation. |
| Numerical recursion | `numerical.ml`, `fibonacci.ml` | Decreasing measures establish totality; tail-recursive and fast-doubling results equal naive Fibonacci. |
| SMT encoding | `verification.ml`, `equality.ml` | Source types map consistently to scalar and opaque SMT sorts. |
| Bigints | `bigints.ml`, `bigint_fibonacci.ml` | Unbounded arithmetic, nonnegative decreasing measures, and Fibonacci proofs beyond machine bounds. |
| SMT datatypes | `datatypes.ml` | Native datatype reasoning covers tuples, records, variants, patterns, and recursive trees. |
| Int-list proofs | `int_lists.ml` | Structural induction proves append identities, associativity, and length and sum homomorphisms. |
| Int-set proofs | `int_set_intf.mli`, `list_int_set.mli`, `int_sets.ml` | A canonical list set exposes verified membership, size, and `===` extensionality through an abstract interface. |
| AVL-set proofs | `avl_sets.mli`, `avl_set_client.ml` | A valid AVL set exposes semantic `equal`; the demo distinguishes it from representation `=`. |
| Immutable arrays | `iarrays.ml`, `iarrays_ordinary.ml` | Immutable-array literals expose exact lengths and elements; safe reads expose normal-return bounds. |
| Bounded search | `array_search.ml` | A decreasing interval establishes termination, safe reads, and matching returned indices. |
| Standard lists | `standard_lists.ml` | Polymorphic lists support structural total functions, logical equality, refined partial operations, and total higher-order operations. |
| Functional queue | `functional_queue.mli`, `queue_client.ml`, `queue_rejected.ml` | An abstract two-list queue implements a sequence model; a separate client proves FIFO behavior and rejects empty dequeue. |
| Standard sets | `sets.ml` | Total comparators enable total operations; refined constructors and lookup expose membership facts while preserving element access. |
| Standard maps | `maps.ml` | Total comparators enable total operations; refined updates and lookup expose membership and value facts while preserving key and value access. |
| Persistent environments | `environments.ml` | Binding shadows its comparator class and preserves observations in a distinct class; retaining the outer environment restores scope. |
| Standard-set model | `avl_stdlib_set.ml` | Pointwise refinement relates the verified AVL implementation to `Set.MakeTotal`, with comparator compatibility explicit. |
| Sparse immutable arrays | `sparse_iarrays.ml` | A polymorphic `Map.MakeTotal` overlay proves read-after-write, overwrite, removal fallback, and safe base-array reads. |
| Regex matching | `regex.ml` | Derivative matching is sound and complete for an independent membership-derivation spec, including nullable and nested stars. |

`unchecked.ml`, accepted at the refinement-former stage, now demonstrates
rejection by VC generation. Solver-dependent tests require Z3 on `PATH` and
skip when it is absent; the default Linux CI job installs pinned Z3.

`principal.ml` checks mode crossing for ordinary polymorphic comparisons in
refinement predicates under `-principal`.

`dependent.ml` checks a client against a proposed well-founded recursion
combinator's signature. It supplies no implementation of that combinator and
does not establish termination through unchecked refinement introductions.

## Solver demo

At the SMT stage, `smt.ml` runs without Z3 and shows the actual serialization.
The real-solver demo uses the existing Dune libraries, rather than pretending
that the compiler already checks refinement VCs:

```sh
VOX_TEST_Z3=true dune runtest --workspace=duneconf/main.ws verification/tests
```

This runs `smt_solver.ml` against Z3 and compares its output with
`smt_solver.reference`, alongside the solver regression tests. Install Z3
4.16.0 explicitly; the test does not download it. Run this command separately
from `./dev`, since both use the worktree's Dune lock.

## Next milestones

`fibonacci.ml` uses static proofs and ordinary wrapping arithmetic. Both
implementations accept indices 0 through 90 and raise outside that range.
At 90, fast doubling's unused second component wraps; its result still fits.
The inductive proof helper still runs at runtime, pending ghost-code erasure.

`bigint_fibonacci.ml` gives both indices and results type `Bigint.t`, maps
negative inputs to zero, and computes Fibonacci 100 without overflow guards.

Structural recursion does not fix the existing totality loophole through
ordinary negative datatypes. A successful demo is not a claim of global
soundness; never execute known-divergent examples.

## Additional demos

`checked_windows.ml` checks input bounds with `assume_`, catches invalid
input, and derives the width with `refine_`.

`clamp.ml` proves bounds, identity, and idempotence for arbitrary inputs using
explicit definition lemmas.

`expressions.ml` now proves constant folding preserves evaluation for every
expression and input. Both evaluations use machine-integer wrapping semantics.

`array_search.ml` proves that returned indices are in range and contain the
target. It does not prove absence or first-match correctness. Its total `at`
observer returns zero outside the array; the result contract separately
establishes bounds.

The queue proves its tail-recursive reversal against an explicit
append/reverse model. Its representation stays behind a `.mli`. Proof helpers
execute at runtime, so this demo makes no amortized-cost claim.

`environments.ml` checks comparator-class distinction at the example boundary,
then statically proves preservation of optional lookup results. It restores
scope using the saved outer map; removing an inner binding does not restore a
shadowed value.

Sparse-array removal and commutation are refined-unit lemmas about the actual
`get` operation at an arbitrary valid probe. A proof functor accepts an abstract
`immutable_data` element type; integer and record clients instantiate it. The record client derives equality of reads
and restoration of base values from these contracts. The examples check bounds
and comparator-class distinction at runtime. The generic `get` operation also
preserves writable access to mutable elements.

`regex.ml` specifies membership by finite derivations: `Membership.valid r p`
checks the regex rules, and `Membership.word p` gives the derived word. Thus
membership of `s` in `r` means that some `p` is valid for `r` and has word `s`.
The spec contains no derivatives and permits empty repetitions in star.

The checked `sound` theorem constructs such a derivation whenever `matches`
returns true. The checked `complete` theorem proves acceptance for every valid
derivation, so rejection also excludes every derivation. Their derivative
lemmas construct derivations in both directions; contraction skips empty star
repetitions by structural recursion on the derivation.

`matches` runs the Boolean derivative algorithm without proof calls.
`recognize` additionally constructs a checked membership derivation on success.
Smart constructors remove empty alternatives, collapse identical alternatives,
annihilate concatenations with `Empty`, and eliminate `Epsilon` operands from
concatenation. Checked derivation transformations in both directions establish
that each rewrite preserves membership. These local rewrites run while
constructing derivatives.

The demo uses integer symbols and makes no complexity claim. Executable checks compare all 3,244 regexes of depth at most two over
symbols 0 and 1 against an independent split-based matcher on all 15 words of
length at most three. Rejection fixtures exercise the soundness and
completeness contracts.
