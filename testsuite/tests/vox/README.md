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
| Expression evaluation | `expressions.ml` | Structural recursion establishes terminating evaluation of an inductive expression tree. |
| Numerical recursion | `numerical.ml`, `fibonacci.ml` | Decreasing measures establish totality; tail-recursive and fast-doubling results equal naive Fibonacci. |
| SMT encoding | `verification.ml`, `equality.ml` | Source types map consistently to scalar and opaque SMT sorts. |

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

SMT datatypes add reasoning about tuples, records, and nonrecursive variants.

Structural recursion does not fix the existing totality loophole through
ordinary negative datatypes. A successful demo is not a claim of global
soundness; never execute known-divergent examples.

## Additional demos

`checked_windows.ml` checks input bounds with `assume_`, catches invalid
input, and derives the width with `refine_`.

`clamp.ml` proves bounds, identity, and idempotence for arbitrary inputs using
explicit definition lemmas.

`expressions.ml` evaluates an inductive expression tree with a caller-supplied
input. At this stage it establishes termination.
