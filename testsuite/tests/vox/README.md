# Vox demos

An executable tour of the PR stack, not a replacement for detailed regression
tests. Each PR adds its demos; run every demo present at the current checkout:

```sh
./dev test vox/
```

Expect files show types, results, and nearby rejected programs. Definitions
whose modes matter live inside modules to avoid the interactive toplevel's
legacy-mode defaults.

| Stage | Files | What is established |
| --- | --- | --- |
| Dev loop | `smoke.ml` | The expect-test workflow runs. |
| Totality | `totality.ml` | Totality constrains function values, not effects producing integers. |
| Refinements | `refinements.ml`, `unchecked.ml`, `principal.ml` | Wrappers and scope checking work; predicates are not yet proved. |
| Dependent functions | `dependent.ml` | Results and recursive callback domains can depend on arguments. |
| Assume | `assume.ml`, `assume_runtime.ml` | Predicates are checked at runtime, including under `-noassert`. |
| SMT interface | `smt.ml`, `smt_solver.ml` | Queries serialize and the solver can prove or refute them. |
| Structural recursion | `structural.ml` | Checked inductive values support terminating recursive traversals. |

These are different claims: **typechecked**, **runtime-checked**, and
**statically verified**. In particular, `unchecked.ml` intentionally exposes
the current lack of VC checking. When VC generation lands, change that demo
to expect rejection; do not preserve acceptance as intended behavior.

`principal.ml` exposes the current mode-crossing failure for ordinary
polymorphic comparisons under `-principal`. Other predicate demos bind the
same integer primitives at monomorphic types to keep their purpose visible.

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

VC generation should turn the unchecked examples into real proof obligations,
with accepted guarded decrements and rejected wraparound cases. Numerical
recursion can then demonstrate a refined recursion combinator, with its
implementation's trust boundary stated explicitly.

Structural recursion does not fix the existing totality loophole through
ordinary negative datatypes. A successful demo is not a claim of global
soundness; never execute known-divergent examples.
