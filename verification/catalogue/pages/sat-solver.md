title: CDCL SAT solver
blurb: A CDCL SAT solver proved to terminate on every CNF formula within its size limits with `Sat` and a satisfying assignment or `Unsat` and a proof that no assignment satisfies it.
status: review-pending
date: 27 September 2026
sources:
  - verification/library/vox_cdcl_total.mli — Public interface of the demo solver, `solve_complete`, and two bounded variants
  - verification/library/vox_sat_spec.ml — Definitions: evaluation, satisfiability, unsatisfiability and the accepted inputs
  - verification/library/vox_sat_spec.mli — The same definitions as checked equations
  - verification/library/vox_sat.mli — DPLL solver and `unsat_at`
  - verification/library/vox_cdcl_total_proof.ml — Implementation and proofs: propagation, conflict analysis, learning, backjumping and the termination measure
  - verification/library/vox_cdcl_total.md — How the search works and why it terminates
  - verification/library/vox_sat_boundary.md — Reading order and the other solvers' contracts
  - verification/library/vox_cdcl.mli — Mutable CDCL solver, sound on return only
  - testsuite/tests/vox/sat_public.ml — Public-only client
  - testsuite/tests/vox/sat_boundary.ml — Compiles the client with only the public interfaces and checks that proof code is erased
  - testsuite/tests/vox/sat_cdcl_progress_rejected.ml — Rejected client
---
The demo is `Vox_cdcl_total.solve_complete n formula`, a CDCL solver with clause learning and backjumping over a CNF formula on variables `0` to `n - 1`. It is `total`: it terminates without raising on every input. For an input on which `Vox_sat_spec.classify_input` returns `None` (`0 <= n <= 256`, at most 4,096 clauses and 65,536 literal occurrences, every variable index in `[0, n)`) it returns `Sat assignment`, where `assignment` has length `n` and satisfies the formula, or `Unsat`, where `unsatisfiable n formula` holds: every one of the `2^n` assignments of length `n` falsifies the formula. It never returns `Unknown`. For any other input it returns the error that `classify_input` names. The termination measure (the number of clauses of at most `2n` literals not yet learned, times `n + 1`, plus the number of unassigned variables) is erased, as are the proofs; there is no runtime certificate, trace or final check of the answer.

The `statistics` in the report are unconstrained by the contract. Totality is a logical property: there is no bound on running time, memory or stack, and the search can take exponential time. The solver scans clauses to propagate and has no watched literals, restarts or clause deletion. The library also has four other solver entry points with weaker contracts, listed under Scope.

## Client example

From the public-only client. `{n : int | p}` is the type `int` refined by the predicate `p`; refinements on arguments are preconditions the caller must prove, and the refinement on the result is proved here. `(f @ total)` declares that `f` terminates without effects. `ghost_ (...)` is proof code, checked and then erased. `classify_input_def` and `check_def` state the definitions of `classify_input` and `check`, which the checker does not unfold on its own. `Vox_sat.unsat_at` turns `unsatisfiable` into the fact that a given list of booleans, of any length, falsifies the formula.

@code testsuite/tests/vox/sat_public.ml "let (decide_cdcl @ total)" "  result"

## A rejected program

The bounded solver `Vox_cdcl_total.solve fuel` may return `Unknown` for any nonnegative fuel, so a client cannot prove that fuel `n + 1` suffices. The same client with `solve_complete n formula` in the last line compiles (`complete_cdcl` in `sat_cdcl_total.ml`).

@code testsuite/tests/vox/sat_cdcl_progress_rejected.ml "let (unsupported_cdcl_depth @ total)" "Vox_cdcl_total.solve (n + 1) n formula"

@text testsuite/tests/vox/sat_cdcl_progress_rejected.compilers.reference

## Interface

The types of `vox_cdcl_total.mli` and the demo solver:

@code verification/library/vox_cdcl_total.mli "type statistics = {" "type input_error"

@code verification/library/vox_cdcl_total.mli "(** Complete CDCL on every accepted input" "| Unknown -> false} @@ total"

The specification is `vox_sat_spec.ml`. Evaluation:

@code verification/library/vox_sat_spec.ml "let[@def] rec lookup" "eval_clause assignment clause && eval_formula assignment rest"

and the results and the accepted inputs:

@code verification/library/vox_sat_spec.ml "let[@def] check n formula assignment" "  else None"

`well_sized n values` says that `values` has length `n` (for `n >= 0`), and `valid_formula n formula` that every variable index is in `[0, n)`; `clauses_fit` and `literals_fit` count clauses and literal occurrences. A missing variable evaluates to `false`. `let[@def]` also generates a lemma such as `check_def` stating the definition's equation, and `[@@decreases remaining]` gives a termination measure. `unsat_at`, in the DPLL module `Vox_sat`, extends `unsatisfiable` to assignments of any length:

@code verification/library/vox_sat.mli "val unsat_at" "@@ total"

## Trusted base

Nothing beyond the shared base. The proofs use `Bigint` and `Vox_sequence.length`, which the shared page covers.

## Scope

- The demo is `solve_complete`. The library's other solvers have weaker contracts, below.
- `Vox_cdcl_total.solve fuel`: the same search with a budget; on an accepted input, `Unknown` is allowed for any fuel `>= 0` and implies `statistics.steps = fuel`. Nothing relates `steps` to the work done, so an implementation that returns `Unknown` at once with `steps = fuel` meets the contract.
- `Vox_cdcl_total.solve_with_fallback fuel depth_fuel`: bounded CDCL, then a DPLL search if it returns `Unknown`; decides every accepted input when `fuel >= 0` and `depth_fuel >= n + 1`.
- `Vox_sat.solve`: DPLL with a fuel precondition `fuel >= 0` instead of an `Invalid_fuel` error, and its own `answer` and `report` types; `Unknown` is allowed.
- `Vox_cdcl.solve`: CDCL over mutable arrays. It is not `total`, its errors are unconstrained, and `Sat` and `Unsat` are proved sound only when it returns.
- `unsat_at` lives in `Vox_sat` and returns its result `@ ghost`; the clients call it inside `ghost_`.
- Only CNF over integer-indexed variables; no incremental solving, assumptions or proof output.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/sat_boundary.ml vox/sat_cdcl_total.ml vox/sat_cdcl_progress_rejected.ml vox/sat_cdcl_fallback_depth_rejected.ml vox/sat_solver_rejected.ml
```

`sat_boundary.ml` compiles, and so checks, the seven SAT modules and `Vox_sequence` with both compilers and the flags of `verification/library/build.sh`; copies only the `.cmi` files of `Vox_sat_spec`, `Vox_sat`, `Vox_cdcl` and `Vox_cdcl_total` into an empty directory, compiles `sat_public.ml` there, links and runs it; searches the `-dlambda` output of the three public modules for named proof functions and counts their remaining calls; and searches the native object of the proof module for named termination-measure helpers. `sat_cdcl_total.ml` checks the implementation and runs it as bytecode on 729 formulas, every sequence of three clauses drawn from the nine clauses over two variables with no repeated variable (against a truth table); runs the bounded solver on a random 3-SAT instance with 50 variables and 218 clauses; and tests 256 variables and inputs just past each size limit.
