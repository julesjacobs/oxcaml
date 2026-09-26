# CDCL SAT solver

`Vox_cdcl.solve fuel n formula` accepts an in-memory CNF with the same input
limits as `Vox_sat.solve`. It uses a trail with decision levels and reason
clauses, unit propagation by scanning clauses, first-UIP conflict analysis,
learned clauses, and nonchronological backtracking. Learned clauses are
append-only. There are no watched literals, restarts, or clause deletion.

Private `Vox_sat_proof.proof_result` is the clause-learning interface. Each result contains a
runtime clause and a ghost derivation from the original formula.
`original_result` introduces an input clause. `resolve_result` learns a
resolvent, and `database_cons` preserves entailment of a learned clause.
These functions have Vox-checked refinements. The CDCL loop carries a
`database_valid` refinement through every recursive call. An incorrect
conflict-analysis choice can lead to `Unknown`, but cannot make an unrelated
clause into a proved learned clause.

At a root conflict, the solver resolves against assignment reasons until it
derives the empty clause. Public `Unsat` is nullary. The public `solve`
refinement proves `Vox_sat_spec.unsatisfiable n formula`;
`Vox_sat.unsat_at n formula assignment` derives that every assignment
falsifies the formula. Ghost derivations erase, and the solver does
not record or replay a proof trace at runtime.

Propagation scans input clauses through a total, verified clause scanner. If
the scan is stable and the current assignment is complete,
`scan_formula_complete` proves that assignment satisfies the input formula.
The SAT branch uses this result directly. It checks only that the returned
assignment has the requested length; it does not reevaluate the formula.

The array-based search and its resource use are not proved total. Vox rejects
`@ total` before checking a decreasing measure: standard `ref`, `Array.make`,
and `Array.length` are partial. `typing/typecore.ml` also marks every `while`
and `for` loop partial. Vox cannot currently state an indexed access bound
using a mutable array in a refinement predicate. The rejection tests are
`testsuite/tests/vox/sat_cdcl_totality_rejected.ml`,
`testsuite/tests/vox/sat_cdcl_loop_totality_rejected.ml`, and
`testsuite/tests/vox/sat_cdcl_array_bound_rejected.ml`. A totality proof of this
implementation needs a state-aware array interface and explicit recursive
loops with decreasing bounds through search, propagation, and conflict
analysis. Assigning `@ total` to mutable primitives alone would add unchecked
contracts. The checked
`solve` refinement guarantees sound answers when it returns. The runtime fuel
budget charges input-formula scans, learned-clause visits,
conflict-analysis steps, and decisions; exhausted
fuel or a failed analysis returns `Unknown`. The solver permits at most 4,096
learned clauses and 65,536 learned literal occurrences.

[`Vox_cdcl_total`](vox_cdcl_total.md) uses persistent state and decreasing
fuel to prove termination while retaining clause learning and backjumping.

## Validation

```sh
./dev test vox/sat_cdcl.ml
make -s vox-library
_install/bin/ocamlopt -extension refinement_types \
  -I _install/lib/ocaml/vox _install/lib/ocaml/vox/vox_borrow.cmxa \
  verification/benchmarks/vox_cdcl_bench.ml -o _build/vox_cdcl_bench.exe
_build/vox_cdcl_bench.exe
```

The test checks clause learning, a direct `unsat_at` invocation, budget errors,
729 formulas against a truth-table oracle, and a fixed 50-variable UNSAT
instance that makes a nonchronological backjump.

See [the review boundary](vox_sat_boundary.md). Kernel tests are separate
from ordinary solver clients.
