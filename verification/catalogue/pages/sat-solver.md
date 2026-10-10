title: CDCL SAT solver
blurb: A CDCL SAT solver proved to terminate on every CNF formula within its size limits, with `Sat` and a satisfying assignment or `Unsat`, proved to mean that no assignment satisfies the formula.
status: owner-review
date: 4 October 2026
sources:
  - verification/library/vox_cdcl_total.mli — Public interface of the demo solver, `solve_complete`, and the bounded `solve`
  - verification/library/vox_sat_spec.ml — Definitions: evaluation, satisfiability, unsatisfiability and the accepted inputs
  - verification/library/vox_sat_spec.mli — The same definitions as checked equations
  - verification/library/vox_sat.mli — `unsat_at`
  - verification/library/vox_sat.ml — Proof of `unsat_at`
  - verification/library/vox_cdcl_total.ml — The public functions: they call the proof module and turn its derivation of the empty clause into `unsatisfiable`
  - verification/library/vox_cdcl_total_proof.mli — Internal interface: the same solvers, with the derivation in the answer
  - verification/library/vox_cdcl_total_proof.ml — The search and its proofs: propagation, conflict analysis, learning, backjumping and the termination measure
  - verification/library/vox_sat_proof.mli — Internal interface: derivations, clause scanning and the learned-clause database
  - verification/library/vox_sat_proof.ml — Resolution derivations and their soundness, clause scanning, and the proof that a derivation of the empty clause rules out every assignment
  - verification/library/vox_cdcl_total.md — How the search works and why it terminates
  - verification/library/vox_sat_boundary.md — Reading order and what the tests check
  - testsuite/tests/vox/sat_public.ml — Public-only client
  - testsuite/tests/vox/sat_boundary.ml — Compiles the client with only the public interfaces and checks that proof code is erased (with `sat_boundary_check.ml`)
  - testsuite/tests/vox/sat_cdcl_total.ml — Tests against a truth table, the size limits and the bounded solver
  - testsuite/tests/vox/sat_cdcl_progress_rejected.ml — Rejected client
  - testsuite/tests/vox/sat_solver_rejected.ml — Rejected use of `unsat_at` on a satisfiable formula
---
The demo is `Vox_cdcl_total.solve_complete n formula`, a CDCL solver with clause learning and backjumping for a CNF formula over the variables `0` to `n - 1`. It is `total`: it terminates without raising on every input. If `Vox_sat_spec.classify_input n formula` is `None` (`0 <= n <= 256`, at most 4,096 clauses and 65,536 literal occurrences, every variable index in `[0, n)`), it returns `Sat assignment`, where `assignment` has length `n` and satisfies the formula, or `Unsat`, where `unsatisfiable n formula` holds: each of the `2^n` assignments of length `n` falsifies the formula. It never returns `Unknown`. For any other input it returns the error that `classify_input` names. The proofs are erased; there is no runtime certificate, proof trace or final check of the answer.

The contract is that of any decision procedure: an enumeration of all `2^n` assignments would meet it too. Learning and backjumping are properties of the code, which the tests observe through the `statistics` in the report; the contract says nothing about the statistics. Totality is a logical property: there is no bound on running time, memory or stack, and the search can take exponential time. The solver scans every clause to propagate, decides on the unassigned variable that occurs most often in the input (setting it to `true`), and has no watched literals, restarts or clause deletion. The library has one other entry point, the bounded `solve`, described under Scope.

## Interface

The model is ordinary CNF evaluation: a formula is a list of clauses, and
an assignment gives one Boolean per variable.

@code verification/library/vox_sat_spec.ml "type literal" "    eval_clause assignment clause && eval_formula assignment rest"

`check` requires an assignment of the right size that satisfies the formula.
`unsatisfiable` enumerates both Boolean choices for every variable:

@code verification/library/vox_sat_spec.ml "let[@def] check" "  0 <= n && valid_formula n formula && rejects_extensions [] n formula"

The [complete model](src:verification/library/vox_sat_spec.ml)
also defines assignment sizes, valid variable indices and the input classifier
(the limits are 256 variables, 4,096 clauses and 65,536 literal occurrences).
The public interface exports these definitions as checked equations.

The complete solver returns a satisfying assignment or proves unsatisfiability:

@code verification/library/vox_cdcl_total.mli "val solve_complete" "| Unknown -> false} @@ total"

[The full solver interface](src:verification/library/vox_cdcl_total.mli)
also provides a bounded solver that may return `Unknown`. Resolution
derivations, learned clauses and search measures stay in the proof modules.

## Trusted base

Nothing beyond the shared base. The proofs use `Bigint` and `Vox_sequence.length`, which the shared page covers.

## Scope

- The demo is `solve_complete`. The only other entry point, `Vox_cdcl_total.solve fuel`, runs the same search with a budget; on an accepted input, `Unknown` is allowed for any fuel `>= 0` and implies `statistics.steps = fuel`. Nothing relates `steps` to the work done, so an implementation that returns `Unknown` at once with `steps = fuel` meets the contract.
- Only CNF over integer-indexed variables; no incremental solving, assumptions or proof output.

## How it is proved

`Sat`: the solver returns an assignment only when propagation has bound every variable without a conflict, and `Vox_sat_proof.scan_formula_complete` proves that such an assignment satisfies every clause.

`Unsat`: each learned clause, and each clause in conflict analysis, carries an erased derivation from the input clauses whose only rules are "input clause" and resolution (`Vox_sat_proof.proof_result`, whose `proof` field is ghost). Analysis resolves the conflicting clause with the reasons of its literals, extending the derivation at each step. A conflict with no decision on the trail yields a derivation of the empty clause, and `Vox_sat_proof.empty_unsatisfiable` proves, by induction over the assignments that `rejects_extensions` enumerates, that the formula is then unsatisfiable. `Vox_cdcl_total.solve_complete` calls it in ghost code.

Termination: `search` decreases the ghost measure `absent * (n + 1) + unassigned`. Here `absent` counts the lists of at most `2n` literals over the variables below `n` that are not in the learned-clause database, and `unassigned` counts the unbound variables. A decision lowers `unassigned`. A learned clause has no repeated literal, so it is one of those lists, and it is new: after the backjump it is unit, and the proof shows that no clause already in the database is unit at that point. Learning therefore lowers `absent` by one, and the resulting drop of `n + 1` outweighs the at most `n` variables that backjumping unbinds. Conflict analysis terminates because each resolution step lowers the latest trail position among the clause's literals. `vox_cdcl_total.md` gives more detail.

## Client example

From the public-only client. `{n : int | p}` is the type `int` refined by the predicate `p`; refinements on arguments are preconditions the caller must prove, and the refinement on the result is proved here. `(f @ total)` declares that `f` terminates without raising. `ghost_ (...)` is proof code, checked and then erased. `classify_input_def` and `check_def` state the definitions of `classify_input` and `check`, which the checker does not unfold on its own. `Vox_sat.unsat_at` turns `unsatisfiable` into the fact that a given list of booleans, of any length, falsifies the formula; its result is ghost, so it is called inside `ghost_`.

@code testsuite/tests/vox/sat_public.ml "let (decide_cdcl @ total)" "  result"

## A rejected program

The bounded solver `Vox_cdcl_total.solve fuel` may return `Unknown` for any nonnegative fuel, so a client cannot prove that fuel `n + 1` suffices. The same client with `solve_complete n formula` in the last line compiles (`complete_cdcl` in `sat_cdcl_total.ml`).

@code testsuite/tests/vox/sat_cdcl_progress_rejected.ml "let (unsupported_cdcl_depth @ total)" "Vox_cdcl_total.solve (n + 1) n formula"

@text testsuite/tests/vox/sat_cdcl_progress_rejected.compilers.reference

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/sat_boundary.ml vox/sat_cdcl_total.ml vox/sat_cdcl_progress_rejected.ml vox/sat_solver_rejected.ml vox/sat_kernel.ml
```

`sat_boundary.ml` compiles, and so checks, `Vox_sequence` and the five SAT modules with `ocamlc` and with `ocamlopt`, using `-principal` where `verification/library/build.sh` does. It copies the `.cmi` files of `Vox_sat_spec`, `Vox_sat` and `Vox_cdcl_total` (and, for native code, the `.cmx` files needed to link) into an empty directory, compiles `sat_public.ml` there, links and runs it. It then checks that the `-dlambda` output of the two public modules names no proof function and makes the expected number of calls, and that the native object of `Vox_cdcl_total_proof` has no symbol for 15 named proof helpers (trail ranks, the clause universe, the measure). `sat_cdcl_total.ml` checks the implementation and runs it as bytecode on 729 formulas, every sequence of three clauses drawn from the nine clauses over two variables with no repeated variable, against a truth table; runs the bounded solver on a random 3-SAT instance with 50 variables and 218 clauses; and tests 256 variables and inputs just past each size limit. `sat_solver_rejected.ml` checks that `unsat_at` cannot be applied to a satisfiable formula, and `sat_kernel.ml` exercises the derivation operations of `Vox_sat_proof` directly.
