# The SAT solver: what to read and what is checked

## What to read

The public contract is in three interfaces, all under `verification/library/`:

1. `vox_sat_spec.mli`: literals and CNF formulas, and equations for
   evaluation, assignment length, valid variable indices, the size limits,
   `check` (an assignment of length `n` that satisfies the formula),
   `unsatisfiable` (every assignment of length `n` falsifies the formula) and
   `classify_input` (the accepted inputs, and which error each rejected input
   gets). `vox_sat_spec.ml` gives the same definitions as recursive
   functions; the compiler checks that they satisfy the equations.
2. `vox_sat.mli`: `unsat_at`, which extends `unsatisfiable` to Boolean lists
   of any length. A missing variable evaluates to `false`, and values past
   position `n` do not affect a formula over `n` variables.
3. `vox_cdcl_total.mli`: `solve_complete` and the bounded `solve`, with the
   exact errors each may return.

These interfaces use only booleans, machine integers, lists, options,
results, algebraic datatypes and logical equality (`===`). They contain no
axiom, `external`, abstract predicate or name from the proof modules.
`Vox_sequence` and `Bigint` appear only in the proofs.

The accepted inputs have `0 <= n <= 256`, at most 4,096 clauses, at most
65,536 literal occurrences and every variable index in `[0, n)`. Duplicate
literals and tautological clauses are accepted. `solve` returns
`Invalid_fuel` for a negative budget.

`solve_complete` returns `Sat` or `Unsat` on every accepted input. `solve`
may also return `Unknown`, which implies `statistics.steps = fuel` and nothing
more; no fuel is promised to suffice, and more fuel is not promised to help.
The other statistics are unconstrained. Totality is proved in Vox's logical
model: memory, stack and running time are not bounded.

## The proof modules

`vox_sat_proof` and `vox_cdcl_total_proof` are internal and are not
installed. `vox_sat_proof` defines derivations (input clauses and resolution),
proves them sound, and proves that a derivation of the empty clause makes the
formula `unsatisfiable`. It also scans clauses under a partial assignment and
holds the learned-clause database, in which each clause carries its
derivation in a ghost field. `vox_cdcl_total_proof` is the search: the trail,
propagation, conflict analysis, learning, backjumping and the termination
measure. `vox_cdcl_total.md` explains the search and its termination
argument. The public wrappers in `vox_cdcl_total.ml` turn the internal
`Unsat` answer, which carries the derivation, into the nullary `Unsat` by a
ghost call to `Vox_sat_proof.empty_unsatisfiable`.

Derivations, invariants and the termination measure are erased. The solver
does not enumerate assignments, record a proof trace or check its answer at
the end. Learned clauses are ordinary runtime data.

## What the tests check

`testsuite/tests/vox/sat_boundary.ml` compiles the library with `ocamlc` and
`ocamlopt`, using `-principal` where `build.sh` does. It copies the `.cmi`
files of the three public modules (and, for native code, the `.cmx` files
needed to link) into an empty directory, compiles
`testsuite/tests/vox/sat_public.ml` there, and links and runs it. That client
proves from the public interfaces alone that `solve_complete` decides every
accepted input, that `Sat` carries a satisfying assignment, and that `Unsat`
rules out any given assignment. `sat_boundary_check.ml` then checks that the
`-dlambda` output of `Vox_sat` and `Vox_cdcl_total` names no proof function
and makes the expected number of calls, and that the native object of
`Vox_cdcl_total_proof` has no symbol for a list of named proof helpers
(trail ranks, prefix invariants, the clause universe, the measure and the
level bound). The list is a targeted check, not a proof that every proof
function is erased.

`testsuite/tests/vox/sat_cdcl_total.ml` runs both solvers on every formula of
three clauses drawn from nine small clauses over two variables and compares
the answers with a truth table. It also checks the size limits and the error
order, 256 variables, duplicate-literal propagation, the statistics of the
bounded solver, and learning and backjumping on a random 3-SAT instance.
`sat_kernel.ml` exercises the derivation operations of `Vox_sat_proof`
directly. `sat_solver_rejected.ml` checks that `unsat_at` cannot be applied
to a satisfiable formula, and `sat_cdcl_progress_rejected.ml` that a client
cannot prove that fuel `n + 1` suffices for `solve`.

What the SAT proofs trust is the shared base of every demo: the type and
refinement checker, the totality check, verification-condition generation,
Z3, ghost erasure and the compiler and runtime.
