# Complete and bounded CDCL

`Vox_cdcl_total.solve_complete n formula` is a total CDCL solver. On every
accepted input it returns `Sat` or `Unsat`, with no budget from the caller.
`Sat` carries an assignment proved to satisfy the formula; the nullary
`Unsat` comes with a proof of `Vox_sat_spec.unsatisfiable n formula`.

Accepted inputs have 0 to 256 variables, at most 4,096 clauses and at most
65,536 literal occurrences, and every variable index is in `[0, n)`.
`Vox_sat_spec.classify_input` defines them and the error for every other
input. Duplicate literals and tautological clauses are accepted.

The public interfaces require no proof objects or invariants from the caller.
The solver has no final check of its answer and records no proof trace.
Totality is proved in Vox's logical model; memory and running time are not
bounded.

## The search

The state is an immutable list of variable bindings (value, decision level
and reason), a trail of assigned literals and a learned-clause database.
Propagation scans the input clauses and the learned clauses for a conflict or
a unit clause, and each unit it assigns lowers the number of unassigned
variables. When propagation is stable and every variable is bound, the
bindings are a satisfying assignment. Otherwise the solver decides on the
unassigned variable that occurs most often in the input, setting it to
`true`; the occurrence counts are computed once per solve.

Every learned clause carries a ghost resolution derivation from the input
clauses. Conflict analysis takes the literal of the conflicting clause that
was assigned last and resolves the clause with that literal's reason,
extending the derivation. Each reason's other literals were assigned
earlier, an invariant kept through assignment, backjumping and learning, so
each step lowers the latest trail position among the clause's literals, and
analysis terminates without fuel. At decision level 0 it produces the empty
clause, and `Vox_sat_proof.empty_unsatisfiable` turns its derivation into
`unsatisfiable`. At a higher level it produces an asserting clause and a
lower backjump level; backjumping unbinds the asserting variable, which the
learned clause then forces.

## Termination

The proof keeps the invariant that the bindings retained at each decision
level below the current one were fully propagated: under them no clause of
the input or the database is unit. After the backjump the new learned clause
is unit, so it is not already in the input or the database. Resolution removes duplicate
literals, so a learned clause has at most `2 * n` literals and belongs to the
finite set of literal lists of that length over the variables below `n`.
Learning therefore lowers the number of lists in that set that are absent
from the database. The search decreases

```
absent_clauses * (n + 1) + unassigned_variables
```

A decision lowers the second term. Learning lowers the first term, which
outweighs the at most `n` variables that backjumping unbinds. The set, the
counts and the measure are ghost values; none exists at run time.

A learned reason holds its database entry directly (through `stored_result`,
an abstract alias of `proof_result`), so no integer index into the database
needs a bound. An input clause is named by its index in the input, which has
at most 4,096 clauses. The trail's literals are valid and distinct, so the
decision level is at most 512 and cannot overflow. The statistics are
machine integers that play no part in the search or its proof.

The solver scans every clause to propagate. It has no watched literals,
restarts or clause deletion, and the bindings are a list, so access is linear
in `n`.

## Bounded search

`solve fuel n formula` runs the same search with a budget of `fuel` search
steps; a step is one round of propagation followed by a decision or by
conflict analysis and learning. It may return `Unknown`, which implies
`statistics.steps = fuel`. The contract does not say that any fuel suffices
or that more fuel helps.

## Tests

`./dev test vox/sat_cdcl_total.ml` checks totality and the public contracts
and runs both solvers against a truth table, the size limits and the error
order, 256 variables, duplicate-literal propagation, the statistics of the
bounded solver, and learning and backjumping on a random 3-SAT instance.
`./dev test vox/sat_boundary.ml` checks the public-only client and that proof
code is erased. [`vox_sat_boundary.md`](vox_sat_boundary.md) gives the reading
order and what each test checks. The benchmark driver
`verification/benchmarks/vox_cdcl_compare.ml` times `solve` and
`solve_complete`; build it with the compiler from `make install`.
