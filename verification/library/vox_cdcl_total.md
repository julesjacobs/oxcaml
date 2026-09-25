# Total CDCL SAT solver

`Vox_cdcl_total.solve fuel n formula` implements CDCL with a persistent
assignment, trail, and learned-clause database. Search, propagation, and
conflict analysis each recurse with decreasing fuel. Vox checks the public
`solve` function as total. Exhaustion returns `Unknown`.

`Sat` carries an assignment proved to satisfy the input formula. `Unsat`
carries a `Vox_sat.proof_result` whose ghost derivation proves the empty clause.
`unsat_at` specializes that result to any assignment. There is no final
formula check or proof-trace replay at runtime. Learned-clause reasons store
stable insertion ordinals because the persistent database prepends clauses.

The fuel bounds recursive search depth and each propagation and analysis
call. It is not an exact count of operations. Both CDCL implementations scan
clauses for propagation; neither has watched literals, restarts, or clause
deletion. The total implementation uses list indexing and persistent updates.
It caches variable occurrence counts once per solve for decision selection.

On the same seeded random 3-CNF formulas, using the installed native compiler
on 2026-09-25 (median of five runs):

| Formula | Mutable CDCL | Total CDCL |
| --- | ---: | ---: |
| 50 variables, 218 clauses (UNSAT) | 0.006 s | 0.009 s |
| 100 variables, 430 clauses (SAT) | 0.172 s | 0.277 s |

Run `./dev test vox/sat_cdcl_total.ml` for the totality, soundness, learning,
backjump, and truth-table tests. The matched benchmark is
`verification/benchmarks/vox_cdcl_compare.ml`.
