# Complete and bounded CDCL

`Vox_cdcl_total.solve_complete n formula` is a total CDCL solver. For every
accepted input it returns `Sat` or `Unsat`, with no caller-supplied budget and
no DPLL fallback. `Sat` carries an assignment proved to satisfy the formula;
nullary `Unsat` proves `Vox_sat_spec.unsatisfiable n formula`.

Accepted inputs have 0–256 variables, at most 4,096 clauses, and at most
65,536 literal occurrences. Every literal index must be in `[0, n)`.
`classify_input` exposes the complete domain and input-error precedence.
Duplicate literals and tautological clauses remain accepted.

The public interfaces require no proof objects or internal invariants. There
is no final formula check, runtime proof trace, or trace replay. Totality uses
Vox's logical execution model; it does not bound available memory or time.

## Search and termination

The solver uses persistent bindings, a trail, and a learned-clause database.
Propagation scans clauses and decreases the number of unassigned variables.
Conflict analysis selects the latest relevant trail variable and resolves its
reason. Strict antecedent order proves that each resolution decreases the
maximum trail rank of the conflicting clause. At the root, analysis produces
the empty clause; otherwise, it produces an asserting clause and a smaller
backjump target. Backtracking makes the asserting variable unassigned.

Every earlier decision prefix is proved fully propagated. At the backjump
target, the new learned clause is unit, so it cannot already be in either the
input formula or the learned database. Resolution removes duplicate literals.
Consequently every learned clause has at most `2 * n` literals and belongs to
a finite universe of literal lists. Learning strictly decreases the number of
clauses in that universe that are absent from the database.

Search decreases the measure

```
absent_clauses * (n + 1) + unassigned_variables
```

A decision decreases the second term. Learning decreases the first term enough
to cover any assignments removed by backtracking. The finite universe, counts,
and measure are ghost values and erase; none is constructed at runtime.

Learned reasons store direct clause references. The learned-database scanner
returns entries directly, without an integer ordinal or a second lookup.
A checked abstract identity representation for stored entries satisfies Vox's
total-pattern restriction on nested data without allocating a wrapper.
Original reasons index the input formula, whose clause count is bounded.

The trail records contiguous decision levels. Its length bounds the current
level; valid, unique trail literals give a bound of 512 over the entire input
domain. Level increments therefore cannot overflow. Statistics use machine
integers and do not control complete search or its termination proof.

Both CDCL implementations scan clauses for propagation. They have no watched
literals, restarts, or clause deletion. Persistent binding access and updates
use lists; variable occurrence scores are computed once per solve.

## Other entrypoints

`solve fuel n formula` runs the same CDCL search with a nonnegative budget.
It may return `Unknown`; that result proves `statistics.steps = fuel`.
The budget counts search steps, including their propagation and analysis,
and does not count individual operations. Its public contract supplies no
sufficient machine-integer fuel bound or fuel-monotonicity theorem.

`solve_with_fallback fuel depth_fuel n formula` retains its separate behavior:
a bounded CDCL attempt followed, on `Unknown`, by persistent DPLL. For accepted
inputs and nonnegative CDCL fuel, `depth_fuel >= n + 1` guarantees a decision.
Its statistics describe only the CDCL attempt. This guarantee remains distinct
from the complete CDCL entrypoint.

## Checks and review surface

`./dev test vox/sat_cdcl_total.ml` checks totality and arbitrary-input public
completeness, truth-table agreement, learning, backjumping, duplicate-unit
propagation, input-error precedence, bounded/fallback statistics, and the
256-variable boundary. The bounded-budget and fallback-depth rejection tests
retain their original scopes.

`verification/clients/check_sat_public.sh _install` derives semantic SAT and
UNSAT results and complete-CDCL decisions using only installed public
interfaces. `check_sat_erasure.sh _install` checks public bytecode/native
Lambda and private native symbols for surviving proof computations. Both
scripts accept an optional second argument to retain their evidence directory.

See [the review boundary](vox_sat_boundary.md) for the exact semantic interface
closure and shared language trust assumptions. The benchmark driver remains
`verification/benchmarks/vox_cdcl_compare.ml`; it must use the compiler produced
by `make install`.
