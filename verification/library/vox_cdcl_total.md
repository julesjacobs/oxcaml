# Bounded CDCL and complete fallback

`Vox_cdcl_total.solve fuel n formula` implements CDCL with a persistent
assignment, trail, and learned-clause database. Search and conflict analysis
recurse with decreasing fuel; propagation decreases the number of unassigned
variables. Vox checks the public
`solve` function as total. This proves termination and sound answers, but does
not prove that sufficient fuel guarantees a decision. Exhaustion returns
`Unknown`. Some internal checks also return `Unknown` and are not yet proved
unreachable from the initial state.

Accepted inputs have 0–256 variables, at most 4,096 clauses, and at most
65,536 literal occurrences. Every literal index must be in `[0, n)`, and fuel
must be nonnegative. These are input bounds, not a sufficient search budget.

`Sat` carries an assignment proved to satisfy the input formula. Public
`Unsat` is nullary and proves `Vox_sat_spec.unsatisfiable n formula`.
`Vox_sat.unsat_at n formula assignment` specializes that semantic claim to
any assignment. There is no final
formula check or proof-trace replay at runtime. Learned-clause reasons store
stable insertion ordinals because the persistent database prepends clauses.

The fuel bounds recursive search depth and each analysis call. It is not an
exact count of operations. Both CDCL implementations scan
clauses for propagation; neither has watched literals, restarts, or clause
deletion. The total implementation uses list indexing and persistent updates.
It caches variable occurrence counts once per solve for decision selection.

Propagation is proved to reach either a stable state or a conflict. The scanner
proves that each unit literal is unassigned, and enqueue reduces the unassigned
count by exactly one. This count is bounded by the assignment length and passed
as an erased ghost argument for the termination measure. Propagation no longer
has a fuel-exhaustion result, and both failed unit-enqueue branches are proved
unreachable. A 256-variable implication chain therefore needs only one search
step; the boundary regression checks it with search fuel `1`.

Trail coverage, consistency, uniqueness, and decision-level ordering are proved
from initialization through enqueue, propagation, decisions, and backtracking.
Every assigned variable occurs exactly once on the trail, with the stored
value. Levels decrease weakly from newest to oldest; a decision is the oldest
assignment at its level, and decision levels are positive.
The current-variable list contains only assigned variables at the current
decision level. These facts prove that `find_latest` succeeds on a nonempty
current-variable list and that its result has a binding.

Stored reason sources are also proved valid throughout search. Original reasons
refer to input clauses; learned reasons have valid insertion ordinals and refer
to existing learned clauses. Prepending a learned clause preserves all old
references, and backtracking preserves the references of retained bindings.
The learned counter equals the database length. An erased bound ties the
learned counter plus remaining search fuel to the initial fuel, proving that
counter increments cannot overflow.

Assignment-length preservation removes the final runtime length check. Failed
decision selection, decision enqueue, conflict-source lookup, trail lookup,
selected-variable lookup, and reason fetch are proved unreachable. The remaining
obligations concern reason clauses being unit when assigned,
preservation of a conflict during resolution, successful asserting-clause
construction, and global search progress. Decision levels are nonnegative and
bounded by the current level. Successful asserting-clause construction proves
a strictly smaller backjump target and a current-level asserting variable;
backtracking makes that variable unassigned, so learned enqueue succeeds.

Conflict analysis cannot select a decision reason when resolution is required:
with multiple current-level variables, another occurs earlier than the selected
variable, contradicting the decision-order invariant. At level zero, positivity
excludes decisions altogether. Learned clauses retain valid input indices;
together with conflict scanning and level bounds, this proves a successful
root analysis returns the empty clause. No eventual-decision theorem is proved
for CDCL alone.
Successful conflict analysis does prove that its clause remains conflicting
and has at most one current-level variable, or none when analyzing at the root.

`solve_with_fallback fuel depth_fuel n formula` first calls the bounded CDCL
solver. If CDCL returns `Unknown`, it calls private `Vox_sat_proof.decide_depth`, a persistent
DPLL search. Its checked public contract strengthens the `Unknown` case to
`depth_fuel <= n`. Therefore, for every accepted input and nonnegative CDCL
fuel, **`depth_fuel >= n + 1` guarantees `Sat` or `Unsat`**. The interface test
`sufficient_depth` checks this implication for arbitrary accepted formulas.

The fallback decreases both depth fuel and the remaining variable count on
each recursive edge. It gives each branch the same remaining depth fuel; this
is a depth bound, not a shared node budget. The worst case is exponential:
up to `2^(n + 1) - 1` search nodes. The guarantee covers the combined solver;
it does not establish CDCL progress. `statistics` records only the CDCL attempt.

The fallback proves its UNSAT result through `refutes`, the DPLL reduction
predicate, and an erased `Exhaustion` derivation. The new derivation case is
proved sound for every assignment by padding or truncating it to `n` values;
valid literal indices make that transformation preserve formula evaluation.
SAT is proved during recursive reconstruction. There is no runtime certificate,
refutation replay, or final formula check on either result path.

On the same seeded random 3-CNF formulas, using the installed native compiler
on 2026-09-25 (median of five runs):

| Formula | Mutable CDCL | Bounded persistent CDCL | Combined solver |
| --- | ---: | ---: | ---: |
| 50 variables, 218 clauses (UNSAT) | 0.006 s | 0.009 s | 0.009 s |
| 100 variables, 430 clauses (SAT) | 0.175 s | 0.259 s | 0.261 s |

The combined solver uses CDCL fuel `1_000_000` and fallback depth `n + 1`.
CDCL decides both instances, so these timings measure its successful path.

Run `./dev test vox/sat_cdcl_total.ml` for the totality, soundness, learning,
backjump, and truth-table tests. The matched benchmark is
`verification/benchmarks/vox_cdcl_compare.ml`.

See [the review boundary](vox_sat_boundary.md). The fallback remains a separate
entrypoint pending the architecture choice; it does not replace bounded CDCL.
