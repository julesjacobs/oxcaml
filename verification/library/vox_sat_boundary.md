# SAT specification/proof boundary

## Exact transitive human-review surface

Read these files in order, all under `verification/library/`:

1. `vox_sat_spec.mli`: literal/CNF types and complete checked equations for
   assignment lookup, literal/clause/formula satisfaction, assignment length,
   valid variable indices, input limits, assignment concatenation, and UNSAT.
   `vox_sat_spec.ml` implements exactly these equations and can be read instead
   for their compact recursive definitions; interface checking connects them.
2. `vox_sat.mli`: bounded DPLL's actual executable contract and the theorem
   specializing semantic UNSAT to any Boolean assignment.
3. `vox_cdcl.mli`: mutable CDCL's actual executable contract, sound on return.
4. `vox_cdcl_total.mli`: total bounded CDCL and the separate total combined
   solver, including exact permitted errors and the fallback depth guarantee.
5. This file: termination/resource conventions and the shared trust boundary.

The semantic dependency closure uses only Boolean and machine-integer
operations, algebraic datatypes, lists, options, results, and structural
logical equality (`===`). There are no SAT-specific axioms, external
primitives, abstract semantic predicates, or proof-module names in these
interfaces. Statistics are operational counters; no exact cost relation is
claimed. `Vox_sequence` and its mathematical integers are private proof
implementation dependencies, not premises of the public SAT contracts.

`rejects_extensions prefix remaining formula` universally enumerates all
Boolean extensions of the prefix, appending one bit per positive remaining
step. Its leaf is failure of ordinary CNF evaluation. `unsatisfiable n formula`
requires nonnegative `n`, valid indices below `n`, and rejection of all
length-`n` assignments. The public checked theorem extends rejection to every
Boolean list: missing values evaluate false and unused trailing values cannot
affect a valid formula. An empty CNF evaluates true; an empty clause false.

The accepted solver domain is `0 <= n <= 256`, at most 4,096 clauses and
65,536 literal occurrences, with valid indices. DPLL requires nonnegative
fuel as a caller premise; CDCL entrypoints return `Invalid_fuel` for a negative
budget. Mutable CDCL's error contract is deliberately unrestricted. The total
solver signatures specify each permitted input error and its ordering.

Bounded DPLL and bounded CDCL permit `Unknown` without a semantic claim.
Combined `solve_with_fallback fuel depth_fuel n formula` additionally proves
`Unknown -> depth_fuel <= n`; accepted inputs and `depth_fuel >= n + 1`
therefore force a decision. This is combined-solver completeness. CDCL eventual
progress remains unproved; the separate fallback has not replaced its API.
The CDCL contract permits `Unknown` at every nonnegative fuel value: the proof
does not exclude search exhaustion, analysis exhaustion, or failed
asserting-clause construction. Fuel monotonicity is not part of the contract.
The fallback can visit exponentially many nodes. Totality uses Vox's logical
execution model, without a bound on available memory, stack, or elapsed time.
Mutable CDCL is only proved sound when it returns.

## Private implementation

`vox_sat_proof`, `vox_cdcl_proof`, and `vox_cdcl_total_proof` contain derivation
representations, validity, learned-clause databases, state invariants, and
auxiliary induction lemmas. Public wrappers translate private outcomes into
ordinary `Sat assignment`, nullary `Unsat`, or `Unknown`. Their ghost calls
establish the semantic contracts. Clients neither inspect a derivation nor
supply a solver invariant. Private interfaces are excluded from installation.

Persistent CDCL preserves strict reason order: every nonpivot variable in a
stored reason occurs older than its assignment on the unique trail. This
invariant survives enqueue, backtracking, and learned-clause insertion, and
applies to the reason selected during conflict analysis. A checked trail-rank
lemma gives each antecedent a smaller rank than its pivot. Analysis still uses
fuel; clause-level decrease, asserting-clause success, and global CDCL progress
remain open.

Learned clauses remain executable solver data. Their derivations erase.
Semantic enumeration is used only in proofs of returned answers; solvers do
not run it, accumulate a proof trace, or perform a final formula check.
DPLL and persistent CDCL also have no final assignment-length check. Mutable
CDCL retains its length check.

## Evidence

`verification/clients/check_sat_public.sh _install` copies only the four
public `.cmi` files to an isolated directory, separately compiles
`verification/clients/sat_public.ml`, and links/runs bytecode and native clients.
Its arbitrary-input theorem derives the sufficient-depth decision guarantee,
SAT satisfaction, and rejection of any proposed assignment on UNSAT.
The client cannot import a private interface.

`verification/clients/check_sat_erasure.sh _install` checks bytecode/native
Lambda for surviving public proof bridges or semantic enumeration calls.
The public `unsat_at` body erases to unit. Native symbol inspection also finds
no unassigned-count, reason-source, reason-order, trail-rank, trail-coverage,
or level-bound helpers in
`vox_cdcl_total_proof.o`; propagation remains executable.

`testsuite/tests/vox/sat_solver.ml`, `sat_cdcl.ml`, and `sat_cdcl_total.ml`
exercise the public APIs. `sat_kernel.ml` separately checks private resolution
operations. Rejection tests retain forged-UNSAT, mutable-totality, loop-totality,
and mutable-array-bound checks. Public-interface rejection tests also prevent
claiming CDCL completeness at fuel `n + 1` or combined completeness at fallback
depth `n`; the positive `sufficient_depth` client proves the latter at `n + 1`.
The build and native benchmark use the
installed compiler produced by `make install`.

Trust includes Vox's type/refinement and termination checking, VC generation,
Z3's reported UNSAT answers, ghost erasure, and the compiler/runtime execution
stack. These are shared language assumptions, not extra SAT premises.
