# DT+LIA rider #1 — scalar completion (task #62, bugreport-03 residual hole #2)

Branch `task/dtlia-scalar-rider` off trunk **17b563afb3** (LAND 59, the DT+LIA
combination). Closes the INEQUALITY-determined half of hole #2: an enum (or any)
datatype coexisting with a standalone Int scalar that is bounded only by LIA
inequalities returned a SOUND `unknown` on an otherwise-SAT mixed problem.

## Mechanism

`Dt.check_model_with_leaf` builds the DT checker model by walking the DT
congruence child's atom terms. A scalar `k` that appears solely inside a
LIA-owned atom (e.g. `(> k 0)`) is never interned by the congruence child, so the
walk never reaches it; the independent `Dt_model_check` then cannot evaluate the
ORIGINAL arithmetic assertion over `k` and fails closed to `unknown`.

`complete_dt_model_with_scalars` (cdclt.ml) completes the checker model from
Cdclt's whole registered-subterm closure (`subterms_sorted t`): for every missing
nullary Int/Bool/Uninterp subterm it reads the value from the accepting combined
scalar model (`CombinedDt.model th`) and adds a `Dt.Leaf`. Only ever reads values
from the accepting model — never invents — so a completed value is always
consistent with the solver's own satisfying assignment. Exact sort/variant
matching keeps it fail-closed: an unresolved scalar stays absent and the checker
still rejects (`unknown`, never wrong-SAT). Selector applications
(`Iarr.length args > 0`) are DT-owned, already in the tree, and never touched.

This is candidate B's reviewed `complete_dt_model_with_scalars` (fa1dff52c2,
review-dtlia-b APPROVE) adapted to trunk's `check_model_with_leaf` seam (candidate
A's landed architecture).

## Scope correction (falsified the chartered congruence extension)

The charter also asked for a "congruence-scalar extension" to close the
EQUALITY-determined cases F2/G2 (`k = 2`), on the hypothesis that such scalars are
"congruence-owned, absent from th_model." **Measured and falsified**: F2/G2 fail
ONLY under presolve; `--no-presolve` decides them SAT already on trunk with no
fix. Instrumentation shows `k` never reaches the completion (no nullary term
surfaces) and `find_class_opt` is `Some` for ZERO RED cases — the congruence path
was dead code. Root cause: presolve equality-elimination substitutes `k := 2` and
drops the atom, so `k` is gone from the solved formula; the DT model path
(session.ml:1966) validates `Dt_model_check` against the ORIGINAL `t.asserted`
(still `(= k 2)`) but never reconstructs eliminated vars, unlike `build_model`'s
`splice_elim_defs` (session.ml:1811). That reconstruction is a distinct
session.ml change — split out as **task #64**. The dead congruence code is not in
this branch; it ships only the reviewed LIA-completion.

## Evidence

- REDs (reviewer battery /usr/local/home/jujacobs/tmp/review-dtlia-b/cases):
  A2 (0<k<5), F3 (disjunctive enum + 0<k<5) now SAT (were `unknown` on trunk both
  presolve and `--no-presolve`); all 9 UNSAT REDs including the 3 adversarial
  wrong-SAT probes stay UNSAT; C1/C3/E3/F1 unchanged SAT. F2/G2 remain SOUND
  `unknown` (task #64).
- New goldens gate the fix: tests/dt-goldens-sat/dt_enum_int_ineq_sat.smt2 (A2)
  and dt_int_only_with_dt_decl_sat.smt2 (F3) — both fail (`unknown`) on trunk and
  pass (SAT) here. dt-sat-gate 33/0.
- make test 0 hard failures; check-frozen 14/14.
- Regression (fix vs trunk 17b563afb3): five-logic byte-id and pure-DT
  verdict-identity (UFDT/QF_UFDT/QF_DT) — see rider62_gates.result.

## Touch map

- smt/interface/cdclt.ml: `complete_dt_model_with_scalars` + wire into the
  TCombinedDt `last_dt_model` arm.
- tests/dt-goldens-sat/dt_enum_int_ineq_sat.smt2, dt_int_only_with_dt_decl_sat.smt2.
