# DT+LIA theory combination — pin report (task #47, bugreport 03 followup)

Pin `177a0d4034` on `task/dtlia-combine`, parent (trunk) `40e8d7392f`.

## Problem

vox2's BST ordering VCs poison oxsmt: z3 proves 49/49, oxsmt returns
`unknown poison-solve:Invalid_argument` on the first ordering obligation. Root cause
(diagnosis in `logs/dtlia-diagnosis.md`): `Cdclt.ensure_theory` installs EXACTLY ONE theory;
a datatype declaration routes to the standalone `TDt`, which has no arithmetic — a `(> k 0)`
classifies `K_foreign` and `Dt.assert_lit` raises `Invalid_argument`.

## Fix — DT as the congruence child of the Nelson-Oppen combinator

`CombinedDt = Combine (Dtlia_router) (Dt_congruence) (Lia_adapter)`, installed UNCONDITIONALLY
for any datatype-declaring problem (replacing the standalone `TDt`). LIA decides the ordering
atoms, DT the structure. A DT-derived Int equality (selector evaluation
`key (Node _ k _) = k`) reaches LIA through the EXISTING disagreement / ℤ-trichotomy split
path — the same mechanism QF_UFLIA uses for `f x = x + y`, no fabric, no no-op of arith atoms.

### Why the classic path suffices (mechanism, verified)

`dtlia_order_unsat`: DT derives `key(t) = k` in its congruence (both are Int interface
members). At Final, `find_disagreement` sees DT-equal / LIA-unequal on `(key(t), k)` →
ℤ-trichotomy split. So the DT-derived Int equality reaches LIA through the model-based
interface split, WITHOUT a propagation Lit — this is now THE load-bearing combination
mechanism for the consumer (the classic path is forced), so it has DIRECT coverage, not just
end-verdict coverage:
- Confirmed the split fires on `dtlia_order_unsat` by instrumenting `combine_models` (the
  `find_disagreement` trichotomy arm prints under a debug env; reverted after confirming).
- `dt-sat-gate` `run_combination_split` asserts `dtlia_order_unsat` → unsat AND
  `Session.splits > 0` (a future change that resolved it by another path would fail this).
- The split logic itself (`find_disagreement` / `equality_split` / the ℤ-trichotomy) is the
  same theory-agnostic combinator code `combine-test` unit-covers with hand-rolled EUF+LIA
  children — the DT+LIA path reuses it unchanged.

The split closes at decision level 0 (LIA theory-propagates the trichotomy disjuncts against
`k>0 ∧ key(t)≤0`, so no SAT branch is needed) — hence `decisions=0` in the counters despite
the split firing.

### Build requirement (lead's #1 review check) — satisfied by construction

CombinedDt emits Sat only after BOTH children certify Final (`combine.ml` `check_off` reaches
`combine_models` only on `A.check Final = Sat` AND `B.check Final = Sat`). `A.check Final =
Dt.check Final` runs the full DT saturation: constructor distinctness, injectivity, selector
evaluation, AND the occurs/acyclicity model check (`dt.ml` `saturate` → `build_witnesses`
clash → `occurs_check` → splits). So the combined Sat REQUIRES DT's genuine axiom-validating
Final. Pinned by `dtlia_acyclic_unsat` (`t = Node Empty k t` + `k>0` → occurs-check UNSAT
survives the wrap).

### Mixed-sat model reconciliation (the load-bearing hunk, named for review)

`commit_sat` gates any datatype-registry sat on `Dt_model_check.check` over `Cdclt.dt_model`.
Two changes make a mixed sat's self-check pass:
1. `Dt.check_model_with_leaf` — the DT constructor tree's Int leaves (and Int scalar leaves)
   take the ARITHMETIC child's values from the merged model (`CombinedDt.model`), not the
   pure-DT per-class default (which has no arithmetic → would give `0` for a LIA-constrained
   variable). Only a genuine `Model.Int` overrides; a pure-DT Int class (merged model's
   realize-me `Uninterp` signal) falls back to DT's own `leaf_value`. (`cdclt.ml` builds the
   override reading `Model.value (CombinedDt.model th)`.)
2. `Dt_model_check.ev_node` now folds Int `Le`/`Arith` over the model's Int leaves (mirroring
   `Model_check`), so `(> k 0)` etc. evaluate; `Real`/`Real_arith` stay `Bad` (Real+DT out of
   fragment). The DT axioms are already certified by the congruence child's Final; this is the
   independent scalar/arith self-check on top.

Pinned by `dtlia_order_sat` (in `tests/dt-goldens-sat`, driven by `dt-sat-gate` — DT sat
surfaces no scalar model to the external-eval harness).

### Fabric handling (lead's named risk) — enforced, not assumed

`Dt_congruence` has no fabric-live seam. `Dtlia_router.fabric_disabled = true` forces
`Combine` onto the classic no-fabric path (`check_off`/`explain_off`, no create-time
merge-consumer setup) REGARDLESS of the global `OXSMT_NO_FABRIC`. The fabric-live
congruence-child methods (`check_fabric`, `assert_fabric_eq`, `drain_merges`, `checkpoint`, …)
are loud fail-closed stubs (`raise Combine.Incomplete "dtlia-fabric-unsupported"` → verdict
`unknown`) — never a quiet wrong answer or a crash. `dt-combine-fabric-gate` runs the four
mixed repros with fabric globally ON and OFF and asserts identical correct verdicts.

### Matcher view (lead's steer #2, MANDATORY) — inner Dt

`live_egraph_view` for `TCombinedDt` reads `CombinedDt.congruence_state` (the inner `Dt.t`)
via `Dt.app_terms_by_symbol` etc., NOT the combinator's EUF-shaped view — so UFDT quantified
matching is identical to the standalone-DT path.

## Touch map

- `smt/combine/combine.mli`, `combine.ml`: two `ROUTER` flags (`fabric_disabled`,
  `congruence_models_datatypes`); shadow `fabric_off`/`fabric_callbacks_off` with the
  per-router OR; gate `require_no_datatype_terms` and `model`'s Datatype arm on
  `congruence_models_datatypes`.
- `smt/combine/dtlia_router.ml`/`.mli`: new router (`include Uflia_router` + the two flags).
- `smt/combine/uflia_router.ml`, `uflra_router.ml`: add both flags = `false`
  (byte-identical to before).
- `smt/theories/dt/dt.mli`/`.ml`: `internalize_term`, `check_model_with_leaf` (refactor of
  `check_model`).
- `smt/interface/dt_model_check.ml`: Int `Le`/`Arith` evaluation.
- `smt/interface/cdclt.ml`: `Dt_congruence` module (wraps `Dt.t`), `CombinedDt`,
  `TCombinedDt` variant (REPLACES `TDt`, removed), unconditional `ensure_theory` arm
  (registry via `Dt_congruence.set_registry` side channel), mixed `dt_model`,
  `live_egraph_view`/`registered_terms`/`clear_last_conflict`/`last_conflict_core` arms.
- `smt/combine/test/combine_test.ml`: control router gains both flags = `false`.
- Tests: `tests/cases/dtlia_{order,dt_only,int_only,acyclic}_unsat.smt2(.expected)`;
  `tests/dt-goldens-sat/dtlia_order_sat.smt2`; `Makefile` `dt-combine-fabric-gate`.

check-frozen unaffected: `combine.mli`/`cdclt.ml`/`dt.mli`/`dt_model_check.ml` are not among
the 14 frozen interfaces; `Theory.THEORY` (frozen) is untouched.

### Registry side channel (review point)

`Dt.create` needs the `Datatype_defs.t ref`, but `Combine.create` is fixed to the frozen
`Theory.THEORY` shape `ctx -> env` (the functor result *includes* `Theory.THEORY`), so the
registry CANNOT be threaded as a `create` argument without breaking the frozen signature —
widening `create` is not an option, the side channel is forced. It is hardened to a
consumed-exactly-once slot, not a bare mutable ref: `set_registry` fills it, `create` reads
AND clears it, and `create` on an EMPTY slot is a HARD ERROR (never a silent empty-registry DT
theory). So a `create` not immediately preceded by its own `set_registry` fails loud — a
future interleaved / parallel Session construction cannot silently cross two registries; worst
case is a loud construction-time failure, never a wrong verdict. Under today's single-threaded
`ensure_theory` (set immediately before create) the slot is filled-then-drained with no
interleaving.

## Gates

- REDs green (new binary): order_unsat/dt_only/int_only/acyclic → unsat; order_sat → checked sat.
- `make` harness 90/90 (0 fail); check-frozen 14/14; dt-sat-gate 31/0 (incl. the direct
  interface-split coverage); combine-test 131/0; dt-multi-query-gate ok; euf-adapter 0-fail;
  dt-combine-fabric-gate ok.
- Five-logic byte-id + UFDT/QF_UFDT/QF_DT 40-file verdict-identity vs trunk `40e8d7392f`:
  ALL 0-diff (table below).

## Verdict-identity vs trunk `40e8d7392f` — COMPLETE

Deterministic samples (sorted head-N per family), mine vs a fresh trunk build, 4-5s wall.

Untouched logics — BYTE-IDENTICAL (full CLI output incl. counters, stronger than verdict):

| logic     | n  | byte-identical | diff |
|-----------|----|----------------|------|
| QF_UFLIA  | 20 | 20             | 0    |
| QF_LRA    | 20 | 20             | 0    |
| QF_LIA    | 20 | 20             | 0    |
| QF_UF     | 20 | 20             | 0    |
| QF_AX     | 20 | 20             | 0    |

DT logics (behaviour intentionally changed → verdict-identity, no regression):

| logic     | n  | verdict-identical | diff |
|-----------|----|-------------------|------|
| UFDT      | 40 | 40                | 0    |  ← mandatory spot (lead steer #2)
| QF_UFDT   | 40 | 40                | 0    |
| QF_DT     | 40 | 40                | 0    |

0 diffs everywhere. The untouched-logic byte-identity confirms the QF_UFLIA/QF_UFLRA
instantiations are bit-for-bit unchanged (the two new router flags are `false`; the
`fabric_off` shadow is `false || false`; every combine.ml guard is keyed on
`congruence_models_datatypes`); UFDT/QF_UFDT/QF_DT verdict-identity confirms the combinator
wrap does not regress the standalone-DT verdict.
