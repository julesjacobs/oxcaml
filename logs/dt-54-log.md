# Task #54 — reset-per-query theory invalidation (contract-A)

Branch `task/dt54` off trunk **db07aa58ca** (which already carries #51: the DT live-ref +
the interim non-monotonicity guard + the disjoint/isolated gate riders). Frozen for dual
review (fable leg = bs-dt-fable, codex leg = bs-codex-driver).

## The problem #54 solves (that #51 + the interim guard did NOT)
The combined/standalone theory is chosen lazily at the first theory-atom intern
(`Cdclt.ensure_theory`) and cached in `Cdclt.t.theory` for the whole Session; it is never
reset when the datatype/array registry is REPLACED for a later query. In a batched
refinement-type VC workload (one reused Session, each VC self-contained under push/pop) this
produced three degrade patterns, all sharing one root (a stale cached theory):
1. **loader overwrite** — each VC's `set_datatypes` replaces the registry;
2. **none→DT** — an early pure-logic VC caches the EUF+LIA stack, a later VC declares a
   datatype the cached theory cannot serve (the most common product shape);
3. **DT→arrays** (and arrays→DT) — theory-CHOICE staleness.

The #51 interim guard fail-CLOSED all three to `unknown`; before it, the by-ref read
produced a wrong `unsat` when a re-used symbol changed datatype role across VCs (the
session-lifetime `ctor_terms`/`seen_cat` of the cached `Dt.t` met a differently-populated
registry — the codex CRITICAL). As collateral, the interim guard ALSO degraded batched
**pure-logic** VC2+ to `unknown` (it fired on `theory_instantiated` regardless of content).

## The fix (reset-per-query; correct at base + fail-loud above base)
On a registry mutation (`set_datatypes` / `set_arrays` / `declare_datatype`) after a theory
is already instantiated, and only when the mutation actually involves datatypes/arrays (a
pure-logic `set_datatypes empty`/`set_arrays empty` is a no-op — batched pure-logic stays
byte-identical), INVALIDATE the cached theory:
- `Cdclt.reset_for_new_query` (new, cdclt.mli — not frozen): drops `theory` (→ `None`) and
  clears the whole SAT-var↔theory-atom bijection (`t2v`/`v2a`/`v2term`/`a2v`/`is_split`/
  `subterms`) + per-check state (`level`/`splits`/model snapshots).
- `Session.invalidate_theory_for_registry_change` (new, private): calls the above, then
  clears the session-side per-query term→var maps (`prop_to_var`, `bool_consts`) and
  last-verdict/model/poison state (`has_theory`, `has_arrays`, `degraded`, `last_model`,
  `last_verdict`, `elim_defs`, `sym_sel`, `lemmas_registered`). Frames / `asserted` /
  `asserted_saved` are NOT touched (an empty pushed frame is legitimate; its `pop` balances).

The next `intern` rebuilds the theory fresh from the new registry (`ensure_theory`) and
re-interns every (possibly re-used) `Term.t` against it — so no stale classification can
survive, and the discarded `Dt.t`'s `ctor_terms` dissolve the #51 wrong-`unsat` landmine.

REMOVES the #51 interim guard (`session.ml` `set_datatypes`/`set_arrays`
`if Cdclt.theory_instantiated → degraded`), replacing fail-closed-`unknown` with the correct
verdict at base + fail-loud above base.

### The landmine (banked in the charter) and what survives a reset
`ctor_terms`/`seen_cat` (dt.ml) and the `v2a`/`a2v`/`t2v` bijection + interned terms are
SESSION-LIFETIME (never popped). A naive `t.theory <- None` alone would strand them (the old
vars/atoms remain in the SAT core bound to a dropped theory). The reset therefore also DROPS
the bijection, so re-interning mints fresh atoms against the fresh theory — the old `Dt.t`
(with its `ctor_terms`) is discarded entirely, not re-registered against a new registry.
SURVIVES: `Env` (declarations), `Context` (hash-consing), the shared registry refs, the
`Sat.t` core, the atom allocator, the effort budget. The prior (already-popped) query's SAT
vars/clauses stay allocated but INERT — their frame selector is free to be false (trivially
satisfiable), and they are absent from the cleared `v2a` so `on_assign` ignores them;
re-interned terms mint fresh vars that never collide. `sat.mli` is frozen and offers no
clause-drop primitive, so the core is NOT recreated — the inert-clause accumulation is
identical to the pre-existing selector-based push/pop frame model (no new leak).

### Fail-LOUD above base (the contract-A ruling)
Resetting is sound only BETWEEN self-contained queries. With live assertions active
(`asserted <> []`, i.e. no `pop` since the last `check_sat`) the cached theory holds
in-flight atoms bound to the bijection being dropped — resetting would strand them (the #51
wrong-answer path). So a registry replacement attempted with live assertions raises a
documented `Invalid_argument` (never a silent reset under live state, never a silent
rebuild). The self-contained-VC pattern (declare → assert → check → pop) reaches the reset
with `asserted = []`.

## Acceptance (all met)
- **Three patterns GREEN**, `tests/solver/dt_multi_query_gate.ml`, now REQUIRED (was
  EXPECTED-degrade under #51): none→DT = sat; loader overwrite-rerank = sat (the codex/fable
  CRITICAL); DT-guard-isolated overwrite = sat; disjoint overwrite = sat. Accumulate
  multi-datatype stays sat. The overwrite-rerank RED is KEPT spec'd "must-not-be-unsat" as a
  world-independent standing soundness gate (Unsat→FAIL in either world).
- **Fail-loud RED** (new): `run_registry_replace_live_assertions_raises` — `set_datatypes`
  with live assertions + an instantiated theory RAISES `Invalid_argument`.
- **Discrimination**: neutering `invalidate_theory_for_registry_change` to a no-op reproduces
  ALL FIVE failures — two wrong-`unsat` (overwrite-rerank + dt-isolated: the codex CRITICAL),
  two `unknown` (none→DT + disjoint), one missing raise. So the reset is load-bearing against
  both the wrong-verdict and the degrade.
- **Interim guard removed** (replaced, not layered).
- **Single-query / corpus identity**: full **8700 QF_DT 0-flip** (trunk db07aa58ca vs fix,
  10s/file, TOTAL=8700 same=8700 FLIPS=0) + **62/62 cross-logic** byte-identical
  verdict+counters (48 `tests/cases` + 11 dt-goldens-sat + 3 others). The reset never fires
  on a single query (theory instantiated only at the one-and-only query; no later registry
  mutation).
- **Gates by exit code**: `make test` 0, `check-frozen` 14/14, `dt-sat-gate` 0, `dt_e2e_test`
  0, `dt-multi-query-gate` 0. ocamlformat 0.29.0 clean on all touched files.
- **Contract docs**: session.mli doc-comments on `set_datatypes`/`set_arrays`/
  `declare_datatype` + DESIGN.md **A15** addendum (A14 reserved by in-flight #53) (reset-per-query, what survives, fail-loud).

## Files
- `smt/interface/cdclt.ml` / `.mli`: `reset_for_new_query`.
- `smt/interface/session.ml`: `invalidate_theory_for_registry_change`; the three doors call
  it (content-gated) in place of the removed guard; `set_arrays` re-derives `has_arrays` from
  `defs`.
- `smt/interface/session.mli`: reset-per-query contract on the three doors.
- `tests/solver/dt_multi_query_gate.ml`: none→DT + disjoint now required-green; new fail-loud
  RED; overwrite-rerank/dt-isolated kept must-not-be-unsat.
- `DESIGN.md`: A15 (A14 reserved by in-flight #53).

Neither `cdclt.mli` nor `session.mli` is among the 14 frozen signatures (`check-frozen`
14/14 with the changes).


## FIX ROUND (codex BOUNCE + fable rider @8ba4609d56 → committed on top)

Both legs independently found the IDENTICAL CRITICAL (fable = APPROVE-with-required-rider,
codex = BOUNCE): `Session.mgr` (the lemma Manager) is an uncleared live per-era channel;
`assert_lemma` bypasses `asserted`; a base-frame lemma survives `pop`, so the `asserted = []`
fail-loud precondition is false. Plus codex MEDIUM (`has_arrays` dropped on a DT reset). The
reset MECHANISM itself was confirmed correct + the datatype/array bijection reset COMPLETE +
discrimination-proven load-bearing by both legs.

### CRITICAL — the lemma Manager is a live per-era channel outside `asserted`
`Session.mgr` is NOT covered by the reset's `asserted = []` check: lemma instances are asserted
via `assert_instance_at_frame`/`Manager`, NOT added to `asserted`, and a BASE-frame lemma
(registered with no push) survives `pop`. So `asserted = []` can hold while `mgr` still holds a
lemma bound to old-era terms → the reset would drop the bijection under it (the #51
stranded-state class). REMEDY (master's REVISED ruling — fail-LOUD, NOT clear): the Manager is
the ADR-0012 store fed by `assert_lemma`, i.e. USER-INPUT state, not a derived consequence, so
silently dropping it in the new era would be a wrong-`sat` channel. `invalidate` now treats a
live lemma EXACTLY like a live assertion: its guard raises `Invalid_argument` when
`asserted <> [] || Manager.has_live_lemma t.mgr`. (The earlier pre-draft cleared `t.mgr` via a
new `Manager.clear`; that was superseded and removed — no new Manager surface.)

RED: dt_multi_query_gate `run_base_lemma_blocks_reset_red`. Registers a BASE-frame forall lemma
(no ground assert → `asserted = []`), instantiates the combined theory via a pushed ground atom
then pops (base lemma survives), then attempts a registry change (`declare_datatype`) — REQUIRED
raises `Invalid_argument`. DISCRIMINATION: drop the `|| Manager.has_live_lemma t.mgr` disjunct
from the guard → the registry change returns instead of raising → RED FAILS, gate EXIT 1.
The guard is a no-op on the whole acceptance battery (none of those cases has a live lemma at
reset — the battery stays green).

### MEDIUM — DT-triggered reset dropped a still-valid `has_arrays`
The reset set `has_arrays <- false`, but only `set_arrays` re-derived it; a `set_datatypes` /
`declare_datatype` reset with a live (unchanged) array registry left `has_arrays = false` while
`ensure_theory` still picks the arrays theory → `commit_sat` non-array branch → spurious
Unknown (completeness only). FIX: `invalidate` now re-derives `t.has_arrays <- not (is_empty
!array_registry)` from the LIVE array registry (unchanged by a datatype mutation); `set_arrays`
still overwrites from its own `defs` on the following line. RED:
`run_has_arrays_survives_dt_reset_red` — instantiate the arrays theory (nonempty array
registry, parsed with the cap-backed `~internal_mint`), pop, `declare_datatype` (reset), assert
`Session.uses_arrays` stays true. DISCRIMINATION: force `has_arrays <- false` in invalidate →
this RED FAILS (`uses_arrays false`), gate EXIT 1.

### Fable NICE (content-gate two-sidedness)
`run_set_datatypes_empty_after_nonempty`: after a DT query, `set_datatypes empty` (the
empty-after-nonempty side of the content gate) resets the cached DT theory — `uses_datatypes`
reads false and a subsequent pure-logic VC is served correctly (sat). Locks that the gate is
two-sided, not fire-only-on-nonempty-defs.

### Surface / re-verification (post-fix)
NO new frozen surface and no new Manager API: the guard reads the existing
`Manager.has_live_lemma`. check-frozen 14/14. Full charter battery + all three new REDs
(base-lemma fail-loud, has_arrays survival, content-gate two-sided) GREEN, each
discrimination-proven (drop the mgr disjunct / force `has_arrays<-false` → the matching RED
fails). make test EXIT 0. 8700 QF_DT 0-flip (logs/dt-54-8700-0flip.txt: TOTAL=8700 same=8700
FLIPS=0) + 62/62 cross-logic byte-identity (logs/dt-54-crosslogic-identity.txt) re-run post-fix
(the lemma guard + has_arrays re-derive fire only on a reset, so single-query is byte-identical).
