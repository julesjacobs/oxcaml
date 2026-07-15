
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
