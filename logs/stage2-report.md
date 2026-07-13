# Fabric Stage 2 — merge-notification callbacks (EUF→LIA): FREEZE REPORT

ADR-0014 §A.3 / §C Stage 2. Branch `task/fabric-stage2` off `task/fabric-stage1b-B @
c050d592c8`. Worktree `worktrees/stage2`. Builder: builder-stage2.

## What shipped

The reverse fabric direction: when the hub (EUF congruence closure) merges two Int classes
shared with LIA, LIA is notified and asserts the entailed equality into its tableau directly
— removing the SAT round-trip that Path-1 forwarding required (and reaching equalities that
have NO Boolean atom at all, which Path-1 cannot forward).

Mechanism (queued, non-reentrant, per ADR §A.3 / F5):

1. **EUF engine merge log** (`euf.ml`/`euf.mli`, non-frozen): `set_record_merges` /
   `take_merges`. The engine calls no foreign code — it appends each actual class union's two
   original endpoint terms to a log (gated OFF by default ⇒ direct-drive/fabric-off is
   byte-identical, zero hot-path cost); the combinator drains it AFTER `check`, never inside a
   merge. Reset on `pop` (an undrained merge from a popped frame is dropped — completeness-safe).
2. **Combinator drain + notify** (`combine.ml`): `check_a` drives EUF then, on a non-conflict
   result, `drain_and_notify` iterates the merge log. `notify_candidate` filters to the
   A4-erratum boundary domain (Int + in the interface set, §B.5a) — LIA already values those,
   so no fresh unconstrained variable is minted. `try_notify_pair` registers a
   **Congruence-origin fabric edge** whose Γ is the EUF congruence proof
   (`fabric_explain_eq`), reusing the Stage-1b F1(b) precedence + F2 acyclicity + recursive
   expansion machinery; H5-transactional (all fallible work before any mutation).
3. **LIA reaction** (`lia_adapter.ml`): `notify_eq` asserts `Eq(s,t)` as a pair of bounds,
   attributed to the fabric edge. Rides LIA's own trail ⇒ reversed by LIA's own frame `pop`
   (F3 co-location, §C Stage 0 item 5). A later LIA conflict citing the edge expands (F2
   chokepoint) to the real trail literals behind the merge.

`reg_entry.origin = Fixed_value of equality_witness | Congruence` distinguishes the two edge
kinds; F7 `on_fabric_eq` emission fires only for `Fixed_value` (a congruence edge is certified
by EUF's own congruence proof + expanded real-Lit Γ — no new virtual proposition, §C Stage 2).

## Soundness frame (F1–F7)

- F1/F1c: notify-OUT justification currency — LIA attributes the eq to the edge; Γ recorded at
  notification time, precedence-valid (EUF `explain`, CONTRACT-EX).
- F2: congruence edges in the same registry; recursive expansion + strictly-smaller-id
  acyclicity (Stage-1b edges allocated earlier).
- F3: LIA reaction on LIA's trail; registry entry trailed on `fabric_frames`; engine log reset
  on pop. Verified by the pop/re-assert test.
- F5: engine logs, combinator drains after check ⇒ non-reentrant; finite merges/check;
  idempotent; deterministic merge-queue order (I6).
- F6: transactional; per-pair skip on `Term.Overflow`/`Combination_unsound` before any mutation.
- F7: Fixed_value edges emit the `on_fabric_eq` event as before. **Congruence edges are a
  KNOWN CERT GAP (codex, corrected here):** `emit_fabric_eq` emits NO event/proof node for a
  callback (congruence) conflict, so a fabric UNSAT routed through a Stage-2 congruence edge is
  NOT independently checkable yet — the whole-VC Lean/grind path is the backstop, as for every
  UNSAT today. The earlier "already certifiable" phrasing was an overclaim; the congruence-proof
  leaf is future work with #153.
- Frozen surfaces untouched: **check_frozen 14/14** (incl. sat.mli). All new surface is
  combinator/adapter/engine-internal (non-frozen).

## Post-freeze fixes (codex review of 90ac9b8087)

- **Codex #2 (FIXED — land blocker):** `expand_justifications` used one never-cleared `visited`
  set, so a SHARED Stage-1b ancestor cited by two edges' Γ (a DAG diamond, first reachable at
  Stage 2) false-positived as a cycle → a valid UNSAT degraded to `unknown` (probably part of
  the 10 A/B losses). Fixed with an on-path recursion stack (removed on unwind) for
  genuine-cycle detection + a separate `expanded` memo for sharing; back-edge raise and
  injection-time `gamma_acyclic_ok` preserved; returned-Lit order unchanged (determinism).
  Discriminating test `test_stage2_shared_ancestor_no_false_cycle` (via a `For_testing` hook):
  the diamond expands cleanly (RED pre-fix), a genuine self-cycle still raises.
- **Codex #3 (deferred note):** `pop` clears the merge log unconditionally (cursors reset, whole
  log dropped). Completeness-safe (a consumer's action on an already-drained merge unwinds via
  its own trailed state), not unsound. Left as-is; noted.
- **Known flow limitation (found while testing #2, not a regression):** a LIA conflict that only
  becomes infeasible AFTER a Stage-2 notify inside `combine_models_fabric` does not surface as
  UNSAT (the loop re-enters on A-Sat without re-checking B). Incompleteness, not unsoundness (0
  mismatches stands). Candidate follow-up: re-check B after a notify-bearing injection round.

## Acceptance evidence

1. **Discriminating unit tests** (each FAILS if its behaviour breaks):
   - `euf_test.test_stage2_merge_log` (engine): fire-on-merge for an asserted equality AND the
     congruence it triggers, correct endpoint reps, drain semantics, OFF-by-default,
     unwind-on-pop, no-fire-after-rewind, re-fire-on-reassert.
   - `combine_test.test_stage2_congruence_notify_unsat` (end-to-end demo, real EUF+LIA): `x=y ∧
     f(x)>=5 ∧ f(y)<=3 ⇒ UNSAT` via a congruence eq with NO Bool atom — reasoning Path-1
     cannot forward. Checks: UNSAT; a congruence edge created; NO `on_fabric_eq` event; and the
     SAT-without-`x=y` + zero-edge discriminator (the UNSAT is merge-driven, not unconditional).
   - `combine_test.test_stage2_pop_reassert`: F3 backtracking — UNSAT under `x=y`, NOT UNSAT
     after `pop`, UNSAT again on re-assert.
2. **`make test` GREEN** (exit 0): combine 108/0, euf 6419 checks/0, cert 51/0, lemma 27/0,
   driver-equiv 48 files/0 divergence (corpus_classify == oxsmt_cli), regress suite dirs absent.
3. **End-to-end demo**: the congruence-notify test above.
4. **Honest A/B** — QF_UFLIA (659 files), deterministic `--max-effort` (counted, not wall, so
   verdicts are concurrency-independent), callbacks ON (default) vs OFF
   (`OXSMT_NO_FABRIC_CALLBACKS=1`):

   | @100000 | ON | OFF |
   |---|---|---|
   | solved-sat | 241 | 232 |
   | solved-unsat | 120 | 111 |
   | unknown | 222 | 240 |
   | **mismatch** | **0** | **0** |

   - **NET +18 solved** (361 vs 343); 28 gained / 10 lost. **0 mismatches** (soundness gate).
   - All flips are solved↔unknown; NO sat↔unsat (G-equiv: only permitted throughput transitions).
   - Gains 12 mathsat + 16 wisas; losses 3 mathsat + 7 wisas — net-positive on the ADR's Stage-1
     target family despite churn.
   - Sign confirmed at a second budget: @300000 partial (307 files) NET +14, 0 mismatch.
   - Adjacent-family byte-identity spot-check: QF_UF 40 files, 0 diffs (no EUF↔LIA crossing ⇒
     no notification fires, as designed).

## Decision / default

Callbacks are **default-ON** (net +18, 0 soundness issues, velocity prime directive), ADDITIVE
to Path-1 (both active) so purely more propagations. `OXSMT_NO_FABRIC_CALLBACKS=1` is the
measured soundness-equivalent OFF fallback (ADR §C Stage 2). The 10 losses are pure throughput
churn (no wrong verdict) — the "eager every round" overhead the Stage-1b comment predicts;
disable-able via the toggle for a regressing workload. A disagreement-driven refinement (notify
only on would-be-split pairs, mirroring the Stage-1b fix-trigger) is the natural next lever if
the churn wants trimming.

## Rebase note

Built on variant B @ c050d592c8. If the Stage-1b winner differs or the tip advances, expect a
small rebase; the changes are localized to the 10 files below and reuse the Stage-1b registry /
expansion machinery, so conflicts should be mechanical.

## Files

- `smt/theories/euf/euf.ml` / `euf.mli` — merge log (set_record_merges/take_merges, pop reset)
- `smt/theories/euf/euf_adapter.ml` / `.mli` — passthroughs + `fabric_explain_eq` + no-op `notify_eq`
- `smt/theories/lia/lia_adapter.ml` / `.mli` — `notify_eq`
- `smt/combine/combine.ml` / `.mli` — FABRIC sig additions, `check_a` drain-and-notify,
  `notify_candidate`, `try_notify_pair`, `reg_entry.origin`, `fabric_callbacks_off` gate
- `smt/theories/euf/test/euf_test.ml`, `smt/combine/test/combine_test.ml` — the tests above
