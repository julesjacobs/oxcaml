# Fabric Stage 2 — merge-notification callbacks (EUF→LIA, ADR-0014 §A.3, §C Stage 2)

Branch `task/fabric-stage2` off `task/fabric-stage1b-B @ c050d592c8` (presumptive Stage-1b
winner). Worktree `worktrees/stage2`. Builder: builder-stage2.

## Goal (ADR §A.3 / §C Stage 2)

When the hub (EUF) merges two classes carrying Int terms shared with LIA, notify LIA so it
asserts the bound-equality directly, instead of the equality round-tripping through the SAT
trail (Path-1). Mechanism 2 of z3 (`new_eq` theory hooks). Stage 2 is verdict-preserving
*infrastructure* (throughput-positive), NOT a solve-rate feature; acceptance = soundness +
equivalence + determinism.

## Foundation read (Stage 1b as landed on variant B)

- `smt/core/fabric.ml/.mli` — currency: `edge_id`, `justification = Real Lit.t | Fabric edge_id`,
  `Explanation.t`, `check_result`, `fixed_bounds`, `equality_witness`, `eq_event`, `trace`.
- `smt/combine/combine.ml` — `Combine(R)(A:FABRIC_CONGRUENCE_CHILD)(B:FABRIC_CHILD)`.
  - registry `edge_id -> reg_entry {gamma; witness; reg_s; reg_t}`, `next_edge` monotone allocator.
  - F2 `expand_justifications` (recursive, visited-set fail-closed, dedup) at both seams.
  - F1-SEM `f1_sem_entails`, F1(b) `gamma_precedence_ok` (assertion-order ledger `order`/`assert_counter`),
    F2 acyclicity `gamma_acyclic_ok`.
  - `try_inject_pair` (Stage-1b fix-trigger, disagreement-driven, at Final in `combine_models_fabric`).
  - `record_props` first-wins+trailed when fabric live; `explain_on` combined_reason cache (trailed).
  - `fabric_frames : (unit->unit, unit) Trail.t` cold-path closure trail; pushed/popped in lockstep.
  - `fabric_off` env gate `OXSMT_NO_FABRIC`.
- `smt/theories/euf/euf.ml` — `merge t a0 b0 reason0` (euf.ml:319): queue to fixpoint; each actual
  union (ra<>rb) is where a class merge happens; congruences re-enqueued as `(p,qq,R_cong)` where
  p,qq are App e-node ids. `Euf.explain a b` gives premise chain (CONTRACT-EX: premises asserted no
  later than the connecting merge). `pop` → `restore_aux`.
- `smt/theories/euf/euf_adapter.ml` — `prem = P_lit|P_axiom|P_fabric`; `justifications_of_prems`;
  `assert_fabric_eq`, `fabric_are_equal = equal_if_registered`.
- `smt/theories/lia/lia_adapter.ml` + `lia.mli` — `Lia.assert_atom t term ~polarity ~premise`
  accepts a **positive Int Eq** (becomes a pair of bounds). Premise token is `Fabric.justification`.
  So LIA can react to `new_eq(s,t)` by asserting `Context.eq s t` with premise `Fabric.Fabric edge`.

## Design (decisions, TCB-grade)

### Architecture: engine logs merges; Combine drains + notifies (queued, non-reentrant)

The EUF engine cannot call foreign code (determinism, no reentrancy). So:

1. **euf engine** accumulates a **merge log** of `(term_a, term_b)` for each actual union, gated by
   `record_merges` (default OFF ⇒ trunk/direct-drive byte-identical, zero hot-path cost). New API:
   `set_record_merges`, `take_merges` (returns `List.rev merges`, clears). Merge order is
   deterministic (merge queue order), so the log is deterministic (I6). Reset to `[]` on `pop`
   (defensive: undrained merges from a popped frame are dropped — a *completeness*-safe drop, never
   unsound; in the normal CDCL(T) loop a `check` drains between every push/pop so this only guards a
   pathological assert-without-check driver). This is the check-local watermark discipline (SW).
2. **Combine** drains after each `A.check_fabric` (Propagate and Final) and after Stage-1b injection.
   For each merged pair `(s,u)`: filter to the A4-erratum boundary domain — BOTH Int-sorted AND
   shared with LIA (`lia_used`), skip otherwise (B.5a: domain filter is combinator-level). Novelty:
   the merge log holds each union once; a re-union is skipped by EUF (`ra=rb`); LIA re-assert is
   idempotent — so no extra novelty set needed (F5).
3. For a surviving pair, register a **congruence-origin fabric edge** in the SAME registry:
   `gamma = A.fabric_explain_eq s u` (the EUF premise justification list, may hold `Fabric` handles
   from Stage-1b edges — those have strictly-smaller ids ⇒ acyclic by construction). F1(b) precedence
   + F2 acyclicity checked (reuse `gamma_precedence_ok`/`gamma_acyclic_ok`); NO F1-SEM (that is the
   LIA-fixed-value Farkas witness; a congruence edge's justification is EUF's proof, sound by the
   proof-producing CC — self-checked by `OXSMT_EUF_SELF_CHECK`). H5 transactional: all fallible work
   (explain, `Context.eq`, checks) BEFORE any mutation; skip pair on `Term.Overflow`/`Combination_unsound`.
4. **LIA** reacts via new `notify_eq : t -> edge_id:edge_id -> Term.t -> Term.t -> unit` (FABRIC_CHILD):
   `Lia.assert_atom lia (Context.eq s u) ~polarity:true ~premise:(Fabric.Fabric edge)`. Rides LIA's
   own trail ⇒ reversed by LIA's own frame pop (F3 co-location, ADR §C Stage 0 item 5). Guarded for
   overflow.

### reg_entry.origin variant (minimal Stage-1b churn)

`reg_entry.witness : equality_witness` → `reg_entry.origin : Fixed_value of equality_witness |
Congruence`. Stage-1b wraps its witness in `Fixed_value`; F7 `on_fabric_eq` emission fires only for
`Fixed_value` (a congruence edge has no Farkas witness — its cert leaf is EUF's existing congruence
proof + the expanded real-Lit Γ, already certifiable; no new virtual proposition, ADR §C Stage 2
"same Shared_eq/congruence cert"). Expansion (F2/F4) reads only `gamma`, untouched.

### Soundness frame satisfied

- **F1/F1c**: notify-OUT justification currency. LIA attributes the asserted equality to the edge
  handle; a LIA conflict citing it expands (F2 chokepoint) to real Γ = EUF premises. Γ recorded at
  notification time, precedence-valid (EUF explain, CONTRACT-EX).
- **F2**: congruence edges live in the same registry; recursive expansion + acyclicity by
  strictly-smaller-id (Stage-1b edges allocated earlier).
- **F3**: LIA reaction on LIA's trail (child pop); registry/edge trailed on `fabric_frames` (same as
  Stage 1b); engine merge log reset on pop.
- **F5**: no reentrancy (engine logs, Combine drains after check; LIA assert can't reenter EUF within
  a check — Stage-1b injection is at Final, a separate cycle); finite merges/check; idempotent.
- **F6**: transactional, per-pair skip on overflow before any mutation.
- **F7**: congruence edge cert leaf = EUF congruence proof + expanded Γ (existing path); no witness.

### Gate: default behaviour + A/B toggle

Callbacks are additive (Path-1 retained — both active), so purely more propagations, verdict-preserving.
Default ON when fabric is on. `OXSMT_NO_FABRIC` disables everything (callbacks need the registry).
Add `OXSMT_NO_FABRIC_CALLBACKS` to disable *just* the notification for the A/B (ON vs OFF).

## Progress

- [x] euf.ml/.mli: merge log + set_record_merges/take_merges + pop reset
- [x] euf_adapter: set_record_merges/take_merges/fabric_explain_eq + notify_eq (no-op hub) + .mli
- [x] fabric/combine: reg_entry.origin variant (Fixed_value | Congruence)
- [x] lia_adapter: notify_eq + .mli (asserts Context.eq as pair of bounds, fabric premise)
- [x] combine: FABRIC sig additions; check_a drain-and-notify; fabric_callbacks_off gate;
      notify_candidate (interface Int filter, B.5a); try_notify_pair (congruence edge, H5 transactional)
- [x] discriminating unit tests: euf_test.test_stage2_merge_log (fire-on-merge asserted+congruence,
      correct reps, drain, unwind-on-pop, re-fire-on-reassert) — engine level
- [x] combine_test.test_stage2_congruence_notify_unsat (end-to-end demo: congruence eq with NO Bool
      atom → LIA UNSAT; congruence edge, NO on_fabric_eq; SAT-without-merge discriminator)
- [x] combine_test.test_stage2_pop_reassert (F3 backtracking unwind/re-fire)
- [x] build clean, frozen 14/14, euf test (6419 checks) + combine test (108 passed) green
- [x] full `make test` GREEN (exit 0; driver-equiv 48/0, cert 51/0, lemma 27/0; regress dirs absent)
- [x] honest A/B (callbacks ON vs OFF) on full QF_UFLIA (659 files), deterministic --max-effort
- [ ] confirm sign-stability at 2nd budget (running)
- [ ] `dune build @fmt --auto-promote` + freeze + report

## A/B RESULT — QF_UFLIA (659 files), deterministic --max-effort=100000 (counted, NOT wall)

corpus_classify ON (default) vs OFF (OXSMT_NO_FABRIC_CALLBACKS=1). Deterministic per
(file,config) — concurrency does NOT affect verdicts (unlike wall-clock wallsweep).

|            | ON  | OFF |
|------------|-----|-----|
| solved-sat | 241 | 232 |
| solved-unsat | 120 | 111 |
| unknown    | 222 | 240 |
| parse-fail | 76  | 76  |
| **mismatch** | **0** | **0** |

- **NET +18 solved** (ON 361 vs OFF 343). 28 gained (ON solves, OFF unknown), 10 lost.
- **0 mismatches** (soundness gate PASSED). All flips are solved↔unknown; NO sat↔unsat flip
  (G-equiv: only the permitted throughput transitions).
- Gains by family: 12 mathsat + 16 wisas. Losses: 3 mathsat + 7 wisas — the ADR's Stage-1
  target family (mathsat/Wisa). Net-positive there despite churn.

Interpretation: eager notification on every boundary merge helps the mathsat/Wisa family
(EUF congruence equalities reach LIA without the Final-split round-trip) more than it hurts
(the churn is the "eager every round" overhead the Stage-1b comment warns about). Net is a
genuine +18 throughput win, verdict-preserving, sound.

Decision: keep callbacks **default-ON** (net-positive, 0 soundness issues, velocity prime
directive); `OXSMT_NO_FABRIC_CALLBACKS=1` is the measured soundness-equivalent OFF fallback
(ADR §C Stage 2). The 10 losses are pure throughput churn (no wrong verdict), disable-able
via the toggle for a regressing workload.

## Discriminating-test rationale (each FAILS if the behaviour breaks)

- merge-log OFF-by-default check FAILS if recording isn't gated (perf/byte-identity guard).
- asserted + congruence fire checks FAIL if `merge` doesn't log, or logs wrong endpoints.
- pop-clears + a,b-not-equal + re-fire checks FAIL if the log isn't reset on pop (stale merge
  after backtrack — the F3 hazard) or if re-assert doesn't re-log.
- congruence-notify-UNSAT FAILS if the callback doesn't reach LIA; edges>=1 FAILS if no edge
  registered; events=[] FAILS if a congruence edge wrongly emits a Farkas witness; SAT-without-x=y
  + edges2=0 discriminator FAILS if the UNSAT were unconditional (not merge-driven).
- pop-reassert FAILS if LIA's callback-asserted eq / its edge don't unwind on pop or re-fire.

## RESCOPE (team-lead, 9am datatypes deadline) — Stage 3 primitives for the DT client

Datatypes-first: deliver the e-graph client surface DT builds against. Committed @ 03e0f2811c
(off Stage-2 90ac9b8087). make test GREEN, frozen 14/14, euf 6426 / combine 108.

Primitives (Euf engine + Euf_adapter forwards, non-frozen, SAT core untouched):
- Per-class tag: set_class_tag/class_tag (witness Term.t = the class's ctor app), trailed
  (U_tag), inherited by surviving root on merge; a two-tagged merge surfaces BOTH tags in
  the merge_event (ctor-clash signal).
- Merge log generalized to Fabric.merge_event {kept;merged;kept_tag;merged_tag} + made
  MULTI-CONSUMER (add_merge_consumer/drain_merges cursors) so DT and the Stage-2 LIA-notify
  each see every merge. merge_event lives in Oxsmt_core.Fabric so Combine reads it without
  depending on the Euf engine.
- Justified merge: existing assert_eq ~premise / assert_fabric_eq ~edge_id.
Seam recommended to team-lead + builder-datatypes: (a) DT bundled as the congruence child's
client → binary Combine + SAT core unchanged (GOALS acceptance). Engine primitives are
seam-agnostic. builder-datatypes stacking task/datatypes on 03e0f2811c.

Tests: euf_test.test_stage3_class_tag (attach/read, inherit, collision-surfaced, pop-restore);
test_stage2_merge_log updated to cursors + two independent consumers.

## LAND PREP (authoritative plan): codex#2 fix + rebase onto trunk

- Codex #2 FIXED (expand_justifications false-cycle on shared fabric ancestor): on-path
  recursion stack (removed on unwind) + separate `expanded` memo; back-edge raise +
  injection acyclicity preserved; determinism preserved. Direct discriminating test via a
  new `Combine.For_testing` hook (diamond expands / genuine self-cycle raises). Report F7
  overclaim corrected (congruence edges emit no cert proof node — known gap); codex#3 noted.
- Squashed the 3 stage2 commits into ONE, rebased `--onto 2509148eee c050d592c8` (trunk had
  landed stage1b-B independently). Conflicts (all keep-both / clean reapply): combine.ml +
  combine.mli FABRIC_CHILD (trunk fabric_verify + my notify_eq), lia_adapter.mli (same),
  combine_test.ml (trunk's pop_owner_strand + f1sem tests interleaved my Stage-2 tests —
  resolved by taking trunk's file and re-applying my mock methods + 3 Stage-2 tests + runner).
- Rebased tip 300bb8d840 (after fmt amend). frozen 14/14; euf 6426/0; combine 116/0.
  Full `make test` running → hand final sha to team-lead + integrator-6.

## RE-REBASE onto 7b4f790291 (trunk advanced: dt-plumbing + stage1b rider)

Trunk moved 2509148eee → 7b4f790291 mid-flight. Re-rebased my single commit onto it.
- combine_test.ml re-conflicted (rider added strand-harness + tie-break tests): resolved
  again by checking out 7b4f790291's combine_test.ml and re-applying my mock methods
  (Make_mock now has a configurable fabric_verify ref — added my methods after it) + 3
  Stage-2 tests + runner calls. ALL trunk tests kept.
- NEW: dt-plumbing added a `Sort.Datatype` constructor → my notify_candidate sort-match was
  non-exhaustive; added a fallthrough `| _ -> false` (sound: non-Int is never a LIA notify
  candidate).
- Rebase mechanics gotcha: an --amend during the rebase's edit-stop folded my changes into
  the rider commit (branch lost 7b4f790291 as parent → not ff-able). Fixed with
  `git reset --soft 7b4f790291 && git commit` → my changes as ONE clean commit on top of
  trunk. Verified `git merge-base --is-ancestor 7b4f790291 HEAD` = ff-able, and my commit's
  diff is ONLY my stage2/3 files (no rider leakage).
- FINAL LAND SHA: 741d25b4d2 (task/fabric-stage2, ff-able on trunk 7b4f790291).
