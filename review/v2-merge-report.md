# v2 -> refinement structural merge report

## Result
Merge commit **16ddcf351071040c4dfc2579c16dec26ddb530fb** on branch `refinement-codex`.
Parents: `6ea72b867b` (refinement line, Q-003 purity gate) + `d1a98621cb` (v2 line, repair
round 5). Topology preserved (true merge commit). Branch is local/unpushed.

The combined tree BUILDS CLEAN and every documented verification suite passes at its
expected count. One semantic reconciliation was required and is validated. One interaction
probe (a) does not match the task's predicted verdict; it is an over-rejection at the
already-marked modes-integration boundary, not a merge regression -- escalated to main for a
ruling (see "Interaction probes" and review/v2-merge-pending-question.md).

## Conflicts and resolutions
Git's 3-way merge produced **zero textual conflicts**. Only 8 files were modified on both
sides (base = shared ancestor 91a0306e3e): btype.ml, ctype.ml, ikind.ml, jkind.ml,
out_type.ml, typecore.ml, typecore.mli, typemod.ml. All auto-merged; the two lines are
largely orthogonal (mode-axes enforcement machinery vs refinement/VC machinery).

One **semantic** reconciliation (clean text, broken meaning) was needed:

- **typing/typecore.ml `with_refinement_typing_frame`** (~line 4702). This helper +
  `type_refinement` are net-new in the refinement line. The frame saved/reset/restored FOUR
  global refs; two of them -- `ambient_total_context`, `ambient_primitive_application` --
  are base-era totality refs that v2's spec-v2 adaptation (a51b54c37b) DELETED entirely,
  replacing the mechanism with the functionally-threaded `expected_mode.enclosing_totality`
  field. v2 has 0 references to the old globals. The clean merge left the refinement helper
  referencing the deleted globals -> build break.

  RESOLUTION (both-sides preservation, modes-deferred): dropped the four lines touching the
  deleted globals; kept the still-valid `delayed_checks`/`allocations` isolation, the region
  lock, and both modes-integration markers (wiring the total/logical refinement context and
  the logical closure lock into this frame is the modes-integration stage). Post-v2 there is
  no global ambient totality state to isolate -- the enclosing totality is a local value on
  `expected_mode`, and `type_refinement` types the predicate at `Mode.Value.legacy`, so no
  ambient state leaks. Rejected alternatives: re-introduce the deleted globals (= undo v2's
  ACCEPTED redesign); map onto `enclosing_totality` here (= that IS modes-integration work,
  out of merge scope). Escalated to main before finalizing; the merge commit is amendable.

No other orphaned references: scanned every v2-deleted symbol (the `total_context` /
`total_context_violation` types + `Total_context_violation` constructor from typecore.mli,
and the ambient refs) -- 0 surviving references in the merged tree.

## Build
`make -s boot-compiler`: **clean, exit 0** (first combined build of the full refinement
stack + repaired mode axes).

## Suite counts (all via test-one; first suite full w/ install_for_test refresh, rest
no-rebuild against the refreshed _runtest)

| suite | passed | failed | expected |
|---|---|---|---|
| refinement | 12 | 0 | 12 |
| refinement-acceptance | 12 | 0 | 12 (incl. bcf_impure_condition + fp_* anchors) |
| refinement-lean | 2 | 0 | 2 |
| refinement-examples | 6 | 0 | 6 |
| typing-modes | 37 | 0 | 37/37 |
| typing-objects | 21 | 0 | 21 |
| comprehensions | 10 | 0 | 10 |
| typing-modal-kinds | 5 | 0 | 5 |
| typing-jkind-bounds | 71 | 0 | 71 |
| implicit-types | 4 | 0 | 4 |
| typing-modules | 54 | 0 | clean (no drift failures) |
| parsetree | 7 | 0 | clean (no drift failures) |

### Drift resolution (typing-modules 54/0, parsetree 7/0)
The pre-merge expectation was that typing-modules and parsetree would still carry the
documented base drifts (aliases / pr7726 / pr7787; source_jane_street). On the merged tree
both suites are fully green (0 failures). The merge RETIRED those documented base failures:
the v2 line's rebaselines (part of the spec-v2 adaptation + repair rounds, which re-promoted
the mode-axis-affected baselines including source_jane_street and the pr* alias references)
are now present on the combined tree, so the previously-expected drifts no longer surface.
This is an improvement, not a regression -- there is no suite that regressed relative to
either parent.

## Interaction probes (batch compilation, _install/bin/ocamlc.opt 5.4.0+ox; pinned Lean
4.31.0 present)

| # | probe | expected | observed | verdict |
|---|---|---|---|---|
| b | `let bad () = while true do () done in expects_total bad` | REJECT | REJECT ("partial ... expected total") | PASS -- F1 repair survives |
| c | `let probe @ total = fun () -> let rec f = fun x -> f x in f 0` | REJECT | REJECT ("(f) is partial ... expected total") | PASS |
| d | `(Obj.magic 0 : int{ _ > 0 })` | REJECT not-proved | REJECT not-proved | PASS -- VC survives |
| e | Q-003: `if bad () > 0 then needs_pos (bad ()) else 0` (impure cond) | REJECT not-proved | REJECT not-proved | PASS -- purity gate survives |
| f | seal: impl `int{_>5}` behind intf `int{_>0}` | ACCEPT via Lean | ACCEPT (exit 0) | PASS -- seal implication survives |
| a | refined annotation inside a `@ total` closure | ACCEPT (typecheck + verify) | **REJECT** ("closes over the value (>) which is partial") | **DIVERGES -- escalated** |

### Probe (a) analysis (the divergence)
The refined annotation itself is fine: at top level (`let _ = (2 : int{ _ > 0 })`) and inside
an ORDINARY closure (`let f = fun () -> (2 : int{ _ > 0 })`) it ACCEPTS and verifies. It is
rejected ONLY when the enclosing closure is required to be `total`, because v2's
closure-capture totality analysis observes the predicate's `>` (a polymorphic comparison,
partial in the totality axis) elaborated inside the closure body and marks the closure
partial ("closes over the value (>) ... which is partial").

This is the un-integrated boundary flagged by the two modes-integration markers in
`with_refinement_typing_frame`: the frame does not yet establish a total/logical refinement
context or add the closure lock that would isolate the predicate's captured/used values from
the enclosing closure's totality. Every useful refinement predicate uses a comparison, so
this is systemic to "refinement predicate inside a total closure", not a probe artifact.

It is NOT caused by the merge resolution: the rejection comes from v2's closure-lock path,
which never consulted the deleted `ambient_*` refs; keeping the (impossible-to-keep) ambient
reset would not have changed it. The behavior is an OVER-rejection (conservative/sound, not
unsound) -- it rejects a program that ought to be accepted, so it does not threaten
soundness; it is a precision/usability gap that belongs to modes integration (task #6).

The task predicted probe (a) would accept ("axes and refinements coexist without
interference"). Per "do not force green", this was escalated. RULING (main, 2026-07-16):
the merge stands as-is; the rejection is the mode-stub gap in canonical form -- predicates
are checked at total with mentioned variables @ logical, so the predicate belongs to a
logical context that must be isolated from the host closure's capture analysis by the
logical closure lock (the second modes-integration marker). Wiring that lock is modes-integration
work (task #6), not merge scope. It is conservative/sound (over-rejects), and non-regression
was proven (accepts at top level and in ordinary closures; the deleted ambients were never
read by the closure-lock path).

Probe (a) is anchored as an expect-test in refinement-acceptance
(refined_annotation_in_total.ml, case id=refined_in_total_closure,
today=REJECT final=ACCEPT stable=no unlocks=modes) as a child commit of the merge, so modes
integration flips it loudly when the closure lock lands.

### Forward pointer for the modes-integration lane (task #6)
`with_refinement_typing_frame` (typing/typecore.ml ~4702) is precisely where total+logical
predicate checking gets wired. Its two modes-integration markers are the entry point: the
frame must (1) establish the total/logical refinement context for predicate elaboration and
(2) add the logical closure lock that presents captured ambient values at logical mode --
i.e. decide the correct env/lock context so a predicate elaborated inside a total closure is
checked at total-with-logical-captures rather than participating in the host closure's
capture/totality analysis. Landing that lock is what flips the refined_in_total_closure
anchor from REJECT to ACCEPT.

### Note on the merge resolution (for the record)
The dropped globals (`ambient_total_context`/`ambient_primitive_application`) were the
SNAPSHOT-era totality machinery that the v2 redesign deliberately deleted under the
binding-constraint-based ruling; re-adding them would resurrect banned machinery. Under v2
the totality context is functionally threaded (`expected_mode` + env ambient locks), so a
fresh elaboration inside `type_refinement` cannot leak ambient totality state and there is
nothing to save/restore -- the isolation the old lines provided is now inherent.

## Modes-integration marker count
4 on HEAD (unchanged) -- all in typing/typecore.ml (2 in `with_refinement_typing_frame`,
2 in `type_refinement`). Modes integration remains the next stage.
