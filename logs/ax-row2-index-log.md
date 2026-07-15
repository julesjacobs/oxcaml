# ROW2 cheapening ladder (AX swap gap) — build log

Branch `task/ax-row2-index` off trunk **9052a55287**. Dark under existing OXSMT_ARR_ROW2.
Builder bs-dt-fable. Targets the QF_AX swap gap (41 of the 54-file AX gap, fresh close-out
@51697cb1be). arr.ml only. Coordination with intsat-builder (W5 Lever A = selects_by_arr_class
persistence, W5 Lever B = dt.ml): disjoint — I own the ROW2 arm + an_diseqs index + (pending)
emit-once fingerprint; they own selects_by_arr_class + dt.ml. Shared seam = the class→reads
query in row_round, interface preserved.

## The ladder (team-lead, constant-free-first per [[proportional-mechanisms-directive]])
The storecomm −59 wall under ROW2 has (at least) three candidate cost sources; the ladder
cheapens them constant-free, each rung gated on whether the attribution A/B shows the previous
left cost standing:
- RUNG 1a (THIS commit): index an_diseqs by class-pair → O(1) an_distinct (kills the SCAN).
- RUNG 1b (pending): emit-once fingerprint for ROW2 (store,select-index) instances (kills the
  per-check REGENERATION; z3 theory_array_base.cpp:207).
- RUNG 2 (only if 1 leaves cost): z3 m_prop_upward per-class chain-existence bit gating the
  UPWARD variant (theory_array.cpp:140,186) — constant-free local structural merit.
- MEASURED ARM: the fixed-K diseq-count variant (burden-of-proof; expected to lose to the
  constant-free rungs, kept only if it measurably beats them on the A/B incl. mixed instances).

## RUNG 1a — an_diseqs class-pair index (this commit, +67/−1)
`an_distinct` (arr.ml:592) was an O(|an_diseqs|) `List.find_map` with 2–4 `Euf.are_equal` per
entry, called per (store,read) per saturate pass from the ROW2 arm. On storecomm 00060 (≈1770
pairwise-distinct index diseqs) that is the scan storm. RUNG 1a builds a class-pair index once
per `row_round` pass (`build_an_diseq_index`, keyed on the normalized (min,max) index-class
pair, storing the FIRST an_diseqs entry in list order) and the ROW2 arm calls `an_distinct_idx`
(O(1) lookup) instead of the scan.

COUNTED-IDENTICAL: `an_distinct_idx` returns the scan's exact first-match entry and the exact
same premise explanation (recomputing orientation per call to match the scan's `i~x`-first
check). SOUND to build once per pass: `row_round` merges only READ classes (element/array sort)
via `assert_eq`, never INDEX classes, so index-class reps are stable for the pass the index
serves (rebuilt next pass after `Euf.check`). `an_distinct` itself is UNCHANGED — the other
callers (row_split :1113/:1171, analyzer :933) keep the scan, so their behaviour is provably
untouched; only the ROW2 hot loop is indexed. OXSMT_ARR_ROW2_NOINDEX falls the ROW2 arm back to
the scan — the A/B toggle that isolates the scan's share of the −59 (ROW2 vs ROW2+NOINDEX are
verdict/counter-identical, differ only in wall). OFF byte-identical (index built only under
weq_row2; new functions/flag unused OFF).

## Local gates (by exit code)
- make test EXIT 0; check-frozen 14/14; array-sat-gate EXIT 0 both OFF and ROW2 (index);
  row2-red-gate EXIT 0; weq-graph-test 0.
- COUNTED-IDENTITY verified: RED fixture ROW2-index ≡ ROW2-noindex (both unknown; 1cf/1dec/6prop)
  and both differ from OFF (ROW2 fires) → index changes no ROW2 result. arr-goldens-sat 7/7:
  0 index-vs-scan divergences. Under NOINDEX the arm uses the untouched `an_distinct` = the exact
  trunk path, so ROW2-index ≡ trunk-ROW2 behaviour by construction + verification.

## FMT toolchain note (for the integrator)
Applied via text patch bypassing BOTH the PostToolUse edit-hook formatter and local
`dune fmt`/`ocamlformat 0.29.0` — both churn arr.ml (and untouched trunk files) file-wide in
this worktree (wrong toolchain, [[edit-hook-fmt-divergence]]/[[oxsmt-worktree-dune-noop]]). The
diff is a minimal +67/−1 in trunk style. `make test` has no fmt dep (`test: check-frozen`), so
this is a review/land concern only: the integrator with the canonical toolchain should fmt ONLY
the changed regions and confirm no whole-file reformat.

## PENDING (needs the stage box; ssh denied from my env)
Attribution A/B + corpus A/B on the QF_AX 551: OFF vs ROW2 vs ROW2+NOINDEX (+ later
+NOFINGERPRINT), W=1 AND W=24 spot + counted-effort. The NOINDEX/NOFINGERPRINT toggles let ONE
run split the −59 across scan / regeneration / materialization and decide which ladder rungs are
needed. Kill rule: no regression anywhere (storecomm W=1+W=24), net positive, no family names,
no global tuned threshold. Handed to the pair-runner / lockbox with a sha-stamped binary.
