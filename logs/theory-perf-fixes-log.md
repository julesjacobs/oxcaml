# Theory-perf fix wave — build log

Branch `task/theory-perf-fixes` off trunk tip `d18a9934c0`. Per-fix commits. Audit source:
logs/perf-audit-theories-fable.md (+ codex digest). Review at freeze goes to another leg.

## Step 0 — codex CRITICAL lia.ml:631 (Diophantine "exponential premise") — REFUTED (measured quadratic)
Codex (and my own reading) modeled the `diophantine_conflict` closure's un-deduped
`List.rev_append` premise accumulation (split_row, lia.ml:631-643; pin at 669) as
Fibonacci/exponential on a chain x_i = x_{i-1}+x_{i-2}. MEASUREMENT REFUTES exponential:
- Trigger: Fibonacci equality chain, x0/x1 pinned by BOUNDS (≤0∧≥0, so no front-end
  constant-fold), final `2z = xn+1` UNSAT over ℤ. Chain confirmed to enter eq_frames
  (assert_atom:300-303), so the closure processes it. All verdicts unsat, decisions=0.
- Scaling (std release binary): n=10→7ms, 50→7ms, 100→9ms, 500→23ms, 1000→64ms, 2000→260ms.
- n=1000→2000 = ~4× wall for 2× n ⇒ O(n²), NOT exponential. F(50)≈1.2e10 would OOM at
  n=50; instead 7ms. So the premise lists are NOT Fibonacci-length in practice.
DISPOSITION: no exponential→polynomial lane. The O(n²) closure cost (n sweeps × n rows,
one pin/sweep) is a LOW-MEDIUM backlog item — 260ms only at a synthetic n=2000; real
QF_LIA equation chains are far shorter and rarely hit repeated nonintegral Final. Added to
the verification backlog. LESSON: a structural "un-deduped append" smell does not imply
the worst-case shape is REACHED — measure before opening the algorithmic lane.

## Fixes (in order)
(1) dead term_of_id delete — DONE @494bd7081a. grep-proof (0 reads) + counted-identity
    (75 files QF_LIA/UFLIA/IDL/NIA/RDL/LRA/ALIA/AUFLIA+UF/AX/BV/DT, effort 50000):
    verdict_diffs=0 effort_diffs=0. lia.ml only.
(2) ROW2 explain-order mirror — DONE @5982c3febf. Split an_distinct into witness (no
    explain) + an_distinct_premise (explain, built only on commit); ROW2 does
    witness→build_select→no-op-check→premise; two `<> None` sites (an_normalize,
    weq_propagate_round) use the witness. build_select register/catalog frequency
    UNCHANGED (still only on witness-hit) ⇒ behaviour-identical. Verify: counted-identity
    30 QF_AX storecomm/swap/storeinv/cvc effort 50000 = 0/0; make test 0; cert 33/33;
    array-sat 14/0; row2-red 2/0. arr.ml only.
(3) stores_by_class/row_split cache (AX_OCCIDX pattern) — REMAINING (not started; clean handoff point)
(4) batched predicate rearm — REMAINING
(5) proof-forest reroot-smaller DARK flag (OXSMT_FOREST_BALANCE) — REMAINING

## Handoff state (after fixes #1, #2)
Branch tip carries fixes #1 (@494bd7081a, lia.ml) and #2 (@5982c3febf, arr.ml), each a
clean per-fix commit, verified byte-identical + gates green. Working tree clean except
this log. Fixes #3/#4/#5 NOT started — a continuation resumes from this committed state.
Specs (from logs/perf-audit-theories-fable.md + the lead's ordering):
- #3 stores_by_class (arr.ml, the store-side mirror of the AX_OCCIDX-cached selects index):
  give it the same rebuild-on-invalidate cache as rebuild_selects_idx/occidx_on — a merge
  cursor (detect-every-change AND receive-events: set_record_merges must be on), invalidate
  BY REBUILD (never key-remap), counted-identity. Rebuilt per row_split (Final), so
  lower-frequency than the selects side.
  RED-mutant obligations (the merge-cursor war-story pair — BOTH must go RED before land):
    (a) detect-every-change mutant: drop/weaken one invalidation trigger (skip a merge that
        moves a store term between classes) so the index goes stale under a genuine class
        change. Must produce a wrong verdict / gate RED. Proves the cache invalidates on
        EVERY class-changing event, not just the obvious ones.
    (b) receive-events mutant: disable set_record_merges (or no-op the merge-cursor advance)
        so the cursor never receives the merge stream and the index silently freezes. Must
        go RED. Proves the "AND receive-events" half — the AX_OCCIDX merge-cursor bug where
        the index looked correct but never saw the events (forgot set_record_merges → stale
        → caught only by this mutant). Both mutants distinct; a single "stale-index" mutant
        does NOT discharge this — one can pass while the other's failure mode ships.
- #4 euf.ml rearm_watch Θ(W²): euf_adapter register (euf_adapter.ml:108-117) calls per-atom
  rearm_watch, each scanning all watches. Fix: skip the scan when `term` is freshly created
  by this registration (comment already says the rearm is a no-op then), or maintain a
  term→watch-index map. counted-identity IF watch-arm order preserved; else dark flag.
- #5 euf.ml add_forest_edge reroot-smaller: DARK flag OXSMT_FOREST_BALANCE. NOT byte-id
  (reroot direction → forest shape → explain paths → premises → learned clauses → search).
  A/B CORPUS (the three theories whose refutations go through the proof forest, run
  PER-FAMILY — separate counted+wall tables per logic, not a pooled aggregate):
    - QF_UF  (the direct EUF forest consumer — primary signal)
    - QF_AX  (arrays explain through Euf.explain, so forest shape feeds ROW2 premises)
    - QF_DT  (datatype conflicts explain through the shared engine forest)
  Each family: counted-identity table (--max-effort N, ON vs OFF) AND a wall table, with
  0 both-solved verdict flips required on EVERY family (a per-family 0-flip gate, since a
  pooled flip count can hide a family that regressed while another improved). cert-corpus
  VALID (not byte-id) ON, byte-id OFF. Document "any forest path is a valid explanation"
  against the euf.ml proof-forest invariants (reroot direction is correctness-neutral;
  add_forest_edge already reroots the first endpoint unconditionally, so union-by-size on
  the reroot choice only changes path lengths, never explanation validity).

## Verification backlog (untraced codex HIGHs + Step-0 residual)
- lia.ml:631 diophantine_conflict O(n²) closure (Step-0 residual; LOW-MEDIUM, synthetic-only so far).
- combine.ml:703 find_disagreement O(n²) pairwise (QF_UFLIA large interfaces) — untraced.
- euf.ml:918 per-check disequality rescan Θ(D)/round — untraced.
- arr.ml:626 upward-read introduction Θ(d²) — untraced.
- dt.ml:675 field-relevance Θ(depth²); dt.ml:715 constructor-split re-sort — untraced.
- simplex.ml:267 column-scan Θ(P·V) — DEDUPE RESOLVED with row-inplace owner (unboxed-builder,
  task/row-inplace @f9c6da0f2e): SEPARATE lane, out of row-inplace scope, worth doing.
  row-inplace changed only ROW STORAGE (linexp Map → mutable sparse Lx, in-place
  add_scaled/remove); it modified the BODIES of the pivot scans but left the scan-all-vars
  iteration untouched. The Θ(P·V) column axis is the three global `Dynarray.iter t.vars` scans
  that probe `coeff k.row col` per var — update (post-row-inplace simplex.ml:518), pivot subst
  (simplex.ml:620), pivot_and_update (simplex.ml:642). Lx is row-major (no column→rows index),
  so storage layout and column-incidence traversal are separable. SEQUENCING: a
  column→(basic rows containing it) incidence index must be maintained incrementally exactly
  where column membership changes — inside row-inplace's add_scaled_in_place/remove_in_place
  (id entering/leaving a row) and pivot — so the column-incidence lane is a clean follow-ON:
  build it on top of row-inplace (or land after), NOT in parallel on trunk, to avoid
  re-plumbing the same mutation sites twice. Not a blocker; sequencing only.
