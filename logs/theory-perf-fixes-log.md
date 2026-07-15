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
  BY REBUILD (never key-remap), per-trigger RED mutants (stale-index mutant must be caught),
  counted-identity. Rebuilt per row_split (Final), so lower-frequency than the selects side.
- #4 euf.ml rearm_watch Θ(W²): euf_adapter register (euf_adapter.ml:108-117) calls per-atom
  rearm_watch, each scanning all watches. Fix: skip the scan when `term` is freshly created
  by this registration (comment already says the rearm is a no-op then), or maintain a
  term→watch-index map. counted-identity IF watch-arm order preserved; else dark flag.
- #5 euf.ml add_forest_edge reroot-smaller: DARK flag OXSMT_FOREST_BALANCE. NOT byte-id
  (reroot direction → forest shape → explain paths → premises → learned clauses → search).
  Full A/B (counted+wall, 0 flips), QF_UF/QF_AX/QF_DT targets; cert-corpus VALID (not
  byte-id) ON, byte-id OFF; document "any forest path is a valid explanation" against the
  euf.ml proof-forest invariants.

## Verification backlog (untraced codex HIGHs + Step-0 residual)
- lia.ml:631 diophantine_conflict O(n²) closure (Step-0 residual; LOW-MEDIUM, synthetic-only so far).
- combine.ml:703 find_disagreement O(n²) pairwise (QF_UFLIA large interfaces) — untraced.
- euf.ml:918 per-check disequality rescan Θ(D)/round — untraced.
- arr.ml:626 upward-read introduction Θ(d²) — untraced.
- dt.ml:675 field-relevance Θ(depth²); dt.ml:715 constructor-split re-sort — untraced.
- simplex.ml:267 column-scan Θ(P·V) — DEDUPE pending with row-inplace owner (messaged unboxed-builder).
