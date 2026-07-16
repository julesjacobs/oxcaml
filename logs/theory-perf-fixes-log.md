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
(3) stores_by_class cache (AX_OCCIDX store-side twin) — DONE (arr.ml, dark OXSMT_AX_OCCIDX).
    See "Fix #3 detail" below. OFF-vs-ON counted-identity GREEN; RED obligation DISCHARGED
    via a targeted theory-API unit test (make arr-store-idx-test) that FORCES the no-pop
    merge window — BOTH spec merge-mutants (detect-every-change, receive-events) go RED there
    (per the lead's ruling; the corpus can't reach the window, but a scripted theory-API
    drive can).
(4) rearm_watch Θ(W²) → O(1) watch_index map — DONE (euf.ml, FLAGLESS byte-identical).
    See "Fix #4 detail" below.
(5) add_forest_edge reroot-smaller — DONE (euf.ml, DARK OXSMT_FOREST_BALANCE, not byte-id).
    See "Fix #5 detail" below; per-family A/B pending.

## Fix #3 detail — stores_by_class store-side cache (arr.ml, dark OXSMT_AX_OCCIDX)
Store-side twin of the landed selects-side AX_OCCIDX cache. New fields store_idx (cached
(class -> store term) Hashtbl) + store_cursor (a SEPARATE Euf.merge_cursor, so both the
selects and store caches see every merge — a shared cursor would let the first drainer
consume the other's events). stores_by_class: OFF ⇒ rebuild_stores_idx every call
(byte-identical to trunk); ON ⇒ drain store_cursor (invalidate-by-REBUILD on any merge),
else return cache. Invalidation triggers: merge-drain (stores_by_class), new store
registration (catalog), pop.
IDENTITY ARGUMENT: a cache hit means the store set AND every store's e-class are unchanged
since the build, so rebuild_stores_idx would produce the identical Hashtbl (same Hashtbl.add
insertion order over t.store_terms ⇒ identical find_all order). Only verdict-affecting caller
is row_split (line ~939); the other caller weq_analyze_final is diagnostic (dark
OXSMT_ARR_WEQ_ANALYZE, emits no lemmas).
OFF-vs-ON COUNTED-IDENTITY: swap 40 + storecomm 40 + storeinv 38 + cvc 1 = 119 files,
--max-effort 30000: 0 divergences, 0 timeouts. GREEN.
RED-MUTANT RESULT (per lead ruling — targeted theory-API unit test):
  CORPUS behaviour first (why the spec's per-mutant RED can't come from a .smt2): on the
  QF_AX corpus the three invalidators are REDUNDANT — store-class changes are correlated with
  catalog/pop events, so any ONE trigger catches the staleness that matters.
  - (a) merge-only off (drain but never invalidate): 119-file OFF-vs-ON = 0 diverge.
  - catalog-only off: read5 GREEN. pop-only off: read5 GREEN.
  - MEGA (merge+catalog+pop ALL off): read5 RED (verdict unsat both; counters 31/344/904 vs
    59/1376/3186 — verdict PRESERVED, since row_split emits SOUND splits regardless of which
    congruent store it picks; staleness perturbs SEARCH, not soundness).
  So "the corpus doesn't hit the merge-only window" is a workload accident, not a guarantee —
  exactly the silent-regression risk the per-trigger RED exists to catch (lead ruling). The
  window IS reachable and merge IS its sole catcher: a same-level array equality that merges
  a store into the queried array's class by congruence registers NO new store (no catalog)
  and needs NO pop.
  UNIT TEST (make arr-store-idx-test, smt/theories/arr/test/arr_store_idx_test.ml) FORCES it
  at the theory API: register a read select(a,j) + a store st=store(b,i,v); Final #1 builds &
  caches the store index (st not congruent to a → Sat, no split); assert a=st at the SAME
  level (a's class padded a=a2=a3 so union-by-size keeps a's root → the surviving root is not
  st's stale key); Final #2 must find st congruent to a and emit a ROW Split.
    - real code: Final #1 = Sat, Final #2 = Split → PASS (2/0).
    - MUTANT (a) detect-every-change (drain, never invalidate): Final #2 = Sat → RED.
    - MUTANT (b) receive-events (store_cursor = None): Final #2 = Sat → RED.
  BOTH spec mutants go RED in the forced window; real code passes. Runs under
  OXSMT_AX_OCCIDX=1 (cache live); passes flag-off too (rebuild path). Mega-mutant RED and
  OFF-vs-ON GREEN remain in the evidence set. Obligation DISCHARGED.

## Fix #4 detail — rearm_watch O(1) via watch_index (euf.ml, FLAGLESS byte-identical)
euf_adapter.register_atom calls Euf.rearm_watch per predicate atom; rearm_watch did a
Dynarray.iteri over ALL watches to find the one matching term ⇒ Θ(#predicates × #watches).
New field watch_index : int Term.Table.t maps w_atom term -> its index in [watched], written
in add_watch (a term is watched at most once: register is idempotent). rearm_watch now does a
GUARDED O(1) lookup (index in range AND w_atom still equals term) instead of the scan.
BYTE-IDENTITY ARGUMENT: since each term is watched exactly once, the scan acts on exactly the
watch the map returns (same set_reported + mark_touched-both-endpoints, same effect), and the
guard makes the lookup return nothing exactly when no live watch matches — so it reproduces
the scan even when the map holds a stale entry (past-truncation index or reused slot; the
map always holds the latest index via Term.Table.replace in add_watch, so no pop bookkeeping
is needed). mark_touched duplicates are already deduped into a set in propagate (Int_set at
the touched-drain), and touched-length/prop_mark bookkeeping is unchanged. New engine field
does not enter Recorder events ⇒ cert bytes unchanged. Covered by the broad batch identity
(default path, all logics) branch-vs-base.

## Fix #5 detail — add_forest_edge reroot-smaller (euf.ml, DARK OXSMT_FOREST_BALANCE)
merge picks the union-find child/root BY SIZE but add_forest_edge unconditionally rerooted
the FIRST endpoint a, whose forest path length is unrelated to class size ⇒ Θ(n²) reroot work
on long equality/congruence chains. Fix (dark): reroot the endpoint in the SMALLER class
(sizes read pre-union at the merge call site: find a / find b are still the two distinct
roots), bounding reroot cost by the smaller subtree — the union-by-size analog the union-find
already applies. NOT byte-identical: the edge is undirected so either orientation is a valid
explanation (the reason rides the a—b edge regardless of which endpoint stores it; the premise
SET along the a—b tree path is orientation-invariant), but the forest SHAPE changes ⇒
explain_core child-ordering/paths ⇒ premise order ⇒ learned clauses ⇒ search. Verdict-
preserving (explanation VALIDITY unchanged), hence dark + per-family A/B, never a silent flip.
OFF (default) takes the exact trunk branch (reroot a; set_fedge a b reason) ⇒ byte-identical,
covered by the broad batch identity. ON: per-family A/B (QF_UF/QF_AX/QF_DT) PENDING.

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

## FREEZE — batch validation (fixes #3 @e8420c0084, #4 @39dda9be26, #5 @b35248f545)
Base = branch base d18a9934c0 (fe2l4-base worktree binary, rebuilt clean). Harness scripts
in logs/ (store-idx-offon-identity.sh, theory-perf-batch-identity.sh, forest-balance-ab.sh).
Box heavily loaded (no wall A/Bs per lead) — counted + byte-identity + cert signals only.

GATES (make test, EXIT 0): check-frozen 14/14; cert_corpus_gate unsat 33 VALID=33 INVALID=0
(+ repeat-emit 33 VALID); array sat-gate 14/0; row2-red-gate 2/0 (both unknown, not unsat);
euf self-test 6440/0 (SELF_CHECK oracle); euf ADAPTER self-test 1493/0 (exercises #4
rearm_watch); lia 587/0; sat 112/0; satpre 41/0; chrono 18/0; lgc 7/0; driver-equiv 0
divergence; harness goldens 69/0.

BROAD MULTI-LOGIC COUNTED-IDENTITY (branch vs base, default flags, effort 30000) — covers
#1, #2, #3-OFF, #4 (flagless), #5-OFF: **141 files, 0 divergences.**
  QF_BV 15/0 (4 timeout), QF_UF 15/0, QF_LIA 15/0, QF_AX 15/0, QF_DT 15/0,
  QF_UFLIA 18/0 (smallest files; the Certora sample is parse-bound, times out identically on
  both binaries), tests/cases 48/0.

FIX #3 (dark OXSMT_AX_OCCIDX) — OFF-vs-ON counted-identity: 119 QF_AX files
(swap 40/storecomm 40/storeinv 38/cvc 1), effort 30000, 0 diverge, 0 timeout. cert 33/33
VALID under OXSMT_AX_OCCIDX=1. RED: mega-mutant (all three invalidators off) diverges on
cvc/read5 (verdict-preserving counter change); single-trigger mutants (merge / catalog / pop)
each GREEN (redundant invalidation — see §Fix #3 detail; ESCALATED to lead for ruling).

FIX #5 (dark OXSMT_FOREST_BALANCE) — per-family ON-vs-OFF, effort 30000, 40 files each:
  QF_UF both_solved 40 verdict_flips 0 counter_diffs 0
  QF_AX both_solved 40 verdict_flips 0 counter_diffs 9  (lever active — reroot choice differs)
  QF_DT both_solved 40 verdict_flips 0 counter_diffs 0
  **0 verdict flips on EVERY family** (reroot direction is verdict-neutral / sound). cert
  33/33 VALID under OXSMT_FOREST_BALANCE=1 (explanations valid, not byte-id). Wall table
  DEFERRED (box loaded — no reliable wall A/B; the perf claim is the Θ(n²)→bounded reroot
  argument, to be wall-measured on a quiesced box before any flip-ON proposal). NOTE:
  counter_diffs=0 on the sampled QF_UF/QF_DT means reroot-smaller coincided with trunk
  reroot-a there; the lever only bites where class sizes are imbalanced (QF_AX here).

STATUS: FROZEN for dual review. #4 is flagless byte-identical (land candidate). #3 and #5 are
DARK (ship inert; default build byte-identical, proven by the 141-file broad identity). #3's
per-mutant RED obligation is PARTIAL — awaiting lead ruling (mega-RED + by-construction vs.
synthetic-input hunt). Do NOT land before review.
