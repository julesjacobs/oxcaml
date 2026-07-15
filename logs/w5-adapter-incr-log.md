# W5 — AX+DT adapter incrementalization (task/adapter-incr, off 9052a55287)

Design basis: brainstorm2-smallgaps-codex.md levers 1-2. Split agreed with bs-dt-fable
(disjoint): I own arr.ml selects_by_arr_class persistence + dt.ml; they own arr.ml ROW2
arm + an_diseqs index + fingerprint. Seam preserved: `Hashtbl.find_all idx (class_of base)`.

## Lever A — OXSMT_AX_OCCIDX (arr.ml), committed @b3c28bca6e
Cache selects_by_arr_class; invalidate-by-REBUILD (never key-remap → no #51 stale-key
hazard) on Euf class union (private merge cursor + Euf.drain_merges), select registration,
or pop. Requires Euf.set_record_merges under occidx_on (see RED below). arr.mli unchanged,
OFF byte-identical.

COUNTED-IDENTITY (OFF vs ON, verdict + conflicts/decisions/propagations byte-identical):
86 QF_AX files, 0 divergence — storecomm 40 / swap 20 / storeinv 25 / cvc 1.
Script: logs/ax-occidx-identity.sh.

RED (invalidation is load-bearing): the intermediate build that registered the merge
cursor but did NOT enable Euf.set_record_merges → drain_merges saw nothing → cache never
invalidated on merges → 3/20 swap files diverged in counters OFF-vs-ON. The counted-identity
gate caught the stale-cache bug directly; fixed by enabling recording under occidx_on.

WALL A/B @2s headline (OFF vs ON):
| pool | files | solved OFF | solved ON | Δ | wall OFF ms | wall ON ms | disagree |
|---|---|---|---|---|---|---|---|
| storecomm | 210 | 188 | 188 | 0  | 107770 | 107285 | 0 |
| swap      | 302 | 256 | 257 | +1 | 150384 | 145014 | 0 |
Combined: net +1 @2s (swap), 0 (storecomm); ON wall faster (swap −3.6%, storecomm −0.5%);
0 verdict disagreements. NOT killed (kill = identity break or net-negative). Marginal
net-positive with a real throughput speedup; the per-check rebuild is NOT the 2s-binding
cost on storecomm's unsolved tail (wall barely moves there), so the report's +8..15 estimate
did not materialize at 2s — the win is the swap +1 + a pervasive small wall reduction.
Script: logs/ax-occidx-ab.sh.

## Lever B — OXSMT_DT_INCR (dt.ml): PENDING
Dirty-class witness cache + no-change gate + lazy explain, replacing per-check
build_witnesses (dt.ml:289) + fresh occurs-DFS (:636). Same invalidate-by-rebuild + trailed
discipline. Target: blocksworld 39. Commit 2, counted-identity + push/pop RED + wall @2s.

## Lever A — push/pop RED mutant (dedicated, mutant-verified)
Neutralized the pop-invalidation (`if occidx_on then t.occ_idx <- None` → `ignore t.occ_idx`)
and re-ran the counted-identity sweep on swap (whose search push/pops heavily): **14/30
files flipped `sat` → `unknown`** OFF-vs-ON — the stale-across-pop cache returns wrong
`class_of` buckets, the ROW read-through mis-fires, and the solver fails to find the model
(degrades to unknown, NOT a wrong-unsat, but a wrong VERDICT). Restoring the line → 0
divergence on all 30. So the pop-invalidation is load-bearing and the counted-identity gate
discriminates it cleanly. (Two independent RED discriminators now exist for Lever A: this
pop mutant, and the merge-recording war story below.)

## THE MERGE-CURSOR WAR STORY (read this before building Lever B)
Lever A's first build was WRONG in a way that produced correct-looking code and passed a
storecomm spot-check, but broke counted-identity on swap. The bug: I registered a private
Euf merge cursor (`add_merge_consumer`) to detect the class unions that invalidate the
cache, but I did NOT call `Euf.set_record_merges engine true` — that toggle is only set in
the `weq_on` branch of `create`, and Lever A runs with weq OFF. With recording off,
`Euf.drain_merges` returns `[]` forever, so the cache NEVER invalidated on a merge, went
stale, and diverged (3/20 swap). The counted-identity gate caught it immediately; the fix
was one line (enable recording under `occidx_on`).

GENERALIZED LESSON for any adapter-incrementalization cache keyed on e-class identity:
`class_of` / class-rep canonicalization changes silently under you on every union, and the
ONLY sound way to reuse anything keyed by it is to (a) detect every class-changing event
and (b) actually RECEIVE those events. Both halves are load-bearing and both are easy to
half-wire. This is the same family as the #51 dt-liveref stale-registry bug
([[dt-liveref-overwrite-wrong-unsat]]): a cache that looks monotone but isn't, across a
state change (merge or pop). The defense that WORKED: invalidate-by-REBUILD (never remap a
persisted key) + a counted-identity sweep over a push/pop-heavy family (swap) as the
discriminator, plus a deliberate mutant to prove each invalidation trigger is load-bearing.

## Lever B SPEC (OXSMT_DT_INCR, dt.ml) — for the fresh builder
GOAL: cache the datatype per-check work that dt.ml currently rebuilds every check, same
invalidate-by-rebuild discipline as Lever A. dt.ml is entirely ours (bs-dt-fable confirmed
they do not edit it). Flag `OXSMT_DT_INCR`, default OFF byte-identical, dt.mli unchanged.

DEFECT SITES (verify at build time — line numbers drift):
- dt.ml:289 `build_witnesses` — rebuilds the class→constructor witness table mid-check,
  scanning all constructors/selectors/testers/dt-terms every check.
- dt.ml:636 the occurs-check DFS — a fresh full DFS every check.
- eager `Euf.explain` when deriving equalities (build the reason LAZILY on demand instead).

DESIGN (three sub-levers, each independently gated/measurable if you want):
1. No-change gate: skip the whole witness rebuild + occurs-DFS at a check when nothing
   datatype-relevant changed since the last check. Detect "changed" with a PRIVATE Euf
   merge cursor — AND REMEMBER TO `Euf.set_record_merges engine true` under the flag (the
   war story above; dt.ml likely also only enables recording if some other feature wants
   it). Also invalidate on dt-term registration and on pop.
2. Dirty-class witness cache: cache the witness table; on a drained merge, rebuild (Lever-A
   shape) — or, as a Stage-2 refinement, update only the merged classes (higher risk, do
   the full-rebuild-on-change version FIRST and prove counted-identity before any
   incremental-append).
3. Lazy explain: defer `Euf.explain` until the reason is actually consumed.

#51-CLASS TRAIL HAZARDS TO WATCH:
- Stale witness cache across POP (merges retract, class reps revert) — the exact mutant
  that flipped 14/30 swap here. Invalidate the cache on pop; mutant-verify it.
- Class-rep canonicalization: any key that is a `class_of` id is only valid until the next
  union; do not persist it across a merge without the cursor-driven invalidation.
- occurs-DFS memo: if you memoize occurs results per class, that memo is invalid after any
  merge that changes the class graph — same invalidation.

GATES (mandatory, in order):
- COUNTED-IDENTITY: OFF-vs-ON verdict + conflicts/decisions/propagations byte-identical.
  Sweep the QF_DT gap family (blocksworld 39 + barrett) AND a push/pop-heavy DT set. Use a
  copy of logs/ax-occidx-identity.sh with OXSMT_DT_INCR. ANY divergence = kill (it is a
  cache, not an inference change).
- RED mutant per invalidation trigger: neutralize the pop-invalidation (and the merge
  invalidation) and show counted-identity DIVERGES (expect sat/unsat flips or counter
  drift), then restore → 0. Mirror the two REDs demonstrated for Lever A.
- make test + check-frozen EXIT 0.

MEASUREMENT: wall A/B @2s on blocksworld 39 (the target) + a broader QF_DT sample.
REALISTIC EXPECTATION (calibrate against Lever A): Lever A was counted-identical + a real
few-% wall cut but only +1 net @2s (swap), 0 on its primary target (storecomm) — the
per-check rebuild was NOT the 2s-binding cost on the hard tail. Expect the same shape for
DT: blocksworld's z3 edge is ~7× FEWER decisions (a splitting-policy/search-size gap, per
the smallgaps report), which incrementalization does NOT close — it only speeds the
rebuild-bound subset. So forecast 0..+small @2s with a pervasive wall reduction; the win, if
any, is a few near-wall files, not the 39. Kill if net-negative or identity break.
