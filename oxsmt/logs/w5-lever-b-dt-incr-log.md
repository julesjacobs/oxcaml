# W5 Lever B — OXSMT_DT_INCR (dt.ml witness/occurs caching)

Branch task/dt-incr, off trunk 9052a55287. Spec: the "Lever B SPEC" section of
adapter-incr's logs/w5-adapter-incr-log.md (committed by intsat-builder @6f338c59f0).
dt.ml only (bs-dt-fable confirmed disjoint); dt.mli unchanged (check-frozen 14/14).

## WHAT SHIPPED
Dark flag `OXSMT_DT_INCR`, default OFF byte-identical. Caches the two per-check rebuilds
dt.ml does redundantly:
- `build_witnesses` (class-id -> canonical witness constructor + clash) — called once per
  saturate round, again at `check` after the fixpoint, and again in every
  `constructor_model_gen` (public model + `check_model`). All but the round-level call
  recompute the SAME table when no class changed since.
- `occurs_check` (acyclicity DFS) — a pure function of the witnesses + class structure.

Design = Lever A's exactly: **invalidate-by-REBUILD, never key-remap** (so no
stale-class-key-across-pop hazard, the #51 / dt-liveref class). A private Euf merge cursor
(`add_merge_consumer`) plus `set_record_merges engine true` ENABLED UNDER THE FLAG (the
war-story half — DT's engine has recording off by default and no other consumer, so
without this the drain returns [] forever and the cache never invalidates on a merge).
Caches dropped (`invalidate_incr`) on: a non-empty merge drain (`sync_merges`, called at
the single cache-read entry `build_witnesses`), a new constructor-term registration
(`catalog`), and push/pop. A cache hit ⇒ constructor-term set AND every one's class
unchanged ⇒ the rebuilt table is identical (same insertion order ⇒ identical witness
selection + conflict premises), and every call site only READS the table
(`Hashtbl.find_opt`), so physical reuse is safe.

Lazy explain (spec sub-lever 3) was NOT done: the `derived_premise`/`reason_of_implied`
explains are precedence-valid only right after the union (dt.ml:162 / :527 contract), so
deferring them risks unsound reasons — out of scope for a counted-identity cache.

## GATES (all EXIT 0)
- make test: exit 0 (sat_test 106/0, satpre 41/0, all theory suites).
- check-frozen: 14/14 interfaces match (dt.mli untouched).
- dt-sat-gate: 28/28 OFF and ON.
- dt-multi-query-gate (the #54 registry-lifecycle gate — the cache is era-bound, so this
  matters): all checks pass OFF and ON, incl. set_datatypes replace/reset fail-loud and
  content-gate.

## COUNTED-IDENTITY (OFF vs ON: verdict + conflicts/decisions/propagations byte-identical)
Script logs/dt-incr-identity.sh.
- Barrett (barrett-jsat/typed/v1), 60 files @10s: identity_diverge=0, timeouts=0.
- blocksworld (target), 120 files @4s: identity_diverge=0 (30 completed, 90 timed out).
- Bouvier, 40 files @2s: identity_diverge=0 (all timed out — no signal, hardest slice).
ANY divergence = kill; none seen.

## RED MUTANTS (each invalidation trigger proven load-bearing; mutant-verified)
Both on Barrett/typed/v1 @10s (search-heavy, push/pop + merges, all 60 complete):
1. **pop-invalidation neutralized** (`if incr_on then invalidate_incr t` → `ignore` in
   `pop`): identity_diverge = **11/60**. Signature: OFF sat/unsat → ON `unknown` with
   decision/propagation counts collapsed (stale post-pop table returns wrong witnesses →
   ROW/selector mis-fire → search fails to close). Restore → 0/60 (binary md5 changed,
   confirming the rebuild is real, not a stale-mutant self-catch).
2. **merge-recording neutralized** (drop `set_record_merges engine true`, keep the cursor —
   the exact Lever-A war story): identity_diverge = **3/60**. Signature: verdict preserved,
   decisions/propagations drift by 1–3 (cache goes stale on an intra-saturation merge).
   Restore → 0/60.

## WALL A/B @2s (OFF vs ON) — script logs/dt-incr-ab.sh
| family | files | solved OFF | solved ON | Δ | wall OFF ms | wall ON ms | disagree |
|---|---|---|---|---|---|---|---|
| blocksworld (target) | 120 | 30 | 30 | 0 | 186368 | 185763 | 0 |
| Barrett | 120 | 120 | 120 | 0 | 937 | 915 (−2.3%) | 0 |
| Bouvier | 120 | 0 | 0 | 0 | 241601 | 241598 | 0 |

Net **Δ=0 @2s, 0 verdict disagreements**, wall neutral-to-slightly-better (Barrett −2.3%
where files actually solve; blocksworld −0.3%; Bouvier flat — its first-120 all time out at
2s regardless). This is Lever A's shape and the spec's forecast to the letter: the
per-check rebuild is NOT the 2s-binding cost on blocksworld's hard tail (z3's edge there is
~7× fewer decisions — a splitting/search-size gap incrementalization does not close), so no
conversions at 2s; the win is a real rebuild-elimination that shows as a small wall cut on
the rebuild-bound (fast-solving) subset.

## VERDICT: KEEP DARK (not killed)
KILL = identity break or net-negative. Neither: 0 identity divergence everywhere, Δ=0 and
wall neutral-to-positive. Sound (counted-identity, invalidate-by-rebuild, both triggers
RED-verified), OFF byte-identical. Posture = Lever A / bv-rw3: lands dark, converts as the
front-end throughput wave lowers the 2s floor. No ON recommendation at this time (no 2s
conversions to justify flipping default).
