# SAT-core perf fix wave — build log (task/satcore-perf-fixes)

Builder: recmin-reviewer (auditor-builds). Base d18a9934c0. Findings from the SAT-core perf
audit (logs/perf-audit-satcore-fable.md). Review at freeze by another leg.

Collision-aware sequencing (sat.ml is also touched by S1 freeze + watch rung-2): PHASE 1 =
low-collision byte-identical fixes (below); PHASE 2 (#1 per-var Dynarray→raw, folding in #3)
HELD until the team lead signals S1 has landed.

FORMATTING NOTE: the Edit-hook ocamlformat differs from trunk's pinned version and reformats
the whole file (526-line churn for ~40 logical lines) — a known divergence. All fixes were
therefore applied via a bash-run string-replace script (no Edit hook), keeping the diff to
exactly the logical change (76 lines, 7 hunks, 0 formatting churn). Do NOT run `make fmt`.

## PHASE 1 — committed, byte-identical bar

Four per-fix commits on task/satcore-perf-fixes:

- **c7541b6516  #4 Option.is_none for the BCP conflict test** (sat.ml propagate).
  `!confl = None` (polymorphic caml_equal C-call per propagated literal) → `Option.is_none`.
  Byte-identical by construction (same None-test).

- **8661bc9b1e  #5 derive cancel_until phase from the trail literal** (both arms).
  `Dynarray.set t.polarity v (Dynarray.get t.assigns v = -1)` → `... (not (sign_of_lit l))`.
  Provably equal: the trail literal `l` is the TRUE literal for `v` (trail invariant
  `lit_val t l = 1`), so by `lit_val` `assigns v = -1` iff `not (sign_of_lit l)` — the value
  `update_best_trail` already uses. Drops one `assigns` read per popped literal per backjump.

- **ddaf821f48  #2a allocation-free LBD via reusable per-level stamp** (clause_lbd/_cref).
  Replaced the Array.init→Array.map→Array.copy+sort materialization (2-3 arrays/call) with the
  canonical Glucose per-level stamp: new reusable `lbd_stamp : int array` + monotone `lbd_gen`
  generation counter; distinct-level count in one pass, no per-call allocation, no closure.
  Result-IDENTICAL to `Search_heuristics.lbd_of_levels` (retained as the tested spec).

- **c4556628ef  #2b skip LBD recompute for stored-glue clauses (identity-checked)** (analyze).
  Guard the LBD-lowering with `if cl_lbd t cr > glue_threshold`. PROOF of behaviour-identity:
  lowering only lowers, so a stored-glue clause stays glue; `reduce_deletions` excludes every
  clause with `lbd <= glue_threshold` from its removable/sorted set (the `removable` filter
  requires `lbd > glue_threshold`), so a glue clause's exact LBD never affects deletion
  selection, order, verdict, or any counter. `cla_bump` stays unconditional (activity is a
  removable-sort tiebreak). Treated as identity-checked-else-dark per the team lead; the
  counted-identity run (below) confirms it needs no dark flag.

### Phase-1 verification
- **build**: EXIT 0 (clean committed version, no churn).
- **make test**: EXIT 0 (all suites 0 failures; includes check-frozen 14/14 and
  cert-corpus-gate 33/33 VALID).
- **counted byte-identity** base d18a9934c0 vs Phase-1 branch (flagless, --max-effort 300000,
  220-file QF_UF/LIA/UFLIA/AX/DT sample): **220/220, 0 divergences**. Confirms all four fixes
  (esp. #2b) are behaviour-identical — #2b needs no dark flag.
- MEASUREMENT: the audit's 14%-Dynarray headline is the STD-build number and is driven mostly
  by PHASE 2 (#1). Phase 1 is byte-identical — its value is a small allocation/instruction
  reduction (LBD arrays, poly-compare, per-pop assigns read) with zero behaviour change; the
  headline win comes with Phase 2. Phase 1 lands on the byte-identical/CI-speed bar.

## PHASE 2 — committed, byte-identical bar

Two per-fix commits on task/satcore-perf-fixes (the headline #1, split for
reviewability into the per-var arrays and the trail stack). Both flagless and
byte-identical to the branch base (d18a9934c0); applied via string-replace
scripts (no Edit hook) so the diff is exactly the logical change, zero fmt churn.

- **4324285ceb  #1 per-var Dynarray→raw arrays** (assigns/level/reason/trail_pos
  int; seen bool). These five are the SAT-core firehose per-var reads (propagate +
  analyze). Without flambda a stdlib `Dynarray.get` is an un-inlined CROSS-MODULE
  call; a raw `array.(i)` is an inlined bounds-checked primitive.
  IDENTITY ARGUMENT: for int/bool arrays every Dynarray op has an exact raw-array
  equivalent (`get`/`set`; `add_last`→`grow_int`/new `grow_bool` + index-set)
  producing identical values, and NO output depends on capacity vs length. The
  per-var used length is `nvars` — these arrays are never truncated and grow one
  var at a time in `ensure_var` (geometric growth, floor 8); `Dynarray.length` is
  never read on any of the five, so capacity is unobservable. Fields made
  `mutable` (a raw array can't grow in place; the grow rebinds the field).

- **81b72148a4  #1 (cont) trail Dynarray→raw int array + `trail_n`.** `lit = int`
  (transparent alias) so a raw int array holds trail literals exactly as the
  `a_lits` arena already does. Read on the propagate cursor and throughout
  analyze/analyze_final. Op mapping (all byte-identical for an int stack, output
  independent of capacity): `length`→`trail_n`; `get i`→`trail.(i)`;
  `set !w l`→`trail.(!w)<-l`; `add_last`→`grow_int`+index-set+`trail_n<-trail_n+1`;
  `truncate n`→`trail_n<-n` (EVERY call site has n<=trail_n: cancel_until targets
  are `trail_lim` entries / the compaction write cursor, both <= current length);
  `iter`→for-loop over `[0,trail_n)`. `trail_lim` STAYS Dynarray (per-decision,
  not the firehose) — the word-boundary in the transform keeps `t.trail_lim` /
  `t.trail_pos` untouched.

- **#3 PARKED (measured-marginal).** The analyze cache-locals variant (bind
  `t.seen`/`t.level`/`t.reason` to loop-local names to drop the per-access record
  field load) is trivially identity-preserving but the expected gain is <2% on top
  of #1's ~20%, and it adds rename churn to the hottest TCB file that S1 /
  watch-layout / lgc-flip are also editing (larger integrator conflict surface).
  Parked per the sub-2%-micro-opt kill rule; the resolution-cref / arena-vs-
  transient chandle reshape it also names crosses the frozen analyze structure and
  is deferred with it.

### Phase-2 verification
- **build**: EXIT 0 (clean committed versions, no fmt churn — 101/79 + trail hunks).
- **make test**: EXIT 0. Includes check-frozen **14/14** (sat.mli unchanged),
  cert-corpus-gate **VALID=33/33** (INVALID=0; repeat-solve re-emit 33/33),
  sat-test **112/0** (40k random CNFs entailment-checked), satpre-test **41/0**,
  and every theory/array/DT/weq gate 0-failure.
- **counted byte-identity** vs Phase-1 tip (which is itself counted-identical to
  base d18a9934c0, so this is base-identity transitively): per-commit **119/119**
  files 0 divergences (perf/cases 19 + Goel hwbench 60 + eq_diamond 40) plus
  pigeonhole_n8 exact counter match; batch broad sweep across 9 logics (QF_UF/LIA/
  UFLIA/AX/DT/IDL/ALIA/AUFLIA/RDL, 108 files sampled corpus-wide) **0 divergences**
  on all 85 files that solved within the sample wall (23 slow files uncompared).
- **instruction count** (perf stat -e instructions, median of 3; same _build /
  toolchain, Phase-1 tip vs Phase-2 tip so the Phase-2 delta is isolated). The
  pre-built `worktrees/*-base` binaries are a different/stale toolchain (n7=1.14B
  vs std 824M) and are NOT used for the delta. Cumulative Phase-2 win:
  - pigeonhole_n6: 101.08M → 83.48M  (**−17.4%**)
  - pigeonhole_n7: 823.79M → 666.19M (**−19.1%**)
  - pigeonhole_n8: 38.17B → 30.29B   (**−20.6%**)
  Split: per-var arrays deliver ~17-20pp of this; the trail stack ~0.5-0.9pp
  (fewer reads). Confirms the audit headline (un-inlined Dynarray ≈14% of Ir on
  search-heavy runs) at the SAT-core firehose.

### Hedge pass (post-review, on top of freeze 33caa31b4d)
Review returned APPROVE-WITH-HEDGE (all 6 fixes byte-identical on reachable
executions; raw-array conversion audit clean on both legs). Two bounded items:

- **0cfa6f7144  H1: unconditional lbd_gen rollover guard.** Codex's only blocking
  finding: #2a's stamp identity rested on 2^62-call unreachability. `lbd_begin`
  now, iff `lbd_gen = max_int`, zeroes `lbd_stamp` and restarts the generation
  before the increment that would wrap negative — so the distinct-level count is
  UNCONDITIONALLY identical to `Search_heuristics.lbd_of_levels`, not merely up to
  the first wrap. Cold path only; the hot `lbd_count_level` reads are untouched, so
  it is a no-op on every reachable execution. Verified: sat-test 112/0 + counted-
  identity vs base 119/119 0-diverge (the guard never fires in these runs — the
  point is that reachable behaviour stays byte-identical while the wrap dependency
  is removed).
- **3edd2873cb  H2: fix 2 stale comments (comment-only).** Reason-encoding header
  (`int Dynarray.t`→`int array`) and ensure_var header (`every per-var Dynarray`→
  `every per-var array`), matching the #1 conversion.

Final-tip gates (H1+H2): make test EXIT 0 — check-frozen **14/14**, cert-corpus
**VALID=33/33** (repeat re-emit 33/33), sat-test **112/0**, satpre **41/0**, all
theory/array/DT/weq/bv gates 0-fail. **New freeze sha: 3edd2873cb.**

## Verification backlog (downgraded / lower-tier audit findings — not in this wave)
- **reduce_db allocation pipeline** (sat.ml reduce_db): `stats` record-array + Seq.filter/
  Array.of_seq for kept_learnt, discarded before the arena rebuild. Periodic (every few
  thousand conflicts) → medium. Candidate: sort a primitive index array, fill survivors
  directly. The fresh arena arrays themselves are inherent to the rebuild (not a bug).
- **watch self-copy when j=i** (propagate 1179/1230): stores `wc.(j)<-cr` even when j=i on the
  blocker-satisfied path. DOWNGRADED (codex rated high): barrier-free int stores, a `j<>i`
  guard may cost as much as the store it elides. Micro-A/B only if it still shows after
  Phase 2 + the watch-arena lane (task #45/#25).

## REBASE onto trunk 370081afd9 (re-derivation)

The pre-rebase freeze (615cbe3753, base d18a9934c0) was rebased onto trunk
370081afd9 (which had since gained fabric S4.1 `on_assign ~level`, lgc, and
watch-layout). Land attempt correctly stopped at the #1 conflict; re-derived here
as a clean linear stack (branch task/satcore-perf-rebased).

METHOD: the 4 Phase-1 commits cherry-picked onto trunk CLEANLY (no conflict).
The #1 conversion was re-derived by re-running the SAME semantic transform scripts
(field + word-boundary + balanced-arg) on trunk's sat.ml — not a textual
conflict-resolve. Because the conversion changes the FIELD TYPES, every
unconverted reader is a compile error; the clean first build proves ALL readers
were converted, with NO silent-miss risk.

New trunk readers converted (vs the original base):
- `assigns`/`level` gained +2 readers → xform1 converted 52 gets (was 50), 20 sets.
  The two are fabric S4.1's `on_assign` firing sites: the enqueue site
  `~level:(Dynarray.get t.level v)` → `~level:(t.level.(v))`, and the theory-replay
  loop `~level:(Dynarray.get t.level (var_of_lit l))` → `~level:(t.level.(var_of_lit l))`.
  Both read the SAME stamped `t.level` value as before (S4.1 firing-site semantics
  preserved — confirmed by chrono-test below).
- The trail-replay `Dynarray.iter (fun l -> th.on_assign l ~level:...) t.trail`
  (S4.1's multi-line ~level form) → a `for i = 0 to trail_n - 1` loop reading
  `t.trail.(i)` and `t.level.(var_of_lit l)`. trail get/set/length/truncate counts
  unchanged (12/9/1/2 — trunk added no trail readers).
- reason/trail_pos/seen: unchanged reader sets.
Surprising: nothing — the compiler enumerated the delta exactly; the only manual
adaptation was updating the trail-`iter` anchor to trunk's S4.1 `~level` form.

Rebased stack (linear on 370081afd9): #4, #5, #2a, #2b (Phase 1, cherry-picked)
then #1 per-var, #1 trail, H1, H2 (re-derived), then this log commit.

### Post-rebase verification (MANDATORY)
- **make test** EXIT 0: check-frozen **14/14**, sat-test **118/0**, satpre 41/0,
  chrono-test **22/0** (the S4.1 RED pins the firing-site level values — PASS
  confirms the raw-array reads deliver the same `t.level`), lgc-test 9/0,
  combine BOTH modes (fabric + OXSMT_NO_FABRIC), cert-corpus **VALID=33/33**
  (repeat re-emit 33/33), all theory/array/DT/weq/bv gates 0-fail.
- **counted byte-identity: rebased tip vs TRUNK 370081afd9 binary** (same toolchain,
  both `make build`): perf/cases 19 + eq_diamond 40 + Goel hwbench 60 + pigeonhole_n8
  (exact counter match 24125/29298/297724) + a corpus-wide 9-logic sample (108 files)
  — **0 divergences** everywhere (rebased tracks trunk's search trajectory exactly,
  which itself differs from the old base since lgc/watch/S4.1 landed).
- **instruction count** (perf stat, median of 3; trunk tip vs rebased tip, same
  `_build`/toolchain): pigeonhole n6 **−38.8%**, n7 **−42.8%**, n8 **−39.2%**. Larger
  than the pre-rebase −17..−21% because trunk's baseline trajectory does heavier
  per-conflict array work (n8: 24k conflicts × ~517K instr vs old base 238k × ~160K);
  the change itself is identical, the baseline moved.
