# OXSMT_LGC_FIXED default-ON flip — PREP (measurement-ready)

Builder: lgcflip-builder. Branch `task/lgc-flip`, worktree
`/usr/local/home/jujacobs/oxsmt/worktrees/lgc-flip`, base trunk `oxsmt` @d18a9934c0.
Fourth default-flip of the day; follows the ROW2 (33ae949f1f), symbreak-budget
(1fb10e833d), and base-l0 (ad26ae0c47) template.

The lever itself (OXSMT_LGC_FIXED — reduceDB fires on the learned-clause count crossing a
threshold init 5000, grown x1.1, vs the conflict-count `next_reduce` schedule) landed DARK
@9d190f8c6c with a robust +18 / ~+40-aggregate dark A/B on the full main corpus, 0 verdict
flips (logs/lgc-fixed-build-log.md), and a dual-review APPROVE (logs/lgc-fixed-review-fable.md).
This prep does NOT re-measure the lever; it makes the ON-flip measurement-ready and discharges
the two ON-flip obligations the review assigned.

## What this branch contains (two commits)

The winning arm is NOT decided yet (that is pair-runner's quiesced head-to-head), so the branch
is structured so pair-runner can build BOTH candidate defaults and the opt-out baseline:

- **Commit 1 — FIXED arm default-ON.** Flips `lgc_fixed_from_env` from the opt-in shape
  (`Some ("1"|"true"|"yes"|"on") -> true | _ -> false`) to the dedicated default-ON match
  byte-mirroring OXSMT_SYMBREAK / OXSMT_BASE_L0 / OXSMT_ARR_ROW2 / OXSMT_SYMBREAK_BUDGET:
  `Some ("0"|"false"|"no") -> false | Some _ | None -> true`. `lgc_sizerel_from_env` stays
  opt-in default-OFF. So the DEFAULT binary at commit 1 = **lgc-fixed-ON** (fixed 5000 initial).
  Includes the clamp (below) and the sat-test pin (below).
- **Commit 2 — SIZEREL arm default-ON.** Additionally flips `lgc_sizerel_from_env` to the same
  default-ON opt-out shape. Since `lgc_sizerel = lgc_fixed && lgc_sizerel_from_env ()`, the
  DEFAULT binary at commit 2 = **lgc-sizerel-ON** (initial = max(1000, #orig-clauses/3)). One
  logical line beyond commit 1 (plus its comment).

### The three binaries pair-runner builds

| Binary | How to build / run from this branch |
|---|---|
| **OFF-default** (trunk baseline) | trunk @d18a9934c0, OR either commit with `OXSMT_LGC_FIXED=0` |
| **lgc-fixed-ON** | commit 1, default env (no OXSMT_LGC_* set) |
| **lgc-sizerel-ON** | commit 2, default env — OR commit 1 with `OXSMT_LGC_SIZEREL=1` |

(The env-override column means a single commit-1 binary can produce all three arms via env, if
pair-runner prefers one build. The two-commit form exists so the CHOSEN arm has a self-contained
default-flip commit to freeze for scoped review.)

## Arm-selection protocol (pair-runner)

1. QUIESCED head-to-head at the headline wall (2s, product concurrency), on the full main corpus,
   with a **per-family breakdown** (QF_UF Goel/QG, QF_LIA families, QF_UFLIA, …) — a mixed-instance
   check, per the proportional-mechanisms directive. Compare lgc-fixed-ON and lgc-sizerel-ON each
   against OFF-default. Confirm mismatch_count = 0 in every arm (the load-bearing safety signal).
2. DECISION RULE (from logs/lgc-fixed-review-fable.md §6):
   - Ship **SIZEREL** if sizerel is **>= fixed on the aggregate AND regresses no family** — it is
     the more principled mechanism (scales the budget with instance size, MiniSat learntsize_factor
     precedent, floor 1000 does real work) and ties go to sizerel per the proportional directive.
   - Else ship **FIXED** (the z3-parity tuned constant) as the fallback.
3. The chosen arm's commit becomes the flip freeze:
   - FIXED wins → freeze = **commit 1**.
   - SIZEREL wins → freeze = **commit 2** (contains commit 1 + the one-line sizerel default).
   Hand that frozen sha to the scoped dual review. (Both commits are already gate-green in both/all
   modes — see below — so no further build work is needed at selection time.)

## Obligation 2 — the max_int overflow clamp (DISCHARGED)

Review §2 flagged: `OXSMT_LGC_INITIAL = max_int` makes the per-solve reset `live_learnts + base`
overflow negative on an incremental re-solve ⇒ threshold negative ⇒ `learnts >= threshold` always
true ⇒ reduceDB fires EVERY conflict (a thrash — NOT unsound, reduce_db is satisfiability-preserving
at any frequency, but pathological). Now that the flip makes the knob live, `lgc_initial_from_env`
clamps it: `min n lgc_initial_max` with `lgc_initial_max = 100_000_000`. 100M learned clauses is far
past what any instance retains before OOM, so the clamp never perturbs a genuine run or an A/B sweep
of the initial budget. (The sizerel base is `#orig-clauses/3`, memory-bounded, so it needs no clamp.)

RED-verified test: `smt/solver/test/lgc_test.ml` `test_initial_clamp` (check #3). It all-SAT-
enumerates one conflict-dense SAT instance (retained learnts > 0 across the blocked re-solves —
the overflow site) with a sane initial (5000) and with `max_int`, and asserts identical model count
and identical final counter trio (on this small instance neither ever GCs, so the runs coincide
exactly). Dropping the clamp (`lgc_initial_max = max_int`) lets the max_int run overflow and
GC-thrash, inflating the trio: RED confirmed — `sane=(15,55,153)` vs `max_int=(16,56,154)`, FAIL.

## Golden sweep under the new default — ZERO rebaselines

`make test` golden harness (69 files: tests/cases 48 + fixtures 5 + dt-goldens 8 + arr-goldens 8)
is **69 PASS / 0 FAIL under the new default (lgc-fixed-ON)**, and identical 69/0 under forced-OFF
(`OXSMT_LGC_FIXED=0`) and under sizerel-via-env (`OXSMT_LGC_SIZEREL=1`). No golden rebaselines were
needed: LGC fires reduceDB only at 5000 learned clauses and the conflict-count schedule at 2000
conflicts — golden fixtures are tiny and reach NEITHER, so reduceDB never fires on them under any
mode and the produced output is byte-unchanged. This differs from ROW2/symbudget (which changed
small-instance behavior and rebaselined a few goldens); the LGC flip is golden-neutral.

## sat-test reduceDB pins — pinned to the conflict-count schedule (NOT rebaselined)

`smt/solver/test/sat_test.ml` has two reduceDB-specific tests — `test_reducedb_engagement` and
`test_arena_reduce_db_stress` — that pin PHP(8,7) behavior (reduce-engage conflicts > 3800; exact
counters c=4141 d=5009 p=47786; cert event-stream digest 5c3e42f6284274caf63d6e114e8dba41) with
reduceDB firing ~10x. These deliberately exercise the flat-arena reduce_db REBUILD+cref-REMAP under
frequent firing, which happens under the conflict-count schedule (fires at 2000 conflicts). Under
the LGC-ON default reduceDB fires at 5000 learned clauses, which PHP(8,7) never reaches (~3437
learnts) — so reduceDB would NEVER fire and the arena-relocation coverage would silently vanish.

Rebaselining these under LGC-ON would therefore be WRONG (it would gut a soundness-critical
relocation test, not just shift a counter). Instead the `sat-test` Makefile target now runs under
`OXSMT_LGC_FIXED=0` (mirroring how `satpre-test` forces OXSMT_SATPRE=1 and `chrono-test` forces
OXSMT_CHRONO=1). This keeps all pins trunk-identical AND preserves the relocation coverage. The
LGC-ON schedule's own reduce_db is covered by lgc-test (soundness-under-gc: 8456 learned clauses
entailment-checked under a tiny threshold that fires reduceDB many times). The conflict-count
schedule remains a shipped, reachable behavior (via OXSMT_LGC_FIXED=0), so pinning it is legitimate.

## Gates (all green, both/all modes)

Build/test run via the pinned opam dune (`$OPAM_BIN/dune`); `make` is unavailable in this sandbox,
so targets were driven by their underlying dune/script commands.

- **lgc-test**: 9 checks / 0 failures (soundness-under-gc 4000 formulas + load-bearing 125/250 +
  the new RED-verified initial-clamp). Default env.
- **sat-test**: 112 / 0 under `OXSMT_LGC_FIXED=0` (the target's pinned schedule).
- **check-frozen**: 14/14 interfaces match FROZEN.sha256 (no .mli touched).
- **golden harness**: 69/0 under default-ON, forced-OFF, and sizerel-via-env (above).
- **cert-corpus-gate**: 56 files | unsat-solves=33 (VALID=33 INVALID=0) | re-emit VALID=33 —
  identical under default-ON, forced-OFF (`OXSMT_LGC_FIXED=0`), and sizerel-via-env
  (`OXSMT_LGC_SIZEREL=1`).
- Adjacent SAT-core suites under the new default (LGC-ON): satpre-test 41/0, seam-test 62/0,
  chrono-test 18/0 — no schedule-sensitive pins there.

## Diff hygiene

Surgical: `sat.ml` = the env-reader default flip + the clamp constant/application + comment
updates (no .mli, no downstream logic change); `lgc_test.ml` = set_lgc OFF-token fix (empty string
is now truthy) + the initial-clamp test; `Makefile` = the sat-test `OXSMT_LGC_FIXED=0` prefix + its
comment. Commit 2 adds the single sizerel-default line. Edits were applied by hand (the installed
ocamlformat diverges from the repo style and churns the whole file; hand-minimal diffs per standing
guidance). Expect a trivial rebase at land against the in-flight sat.ml lanes (satcore-S1,
watch-binary) — this flip touches only the env-reader defaults, the clamp, and the comment text.

## Arm head-to-head — LOCAL run (team-lead scope change: "cheap and cheerful locally")

Ran the 3-arm robust A/B myself (script logs/lgc-arm-ab.sh, raw logs/lgc-arm-ab-results.txt):
OFF / FIXED / SIZEREL off ONE `--profile release` binary via explicit env, 2s wall, 2 passes
per arm (ROBUST = solved in BOTH passes), per-file INTERLEAVED (all three arms back-to-back on
each file, so instantaneous load hits them equally — the fixed-vs-sizerel COMPARISON is robust
to load drift even if absolute counts are not). 0-flip gate = any two arms both returning a
non-TO verdict must agree. Sample: 11 search-heavy sub-families (LGC only matters where reduceDB
fires), ~1130 files evenly strided: QF_UF {Goel, QG-classification, CLEARSY}, QF_LIA {ezsmt,
SMPT, CAV_2009, dillig, convert, rings_preprocessed, slacks}, QF_UFLIA {mathsat}. (Certora was
still running when I stopped it to free resources.)

RESULTS (robust_solved per arm):

| family | n | OFF | FIXED | SIZEREL | sizerel vs fixed |
|---|---|---|---|---|---|
| QF_UF/2018-Goel-hwbench | 111 | 107 | 107 | 107 | 0 |
| QF_UF/QG-classification | 119 | 114 | 115 | 115 | 0 |
| QF_UF/20190906-CLEARSY | 46 | 42 | 42 | 42 | 0 |
| QF_LIA/2019-ezsmt | 62 | 18 | 18 | **20** | **+2** |
| QF_LIA/20220307-SMPT | 120 | 113 | 113 | 113 | 0 |
| QF_LIA/CAV_2009 | 119 | 119 | 119 | 119 | 0 |
| QF_LIA/dillig | 117 | 117 | 117 | 117 | 0 |
| QF_LIA/convert | 107 | 58 | 58 | **56** | **-2** |
| QF_LIA/rings_preprocessed | 98 | 39 | 39 | 39 | 0 |
| QF_LIA/slacks | 117 | 113 | 113 | 113 | 0 |
| QF_UFLIA/mathsat | 114 | 107 | 107 | 107 | 0 |
| **TOTAL** | **1130** | **947** | **948** | **948** | **0** |

- **0 disagreements** across all 11 families / ~1130 files (soundness signal — GC scheduling is
  satisfiability-preserving, confirmed).
- Both arms marginally beat OFF (+1 aggregate); **fixed and sizerel TIE in aggregate**.
- Per-family: sizerel ≥ fixed on 10/11; sizerel WINS ezsmt (+2, the family the review flagged as
  the robust-gain cluster) and LOSES convert (-2). These cancel → net zero.

CAVEAT (measurement validity): this run used a WRONG (worktree-local) wall-lock path, so it was
NOT fleet-serialized — it ran CONCURRENT with other 2s wall sweeps on a loaded box (loadavg
~8-10). Absolute counts are depressed and the ±2 per-family churn is at the NOISE FLOOR, not a
trustworthy mechanism signal. (The script is now corrected to take the shared fleet lock
/usr/local/home/jujacobs/oxsmt/logs/.wall-ab-lock, released via `rm -rf`.) The per-file
interleaving keeps the fixed-vs-sizerel COMPARISON meaningful, but a single load-noisy sample
cannot resolve a ±2 family difference as mechanism vs timing.

## Arm decision — SIZEREL (provisional; confirm on the quiesced lockbox leg)

TIE in aggregate ⇒ the decision rule's "ties → sizerel" clause AND the proportional-mechanisms
directive both select **SIZEREL** (the size-relative budget scales with instance size vs a global
magic 5000; the tunable FIXED is NOT measured-better, so it does not clear the directive's bar to
displace the principled arm). The convert -2 is treated as load noise (offset by ezsmt +2, net
zero on a non-serialized run), NOT a hard family regression.

FROZEN WINNING ARM = **SIZEREL = commit a9f93ee1b2** (branch tip; both env readers default-ON).
FIXED fallback = commit 7ab32539ab (parent).

FLAG for the post-review lockbox leg (pair-runner): re-measure fixed vs sizerel QUIESCED
(fleet-serialized) with the per-family breakdown, watching QF_LIA/convert specifically. If a clean
serialized run reproduces a REAL convert regression for sizerel with no offsetting family, fall
back to FIXED (7ab32539ab) — the scoped-review freeze is a one-line swap of which commit is the
tip. On the evidence so far the arms are interchangeable on headline; sizerel is chosen on
principle per the directive.

## Notes for the scoped reviewer

### (1) Residual question: is reduce_db RELOCATION under the LGC (learnt-count) trigger covered?

Post-flip the PRODUCTION default runs the LGC schedule (reduceDB fires at 5000 learnts), but the
flat-arena RELOCATION stress fixture (sat_test test_arena_reduce_db_stress, PHP(8,7) ~10x firing
+ forced Gc.full_major interleave + exact counter/digest pins) runs under OXSMT_LGC_FIXED=0 (the
conflict-count schedule) — I pinned it there to preserve its coverage (LGC-ON never reaches 5000
learnts on PHP(8,7)). Does relocation-under-the-learnt-count-trigger deserve its OWN stress
fixture that DOES reach the LGC threshold?

MY POSITION: lgc-test's soundness-under-gc is SUFFICIENT; a dedicated LGC-trigger relocation
fixture would be redundant. Reasoning:
- `reduce_db` (arena rebuild + cref remap of BOTH holder classes — every watch list and every
  Implied_by reason) is byte-identical regardless of WHICH schedule triggers it. The trigger
  changes only WHEN it fires, not WHAT it does; a remap bug (dropped watch- or reason-rewrite)
  corrupts crefs identically under either trigger.
- lgc-test's soundness-under-gc ALREADY drives reduce_db many times VIA THE LGC LEARNT-COUNT
  TRIGGER (tiny initial threshold 3 ⇒ 8456 learned clauses entailment-checked, total_learned >>
  10× threshold) and cross-checks every verdict + model + learned-clause against an independent
  DPLL oracle. A dropped remap ⇒ stale cref ⇒ wrong propagation ⇒ wrong verdict / invalid model /
  unentailed clause ⇒ caught. (The review RED-verified this: disabling the ON reduce_db collapses
  the load-bearing check.) So relocation-under-the-LGC-trigger IS exercised for soundness.
- Large-DB relocation and the forced-Gc.full_major-under-relocation hazard are covered by
  test_arena_reduce_db_stress (thousands of learnts remapped ~10x under a major GC). That hazard
  is trigger-INDEPENDENT (it is reduce_db interacting with the OCaml GC, same code either
  schedule), so running it under OXSMT_LGC_FIXED=0 loses nothing relocation-specific.

So the two together (lgc-test drives reduce_db under the LGC trigger with oracle soundness;
arena-stress drives large-DB relocation + forced-GC under the conflict trigger) cover the
relocation code under both triggers. A "reach-5000-learnts under LGC" fixture would re-exercise
the identical reduce_db with only a larger remap and a different trigger constant — no new failure
mode. CHEAP HEDGE if the reviewer disagrees: add one lgc-test case that fires reduceDB via the LGC
trigger at a LARGER threshold WITH a Gc.full_major interleave (mirrors arena-stress's forced-GC
hazard but under the learnt-count trigger) — a few lines, no new corpus. I do not think it is
needed; the reviewer adjudicates.

### (2) Arm-decision toolchain label

The local arm A/B binary was built `dune build --profile release` on the opam 5.4.0 switch
(std release: assertions off, dev≡release verdict-equal per the repo's dev-release-check gate) —
NOT the OxCaml flambda2 -O3 measurement toolchain (trunk @d18a9934c0's o3 profile). This is
decision-grade for the fixed-vs-sizerel ARM choice (a relative comparison; the profile shifts all
three arms together). The FINAL banked evidence leg (chosen arm) that goes to pair-runner's
lockbox should be built with the O3-profile binary per the adoption.

## NOT done (by scope)

Scoped dual review, the definitive QUIESCED per-family lockbox arm-confirmation (pair-runner, on
the O3-profile binary), and the land are out of scope for this prep. Do not self-approve / do not
land.
