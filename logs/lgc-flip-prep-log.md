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

## NOT done (by scope)

Lockbox measurement (pair-runner), scoped dual review, and the land are out of scope for this prep.
Do not self-approve / do not land.
