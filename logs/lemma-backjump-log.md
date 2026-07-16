# Lemma-backjump seam — non-restarting asserting-lemma delivery (task #25)

Branch `task/lemma-backjump` off trunk `15e6d150f0`. Dark flag `OXSMT_LEMMA_BACKJUMP`
(default OFF, byte-identical). Follows the task #22 early-gate STOP: the Propagate-effort
`Theory.Lemma` delivery does an unconditional `cancel_until 0` per lemma (the LCG delivery
tax). This lane builds the non-restarting `add_asserting_lemma` seam extension sketched
there.

## Mechanism (smt/solver/sat.ml only; no .mli, no frozen file touched)

`add_theory_lemmas` is now flag-gated:
- **OFF**: textually the original — `cancel_until 0; List.iter add_clause`. Byte-identical.
- **ON**: each clause goes to `try_lemma_backjump`; anything it declines falls back to the
  exact `cancel_until 0` + `add_clause` path FOR THAT CLAUSE.

`try_lemma_backjump t ls` (the extension): dedup + tautology filter (as `add_clause` does).
A clause is **asserting** iff exactly one literal is non-false under the current assignment
and it is UNASSIGNED (the head `h`); every other literal is false. Then:
- `bt` = the HIGHEST decision level among the false literals (see "charter wording" below);
- require `bt >= 1` (else fall back: `bt = 0` means `cancel_until bt = cancel_until 0`, no
  benefit, and it avoids reproducing `add_clause`'s level-0 `on_unit` cert declaration);
- reorder to `[| h; l1; rest… |]` with `l1` a false literal at level `bt` (the 2WL
  invariant: index 1 is the highest-level false literal);
- emit the cert leaf `on_input ~origin:Theory_lemma` and REUSE its id for the arena clause
  (`mk_clause_with_id`), identical to `add_clause`;
- `cancel_until bt` (keeps every false literal — all at level ≤ bt — and leaves `h`
  unassigned), `attach`, `unchecked_enqueue h` with the clause as its `Implied_by` reason.
  `enqueue_level` stamps `h` at `bt` (= decision_level after the backjump, = max level among
  the clause's other literals under CB) — an ordinary Boolean propagation, so no eager cert
  event is owed; the reason is materialized lazily by `analyze`, exactly as for the head
  the OFF path propagates by BCP after its restart.

This is the learned-clause install path (`record_learnt` shape), so it inherits its
exception-safety: `attach` is both-or-neither (2WL never half-formed) and `unchecked_enqueue`
places `h` on the trail — a raise leaves the solver restorable by `cancel_until 0`
(the invariant learned-clause installs already hold).

### Charter wording (load-bearing — flagged for review)
The charter says "bt = second-highest decision level among its false literals." Taken
literally that is WRONG: backjumping to the second-highest false level leaves the
highest-false literal unassigned above `bt`, so the clause has two unassigned literals and is
NOT unit — no propagation. The correct CDCL backjump level (and the learned-clause-install
analogy the charter itself invokes) is the HIGHEST false-literal level: the unassigned head
is the notional highest literal, so `bt` is "second-highest counting the head." Implemented
as max false-literal level. This is the only sound reading; confirmed by the cert gate.

## Gates (all green)

- **Full build** clean; **check-frozen 14/14** (no .mli / frozen file changed).
- **OFF byte-identity vs trunk `15e6d150f0`**: 10/10 files identical (verdict+model+counters)
  across QF_LIA {convert, rings, bofill, SMPT}, QF_UF, QF_IDL, QF_RDL, both flags unset; and
  `OXSMT_LEMMA_BACKJUMP=0` ≡ unset.
- **Cert corpus gate (tests/cases + tests/dt-goldens): 33/33 VALID in ALL FOUR combos**
  {backjump OFF/ON} × {HNF_CUTS OFF/ON}, including repeat-solve re-emit (VALID=33 bad=0). The
  lazy-B ordered-RUP hazard did NOT bite: `add_theory_lemmas` emits the same single
  `on_input` per clause in the same order in both states; no theory PROPAGATIONS are
  reordered (only the trail depth at which the head is propagated changes).
- **sat_test 118/0** (with `OXSMT_LGC_FIXED=0` per the Makefile recipe), **seam_test 71/71**,
  **cdclt_lemma_test 14/14** — identical in both flag states.
- **Pre-existing HNF_CUTS cert note**: on a 3-file rings HNF-cut sample the gate reports
  VALID=2 INVALID=1 — but IDENTICALLY under trunk (HNF_CUTS=1, no backjump) and under
  backjump ON. So the INVALID is a pre-existing HNF_CUTS+cert limitation on rings, NOT
  introduced by this change; backjump is cert-neutral on it.

## RED / mechanism metric

A throwaway probe (`OXSMT_LCG_PROBE`, reused+repurposed from #22) emits ASSERTING synthetic
lemmas at depth (head = first unassigned var; body = negations of the deepest true trail
literals) so the backjump path actually engages, and counts total trail entries discarded
across all deliveries (`trail_discarded`, answer-agnostic). RED discrimination:
- backjump OFF → `backjumps=0` (every delivery resets to level 0);
- backjump ON → `backjumps>0`, `trail_discarded` sharply lower.

convert-jpg2gif-query-1139, depth 8:

| cap  | backjump | verdict | deliveries | backjumps | trail_discarded |
|------|----------|---------|-----------|-----------|-----------------|
| 100  | OFF      | sat     | 158       | 0         | 17591           |
| 100  | ON       | sat     | 150       | 100       | **7120**        |
| 500  | OFF      | sat     | 541       | 0         | 50248           |
| 500  | ON       | (probe artifact) | 183 | 155  | 3352            |
| 1000 | OFF      | sat     | 1046      | 0         | 96350           |
| 1000 | ON       | (probe artifact) | 183 | 155  | 3352            |

Headline (cap=100, verdicts AGREE sat/sat): **backjump cuts trail discarded 17591 → 7120,
a 60% reduction**, with all 100 asserting probe lemmas taking the partial backjump.

**Caveat**: the synthetic asserting probe is deliberately NOT answer-preserving (forcing
arbitrary heads over-constrains the instance), so at cap≥500 it spuriously flips sat→unsat
and the props/verdict columns are probe artifacts, NOT a backjump bug. `trail_discarded` is
the clean, answer-agnostic mechanism metric; the answer-preserving soundness evidence is the
cert gate (33/33, verdicts preserved). The #22 props-blowup curve is therefore not
reproduced identically (it used tautology lemmas, which this path correctly declines); the
mechanism win is stated as trail-discard reduction instead.

## KEY FINDING — the current real producer does not benefit yet

With `OXSMT_HNF_CUTS=1` (the only real Propagate-lemma producer), the backjump path is
almost never taken: on rings files the cut lemmas fall back via **`fb_nonassert`** (NOT
`fb_bt0`) — i.e. they present to the SAT core with ≥2 non-false literals, so they are not
asserting. Cause: an HNF cut is `cut ∨ ¬ant₁ ∨ …` where the antecedent bounds are LIA
theory-internal and often have NO assigned Boolean literal on the trail, so `¬antᵢ` is
UNASSIGNED (not false) at the SAT level. Result: 9/9, 6/6, 15/16 deliveries fall back;
verdict and counters are identical to backjump OFF.

Consequence: the seam extension is correct, sound, cert-clean, and demonstrably reduces
trail-discard for genuinely asserting-at-depth lemmas — but it delivers NO immediate win,
because no current producer emits such lemmas. The win is unlocked only by a producer that
(a) materializes its antecedents as assigned Boolean literals so the clause is asserting at
the SAT level, and (b) fires during search (not over level-0 tight rows). That is exactly
the future LCG bound producer (task #22) — this lane is its enabler, not a standalone win.
It connects directly to #22's finding (bofill/SMPT/rings are level-0/theory-internal
dominated) and `rings-win-is-during-search-boundprop`.

## Recommendation

Land-ready as a dark, sound, cert-clean seam extension (OFF byte-identical), OR hold behind
the LCG producer. It is a PREREQUISITE for the LCG lazy-bound lane, not a self-justifying
perf win. Per the charter: STOP here — producer revival is a separate lead decision.

Files: `smt/solver/sat.ml` only. Probe/counters are clearly-marked throwaway instrumentation
(gated by `OXSMT_LCG_PROBE`; byte-identical when unset).
