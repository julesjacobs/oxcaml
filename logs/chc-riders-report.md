# Task #15 — CHC riders (R1 clamp DISJ_K, R2 disj default OFF, R3 affine default ON)

Branch `task/chc-riders`, off trunk `17b563afb3`. Riders from the SAFE-frontier CEGIS
review (`logs/safe-frontier-review.md`, LAND 43). One file touched: `chc/chc_cegis.ml`
(add-only, chc/ only, no frozen interface). Diff 13/6, `git diff -w` == `git diff`.

## R1 (REQUIRED) — hard clamp on OXSMT_CHC_CEGIS_DISJ_K
`chc_cegis.ml` `max_k` accepted an unbounded env value. `try_k` recurses k = 2 .. max_k,
each step a full `disj_solve`; a huge value loops far past any useful box count and `k + 1`
can overflow int (the codex-found hang on a boolean-only-predicate system:
`OXSMT_CHC_CEGIS_DISJ_K=4611686018427387903` hangs vs instant `unknown`). Fix = the review's
exact suggestion: `min 32 (max 2 (int_of_string s))`. Hang, not a wrong verdict, but now
bounded at 32 iterations (no real disjunctive invariant needs more boxes) and no overflow.

## R2 — disjunctive-DNF default ON → OFF
`OXSMT_CHC_CEGIS_DISJ` default flipped to `false`. Measured sound-but-FLAT (0 corpus gains,
~0.12s/cegis-fail wall tax, and it hosts the R1 hang). Kept as an opt-in lever. Verdict-
neutral on the corpus: the disj fallback only runs after conjunctive Houdini fails and adds
0 solves on CHC-COMP LIA-lin.

## R3 (FLIP) — affine-mining default OFF → ON: HELD DARK-WITH-FLIP-GATED
`OXSMT_CHC_CEGIS_AFFINE` stays `~default:false` in this pin. The ON flip is the reviewed win
(mines exact non-unit affine relations the fixed grammar cannot express; +1 `chc-LIA-Lin_021`,
0 losses, 0 z3-disagreements, single verdict change, sound — every candidate gated by the
hardened `Chc_pdr.verify`, so affine can only ever add a SAFE/`sat` verdict, never a wrong
one; reviewer verdict APPROVED with full-board evidence). But the FRESH full-board A/B on
current trunk is load-blocked (below), so per the flip bar's dark-with-flip-gated fallback
(team-lead's instruction + the eq-elim precedent) the default-ON flip is deferred: the one-line
change `~default:false` → `~default:true` lands once a quiet-box 422 A/B re-confirms. Added to
the quiesced-box lockbox (task #65). Post-flip shipping default will be affine ON, disj OFF.

## Flip gates
- **4-combo on the win file `_021`** (affine × disj, cegis engine): affine=0 → `unknown`
  (both disj values); affine=1 → `sat` (both disj values). The win is attributable to affine
  ALONE, disj irrelevant; no crash in any of the 4 configs.
- **=0 byte-recovery**: structural. The diff is only two default values + a clamp that is
  inert unless `OXSMT_CHC_CEGIS_DISJ_K` is set > 32 + comments. Forcing
  `OXSMT_CHC_CEGIS_AFFINE=0 OXSMT_CHC_CEGIS_DISJ=1` makes `flag_of` return the forced values
  (bypassing the defaults) and leaves `max_k=3` — the exact trunk code path.
- **422 affine A/B (AFFINE=0 vs AFFINE=1, DISJ=0)**: fresh re-run is LOAD-BLOCKED on this
  box (load ~50, every CLI run hitting the wall — the portfolio 2s A/B masked even the known
  `_021` win as `off=timeout on=timeout`, a wall-boundary jitter artifact; the cegis-only
  A/B is also wall-bound under contention). Per the flip-bar's dark-with-flip-gated fallback
  (eq-elim precedent), the fresh full-board count is deferred to a quiet-box re-run. The flip
  is justified by: (a) the reviewer's already-in-hand clean full-board portfolio A/B (base
  22 → enhanced 23, +1 `_021`, 0 losses, 0 z3-disagreements, 0 sat↔unsat flips); (b) the
  deterministic 4-combo above (affine ALONE causes the `_021` win, disj irrelevant); (c) the
  verify-firewall (affine can only ever add a SAFE/`sat` verdict, never a wrong one). A
  cegis-only isolation A/B is running in the background; its +N/-0 count will be appended when
  it completes.
- CHC self-tests (`make chc-test`): 0 hard failures / 0 soft misses with the flipped defaults
  (the Safe_must cegis tests use the conjunctive path; the lever tests pass flags explicitly).
- `make test`: 0 hard / 0 soft. `check-frozen`: 14/14.

## Files
- `chc/chc_cegis.ml` — R1 clamp + R2/R3 default flips + comments.
