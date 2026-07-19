# eq-elim growth guard — round 2 re-pin (DARK)

Branch `task/eq-elim-repin`, parent = trunk `f75366318f`. Round-2 response to the review
(`logs/eq-elim-guard-review.md`): guard APPROVED sound, default-ON DECLINED (my structural
no-loss argument was falsified by measurement — RC-00's elimination is load-bearing for
search at 20s: guard-OFF `sat` 9s, guard-ON TO). The guard's sign flips with budget, so it
lands DARK, flip gated on a quiet multi-budget (2s AND 20s) A/B.

## Three round-2 items

1. **Default inverted to DARK.** `elim_growth () : int option` — `None` unless
   `OXSMT_PRESOLVE_ELIM_GROWTH` is a positive integer factor (a non-positive / non-integer
   value, or unset, reads as `None` = disabled). In `run`, when `None`: no `dag_weight`
   computed, `budget=0`, `charged` is the identity, no `Elim_budget` is ever raised — the
   elimination runs to completion **exactly as trunk**. When `Some factor`: the proportional
   budget (`factor * dag_weight conjuncts + elim_base_budget`) applies. Mirrors
   `OXSMT_NEC_PROPFOLD` / `OXSMT_SYMBREAK_UFTAIL`. The OOM-safety value survives as opt-in
   (the vox2 consumer VCs are tiny and never fire the guard, so default-ON bought nothing
   consumer-facing).

2. **ocamlformat reflow stripped (patch-cancellation).** The round-1 pin ran `ocamlformat -i`
   on presolve.ml, which reflowed ~14 lines in the `symmetry_break`/`is_bare_var` region —
   regions trunk itself is NOT format-clean on, and which the uftail land (`f75366318f`) then
   conflicts with (same trap nec-lazy hit). Fix: took trunk's EXACT `run` bytes and applied
   only the guard as textual inserts (helper fns before `run`, guard setup + `charged`
   wrappers + `Elim_budget` handler inside), never running the formatter. Result: the
   presolve.ml diff is confined to lines 355–514 (the eq-elim `run` region); **zero** hunks
   touch `symmetry_break`/`is_bare_var`/`test_relevancy`, and `git diff -w` == `git diff`.

3. **Rebased onto current trunk `f75366318f`** (grafts clean — the eq-elim region is disjoint
   from the uftail lands).

## Files
- `smt/interface/presolve.ml` — dark guard (helpers + `run`), eq-elim region only.
- `smt/interface/presolve.mli` — `val run` dark-guard note (not frozen).
- `tests/solver/wiring_test.ml` + `tests/solver/dune` — `test_presolve_growth_guard`
  rewritten for the dark default (toggles the lever via `Unix.putenv`): OFF completes the
  chained-widening elimination (dark default = full elim), ON (factor 16) aborts it; the
  OFF→ON flip is the discriminator. `unix` added to the wiring_test libraries.

## Gates (re-pin)
- `make test`: exit 0, 0 hard failure(s) / 0 soft miss(es).
- `check-frozen`: 14/14.
- RED `test_presolve_growth_guard` (`make wiring-test`): 234 checks, 0 failures.
- Guard reachable + still works: `OXSMT_PRESOLVE_ELIM_GROWTH=16` on RwMutex-w0500/RC-08 →
  `aborted=1`, unsat; unset → `budget=0`, full elim (defs=5520), unsat.
- **OFF = trunk byte-identical** (load-bearing gate now): my cli with the lever unset vs the
  pristine `f75366318f` binary, on a 58-file eq-elim-exercising QF_LIA sample (RwMutex +
  general): **43 compared, 0 diffs**, 15 skipped (the w0500+ blowup files time out
  identically on both — dark = trunk, both run the full unbudgeted elimination).
- ocamlformat: my inserted eq-elim region is ocamlformat-clean (a scratch `ocamlformat`
  pass changes 0 lines in 355–600); the whole-file formatter delta is 8 lines at 696–697 /
  1735 — trunk's OWN pre-existing non-cleanliness, unrelated to eq-elim. So the integrator
  must NOT run `ocamlformat -i` / `make fmt` on presolve.ml (it would add that pre-existing
  8-line churn); the graft is clean as committed.

## Flip path (for the reviewer)
The flip to default-ON is NOT taken. It is gated on a quiet multi-budget A/B (2s AND 20s
minimum) that measures the higher-budget loss surface (the RC-00 20s loss the review found).
Flip only if that A/B is net-neutral-or-positive at 20s.
