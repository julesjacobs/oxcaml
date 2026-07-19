# Consumer-contract commit: core sufficiency doc + verdict-first degradation (bugreports 01/02)

Branch `task/core-min-contract`, a single commit on the real **post-LAND-50 trunk**
`2d24673ee8` (LAND 50 = the clause-set refinement; landed as `c2300bf41c` + LAND-50 content).
No stale-base caveat: it is rebased directly onto the landed tip.

Delivers the two commitments made to the live consumer in `bugreports/`:
`01-check_sat_assuming-empty-core` and `02-check_sat_assuming-minimization-unknown`.

## (1) session.mli doc hardening — bugreport 01

`check_sat_assuming`'s doc now states explicitly, as guarantees:

- **Sufficiency (replay-verified)**: on `Unsat` with `Some core`, `(active assertions) ∧
  (returned core)` is unsatisfiable — the implementation re-solves exactly the candidate core
  and only publishes it if that replay is `Unsat`, never a non-covering subset.
- **Empty-core semantics**: `unsat_core = Some []` on `Unsat` occurs ONLY when the active
  assertions alone are unsatisfiable (the empty-core replay ran with zero assumptions); an
  empty core can never hide a necessary assumption. This is the exact property the consumer's
  "selector outside the core ⇒ fact unused" fade logic relies on.

The `assumption_check` type doc was also updated (see (2)).

## (2) Verdict-first degradation — bugreport 02 (behavior change)

Previously any core-extraction degradation returned `{ verdict = Unknown; unsat_core = None }`.
Now, since the initial solve already certified `(actives ∧ assumptions)` unsat via a completed
solve, the verdict is **never downgraded below what `check_sat` would report**: all three
degradation arms return `verdict = Unsat` with `unsat_core = None` and a diagnostic tag in
`last_unknown_reason`:

| arm | trigger | reason tag |
|-----|---------|------------|
| minimization | a deletion probe degrades to `Unknown` | `assumption-core-minimize-unknown` |
| replay | final core replay degrades to `Unknown` | `assumption-core-recheck-unknown` |
| replay | final core replay returns `Sat` (inconsistent core, refused) | `assumption-core-recheck-sat` |

Code: a `degrade_core reason` helper in `check_sat_assuming`'s `Unsat` arm (session.ml) sets
`last_verdict <- Unsat`, `last_model <- None`, `unknown_reason <- reason`, returns
`{ verdict = Unsat; unsat_core = None }`. The `.mli` invariant text changed from "`Some`
exactly when `Unsat`" to "`Some` on `Unsat` unless core extraction degraded — `None` then",
and `last_unknown_reason`'s doc now notes it also carries the degradation tag under an `Unsat`
verdict. The arms BEFORE the initial solve (decline paths: degraded / reserved / cert-active /
live-lemma / pure-bv / internalization-fail) are unchanged — they never established `Unsat`,
so they still return `Unknown`.

### Fault-injection hook + RED test (the arms are unreachable naturally)

The degradation arms cannot be hit by ordinary inputs (the replay re-certifies a core the
minimizer just derived). Added two test-only hooks in session.ml/.mli — `verdict option ref`s
`inject_deletion_verdict` / `inject_replay_verdict`, set via
`inject_{deletion,replay}_verdict_for_test`, consumed once and only inside
`check_sat_assuming`'s `Unsat` arm (no non-assumption path reads them). `core_min_test`:

- **control** (no injection): the query returns `Unsat` with a verified 2-literal core and an
  empty reason — so the injection is exactly what exercises each arm.
- **three degradation cases**: inject the arm, assert `verdict = Unsat` (old code returned
  `Unknown` here — this is the RED), `unsat_core = None`, and `last_unknown_reason` equals the
  arm's exact tag. The tag assertion confirms the specific arm executed (a non-firing
  injection would leave the reason empty and the core `Some`, failing the test).

## (3) Multi-MUS test — review rider 1

Added to `core_min_test`: overlapping MUSes (assume `a,b,c`; hard `~a|~b`, `~a|~c`; minimal
cores `{a,b}` and `{a,c}`). Under both strategies the returned core is asserted subset-minimal,
size 2, holding the shared essential `a` and exactly one of `b`/`c` — never the non-minimal
`{a,b,c}`. The two strategies may pick different MUSes; both must be valid.

## Gates

- `core-min-test`: 260/260 (was 235; +25 for the sections above).
- `session-cores-test` 79/79, `optimize` 24, `omt` 32, `interpolation` 17.
- `check-frozen` 14/14 (session.mli is not a frozen interface; only `sat.mli` is, untouched).
- `make test`: exit 0 (0 hard failures / 0 soft misses).

## Scope

Still confined to `check_sat_assuming`'s nonempty-assumption arm plus the two test-only hooks
and doc text. Ordinary `check_sat` is unaffected; the fault-injection refs are `None` in
production and read only inside the assumption arm.
