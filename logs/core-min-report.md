# Core-minimization upgrade: clause-set refinement (task #36)

Branch `task/core-min` (worktree base 25579d3249). Upgrades
`Session.check_sat_assuming`'s unsat-core minimizer from linear one-probe-per-member
deletion to z3-style **clause-set refinement** (`mus.cpp:80` `mus::imp::get_mus1`). This is a
**cost** change: the documented API guarantee is preserved exactly.

## What changed

`smt/interface/session.ml`, the `minimize` loop inside `check_sat_assuming`'s `Unsat` arm:

- **Before**: after the SAT core's final conflict yields an initial candidate core, achieve
  subset-minimality by one incremental re-solve per candidate member — the probe's return
  (Sat/Unsat) is used only to decide the tested literal, and always exactly
  `|initial_core|` deletion probes are spent.
- **After**: keep a `necessary` set (transition literals already witnessed necessary) and a
  working tail. When a deletion probe returns **Unsat**, the SAT solver's OWN
  failed-assumption set is itself an unsat sub-core of the probed candidate, so every tail
  literal absent from it is redundant and dropped in that single probe (`mus.cpp:106`,
  "unknown := core \ mus") — not one re-solve per member. When a probe returns **Sat** the
  tested literal is necessary and moves to `necessary`. The final core is replayed exactly
  as before (comment preserved) so evidence/model/stats/failed-assumptions describe the
  public result.

### Why the guarantee is preserved (invariant argument)

- Invariant maintained every iteration: `necessary ∪ tail` is Unsat. Each probe tests the
  entire current working set minus one literal, so `necessary` ends subset-minimal (for
  each member, the whole-set-minus-it was witnessed Sat; every later working set is a subset,
  and dropping assumptions only relaxes, so it stays Sat).
- Refinement never discards a confirmed-necessary literal: a `necessary` literal's removal
  was already witnessed Sat, so it lies in **every** unsat core of any subset of the working
  set — hence it necessarily reappears in the probe's failed set.
- Refinement probe set ⊆ linear probe set (same input order, refinement only shrinks the tail
  faster), so refinement never spends MORE probes and is strictly fewer whenever a single
  Unsat probe eliminates ≥ 2 not-yet-processed candidates.

The `.mli` guarantee text ("subset-minimal, duplicate-free subset of assumptions, in input
order ... deleting any one core literal makes the remainder Sat ... initial candidate from
the SAT core's failed assumptions and is then deletion-minimized") remains accurate and was
left unchanged.

## A/B lever + measurement plumbing

- `OXSMT_CORE_MIN_LINEAR` (default OFF ⇒ refinement). Set to `1`/`true`/`yes` selects the
  pre-refinement linear walk — the A/B baseline and a fallback. Read inside the assumption
  path only.
- `Session.minimize_probes : t -> int` (additive, test-only diagnostic; never consulted by
  the solver): incremental re-solves the last `check_sat_assuming` spent (initial solve +
  deletion/refinement probes + final replay). `session.mli` is NOT a frozen interface;
  `check-frozen` stays 14/14 (only `sat.mli`, untouched, is frozen).

## Tests

`smt/interface/test/core_min_test.ml` (`make core-min-test`, wired into `make test`). Gadget:
one wide clause `[c_1;…;c_r; a; b]` plus the four x/y clauses that make `{¬a,¬b}` unsat — so
assuming every `¬c_i,¬a,¬b` gives a loose first conflict naming all of them, while the unique
minimal core is `{¬a,¬b}` and every `¬c_i` is redundant.

- **Subset-minimality (randomized)**: decoy counts r∈{0,1,2,5,12} × seeds {1,2,3,7,42},
  shuffled assumption order. The returned core re-solves Unsat, every one-literal deletion
  re-solves Sat, and it equals `{¬a,¬b}`. Verified on the producing session (core atoms live
  in that session's context).
- **Equivalence**: linear and refinement return the same minimal class; refinement spends
  `≤` probes and, with `≥ 2` decoys, strictly `<`.
- **Discrimination**: the minimality checker REJECTS the non-minimal full set (deleting a
  decoy stays Unsat), so a broken minimizer that left decoys in — or dropped an essential
  (replay would go Sat) — would fail the subset-minimality check.

235/235 checks pass. Existing suites green: `session-cores-test` 79/79 (its bool + LIA
`require_minimal_core` cases), `optimize-test` 24/0, `omt-test` 32/0, `interpolation-test`
17/17. `check-frozen` 14/14.

## Benchmark (old linear vs new refinement)

N assumptions, planted 2-literal MUS, N-2 redundant decoys (`make core-min-test` tail):

| N   | probes (linear) | probes (refine) | ms (linear) | ms (refine) |
|-----|-----------------|-----------------|-------------|-------------|
| 20  | 22              | 5               | 0.08        | 0.06        |
| 100 | 102             | 5               | 0.45        | 0.34        |
| 500 | 502             | 5               | 10.22       | 1.71        |

Linear spends `N+2` probes (initial + N deletions + replay); refinement is a flat 5
regardless of N (initial + one Unsat probe that collapses the decoy block + two Sat probes
for the essentials + replay). Wall tracks probe count: ~6x at N=500. The win scales with the
redundancy of the SAT core's first conflict; on inputs whose first conflict is already tight,
refinement matches linear (never worse — proven above and confirmed at r=0/r=1).

## Scope / byte-identity of non-assuming paths

The entire change is confined to `check_sat_assuming`'s nonempty-assumption arm.
`solve_prepared_assumptions` is called only from that arm (session.ml:2342/2390/2419), and
`reset_assumption_check` only from it (2289). Ordinary `check_sat` never enters any changed
code; the only always-on change is one inert `int` field (`minimize_probes`, initialized 0 in
`create`, mutated only along the assumption path) that no solve reads. The `.smt2` corpus
harness drives `check-sat`, not `check-sat-assuming`, so five-logic solving is behaviorally
unaffected — no verdict-affecting surface on the non-assuming path.

## Not done (deferred)

- **Model rotation (item 2)** intentionally not attempted: item 1 was the bounded
  deliverable and lands clean on its own. Rotation would mark additional necessary literals
  per Sat probe; it is a further cost win, orthogonal to correctness, and can be a second
  commit.
