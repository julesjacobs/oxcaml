# Task board

Status: `todo` · `in progress` · `blocked` · `done`. One row per task; acceptance
criteria are the contract. Specs live in the task dispatch, not paraphrased here.

## M0 — the gate (the oracle exists before the solver)

| id | title | status | owner | acceptance |
|----|-------|--------|-------|-----------|
| M0-bootstrap | Constitution, skeleton, build entry points | done | (bootstrap) | `make build` green, `make fmt` clean, initial commit on `oxsmt` |
| ADR-0003 | Term/sort representation design pass | in progress | (separate agent) | ADR written + adversarial review; freezes `core` API |
| M0-core | Implement `smt/core` (Sort, Symbol, Term, hash-consing, smart constructors, `Debug.check`) | blocked (ADR-0003) | — | I1/I2 hold; `Debug.check` validates invariants; unit self-checks |
| M0-smtlib | SMT-LIB2 printer (shipped) + test-only parser | todo | — | prints `QF_UFLIA` sessions; round-trips our dumps |
| M0-harness | `.smt2` golden/expect harness + promote workflow | done (branch `task/harness`) | (harness agent) | runs `tests/cases`+fixtures, digest to `../logs`, promote accepts goldens, bucketed counters + label check + stats sidecar. STATUS.md *generation* deferred to CI/nightly (harness feeds the stats sidecar only — DESIGN.md §11) |
| M0-gate | Lean encoder + certification CI job + content-addressed cache | todo (master-only) | — | unsat→Lean theorem, sat→`decide`; cache keyed by hash+encoding+toolchain+grind cfg |
| M0-honeypots | Nightly known-wrong verdicts + seeded mutants must turn the gate red | todo (master-only) | — | gate goes red on injected faults; validates the encoder during M0 |
| M0-corpora | Fetch public SMT-LIB sets (QF_UF, QF_LIA, QF_UFLIA) into `../corpora` | in progress | (separate agent) | sets present, pre-labeled, not tracked in git |
| M0-harness-hygiene | Harness follow-ups from review (LOW/NIT, none blocking; see `../logs/harness-review.md`) | todo | — | (1) parameterize Makefile's absolute opam path before off-box CI; (2) default `LOGS=../logs` resolves to `worktrees/logs` from a worktree, only correct from `main/`; (3) doc caveat that no-`:status` files have regression protection only, not a soundness gate; (4) `.err` temp file / pipe FDs leak on mid-function exception in `run_solver_raw`; (5) counter > `max_int` yields `Fail_error` instead of clamping to `>=10k` |

## Later milestones (see DESIGN.md §9)

| id | title | status | acceptance |
|----|-------|--------|-----------|
| M1-cdcl | Clausifier + CDCL SAT core | todo | passes public SAT benchmarks; `THEORY` frozen at end of M1 |
| M2-euf | EUF congruence closure | todo | QF_UF passes |
| M3-lia | LIA simplex + branch-and-bound | todo | QF_LIA passes |
| M4-interface | Session API + combination (QF_UFLIA) + unsat cores | todo | QF_UFLIA passes; stage-1 feature-complete |
| M5-certs | Certificates + Lean replay | todo | reasons replay as Lean proofs; oracle becomes a checker |
