# ADR-0009: Async review pipelining

Status: Accepted 2026-07-11 (user directive). Extends the DESIGN.md §11
integrator workflow ("rebase → test → fast-forward only"); adopted operationally
during the M4 convoy, when several branches were under review and test
concurrently.

## Context

DESIGN.md §11 fixes the merge mechanic: the integrator rebases a task branch onto
the current `oxsmt` tip, runs the full PR suite on the *rebased* head, and lands
with `--ff-only`, so trunk is always the exact commit CI tested and history stays
linear. As written, that reads as a *serial* pipeline — review, then rebase, then
test, then land, one branch at a time through the integrator's queue.

Under a zero-human-review, agent-driven workflow the serial reading wastes the two
things we have most of: parallel agents and wall-clock during slow oracle/codex
passes. Reviews (same-model adversarial + the ADR-0007 blocking cross-model codex
pass), full-suite runs, and merge preparation are largely independent of each
other and of other branches, yet a serial queue makes them wait. The convoy also
surfaced a real cost: re-running a full suite and a fresh review after a purely
mechanical rebase (a formatting-only delta, or hunks disjoint from everything that
landed since the reviewed base) buys no safety and blocks the queue.

## Decision

Adopt an **async review model**. Agents produce "PR" branches; reviews and test
runs happen **independently, against pinned shas**, in parallel and
speculatively. The safety invariant is unchanged from §11: **trunk stays linear
and every landed commit was fully reviewed AND tested at its exact rebased sha**
(rebase → test → ff-only).

1. **Reviews target pinned shas and never block other lanes.** Multiple
   reviewers and rounds run concurrently — same-model and codex in parallel, and a
   second codex driver when both a TCB (blocking) and a non-TCB pass are pending.
   A review verdict is recorded against the sha it examined; a later tip does not
   silently inherit it (see [[freeze-tip-during-review]]: once a review is
   dispatched against a sha, the task-branch tip is not advanced until the verdict
   lands).
2. **The integrator speculates.** It pre-rebases and pre-tests queued branches
   *before* final verdicts land, so landing is instant on approval. If the tip or
   trunk moves, it re-runs — cheap and mechanical.
3. **Merge gating** stays: a sha-pinned master approval ("APPROVED FOR MERGE:
   task/X at `<sha>`") plus a green suite on the rebased head.
4. **Triviality exception to the exact-sha re-test rule** (user, 2026-07-11). For
   a *trivial* rebase — a formatting-only delta, or hunks fully disjoint from
   everything that landed since the reviewed base — the integrator carries the
   existing verdicts forward and lands with the fast suite (a whitespace-only
   rebase reduces to a build check). The integrator **classifies each rebase and
   records the classification** in the integration report. A rebase with semantic
   overlap, touched reviewed hunks, or a conflict resolution is **not** trivial:
   it gets a full re-test and a scoped re-review of the affected hunks.

## Consequences

- More parallelism and speculation: verdicts, suite runs, and merge prep overlap
  instead of serializing; a branch can accumulate more review rounds per unit of
  wall-clock.
- **Async is not post-merge review for blockers.** A blocking review (the
  ADR-0007 cross-model TCB pass) must still land *before* the merge. The only
  post-merge review lane remains the DESIGN §10 trailing cross-model reviewer,
  which reviews committed trunk off the merge path and files findings as board
  rows.
- The triviality exception concentrates trust in the integrator's classification.
  The classification is recorded (auditable), and the conservative default is
  "not trivial" — anything touching a reviewed hunk or resolving a conflict falls
  back to full re-test + scoped re-review.
- Message races between approval, tip movement, and speculative rebases are
  endemic; sha-pinned approvals and teammate read-backs are the safety net (a
  verdict or approval that does not name a sha is not actionable).
- Complements [[freeze-tip-during-review]] (pins stay load-bearing) and the §11
  linear-trunk / every-commit-green invariant, which the exact-sha rule and its
  triviality exception preserve.
