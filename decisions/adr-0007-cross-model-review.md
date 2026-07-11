# ADR-0007: Cross-model review is blocking for TCB-path merges

Status: Accepted 2026-07-11. Adopted operationally before this formal commit
(the codex retrospective that surfaced G1–G4 is what motivated it); landed with
the M1 THEORY freeze. The task/gate3 dual review (same-model + codex) is its
first blocking run.

## Context

DESIGN §10 names reviewer/author correlation as the one structural limitation
of agent review: reviewer and author share a base model and can share blind
spots. Until today that risk was mitigated only indirectly (honeypot
calibration, the external Lean gate).

On 2026-07-11 a cross-model retrospective review (codex CLI, gpt-5.6-sol,
reasoning=high; board #108) of the merged trunk found **four verified holes in
tests/gate's reader/lexer** — G1 quoted-token kind loss (|0| ≡ 0 → a false
unsat certified GREEN), G2 missing string-literal lexing (command injection
via :source), G3 check-sat as a no-op (asserts silently unioned across
queries), G4 div/mod → MALFORMED, which is not red (silent oracle bypass).
Same-model adversarial review had cleared this exact code ("no attack found"
on the reader) across two review rounds that *did* catch other serious bugs
(the cache-collision exploit). The signal is precise: same-model review is
strong but correlated; a different base model caught what ours could not see,
in the one component where a miss is unaffordable.

## Decision

1. **TCB-path merges require a cross-model review pass before merge.** TCB
   paths (per DESIGN §10's residual-TCB list, made concrete):
   - `tests/gate/**` (reader, encoder, cache, outcome classification)
   - `smt/smtlib/printer*` (the shipped printer)
   - `tools/check_frozen*`, `FROZEN.sha256` mechanics
   - any change to canonicalization/cache-key code, wherever it lives
   The integrator must not land a diff touching these paths without a recorded
   cross-model verdict (codex runbook: logs/codex-review-runbook.md) in
   addition to the standard same-model adversarial review. Findings are
   triaged by the master as usual; "no findings" is a recorded verdict.
2. **Everything else stays async/non-blocking.** The latency argument stands:
   ordinary merges proceed on same-model review; the cross-model agent reviews
   deltas continuously off the merge path and files findings as board rows.
3. **Calibration feedback loop.** Cleared-then-caught material (a same-model
   review "no attack found" later falsified by the cross-model pass or any
   other oracle) is fed back to the reviewer pool as calibration data: the
   reviewer that cleared the code writes a short calibration note (what
   attack-brief item would have caught it; what to probe differently), appended
   to the relevant review log. This is DESIGN §10's reviewer-calibration
   honeypot loop running on real data instead of seeded data.

## Consequences

- TCB merges gain latency (one cross-model pass). Acceptable: such changes are
  few, small, and catastrophic-if-wrong.
- A second model dependency (codex/gpt-5.6 via the JS ai-api-proxy) enters the
  merge path for TCB changes only. If the tool is unavailable, TCB merges wait
  or the master records an explicit, logged exception — silence is not an
  option.
- **A "no findings" verdict must be a validated genuine run, not a filtered
  null.** Discovered operationally (2026-07-11): the provider's content filter
  can refuse an adversarially-framed review while still exiting 0 with zero
  findings — indistinguishable from a clean pass unless checked. The runbook's
  sanity-check (grep for refusal markers + require the transcript to contain
  substantive analysis) is mandatory before recording a cross-model verdict;
  review prompts use defensive-correctness framing (see
  logs/codex-review-runbook.md).
- AGENTS.md gains the TCB-path list + the rule; the integrator's checklist
  gains the verdict check.

## Evidence

- logs/codex-review/SUMMARY.md, logs/codex-review/gate.md (G1–G4, verified)
- logs/gate-review.md (the same-model reviews that cleared the reader while
  catching the cache-collision exploit — strong but correlated)
- Remediation: board #119 (fixes + permanent honeypots + encoding_version bump
  + retroactive re-certification, logs/gate3-recertification.md)
