# Progress

Last updated: 2026-05-25.

## Current Claim

Agent workspace is initialized for iterative LLVM backend code-quality cleanup.

## Evidence

- OxCaml branch: `jujacobs/llvm-backend-code-quality`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/16
- Initial state commit: `ac299acb8526`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-backend-code-quality`
- Plan: `agent-state/llvm-backend-code-quality/PLAN.md`

## Current Blocker

None.

## Testing Story

Before each commit, record:

- Change: what the commit changes.
- Risk: what could break if the change is wrong.
- Validation: exact commands or checks to run.
- Full LLVM validation: `make llvm-test LLVM_PATH="$LLVM_PATH"`, or why not.
- Stage2 verification: `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"`, or
  why not.
- Why this is enough: why the validation matches the risk.
- Deferred validation: broader tests not run for this commit, with the reason.

## Testing Story for This Commit

Change: require per-commit testing stories and explicit full LLVM plus stage2
validation for implementation commits.
Risk: the agent may still treat broad validation as optional before finalizing
implementation work.
Validation: inspect the changed `GOAL.md`, `PLAN.md`, and `PROGRESS.md`.
Full LLVM validation: not run; this commit only changes agent instructions.
Stage2 verification: not run; this commit only changes agent instructions.
Why this is enough: this commit only changes agent instructions.
Deferred validation: no OxCaml tests; no compiler or runtime code changed.

## Next Step

Inspect the LLVM backend, propose 3-5 candidate cleanup targets, then run
human-like review on the target list before editing.
