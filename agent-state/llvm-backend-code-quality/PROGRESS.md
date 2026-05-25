# Progress

Last updated: 2026-05-25.

## Current Claim

Iteration 1 is reviewed, validated, and ready to commit.

## Evidence

- OxCaml branch: `jujacobs/llvm-backend-code-quality`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/16
- Initial state commit: `ac299acb8526`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-backend-code-quality`
- Plan: `agent-state/llvm-backend-code-quality/PLAN.md`

## Current Blocker

None.

## Iteration State

- Target: 10 completed iterations.
- Completed iterations: 1.
- Committed cleanups:
  - Iteration 1: simplified deopt argument list construction in
    `backend/llvm/llvmize.ml` with `List.concat_map`.
- Dropped ideas: none yet.
- Stop condition: stop after 10 completed iterations, then summarize committed
  cleanups, dropped ideas, and remaining promising targets.

## Iteration 1 Candidate

Path: self-proposed candidate.

Candidate: simplify deopt argument list construction in `backend/llvm/llvmize.ml`
by replacing local `List.map ... |> List.concat` forms with `List.concat_map`.

Why likely useful: the same file already uses `List.concat_map`; the candidate
keeps the deopt code on the existing local pattern and removes avoidable
intermediate list construction without changing the emitted argument order.

Review gate: passed after five human-like reviews of the final diff.

Review result: four reviewers said keep with no findings. One reviewer said
revise only because this progress file still described the previous
progress-only commit and stale next step; that finding was accepted and fixed.
No reviewer objected to the `llvmize.ml` cleanup.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.
- Initial validation setup note: the checkout was missing generated configure
  files, so `autoconf` was run and `./configure` was attempted once with the
  ambient OCaml 4.14.2 switch. That failed before creating a build config with
  `OCaml 5.4.x is required but 4.14.2 was found`. Configuration was then run
  successfully with `opam exec --switch=oxcaml-5.4.0+oxcaml -- ./configure`.

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

Change: replace three local `List.map ... |> List.concat` forms in
`backend/llvm/llvmize.ml` with `List.concat_map`, and record iteration 1 state
in this progress file.
Risk: if the rewrite is wrong, deopt argument order or empty-list behavior could
change, or the file could fail to compile.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the source rewrite is a direct
stdlib equivalence, has no behavior change, and does not alter emitted LLVM IR
or test fixtures.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: manual review and five human-like reviews checked that
`List.concat_map f xs` preserves the order and empty-list behavior of
`xs |> List.map f |> List.concat`; the build check covers type and syntax
validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a
behavior-preserving local list idiom.

## Next Step

Commit iteration 1, push it to the draft PR branch, then choose the next
reviewed cleanup candidate.
