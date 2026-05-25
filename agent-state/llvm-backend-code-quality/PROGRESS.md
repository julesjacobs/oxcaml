# Progress

Last updated: 2026-05-25.

## Current Claim

Iterations 1, 2, 3, 4, 5, and 6 are committed or ready to commit; iteration 6
is reviewed and validated.

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
- Completed iterations: 6.
- Committed cleanups:
  - Iteration 1: simplified deopt argument list construction in
    `backend/llvm/llvmize.ml` with `List.concat_map`.
  - Iteration 2: simplified module metadata prefix/suffix checks in
    `backend/llvm/llvmize.ml` with `String.starts_with` and
    `String.ends_with`.
  - Iteration 3: simplified digit detection in
    `backend/llvm/llvmize.ml`'s `normalize_toplevel_names`.
  - Iteration 4: reused `Format.pp_comma` in
    `backend/llvm/llvm_ir.ml`'s `Type.pp_t`.
  - Iteration 5: shared operand-bundle pretty-printers in
    `backend/llvm/llvm_ir.ml`'s `Instruction.pp_t`.
  - Iteration 6: shared integer vector immediate construction in
    `backend/llvm/llvmize.ml`.
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

## Iteration 2 Candidate

Path: self-proposed candidate.

Candidate: simplify the prefix/suffix check in
`backend/llvm/llvmize.ml`'s `module_name_from_symbol` by using
`String.starts_with` and `String.ends_with` instead of manually comparing
substrings.

Why likely useful: the same file already uses `String.starts_with` and
`String.ends_with`; using them here names the intended string contract directly
and leaves the existing `String.sub` only for extracting the module name.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers judged the `llvmize.ml` cleanup clean,
idiomatic, and proportionate. The only findings were progress-file bookkeeping:
an accidental duplicate `Next Step` heading and stale next-step wording. Those
findings were accepted and fixed.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: replace manual prefix and suffix substring comparisons in
`backend/llvm/llvmize.ml`'s `module_name_from_symbol` with
`String.starts_with` and `String.ends_with`, and record iteration 2 state in
this progress file.
Risk: if the rewrite is wrong, module metadata symbol validation could accept or
reject the wrong symbol shape, or the extracted module name could be computed
from an invalid string.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the source rewrite keeps the
existing combined length check and substring extraction, and only names the
existing prefix/suffix checks with equivalent stdlib helpers.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews checked that preserving the length
guard avoids prefix/suffix overlap issues and that the helper calls preserve the
old string contract; the build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a
behavior-preserving local string idiom.

## Iteration 3 Candidate

Path: self-proposed candidate.

Candidate: simplify digit detection in `normalize_toplevel_names` by using a
local `is_digit` helper with a character range pattern instead of repeating
manual `Char.code` bounds.

Why likely useful: the helper names the contract being checked and removes a
duplicated low-level digit test in code that is otherwise about normalizing
temporary toplevel names.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers judged the `llvmize.ml` cleanup clean,
idiomatic, and proportionate. Three reviewers requested progress-file fixes for
an accidental duplicate `Next Step` heading and stale next-step wording. Those
findings were accepted and fixed. Two reviewers had no findings.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: simplify digit detection in `normalize_toplevel_names` by using a local
`is_digit` helper with a character range pattern instead of repeated
`Char.code` bounds, and record iteration 3 state in this progress file.
Risk: if the rewrite is wrong, expect-test normalization could stop normalizing
temporary `camlTOP` names, or it could normalize names that are not followed by
a digit.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the source rewrite preserves the
old contract that normalization only happens for `camlTOP` followed by at least
one digit.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews checked that `is_digit` preserves
the old ASCII digit test and that the `i + 7 < len` guard still requires a
following digit before normalization; the build check covers type and syntax
validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a
behavior-preserving local expect-output normalization idiom.

## Iteration 4 Candidate

Path: self-proposed candidate.

Candidate: use the existing `Format.pp_comma` separator in
`backend/llvm/llvm_ir.ml`'s `Type.pp_t` instead of an anonymous comma printer.

Why likely useful: `llvm_ir.ml` defines `pp_comma` for this exact formatting
contract and uses it throughout the rest of the file. This keeps type printing
on the same local style without changing emitted text.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers said keep with no findings. Reviewers checked
that `Format.pp_comma` emits the same text as the anonymous separator, is in
scope through `let open Format`, and preserves the general `pp_print_list`
behavior for empty and single-element lists.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: use the existing `Format.pp_comma` separator in
`backend/llvm/llvm_ir.ml`'s `Type.pp_t`, and record iteration 4 state in this
progress file.
Risk: if the rewrite is wrong, LLVM type printing for struct types could change
or the file could fail to compile.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the helper has the same
implementation as the replaced anonymous separator and only affects one local
pretty-printer separator.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews checked that the emitted separator
text is unchanged and that no list special cases are introduced; the build check
covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a
behavior-preserving pretty-printer helper usage.

## Iteration 5 Candidate

Path: self-proposed candidate.

Candidate: share the operand-bundle pretty-printers used by `Invoke` and `Call`
inside `backend/llvm/llvm_ir.ml`'s `Instruction.pp_t`.

Why likely useful: `Invoke` and `Call` currently duplicate the same two local
pretty-printers. Hoisting them once inside `Instruction.pp_t` keeps the
abstraction local to the only function that uses it and reduces duplicate
formatting logic without changing emitted text.

Review gate: passed after five human-like reviews of the final diff.

Review result: three reviewers said keep with no findings. Two reviewers said
revise only because this progress file had an accidental duplicate `Next Step`
heading before the iteration 5 section. That finding was accepted and fixed. No
reviewer objected to the `llvm_ir.ml` cleanup.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: share the operand-bundle pretty-printers used by `Invoke` and `Call`
inside `backend/llvm/llvm_ir.ml`'s `Instruction.pp_t`, and record iteration 5
state in this progress file.
Risk: if the rewrite is wrong, call or invoke operand bundles could print
differently, especially for empty, single, or multiple bundle lists.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the change hoists identical local
formatting helpers without changing their bodies or call sites.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews checked that the helper bodies and
formatting paths are unchanged and remain scoped to `Instruction.pp_t`; the
build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes
behavior-preserving pretty-printer helper sharing.

## Iteration 6 Candidate

Path: self-proposed candidate.

Candidate: share integer vector immediate construction between
`int_vector_constant` and `int_vector_constant_like` in
`backend/llvm/llvmize.ml`.

Why likely useful: both helpers currently build the same textual vector
immediate from an integer element type, element count, and integer value. A
single local helper keeps that formatting contract in one place without
changing the emitted LLVM IR text.

Review gate: passed after five human-like reviews of the final diff.

Review result: one reviewer said keep with no findings. Four reviewers said
revise only because this progress file had an accidental duplicate `Next Step`
heading before the iteration 6 section. That finding was accepted and fixed. No
reviewer objected to the `llvmize.ml` cleanup.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: share integer vector immediate construction between
`int_vector_constant` and `int_vector_constant_like` in
`backend/llvm/llvmize.ml`, and record iteration 6 state in this progress file.
Risk: if the rewrite is wrong, vector integer constants could be emitted with a
different element type, element count, separator, or scalar fallback behavior.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the helper preserves the existing
format string, separator, element repetition, and scalar `T.Int _` fallback.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews checked that the helper remains
local, preserves the existing vector string construction path, and does not add
special cases; the build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes
behavior-preserving SIMD immediate construction sharing.

## Next Step

Commit iteration 6, push it to the draft PR branch, then choose the next
reviewed cleanup candidate.
