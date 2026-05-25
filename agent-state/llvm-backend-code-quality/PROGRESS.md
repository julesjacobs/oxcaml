# Progress

Last updated: 2026-05-25.

## Current Claim

Iterations 1 through 13 are committed or ready to commit; iteration 13 passed
review with no findings and focused validation passed.

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

- Target: open-ended follow-up after the initial 10 completed iterations.
- Completed iterations: 13.
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
  - Iteration 7: combined type-to-string and sanitize steps in
    `backend/llvm/llvmize.ml`'s `add_c_call_wrapper`.
  - Iteration 8: reused `runtime_reg_types` in
    `backend/llvm/llvmize.ml`.
  - Iteration 9: shared fresh unwind label creation in
    `backend/llvm/llvmize.ml`.
  - Iteration 10: used `In_channel.with_open_text` for LLVM backend file reads
    in `backend/llvm/llvmize.ml`.
  - Iteration 11: iterated external-call stack argument pairs directly in
    `backend/llvm/llvmize.ml`.
  - Iteration 12: used `List.rev_append` in LLVM expect-output extraction in
    `backend/llvm/llvmize.ml`.
  - Iteration 13: shared external-call argument register loading in
    `backend/llvm/llvmize.ml`.
- Dropped ideas: none yet.
- Stop condition: continue only while a small candidate passes five-reviewer
  review with no accepted findings and focused validation.

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

## Iteration 7 Candidate

Path: self-proposed candidate.

Candidate: combine the type-to-string and sanitize steps in
`backend/llvm/llvmize.ml`'s `add_c_call_wrapper` signature construction.

Why likely useful: signature construction currently maps over the same list
twice. Combining the two pure per-type transformations keeps the output the same
while making the intended transformation from type to sanitized component
direct.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers judged the cleanup behavior-preserving and
proportionate. Three reviewers requested removing a duplicate `Next Step`
heading in this progress file; that finding was accepted and fixed. One
reviewer suggested a plainer lambda style, and one reviewer found that the first
accepted lambda was not ocamlformatted; both findings were accepted and fixed.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: combine the type-to-string and sanitize steps in
`backend/llvm/llvmize.ml`'s `add_c_call_wrapper` signature construction, and
record iteration 7 state in this progress file.
Risk: if the rewrite is wrong, generated C-call wrapper names could change,
causing wrapper reuse or declaration names to differ.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the rewrite preserves the same
per-type transformation, order, empty-list behavior, and `String.concat`
separator.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews checked that the wrapper-name
components are unchanged; the build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a
behavior-preserving local list transformation.

## Iteration 8 Candidate

Path: self-proposed candidate.

Candidate: derive `runtime_reg_types` once next to `runtime_regs` in
`backend/llvm/llvmize.ml`, then reuse it in `make_ret_type` and
`make_arg_types`.

Why likely useful: both argument and return type construction currently rebuild
the same list from `runtime_regs`. A single local binding keeps the invariant
next to the runtime-register definitions and avoids duplicated construction.

Review gate: passed after five human-like reviews of the corrected final diff.

Review result: the first review round found one progress-file wording mismatch:
the candidate said `runtime_reg_types` was derived next to `runtime_reg_idents`,
but the code derives it next to `runtime_regs`. That finding was accepted and
fixed. A fresh review of the corrected diff then returned five keep verdicts
with no findings.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: derive `runtime_reg_types` once next to `runtime_regs` in
`backend/llvm/llvmize.ml`, reuse it in `make_ret_type` and `make_arg_types`,
and record iteration 8 state in this progress file.
Risk: if the rewrite is wrong, LLVM function argument or return type shapes
could change for runtime registers.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the rewrite preserves the same
`List.map (fun _ -> T.i64) runtime_regs` value and only reuses it in the two
places that previously rebuilt it.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews of the corrected diff found no
issues and checked that the argument and return type shapes are unchanged; the
build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a
behavior-preserving local binding.

## Iteration 9 Candidate

Path: self-proposed candidate.

Candidate: share the creation of fresh unwind labels from exception entries in
`backend/llvm/llvmize.ml`.

Why likely useful: block terminator translation and basic-instruction unwind
setup currently repeat the same `Option.map` over an exception entry. A small
helper names the contract and keeps the fresh-label behavior in one place.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers said keep with no findings. Reviewers checked
that `Cmm.new_label ()` is still called only for `Some` exception entries, that
fresh-label allocation is not shared across sites, and that the progress update
is compact.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: share the creation of fresh unwind labels from exception entries in
`backend/llvm/llvmize.ml`, and record iteration 9 state in this progress file.
Risk: if the rewrite is wrong, unwind labels could be allocated too early, not
allocated for some exception entries, or accidentally shared between call sites.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the helper preserves the exact
`Option.map` body and each call site still calls it independently.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews found no issues and checked the
fresh-label allocation behavior; the build check covers type and syntax
validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes
behavior-preserving helper sharing.

## Iteration 10 Candidate

Path: self-proposed candidate.

Candidate: use `In_channel.with_open_text` for the LLVM expect-output reader
and LLVM IR dump reader in `backend/llvm/llvmize.ml`.

Why likely useful: the expect-output reader manually implemented the same
open/read/close pattern as `In_channel.with_open_text`, while the dump reader
manually closed after a successful read. The standard helper is shorter and
keeps channel cleanup tied to the file read.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers said keep with no findings. Reviewers checked
that the change preserves text-mode reads, removes manual close handling without
adding abstraction, and improves exception cleanup in the dump reader.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: replace manual LLVM backend text-file open/read/close code with
`In_channel.with_open_text`, and record iteration 10 state in this progress
file.
Risk: if the rewrite is wrong, expect-output normalization or LLVM IR dumping
could fail to read the generated files, or channel close timing could change in
an observable way.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the rewrite preserves text-mode
file reads and only replaces manual resource handling with the standard helper.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews found no issues and checked the
channel-handling behavior; the build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes local file
read resource handling.

## Iteration 11 Candidate

Path: self-proposed candidate.

Candidate: iterate the existing `(reg, typ)` pairs in `stack_args` directly
when filling external-call stack argument slots in `backend/llvm/llvmize.ml`.

Why likely useful: the code already partitions a pair list, then rebuilt
separate register and type lists only to immediately consume them with
`List.iter2`. Direct iteration keeps the existing pair shape and avoids two
temporary lists.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers said keep with no findings. Reviewers checked
that the removed `List.iter2` length check was redundant because both lists came
from the same `stack_args` value, and that direct pair iteration matches the
nearby code shape.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: replace `List.iter2` over `List.map fst stack_args` and
`List.map snd stack_args` with direct `List.iter` over `stack_args`, and record
iteration 11 state in this progress file.
Risk: if the rewrite is wrong, external calls with outgoing stack arguments
could store the wrong register value or LLVM type into a stack argument slot.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the rewrite preserves the
existing `(reg, typ)` pair order and only removes redundant list projection.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews found no issues and checked the
pair-list behavior; the build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes a local
list iteration.

## Iteration 12 Candidate

Path: self-proposed candidate.

Candidate: replace `List.rev function_lines @ acc` with
`List.rev_append function_lines acc` in the LLVM IR and assembly expect-output
extraction loops in `backend/llvm/llvmize.ml`.

Why likely useful: this is the standard OCaml helper for the existing
reverse-then-append accumulator pattern. It preserves order while avoiding the
manual append expression.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers said keep with no findings. Reviewers checked
that `List.rev_append function_lines acc` is equivalent to
`List.rev function_lines @ acc`, including empty and singleton lists, and that
the change is limited to the intended accumulator pattern.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: use `List.rev_append` in LLVM expect-output extraction loops, and
record iteration 12 state in this progress file.
Risk: if the rewrite is wrong, normalized LLVM IR or assembly expect output
could reorder extracted function lines.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the rewrite is the standard
equivalent of the existing reverse-then-append expression.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews found no issues and checked the
line-order behavior; the build check covers type and syntax validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes local list
accumulation.

## Iteration 13 Candidate

Path: self-proposed candidate.

Candidate: add a local `load_arg_regs` helper inside `extcall` in
`backend/llvm/llvmize.ml` to share duplicated `List.map2` argument loading in
the allocating C-call path and wrapped C-call path.

Why likely useful: both paths loaded argument registers with the same
`List.map2 (fun reg typ -> load_reg_to_temp ~typ t reg)` expression. A local
helper keeps the operation named and scoped to the only function that needs it.

Review gate: passed after five human-like reviews of the final diff.

Review result: all five reviewers said keep with no findings. Reviewers checked
that the helper is local to `extcall`, preserves the existing `List.map2` order
and arity behavior, and intentionally leaves the stack-argument path unchanged
because that path works on filtered `(reg, typ)` pairs.

Validation result:

- `git diff --check`: passed.
- `eval "$(../../../scripts/agent-tmp-env)" && opam exec
  --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`: passed.

## Testing Story for This Commit

Change: share external-call argument register loading through a local
`load_arg_regs` helper, and record iteration 13 state in this progress file.
Risk: if the rewrite is wrong, external C calls could load argument registers
with the wrong LLVM type or in the wrong order.
Validation: `git diff --check`; `eval "$(../../../scripts/agent-tmp-env)" &&
opam exec --switch=oxcaml-5.4.0+oxcaml -- make boot-compiler`.
Full LLVM validation: not run for this commit; the rewrite preserves the exact
`List.map2` body and only reuses it in the two sites that previously duplicated
it.
Stage2 verification: not run for this commit; self-stage2 is intentionally
deferred for the same reason as full LLVM validation.
Why this is enough: five human-like reviews found no issues and checked the
argument order and helper scope; the build check covers type and syntax
validity.
Deferred validation: `make llvm-test LLVM_PATH="$LLVM_PATH"` and
`make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` are deferred because they
are broad LLVM backend validation runs, and this commit only changes local
helper sharing.

## Next Step

Commit iteration 13 and push it to the draft PR branch.
