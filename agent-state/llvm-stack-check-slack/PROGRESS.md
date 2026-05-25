# Progress

Last updated: 2026-05-25.

## Current Claim

The native-style slack stack-check implementation is in place for AArch64
`-llvm-backend`, with focused tests for the IR contract, prologue elision,
ordinary stack checks, runtime stack growth, and a reduced compile-time stack
stress case.

## Evidence

- OxCaml branch: `jujacobs/llvm-stack-check-slack`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/15
- Initial state commit: `2d58d583e085`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-stack-check-slack`
- Design guide: `agent-state/llvm-stack-check-slack/DESIGN.md`
- Baseline merged: `jujacobs/stack-check-size-contract`
- OxCaml LLVM IR now emits ordinary AArch64 stack checks for CFG
  `Stack_check` instructions and calls `caml_llvm_call_realloc_stack` on the
  slow path.
- Vendored LLVM AArch64 prologue checks now use the
  `"oxcaml-stack-check-bytes"` contract:
  - missing contract stays conservative;
  - nonzero ordinary CFG checks keep a prologue check unless the LLVM prologue
    plus pre-check OCaml stack use is exactly zero;
  - zero-byte CFG checks allow prologue elision only while preserving runtime
    helper emergency slack.
- LLVM ordinary stack checks call `caml_llvm_call_realloc_stack`; LLVM prologue
  slow paths use the existing stack-argument `caml_call_realloc_stack` protocol
  with an explicit helper reserve in the prologue check.
- Added `tests/llvm-stack-checks/challenges.ml` for runtime challenge programs.
- Added `tests/llvm-stack-checks/compile_challenges.ml` as a reduced
  compile-time stack-growth stress test.
- Expanded `tests/llvm-codegen/stack_check_size_contract.sh`.
- The self-stage2 compiler segfault was reduced to the prologue-elision
  boundary for nonzero ordinary stack checks. Allowing nonzero checks to spend
  pre-check stack slack reproduced crashes in:
  - `middle_end/flambda2/simplify/simplify_static_const.ml` with
    `OCAMLRUNPARAM=b=1,l=32768`;
  - `middle_end/flambda2/types/provers.ml` with
    `OCAMLRUNPARAM=b=1,l=32768`.
  The focused boundary cases and compile-time stack challenge now cover that
  class of failure directly.

## Validation

- `ninja -C "$OXCAML_AGENT_TMP/llvm-build" clang` passed after vendored LLVM
  changes.
- `make llvm-install LLVM_BOOT_BACKEND=0 LIST=` passed.
- Focused tests passed:
  - `tests/llvm-stack-checks/challenges.ml`
  - `tests/llvm-stack-checks/compile_challenges.ml`
  - `tests/llvm-codegen/stack_check_size_contract.ml`
  - `tests/llvm-codegen/stack_check_size_contract_no_cfg_stack_checks.ml`
  - `tests/llvm-codegen/allocation.ml`
  - `tests/llvm-codegen/store_modify.ml`
- `make llvm-self-stage2-test LLVM_BOOT_BACKEND=0 LIST=` completed without the
  previous `middle_end/flambda2/types/provers.ml` / `Misc.try_finally`
  compiler segfault and reached the known 7 failures:
  - `tests/frame-pointers/c_call.ml`
  - `tests/frame-pointers/effects.ml`
  - `tests/frame-pointers/exception_handler.ml`
  - `tests/frame-pointers/reperform.ml`
  - `tests/frame-pointers/stack_realloc.ml`
  - `tests/frame-pointers/stack_realloc2.ml`
  - `tests/typing-small-numbers/test_matching_native.ml` (`toplevel.opt`
    `Trace/BPT trap`)
  - 6655 passed
  - 268 skipped
  - 7 failed
  - 0 unexpected errors
  - log: `/tmp/llvm-stack-check-slack-self-stage2-test-nonzero-prefix-conservative.log`
- `make llvm-test LLVM_BOOT_BACKEND=0 LIST=` completed with the same known 7
  failures, no unexpected errors:
  - `tests/frame-pointers/c_call.ml`
  - `tests/frame-pointers/effects.ml`
  - `tests/frame-pointers/exception_handler.ml`
  - `tests/frame-pointers/reperform.ml`
  - `tests/frame-pointers/stack_realloc.ml`
  - `tests/frame-pointers/stack_realloc2.ml`
  - `tests/typing-small-numbers/test_matching_native.ml` (`toplevel.opt`
    `Bus error`)
  - 6681 passed
  - 281 skipped
  - 7 failed
  - 0 unexpected errors
  - log: `/tmp/llvm-stack-check-slack-full-llvm-test-nonzero-prefix-conservative.log`
- Cheap final checks passed:
  - `sh -n testsuite/tests/llvm-codegen/stack_check_size_contract.sh`
  - `sh -n testsuite/tests/llvm-stack-checks/challenges.sh`
  - `sh -n testsuite/tests/llvm-stack-checks/compile_challenges.sh`
  - `git diff --check`

## Current Blocker

None.

## Next Step

Commit the current implementation and test additions when ready.
