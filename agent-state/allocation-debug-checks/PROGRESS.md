# Progress

Last updated: 2026-05-24.

## Current Claim

Normal LLVM backend heap allocation no longer emits minor-heap debug helper
calls. The helper calls were temporary instrumentation, not supported debug
functionality.

## Evidence

- OxCaml branch: `jujacobs/allocation-debug-checks`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/7
- Initial state commit: `a3bf6d3f68ce`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/allocation-debug-checks`
- Goal recorded in `agent-state/allocation-debug-checks/GOAL.md`
- Source audit:
  - `backend/llvm/llvmize.ml` was the only LLVM backend source file emitting
    `caml_debug_check_minor_heap` or `caml_debug_check_minor_heap_head`.
  - `testsuite/tests/llvm-codegen/allocation.ml` was the only LLVM codegen
    expected-output test containing these helper calls.
  - `runtime/minor_gc.c` and `runtime/caml/minor_gc.h` still contain no-op
    helper definitions/declarations, but they are no longer referenced by LLVM
    backend codegen or LLVM codegen tests.
- Native backend comparison:
  - `backend/amd64/emit.ml` and `backend/arm64/emit.ml` lower heap allocation
    by subtracting from the allocation pointer, comparing with
    `young_limit`, and recording the GC frame. They do not emit minor-heap
    debug helper calls.
  - Runtime minor-heap checking conventions use normal runtime assertions and
    `DEBUG`-conditioned checks, not unconditional compiler-emitted no-op calls.
- Implementation:
  - Removed LLVM codegen emission of `caml_debug_check_minor_heap`.
  - Removed LLVM codegen emission of `caml_debug_check_minor_heap_head`,
    including the function-name-filtered
    `debug_check_minor_heap_head_for_current_repro` call sites on calls and
    raises.
  - Updated `testsuite/tests/llvm-codegen/allocation.ml` expected IR/assembly
    to prove the default allocation path has no debug helper calls.
- Verification:
  - `rg -n "caml_debug_check_minor_heap(_head)?|debug_check_minor_heap|minor_heap_head|current_repro" backend/llvm testsuite/tests/llvm-codegen/allocation.ml testsuite/tests/llvm-codegen`
    finds no matches. This command is long; keep it as a single shell command
    when rerunning.
  - Direct compile with installed compiler and agent-local patched clang wrapper
    passed:
    `OCAMLPARAM="_,llvm-path=$LLVM_PATH" ./_install/bin/ocamlopt.opt -O3 -llvm-backend -keep-llvmir -S allocation.ml`.
    The generated `.ll` and `.s` files contained no
    `caml_debug_check_minor_heap`, `caml_debug_check_minor_heap_head`,
    `debug_check_minor_heap`, or `minor_heap_head`.
  - `make -s boot-compiler` passed.
  - Built agent-local clang from
    `/Users/julesjacobs/git/oxcaml-llvm/main/llvm-project` at commit
    `1fc49b70ffad` into
    `/tmp/oxcaml-agent-allocation-debug-checks/llvm-build-1fc`, then pointed
    the agent clang wrapper at that binary.
  - Focused expected-output check passed with the matching patched clang:
    `OCAMLPARAM="_,llvm-path=$LLVM_PATH" make promote-one-no-rebuild TEST=llvm-codegen/allocation.ml`
    after `unset LIST`.
- Test note:
  - `make promote-one TEST=llvm-codegen/allocation.ml` needed `LIST` unset
    because `agent-tmp-env` exports `LIST`.
  - The focused promotion requires `OCAMLPARAM="_,llvm-path=$LLVM_PATH"` so
    the test compiler uses the agent-local patched clang wrapper.

## Current Blocker

None.

## Next Step

Ready for review.
