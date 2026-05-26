# Progress

Last updated: 2026-05-25.

## Current Claim

The fast-path root-slot optimization has been ported onto
`jujacobs/llvm-backend-integration` while preserving the integration branch's
stack-check slack machinery.

For eligible heap allocation and poll safepoints, the LLVM backend now keeps
roots in ordinary SSA/register values on the fast path and writes static GC root
slots only in the cold `caml_call_gc` slow path. Safepoints with unwind edges,
terminator calls, or active trap handlers stay on the existing conservative
root handling.

## Evidence

- OxCaml branch: `jujacobs/llvm-fast-path-roots-integration`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/18
- Initial state commit: `af33d12129d5`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-fast-path-roots-integration`
- Built a branch-local LLVM clang from the checked-out vendored LLVM sources:
  `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/clang`
- Built and installed OxCaml with:
  `OCAMLPARAM="_,llvm-path=$LLVM_PATH" make install`
- Focused tests run with the branch-local clang wrapper:
  - `make test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml` passed
  - `make test-one-no-rebuild TEST=llvm-codegen/allocation.ml` passed after
    promoting the intended fast-path-root code shape
  - `make test-one-no-rebuild TEST=llvm-codegen/poll_statepoint.ml` passed
  - `make test-one-no-rebuild TEST=llvm-codegen/allocation_frametable.ml`
    passed
  - `make test-one-no-rebuild TEST=llvm-codegen/effect_preemption.ml` skipped
    the poll-insertion action in this local predicate set
- Added an `expectnat` anchor for `_caml_llvm_eh_personality`; without it,
  native toplevel fragments using LLVM EH failed at `dlopen` with a missing
  personality symbol.
- Reconfigured with `--enable-frame-pointers`, which is required by
  `LLVM_BACKEND=1`, and ran:
  `OCAMLPARAM="_,llvm-path=$LLVM_PATH" make llvm-test`
  - Result: 6717 passed, 285 skipped, 10 failed, 0 unexpected errors.
  - Two failures were stale LLVM codegen expect output caused by frame-pointer
    prologues and `oxcaml_fpcc`; promoted those expectations.
  - Reran `make test-one-no-rebuild DIR=llvm-codegen LLVM_BACKEND=1`: 66
    passed, 2 skipped, 0 failed.
  - Remaining full-suite failures after the first run were:
    `tests/frame-pointers/{c_call,effects,exception_handler,reperform,stack_realloc,stack_realloc2}.ml`,
    `tests/llvm-stack-checks/challenges.ml`, and
    `tests/typing-small-numbers/test_matching_native.ml`.

## Current Blocker

Full `make llvm-test` still has non-codegen failures listed above.

## Next Step

Investigate whether the remaining full-suite failures are pre-existing LLVM
backend gaps or regressions from the integration branch.
