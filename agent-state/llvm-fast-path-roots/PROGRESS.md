# Progress

Last updated: 2026-05-25.

## Current Claim

Implemented LLVM fast-path root slots for eligible basic allocation and poll
safepoints, with expect tests covering the optimized path, const-int filtering,
trap/unwind fallback, no-root safepoints, multiple live-root counts, aliased
logical roots, and interaction with ordinary call root preservation.

## Evidence

- OxCaml branch: `jujacobs/llvm-fast-path-roots`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/14
- Rebasing: branch rebased on `upstream/main` on 2026-05-25 after debug checks
  were moved behind a flag upstream.
- Initial state commit: `4fbbb1a88324`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-fast-path-roots`
- Design plan: `agent-state/llvm-fast-path-roots/PLAN.md`
- Code path: `backend/llvm/llvmize.ml`
- Tests:
  - `testsuite/tests/llvm-codegen/allocation.ml`
  - `testsuite/tests/llvm-codegen/fast_path_roots.ml`
- Human-like review: 5 agents run after the expanded expect coverage. Addressed
  the actionable findings by adding 10 allocation-focused codegen cases,
  including no-root allocation, const-int filtering, allocation trap/unwind
  fallback, call-preservation interaction, boxed float roots, aliased logical
  roots, multiple allocation slow paths with varying root counts, and poll plus
  allocation slot reuse; renaming the aliased-root and call-then-allocation
  cases; and clarifying the conservative slow-path slot preallocation count.
- Focused validation passed:
  `llvm-codegen/fast_path_roots.ml`, `llvm-codegen/allocation.ml`,
  `llvm-codegen/effect_preemption.ml`,
  `typing-small-numbers/test_matching_native.ml`.
- Latest focused validation after rebase and extra tests:
  `make test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml
  LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"` passed.
- Formatting passed:
  `ocamlformat --check backend/llvm/llvmize.ml
  testsuite/tests/llvm-codegen/effect_preemption.ml
  testsuite/tests/llvm-codegen/fast_path_roots.ml`.
- Full LLVM test suite passed after the `effect_preemption.ml` predicate fix:
  `make llvm-test LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`.
- Full self-stage2 LLVM test suite passed:
  `make llvm-self-stage2-test LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`
  with 6363 passed, 271 skipped, 0 failed. The successful run used
  agent-local `RUNTIME_WS` and `MAIN_WS` paths to avoid global `/tmp`
  workspace collisions, and set `DUNE_BUILD_FLAGS="--display short"` to avoid
  the empty Bash-array `set -u` failure in the stage script.
- Clang wrapper evidence:
  - Final self-stage2 test run: 6672 wrapper lines, 3328 fresh IR compiles.

## Current Blocker

None.

## Next Step

Commit and push the completed branch state to the PR.
