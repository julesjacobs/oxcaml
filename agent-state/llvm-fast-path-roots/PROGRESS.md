# Progress

Last updated: 2026-05-25.

## Current Claim

Implemented LLVM fast-path root slots for eligible basic allocation and poll
safepoints, with expect tests covering the optimized path, const-int filtering,
and trap/unwind fallback.

## Evidence

- OxCaml branch: `jujacobs/llvm-fast-path-roots`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/14
- Initial state commit: `4fbbb1a88324`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-fast-path-roots`
- Design plan: `agent-state/llvm-fast-path-roots/PLAN.md`
- Code path: `backend/llvm/llvmize.ml`
- Tests:
  - `testsuite/tests/llvm-codegen/allocation.ml`
  - `testsuite/tests/llvm-codegen/fast_path_roots.ml`
- Human-like review: 5 agents run. Addressed the actionable findings by adding
  the trap/unwind fallback test, keeping the new test tracked, and refactoring
  duplicate slow-path GC-call emission through one helper.
- Focused validation passed:
  `llvm-codegen/fast_path_roots.ml`, `llvm-codegen/allocation.ml`,
  `typing-small-numbers/test_matching_native.ml`.
- Formatting passed:
  `ocamlformat --check backend/llvm/llvmize.ml
  testsuite/tests/llvm-codegen/allocation.ml
  testsuite/tests/llvm-codegen/fast_path_roots.ml`.
- Full LLVM test suite passed:
  `make llvm-test LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`
  with 6669 passed, 280 skipped, 0 failed.
- Full self-stage2 LLVM test suite passed:
  `make llvm-self-stage2-test LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`
  with 6643 passed, 267 skipped, 0 failed. The successful run used
  agent-local `RUNTIME_WS` and `MAIN_WS` paths to avoid global `/tmp`
  workspace collisions, and set `DUNE_BUILD_FLAGS="--display short"` to avoid
  the empty Bash-array `set -u` failure in the stage script.
- Clang wrapper evidence:
  - Full LLVM test: 5982 wrapper lines, 2983 fresh IR compiles.
  - Self-stage2: 6488 wrapper lines, 3229 fresh IR compiles.

## Current Blocker

None.

## Next Step

Commit and push the completed branch state to the PR.
