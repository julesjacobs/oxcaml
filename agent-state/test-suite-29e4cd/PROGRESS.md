# Progress

Last updated: 2026-06-07.

## Current Goal

Keep the LLVM backend self-stage2-clean, then improve runtime performance until
the LLVM-built compiler beats both native and the older LLVM baseline on total
microbench, minibench, and compiler benchmark time.

## Current Change

- Committed as `0067a378da` and currently being review-revised.
- Default mode is back to normal RS4GC/gc.relocate lowering; the global
  all-volatile-root-slot experiment is not the active path.
- `RewriteStatepointsForGC.cpp` now deduplicates equivalent OxCaml explicit
  exception root slots. This removes redundant handler-root stack slots and
  duplicate stores when multiple generated slots carry the same value.
- Review fix: dedupe now classifies explicit exception root slots from the
  actual `ExplicitRootSlots` map, not by matching `exnroot` in alloca names.
- Handler-only values for invokes with explicit exception roots are not also
  added to the normal `gc-live` set.
- GC recovery PHI root slots are marked as statepoint spill slots and use
  volatile accesses, so the statepoint frame-table lowering treats them as
  indirect roots.
- The self-stage scripts now allow explicit `LLVM_EXTRA_FLAGS` when needed and
  preserve clean native/LLVM separation.

## Evidence

- Rebuilt custom LLVM `opt` and `clang` in `../llvm-build`.
- Focused LLVM test passes:
  `../llvm-build/bin/llvm-lit -v vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-volatile-root-allocas.ll`.
- Full self-stage2 passes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage_after_filter_20260607_095720.log`.
- Current vs native totals:
  - Micro: LLVM/native `0.944033`, `5.93%` speedup.
  - Minibench: LLVM/native `0.928584`, `7.69%` speedup.
  - Compiler module medians: LLVM/native `0.977002`, `2.35%` speedup.
- Current vs old LLVM baseline at `57e9764b3c`:
  - Old micro ratio was `0.982986`.
  - Old minibench ratio was `0.932000`.
  - Old compiler module-median ratio was `0.981823`.

## Remaining Work

- Finish the code-review-revise loop on `0067a378da`; amend if no more issues
  are found.
- Remaining individual slowdowns are still worth investigating:
  `boyer_like_failed_unify` about `1.24x`,
  `catch_failure_then_unify` about `1.23x`,
  `closure_env_in_try_hit` about `1.09x`, and minibench `boyer` about `1.10x`.
- Do not resurrect the global all-volatile-root-slot mode as the default without
  fresh evidence; it regressed total micro time in the latest run.
