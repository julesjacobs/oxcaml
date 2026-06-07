# Progress

Last updated: 2026-06-07.

## Current Goal

Make the volatile-root RS4GC mode correct enough for self-stage2, then keep
optimizing until LLVM beats both native and the previous LLVM mode on micros,
minis, and compiler benchmarks.

## Current Change

- Added `-rs4gc-oxcaml-volatile-root-allocas` in
  `RewriteStatepointsForGC.cpp`.
- In that mode, OxCaml statepoints list volatile root slots in `gc-live` and do
  not emit `gc.relocate` or separate exception-root allocas.
- Fixed a correctness bug found by `array_set_young_values`: when a statepoint
  is on one predecessor of a join, later uses of a base-equivalent value must
  reload from the root slot. The pass now chooses root slots by `PointerToBase`
  and rewrites all base-equivalent SSA names after a relevant statepoint.
- Added `oxcaml-volatile-root-allocas.ll`, including the join-predecessor
  regression shape.

## Evidence

- Rebuilt custom LLVM `opt` and `clang` in `../llvm-build`.
- Focused tests pass:
  `../llvm-build/bin/llvm-lit -q vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-*.ll`.
- Full representative micros with
  `LLVM_EXTRA_FLAGS='-mllvm -rs4gc-oxcaml-volatile-root-allocas'` complete:
  44 cases, total LLVM/native ratio `0.903568`, about `10.67%` faster by total
  time. Worst case is `array_fold_tuple_sum_squares` at `1.6113x`.
- The previous wrong-code case `array_set_young_values` now passes and is
  faster than native: ratio `0.8479x`.
- Minibenches complete, but do not yet beat native overall: 10 cases, total
  LLVM/native ratio `1.015214`. Worst group is `boyer_no_exc`, `kb`, `bdd`,
  `kb_no_exc`, `raytrace`, and `boyer` around `1.25x` to `1.28x`.

## Next Steps

1. Commit the current correctness/performance improvement and review it.
2. Run self-stage2 with the fixed custom LLVM.
3. If self-stage2 fails, reduce the first failing invocation or source shape.
4. After self-stage2 passes, benchmark compiler and compare against native and
   old LLVM mode.
5. Continue optimizing remaining slowdowns, starting with
   `array_fold_tuple_sum_squares` for micros and `boyer`/`kb`/`bdd` for minis.
