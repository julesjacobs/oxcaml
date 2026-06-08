# Progress

Last updated: 2026-06-08.

## Current Goal

Keep the LLVM backend self-stage2-clean, then improve runtime performance until
the LLVM-built compiler beats both native and the older LLVM baseline on total
microbench, minibench, and compiler benchmark time.

## Current Change

- Current branch HEAD includes `dbffad1ba3` (`Model raise_notrace trap edges in
  LLVM`).
- Default mode is back to normal RS4GC/gc.relocate lowering; the global
  all-volatile-root-slot experiment is not the active path.
- `685d252ac0` was unsafe: the exception-root merge/filtering change let the
  LLVM self-stage compiler segfault while compiling `stdlib/bytes.ml`.
- `9f38c181d9` reverts the unsafe LLVM source/test changes from `685d252ac0`,
  while keeping the useful self-stage script fixes.
- The self-stage scripts now allow explicit `LLVM_EXTRA_FLAGS` when needed and
  preserve clean native/LLVM separation.
- Boyer remains a useful slowdown case. A fresh run showed native median
  `0.08898s`, LLVM median `0.09533s`, ratio `1.0714x`.
- In `rewrite_with_lemmas`, pre-RS4GC has one source value `%2` (`term`) live
  through several handler roles: returned when lemmas are exhausted and reused
  when caught `Unify` retries the loop. RS4GC materializes those roles as
  separate exception-root slots (`%exnroot`, `%exnroot124`, and related PHI
  roots), so the first protected call stores the same `%2`-derived value into
  multiple volatile exnroots. This is a conservative artifact of the current
  handler-boundary materialization, not a distinct source value.

## Evidence

- Rebuilt custom LLVM `opt` and `clang` in `../llvm-build`.
- Focused LLVM tests pass:
  `oxcaml-volatile-root-allocas.ll`,
  `oxcaml-self-base-phi-exception-root.ll`, and
  `oxcaml-statepoint-stable-phi-root-home.ll`.
- Full self-stage2 now passes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage_after_rollback_rerun_20260607_105819.log`.
- Current vs native totals after the rollback:
  - Micro: LLVM/native `0.971072`, `2.98%` speedup,
    log `agent-state/test-suite-29e4cd/micro_after_rollback_20260607_110522.log`.
  - Minibench: LLVM/native `0.927656`, `7.80%` speedup,
    results `agent-state/test-suite-29e4cd/minibench_after_rollback_results.json`.
  - Compiler module medians: LLVM/native `0.965285`, `3.60%` speedup,
    results `agent-state/test-suite-29e4cd/compiler_bench_current_vs_native_20260607_110910.json`.
- This beats the saved old LLVM baseline at `57e9764b3c` on all three totals:
  - Old micro ratio was `0.982986` (`1.73%` speedup).
  - Old minibench ratio was `0.932000` (`7.30%` speedup).
  - Old compiler module-median ratio was `0.981823` (`1.85%` speedup).
- Code-review-revise loop on the net improvement stack:
  - `git diff --check 57e9764b3c..HEAD` passed.
  - Focused LLVM tests above passed again.
  - Reviewed stable root home lowering, statepoint slot allocation, regmask
    call-splitting default, and the inactive volatile-root mode. No source fix
    was needed after the rollback.

## Follow-up Opportunities

- Individual slowdowns are still worth investigating:
  `closure_env_in_try_hit` about `1.29x`,
  `closure_env_in_try_no_raise` about `1.25x`,
  `catch_failure_then_unify` about `1.24x`,
  `boyer_like_failed_unify` about `1.23x`, and minibench `boyer` about `1.13x`.
- Do not resurrect the global all-volatile-root-slot mode as the default without
  fresh evidence; it regressed total micro time in the latest run.
