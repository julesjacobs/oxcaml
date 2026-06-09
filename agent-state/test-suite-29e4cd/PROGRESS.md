# Progress

Last updated: 2026-06-09.

## Current Goal

Keep the LLVM backend self-stage2-clean, then improve runtime performance until
the LLVM-built compiler beats both native and the older LLVM baseline on total
microbench, minibench, and compiler benchmark time.

## Current Change

- RS4GC now classifies each pre-RS4GC addrspace(1) value live over a
  safepoint into exactly two categories: handler-live values get one volatile
  root slot stored once at the value's definition (per-invoke stores remain
  only for PHIs defined on unwind edges), reloaded after each safepoint and at
  recovery entries, with the slot registered on the statepoints; everything
  else uses standard gc.relocate.  This replaces the per-role slot
  materialization that gave boyer's `rewrite_with_lemmas` three duplicate term
  slots re-stored before every protected invoke (now: 2 slots, 1 store each
  per definition, none per invoke).
- Boundary rejoins use lazy per-edge reloads of the single slot; selectors and
  recovery loads alias the underlying slot (single-defining-store slots only),
  so SSA repair does not resurrect relocates or per-invoke stores.
- Fixed a latent null `Info.StatepointToken` read in the old
  `canonicalizeExplicitRootHomesAndFilterLiveSets`; homes are now assigned per
  record from the function-wide value→slot map against the original call.
- Benchmarks (this machine, medians): boyer native `0.0854s`, old-HEAD LLVM
  `0.0933s` (1.093x), new LLVM `0.0869s` (1.018x).  `many_handler_live_roots_raise`
  0.66x vs native.  `boyer_like_failed_unify`/`catch_failure_then_unify`
  (synthetic always-raise loops) are ~4-7% slower than old HEAD because the
  old per-invoke stores fed store-to-load forwarding into the handler reload;
  accepted trade for the normal-path win.
- Lit: all oxcaml RS4GC tests pass incl. new
  `oxcaml-exception-root-single-slot-per-value.ll`; the 13 generic RS4GC and
  7 AArch64 oxcaml failures are pre-existing (identical sets at 57e9764b3c).
- The first self-stage run hit two real bugs, found with a deterministic
  reproducer (`_llvm_boot_*` boot compiler compiling `stdlib.pp.ml` under
  `OCAMLRUNPARAM=s=4k`) and an object-level bisect harness (`../llstash`,
  swaps per-module `.o` between head-clang and new-clang dune trees, re-ars
  archives, relinks, reruns the repro):
  - `Info.ExplicitRootHomes` held raw `Value*`; when a homed value was the
    result of an earlier statepoint-rewritten call, the deferred RAUW left
    the home dangling and the replacement gc.result was never SSA-repaired.
    Fixed with `WeakTrackingVH` handles.
  - Slot aliasing (selector/reload sharing a root slot) is unsound when the
    slot's defining store can re-execute during the alias's lifetime; guarded
    by `CanAliasToSlot` (defining store must dominate the alias and not be
    reachable from it).
  - After those fixes, homing live slot values at CALL statepoints (removing
    them from gc-live in favor of the slot) still corrupts the heap under GC
    pressure even though IR-level SSA repair is provably complete; the
    miscompiled module was `lambda_to_flambda` (`cps` function) and the
    failure is below the IR (call-statepoint lowering of slot-homed frames is
    suspected, possibly the CSR root map).  Homes default to INVOKE
    statepoints only (`-rs4gc-oxcaml-value-slot-homes`, 0=off 1=invokes
    2=calls 3=all); calls keep full relocation.  Investigating `=2` with the
    bisect harness is the open follow-up.
- With invokes-only homes: boyer `1.020x` vs HEAD `1.048x` on the same
  machine/run; the boot compiler passes the `s=4k` GC-stress repro at 4k/16k/
  256k minor heaps (head-clang boots pass it too; the fully-homed build did
  not).
- Self-stage validation of the invokes-only configuration: in progress.

## Previous Change

- Current branch HEAD includes `b6c22b9142` (`Enable comballoc for LLVM
  backend`).
- Default mode is back to normal RS4GC/gc.relocate lowering; the global
  all-volatile-root-slot experiment is not the active path.
- Pending validation fixes:
  - LLVM-backend statmemprof native variants for
    `discard_in_callback.ml` and `stop_start_in_callback.ml` now expect the
    `combined-f33` profile shape, matching LLVM comballoc.
  - `tools/setup-llvm-stage4-ocamltest.sh` now builds a real fake-root
    `otherlibs/systhreads` directory and links generated `threads.h` and
    `st_pthreads.h` into it when present. This fixes self-stage2 ocamltest
    compilation of `tests/lib-systhreads/swapgil.ml`.
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

- Full installed-compiler LLVM-backend tests passed after the comballoc test
  fixes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_current_install_llvm_backend_comballoc_fixed_20260608_153338.log`.
- Self-stage build using `_install` as stage 0 passed:
  log `agent-state/test-suite-29e4cd/self_stage2_comballoc_fixed_20260608_153957.log`.
- Second self-stage build using `_llvm_self_stage_install` as stage 0 passed
  and produced `_llvm_self_stage2_install`:
  log `agent-state/test-suite-29e4cd/self_stage2_second_comballoc_fixed_20260608_154430.log`.
- Full self-stage2 ocamltest rerun passed:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage2_comballoc_fixed_rerun_20260608_155852.log`.
  The previous full run hit a one-off `tests/lib-threads/signal.ml` native
  output miss; focused `tests/lib-threads` rerun and the full rerun both passed
  that test.
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
