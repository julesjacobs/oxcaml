# Progress

Last updated: 2026-06-10.

## Current Goal

Keep the LLVM backend self-stage2-clean, then improve runtime performance until
the LLVM-built compiler beats both native and the older LLVM baseline on total
microbench, minibench, and compiler benchmark time.

## Current Change (v3: full call homes, non-volatile slots, init-store pruning)

- Root-caused and fixed the call-statepoint homes miscompile: init stores were
  classified by BLOCK (any entry-block store was treated as initialization),
  so a consumer-NCD defining store that legitimately sits in the entry block
  below interior call statepoints (call statepoints do not split blocks) was
  skipped by `SlotDefStoreDominates`/`oxcamlRootSlotHasSingleDefiningStore`/
  `CanAliasToSlot`.  A channel in `Cmi_format.read_cmi_lazy` was homed at a
  statepoint above its defining store, the reload read the init immediate `1`,
  and SSA repair rewired the defining store to that reload.  Init stores are
  now classified by VALUE: constant operand and entry block
  (`isOxCamlRootSlotInitStore`).  Verified on the isolated cmi_format repro
  (exit 139 -> 0), the exact culprit ordinal (budget=67 skip=66), and an
  829-module object swap of the whole boot compiler.
- `-rs4gc-oxcaml-value-slot-homes` default is now `3` (invokes AND calls).
- Exception-root slot accesses are now NON-volatile by default
  (`-rs4gc-oxcaml-volatile-exnroot-slots`, default false).  Slots escape into
  statepoint gc-live bundles, so alias analysis already forbids forwarding
  across registered statepoints; RS4GC runs in `addIRPasses` before
  CodeGenPrepare/ISel, and non-volatility lets ISel drop dead reloads and
  forward between statepoints.
- Entry-block init stores are pruned when the slot's single defining store
  dominates every load and every statepoint listing the slot
  (`-rs4gc-oxcaml-prune-root-slot-init-stores`, default true).  This removes
  3 dead stores + an immediate materialization from `unify1`-shaped hot
  paths (probes had been paying them on every call).
- Static verifier added: `-rs4gc-oxcaml-verify-root-slots` checks every slot
  reload is dominated by the slot's defining store; clean across all 839
  stashed modules with the final configuration.
- Probes (medians, ratio vs native; layout-luck caveat below): boyer 0.99x
  (was 1.038x committed, 1.052x at head), `catch_failure_then_unify`
  1.256x -> ~1.08-1.11x, `nested_failed_unify` ~1.0-1.2x depending on code
  layout.  NOTE: these tiny probes swing +/-15% from function-alignment
  layout luck alone (verified: same binary +64B alignment moved
  nested_failed_unify 1.398x -> 1.025x); `-align-all-functions` was tested
  and rejected (helps one probe, hurts another).  Only
  `boyer_like_failed_unify` keeps a layout-stable residual (~1.24x).
- Validation of the final configuration: lit at the known pre-existing
  failure sets; cmi_format + 829-module swap repros clean; three fresh boots
  (homes=3, +non-volatile, +pruning) each with the stdlib.pp.ml GC-stress
  sweep s=1k..256k clean, with an always-on young-root checker runtime
  installed during the sweeps; self-stage1 clean
  (`self_stage_v5.log`).
- Stage2 is currently BLOCKED by the pre-existing bug below: the v5 stage1
  compiler hit the latent parser.pp.ml SEGV during the stage2 boot phase in
  two consecutive builds (`self_stage2_v5.log`, `self_stage2_v5_retry.log`),
  while the committed-state stage2-v4 run had passed by luck (its compiler
  crashes on the same repro at s=1k).  Re-running the exact failing command
  standalone passes at default heap params - the in-build trigger is
  layout/timing sensitive.  The full ocamltest suite was therefore run
  against the v5 STAGE1 install instead (`SELF_STAGE=1`):
  `6756 passed`, `284 skipped`, `0 failed`
  (`ocamltest_stage1_v5.log`).
- Final benchmark totals for the v5 configuration (2026-06-10, this machine):
  - Compiler module medians: LLVM/native `0.9732`, `2.75%` speedup
    (`compiler_bench_current_vs_native_20260610_002708.json`).
  - Micro (44 cases): total-time ratio `0.707`, geomean `0.658`, worst case
    `1.13` (`micro_v5_20260610.log`).
  - Minibench: total-time ratio `0.885` (`11.5%` speedup), boyer `1.012x`
    (was ~1.13x), worst case binary_trees `1.117x`
    (`minibench_v5_20260610.log`).

## Known Pre-existing Bug (discovered 2026-06-10, NOT caused by v3)

The stage2-v5 build initially failed: the stage1 compiler segfaulted
compiling `parser.pp.ml`.  Investigation showed a LATENT GC-stress
miscompile that PRE-EXISTS this work entirely:

- Deterministic repro: any LLVM-built stage1/stage2 `ocamlopt` compiling
  `parser.pp.ml` (cwd `_llvm_self_stage2_boot_build/default`) with
  `OCAMLRUNPARAM=s=1k..64k` segfaults 100%; at default heap params it is
  flaky (this is why stage2 builds usually pass and occasionally die).
- Crash: `Ident.compare` receives corrupt arguments (`0` and `1`) via
  `find_value_approximation`'s map lookup; fp-chain:
  `Closure_conversion.cont_96_350+5052` -> `classify_fields_of_block+152`
  -> `List.map` -> `compare`.
- Pre-existence proven: the committed-state stage2-v4 compiler crashes
  identically; so do stage1 builds with EVERY feature combination including
  homes=0/volatile/no-pruning; so does the BASELINE-era clang
  (`llvm-build-clean-head`, 57e9764b3c) and the pre-redesign clang
  (`llvm-build-old-rs4gc`).  The native compiler is clean at s=1k.
  This is very likely the same deep bug previously blamed on call homes via
  the `Lambda_to_flambda.cps` ordinal-304 repro.
- Module isolated: with ONLY `flambda2_from_lambda__Closure_conversion.o`
  LLVM-built (every other object native), the crash reproduces; all-native
  is clean.  Repro loop ~90s: `llstash/test-cc.sh "<extra clang flags>"`
  (stashed IR `/tmp/ccstash/flambda2_from_lambda__Closure_conversion.*.ll`,
  object swap + relink via `llstash/driver5.py <module-list-file>`, good
  tree `_native_build/main`, bad tree `_llvm_self_stage_main_build/main`).
- Ruled out: value-slot homes (crashes at homes=0), slot aliasing, lazy
  boundary loads, volatile vs non-volatile slots, init-store pruning,
  register roots at non-alloc callsites (only at realloc_stack /
  local_realloc / caml_call_gc sites, which save all regs), trap-byte
  offset mismatches in `find_value_approximation` (descriptors audited
  consistent: framesize 64 / offset 40 under active trap vs 48/24 outside),
  missed-young-root scanning (always-on checker runtime silent - though it
  shares the frametable, so it is blind to frametable holes).
- The DEBUG runtime (`-runtime-variant d`) does NOT reproduce (compile
  succeeds at s=1k) - the bug needs the stock runtime's exact allocation
  pattern.
- Remaining suspects: statepoint live-set hole or stack-slot sharing in one
  of `Closure_conversion`'s large functions, or a runtime/frametable scan
  disagreement only visible under precise minor-GC timing.
- The `parser.pp.ml` s=1k repro should be added to the standard validation
  gate once fixed; the stdlib.pp.ml sweep alone does NOT catch it.

## Previous Change (v2: consumer-NCD store placement)

- Refined the uniform root design: the slot's single defining store now sits
  at the latest point that dominates every protected invoke of the regions
  sharing the slot (nearest common dominator of the consumers), hoisted out
  of cycles while the value stays available, falling back to the definition
  when the value does not dominate that point.  This fixes a 35% regression
  on `nested_failed_unify`: store-at-def had hoisted argument slot stores to
  the function entry, taxing every call of functions like `unify1` whose
  protected region sits on a conditional path.  Shared slots migrate their
  store upward as later regions join (`EnsureDefStoreCovers`), and
  value-slot homes only apply at statepoints dominated by the store.
- Probes after the change (medians, same machine/run, ratio vs native):
  `nested_failed_unify` 1.252x (head 1.250x), `boyer_like_failed_unify`
  1.251x (head 1.228x), `catch_failure_then_unify` 1.224x (head 1.245x),
  `closure_env_in_try_hit` 1.088x (head 1.089x),
  `many_handler_live_roots_raise` 0.666x (head 0.670x), boyer 1.038x (head
  1.052x).  No bad regressions remain; boyer's win is retained.
- Debug tooling added behind flags: `-rs4gc-oxcaml-call-home-budget/-skip/
  -dump` to bisect call-statepoint homing by ordinal.  Using them, the
  call-homes failure was narrowed to a SINGLE homed call statepoint in
  `Lambda_to_flambda.cps` (skip=304 budget=305 with the llstash harness
  reproduces in ~10s); its IR, frametable record, regalloc slot sharing and
  backedge wiring all audit as consistent, so the residual bug is deeper in
  call-statepoint lowering.  Homes stay invoke-only by default.
- Validation: full RS4GC + AArch64 oxcaml lit suites at the pre-existing
  known-failure sets; fresh boot passes the stdlib.pp.ml GC-stress repro at
  s=1k/4k/64k; self-stage log
  `agent-state/test-suite-29e4cd/self_stage_uniform_roots_v4.log`.

## Previous Change

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
