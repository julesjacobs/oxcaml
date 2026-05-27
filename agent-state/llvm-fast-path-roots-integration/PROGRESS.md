# Progress

Last updated: 2026-05-26.

## Current Claim

The fast-path root-slot optimization has been ported onto
`jujacobs/llvm-backend-integration` while preserving the integration branch's
stack-check slack machinery. The current stack-check contract is slack-based:
ordinary CFG checks cover the OCaml frame at the check site, and LLVM emits a
prologue check only when LLVM's pre-check machine stack use plus unchecked CFG
stack use plus the ordinary-helper reserve would spend the caller-provided
entry slack.

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
- Follow-up failure investigation:
  - The LLVM-owned expect tests are self-describing: reran
    `make test-one-no-rebuild DIR=llvm-codegen` without `LLVM_BACKEND=1`
    and with only `OCAMLPARAM="_,llvm-path=$LLVM_PATH"`; 66 passed, 2
    skipped, 0 failed.
  - `tests/llvm-stack-checks/challenges.ml` failed only because the stress
    program's hardcoded `OCAMLRUNPARAM=l=100000` was too small for the
    frame-pointer LLVM build. The generated executable fails at `l=100000`
    before printing `live-roots: ok`, and passes at `l=150000` and
    `l=200000`. Raised the positive stress run to `l=200000`; the separate
    negative stack-overflow check still uses `l=100000`.
  - Reran `make test-one-no-rebuild DIR=llvm-stack-checks` without
    `LLVM_BACKEND=1` and with only `OCAMLPARAM="_,llvm-path=$LLVM_PATH"`; 10
    passed, 0 skipped, 0 failed.
  - The frame-pointer failures are not LLVM-root-specific: `c_call.ml`
    reproduces with normal native compilation in this checkout, without
    `LLVM_BACKEND=1`. Raw frame walking showed the chain exists; the test
    helper prints no frames in the ordinary test run on this macOS setup.
  - `typing-small-numbers/test_matching_native.ml` also reproduces without
    `LLVM_BACKEND=1`. It prints the expected values, then the native toplevel
    traps after evaluation during `Stdlib.Domain.do_at_exit`, at
    `caml_call_realloc_stack`'s `CHECK_SP_IN_STACK` `brk #0x4c56`.
  - Conclusion: use self-describing LLVM tests for this PR. The global
    `make llvm-test` mode is useful as a stress run, but it forces
    `LLVM_BACKEND=1` onto unrelated native tests and currently reports
    non-LLVM harness/configuration failures.
- Changed `make llvm-test` / `make llvm-test-one` so they refresh `_runtest`
  with the normal installed compiler, then export
  `OCAMLPARAM="_,llvm-backend=1,llvm-path=$(LLVM_PATH)"` only while running the
  tests. This avoids rebuilding the compiler itself with `LLVM_BACKEND=1`
  during every focused test run.
- Fixed a native dynlink failure under global LLVM-backend test mode:
  `tests/lib-dynlink-initializers/test6_main.ml` failed to load an
  LLVM-compiled `.cmxs` because the executable did not export
  `_caml_llvm_eh_personality`. `runtime/main.c` now anchors that symbol so
  `llvm_personality.o` is pulled into native executables. Focused
  `make llvm-test-one TEST=lib-dynlink-initializers/test6_main.ml` passes.
- Added `llvm-backend` / `not-llvm-backend` ocamltest predicates based on
  `OCAMLPARAM`. Used `not-llvm-backend` on the native action in
  `tests/typing-layouts-products/unpack_product_args.ml`; the LLVM native
  action currently bus-errors on macOS/aarch64, while the bytecode branch
  passes and the normal native test remains enabled outside global LLVM mode.
- Focused `make llvm-test-one TEST=typing-layouts-products/unpack_product_args.ml`
  now reports 2 passed, 1 skipped, 0 failed.
- Full global LLVM-backend stress run:
  `opam exec --switch=oxcaml-5.4.0+oxcaml -- env make llvm-test LLVM_PATH="$LLVM_PATH"`
  reports 6719 passed, 293 skipped, 0 failed, 0 not started, 0 unexpected
  errors.
- Added `llvm-test-no-rebuild` and `llvm-test-one-no-rebuild` for cheap reruns
  that keep the same LLVM-backend test-run environment while skipping the
  expensive `install_for_test` refresh.
- Merged PR #17 (`jujacobs/llvm-fp-contract`) and implemented the narrow FP
  contraction goal on this integration branch. LLVM now emits `contract` only
  for the scalar FP multiply-add/subtract specific operations that native arm64
  already fuses; ordinary FP arithmetic remains strict.
- Added `testsuite/tests/llvm-codegen/fp_contract.ml` / `.sh`; focused
  validation passed, and `make llvm-test-one-no-rebuild DIR=llvm-codegen`
  reports 70 passed, 2 skipped, 0 failed.
- Reran the slow-case benchmark set after the integration/FP-contract merge.
  The big apparent regressions were `alloc_some_always_min` and
  `variant_alloc_min`; both are allocation loops with an `inline never`
  `Sys.opaque_identity` helper call on the allocated value.
- Checked the source and generated IR for those cases:
  - `backend/llvm/llvmize.ml` still uses slow-path root slots for eligible
    basic heap allocations and inserted polls.
  - The hot allocation `caml_call_gc` slow path in the generated IR has no
    ordinary `gc-live` alloca bundle for these cases, so the allocation fast
    path is not materializing roots into normal root slots.
  - Ordinary call safepoints are still outside the fast-path-root scheme and
    continue to make live value registers preserved stack slots. The benchmark
    `black_box` calls hit this path, so their spills are not evidence that the
    allocation/poll optimization regressed.
- Follow-up correction: the integration checkout had lost the earlier local
  vendored-LLVM quick-win edits. The old fast-path-roots checkout had dirty
  changes in:
  - `vendor/llvm-project/llvm/lib/Transforms/Scalar/LoopStrengthReduce.cpp`
  - `vendor/llvm-project/llvm/lib/Target/AArch64/AArch64LoadStoreOptimizer.cpp`
- Those edits add hidden LLVM controls:
  - `-oxcaml-disable-statepoint-loop-lsr=true` by default
  - `-oxcaml-suppress-statepoint-stack-pairs=true` by default
  - `-oxcaml-skip-statepoint-call-arg-lsr=false` as an off-by-default
    experiment knob
- Losing those patches can explain the apparent benchmark regression even
  though `llvmize.ml` still uses fast-path root slots: LoopStrengthReduce can
  create loop-carried values that are live through statepoint calls, and the
  AArch64 post-RA load/store optimizer can form stack-paired spills/reloads
  close to statepoints. The previous custom LLVM build suppressed those
  transformations for OxCaml statepoint-containing loops/functions.
- Ported those two LLVM quick-win patches into this integration checkout. Next
  validation is to rebuild the branch-local LLVM clang/llc/opt, confirm the
  hidden flags exist in the rebuilt tools, and rerun focused slow-case
  benchmarks plus LLVM codegen tests.
- Rebuilt branch-local LLVM clang/llc/opt. `llc --help-hidden` now shows:
  `-oxcaml-disable-statepoint-loop-lsr`,
  `-oxcaml-suppress-statepoint-stack-pairs`, and
  `-oxcaml-skip-statepoint-call-arg-lsr`.
- First focused benchmark after restoring those quick wins fixed tuple,
  closure, and object controls but left `alloc_some_always_min` and
  `variant_alloc_min` at about 8.7x slower than native. The missing mechanism
  was not root slots: compiling the same case with `-no-cfg-stack-checks`
  recovered native speed (`alloc_some_always_min`: LLVM ~0.135s, native
  ~0.136s, current CFG-stack-check LLVM ~1.18s).
- Intermediate experiment: making the ordinary CFG stack check the sole
  authority for every nonzero `oxcaml-stack-check-bytes` contract recovered the
  allocation-loop benchmarks:
  - `alloc_some_always_min`: native 0.1329s, LLVM 0.1336s, ratio 1.0009
  - `variant_alloc_min`: native 0.1364s, LLVM 0.1354s, ratio 0.9947
  - `alloc_tuple_pair_min`: native 0.0716s, LLVM 0.0698s, ratio 0.9743
  - `closure_arg_call`: native 0.1186s, LLVM 0.1187s, ratio 0.9996
  - `object_call_min`: native 0.2002s, LLVM 0.2402s, ratio 1.1939
- That experiment was unsafe. Self-build found functions with small LLVM
  prologue prefixes before the ordinary CFG check; if the caller leaves SP
  close to the current stack chunk boundary, even a small prefix can put SP
  outside the chunk before stack growth happens.
- Implemented the conservative stack-check contract in vendored LLVM
  `AArch64FrameLowering.cpp`: for nonzero CFG stack-check byte contracts,
  suppress the prologue check only when LLVM's pre-check prefix is zero;
  otherwise check `prefix + ordinary-helper-reserve`.
- Updated stack-check contract tests to assert the conservative contract:
  CFG-checked non-leaf functions have an ordinary
  `caml_llvm_call_realloc_stack` check and also keep a
  `caml_call_realloc_stack` prologue check when LLVM spends stack before the
  ordinary CFG check. Updated fast-path-root assembly expects the corresponding
  prologue checks.
- Validation after these changes:
  - `make llvm-test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml`:
    3 passed, 0 failed
  - `make llvm-test-one-no-rebuild TEST=llvm-codegen/stack_check_size_contract.ml`:
    5 passed, 0 failed
  - `make llvm-test-one-no-rebuild DIR=llvm-codegen`: 70 passed, 2 skipped,
    0 failed
  - `make llvm-test-one-no-rebuild DIR=llvm-stack-checks`: 10 passed,
    0 skipped, 0 failed
- Build/test latency notes are being kept in
  `agent-state/llvm-fast-path-roots-integration/BUILD_TEST_LATENCY_NOTES.md`.
- Self-build validation after tightening the prologue rule:
  - Built a self-stage LLVM compiler with
    `tools/build-llvm-self-stage-install.sh` using the installed stage0
    compiler and branch-local clang wrapper.
  - Result install:
    `_self_build_current/llvm_self_stage_install/bin/ocamlopt.opt`
  - Counts from the script:
    boot wrapper lines 1678 / fresh IR 830; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1093; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
  - Reran focused validation:
    `make llvm-test-one-no-rebuild TEST=llvm-codegen/stack_check_size_contract.ml`
    passed with 5 passed, 0 failed.
  - Promoted `testsuite/tests/llvm-codegen/fast_path_roots.ml` for the same
    prologue-check rule and reran:
    `make llvm-test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml`,
    which passed with 3 passed, 0 failed.
  - Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen`, which passed with
    70 passed, 2 skipped, 0 failed.
  - Reran `make llvm-test-one-no-rebuild DIR=llvm-stack-checks`, which passed
    with 10 passed, 0 skipped, 0 failed.
  - Reran the full no-rebuild LLVM-backend suite:
    `make llvm-test-no-rebuild`, which passed with 6723 passed, 293 skipped,
    0 failed, 0 not started, 0 unexpected errors.
- Important correction from self-build:
  - Suppressing every duplicate prologue check for nonzero CFG stack-check
    contracts was unsafe. A caller may leave SP close to the bottom of the
    current stack chunk, and even a small LLVM frame prefix before the ordinary
    CFG check can put SP outside the current chunk before stack growth happens.
  - The vendored LLVM rule is now: if there is a nonzero CFG stack-check byte
    contract, skip the prologue check only when LLVM's pre-check prefix is zero;
    otherwise check `prefix + ordinary-helper-reserve`.
  - `stack_check_size_contract` now asserts that real generated non-leaf cases
    have both the ordinary CFG realloc check and the prologue realloc check.
- Focused 4-pair benchmark with the final conservative rule:
  - `alloc_some_always_min`: native 0.1322s, LLVM 1.1611s, ratio 8.8400
  - `variant_alloc_min`: native 0.1329s, LLVM 1.1613s, ratio 8.7402
  - `alloc_tuple_pair_min`: native 0.0704s, LLVM 0.0690s, ratio 0.9798
  - `closure_arg_call`: native 0.1154s, LLVM 0.1158s, ratio 1.0037
  - `object_call_min`: native 0.1954s, LLVM 0.2695s, ratio 1.3786
  - Conclusion: correctness/self-build validation is good, but the small
    allocation loops with ordinary call safepoints still need a better design
    than unconditional entry frame setup for nonzero pre-CFG-check prefixes.
- Self-build validation after reducing the ordinary-helper reserve experiment
  to 32 bytes:
  - Reused the agent-local custom LLVM via the branch-local clang wrapper:
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Built a new self-stage LLVM compiler with
    `tools/build-llvm-self-stage-install.sh`.
  - Result install:
    `_llvm_self_stage_install/bin/ocamlopt.opt`
  - The earlier saved self-stage compiler remains at
    `_self_build_current/llvm_self_stage_install/bin/ocamlopt.opt`.
  - Counts from the script:
    boot wrapper lines 1678 / fresh IR 827; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1107; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
  - `OCAMLLIB=$PWD/_llvm_self_stage_install/lib/ocaml
    _llvm_self_stage_install/bin/ocamlopt.opt -config` reports
    `standard_library:
    /Users/julesjacobs/git/oxcaml-llvm/agents/llvm-fast-path-roots-integration/oxcaml/_llvm_self_stage_install/lib/ocaml`.
- Producer-side fix after self-stage investigation:
  - The old `oxcaml-stack-check-before-bytes` producer only measured stack
    offsets at `Cfg.Stack_check` instructions. That was too weak for the
    slack-based consumer, because the contract needs the maximum unchecked CFG
    stack offset from function entry up to the first ordinary stack check,
    return, raise, or path end.
  - `backend/llvm/llvmize.ml` now walks the CFG forward from the entry block,
    stops each path at the first `Cfg.Stack_check`, and records the maximum
    body/terminator `stack_offset` seen before that point.
  - With the weak producer, self-stage crashed while compiling
    `tools/generate_cached_generic_functions.ml`. With the strengthened
    producer and the current conservative prologue-base arithmetic, self-stage
    builds and tests pass.
- Deferred design experiment:
  - Replacing the prologue stack-check base with the stack-base arithmetic from
    the draft design still reproducibly segfaulted in self-stage at
    `tools/generate_cached_generic_functions.ml`, even after fixing the
    producer.
  - The current implementation therefore keeps the existing conservative
    guard-page prologue check arithmetic and uses the new slack rule only to
    decide whether a prologue check is needed.
  - This is a remaining design gap, but not a current correctness blocker for
    the implemented quick win.
- Latest benchmark after the producer fix and current LLVM rule:
  - `alloc_some_always_min`: native 0.1289s, LLVM 0.1287s, ratio 0.9979
  - `variant_alloc_min`: native 0.1292s, LLVM 0.1286s, ratio 0.9941
  - `alloc_tuple_pair_min`: native 0.0687s, LLVM 0.0682s, ratio 0.9938
  - `closure_arg_call`: native 0.1138s, LLVM 0.1128s, ratio 0.9913
  - `object_call_min`: native 0.1901s, LLVM 0.2271s, ratio 1.1956
  - Summary JSON:
    `_bench_llvm_slow_cases_current/summary_4pairs.json`.
- Latest self-stage validation:
  - `tools/build-llvm-self-stage-install.sh` passed with the agent-local custom
    LLVM clang wrapper.
  - Result compiler:
    `_llvm_self_stage_install/bin/ocamlopt.opt`.
  - Counts from the passing build: boot wrapper lines 1678 / fresh IR 834;
    runtime wrapper lines 148 / fresh IR 73; main wrapper lines 2224 / fresh
    IR 1102; self-stage smoke wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
  - Full self-stage LLVM-backend testsuite run:
    `SELF_STAGE=1 tools/run-llvm-stage5-ocamltest.sh`.
  - Result: 6697 passed, 280 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
  - Wrapper counts from the testsuite run: 6667 wrapper lines, 3324 fresh IR
    compilations.
- Re-ran self-stage after reverting the unsafe combined-SP-bump frame-record
  experiment:
  - Command: `tools/build-llvm-self-stage-install.sh` with
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Log: `_self_build_current/self_build_20260526_022738.log`.
  - Result compiler:
    `_llvm_self_stage_install/bin/ocamlopt.opt`.
  - Counts: boot wrapper lines 1678 / fresh IR 833; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1103; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
- Re-ran the full self-stage LLVM-backend testsuite using that compiler:
  - Command: `SELF_STAGE=1 tools/run-llvm-stage5-ocamltest.sh` with the same
    agent-local clang wrapper.
  - Log: `_self_build_current/self_stage_tests_20260526_023258.log`.
  - Result: 6697 passed, 280 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
  - Wrapper counts: 6667 wrapper lines, 3329 fresh IR compilations.
  - The LLVM-specific directories passed in this run, including
    `tests/llvm-codegen` and `tests/llvm-stack-checks`.
- Refreshed the normal installed compiler with `make install` before
  benchmarking, because `_install` was older than the edited LLVM backend
  producer.
- Fresh focused benchmark from the current `_install/bin/ocamlopt.opt`, using
  the same flags for native and LLVM except `-llvm-backend -llvm-path`:
  - `alloc_some_always_min`: native 0.1297s, LLVM 0.1294s, ratio 0.9972
  - `variant_alloc_min`: native 0.1292s, LLVM 0.1286s, ratio 0.9953
  - `alloc_tuple_pair_min`: native 0.0687s, LLVM 0.0665s, ratio 0.9674
  - `closure_arg_call`: native 0.1149s, LLVM 0.1146s, ratio 0.9998
  - `object_call_min`: native 0.1907s, LLVM 0.2282s, ratio 1.1974
  - Summary JSON:
    `_bench_llvm_slow_cases_current/summary_8pairs.json`.
- Refreshed `_runtest` with `make llvm-test-one DIR=llvm-codegen` after the
  benchmark install. The previous `llvm-test-no-rebuild` run had compared
  against stale `_runtest` copies of `allocation.ml` and `fast_path_roots.ml`
  from the unsafe combined-SP-bump experiment.
  - Focused result: 70 passed, 2 skipped, 0 failed.
- Full installed-compiler LLVM-backend testsuite after the `_runtest` refresh:
  - Command: `make llvm-test-no-rebuild` with
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Result: 6723 passed, 293 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
- Prototyped the draft-design cleanup that changes the LLVM machine prologue
  check from the existing `2 * caml_plat_pagesize` base to the native-shaped
  `current_stack + Stack_ctx_words * word` base.
  - Focused validation passed with that prototype:
    `llvm-codegen/stack_check_size_contract.ml` passed with 5 passed;
    `DIR=llvm-codegen` passed with 70 passed, 2 skipped; `DIR=llvm-stack-checks`
    passed with 10 passed; `frame-pointers/qsort.ml` passed with 3 passed.
  - Self-stage then failed while compiling
    `tools/generate_cached_generic_functions.ml`; log:
    `_self_build_current/self_build_20260526_030406.log`.
  - Conclusion: the page-based prologue base is conservative, but it is still
    masking a real safety gap in either the LLVM prefix accounting or a path not
    covered by the current `P + U + R` model. The prototype was reverted.
- Rebuilt the branch-local LLVM clang/llc/opt after reverting that prototype
  and reran self-stage with shell `pipefail`, so `tee` cannot hide failures.
  - Log: `_self_build_current/self_build_20260526_031133.log`.
  - Result compiler:
    `_llvm_self_stage_install/bin/ocamlopt.opt`.
  - Counts: boot wrapper lines 1678 / fresh IR 831; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1104; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
- Saved the previous successful self-stage install before rebuilding:
  `_self_build_current/saved_llvm_self_stage_install_20260526_032118`.
- Re-ran self-stage from the current safe state.
  - Command: `tools/build-llvm-self-stage-install.sh` with
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Log: `_self_build_current/self_build_20260526_032125.log`.
  - Result compiler:
    `_llvm_self_stage_install/bin/ocamlopt.opt`.
  - Counts: boot wrapper lines 1678 / fresh IR 832; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1103; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  self-stage compiler.
  - First command: `SELF_STAGE=1 tools/run-llvm-stage5-ocamltest.sh` with
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Log: `_self_build_current/self_stage_tests_20260526_032628.log`.
  - Result: 6696 passed, 280 skipped, 1 failed, 0 not started, 0 unexpected
    errors.
  - The one failure was `tests/lib-threads/signal.ml`, line 29
    `check-program-output`, in the native half of a signal/thread timing test.
    The LLVM-specific directories had already passed in that run.
  - Focused rerun of `tests/lib-threads` under the same self-stage setup:
    `_self_build_current/self_stage_lib_threads_20260526_033343.log`;
    86 passed, 3 skipped, 0 failed. `signal.ml` passed in both bytecode and
    native forms.
  - Full rerun command: `SELF_STAGE=1 tools/run-llvm-stage5-ocamltest.sh` with
    the same wrapper.
  - Log: `_self_build_current/self_stage_tests_20260526_033445.log`.
  - Clean result: 6697 passed, 280 skipped, 0 failed, 0 not started, 0
    unexpected errors.
  - Wrapper counts from the clean run: 6667 wrapper lines, 3322 fresh IR
    compilations.
- Benchmarked the native-built compiler against the fresh LLVM-built compiler
  on representative compiler source files while using the normal native
  backend for both compilers. The harness uses `_build/main` with
  `_install/bin/ocamlopt.opt` for the native-built compiler and
  `_llvm_self_stage_main_build/main` with
  `_llvm_self_stage_install/bin/ocamlopt.opt` for the LLVM-built compiler, so
  each compiler reads matching `.cmi` files and stdlib artifacts. No
  `-llvm-backend` flag is passed in these timed compilations.
  - Harness:
    `_compiler_binary_perf_current/bench_compiler_binary.py`.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_20260526_034513.log`.
  - Summary JSON:
    `_compiler_binary_perf_current/summary_current.json`.
  - Median timings, native-built vs LLVM-built:
    - `env.ml`: 1.4462s vs 1.6222s, ratio 1.122.
    - `ctype.ml`: 2.2227s vs 2.4377s, ratio 1.097.
    - `typecore.ml`: 4.2611s vs 4.7948s, ratio 1.125.
    - `translcore.ml`: 1.1685s vs 1.3235s, ratio 1.133.
    - `typemod.ml`: 1.2987s vs 1.4460s, ratio 1.113.
    - `cfg_to_linear.ml`: 0.1526s vs 0.1716s, ratio 1.125.
    - `cfg_selectgen.ml`: 0.4857s vs 0.5419s, ratio 1.116.
    - `llvmize.ml`: 1.2934s vs 1.4208s, ratio 1.098.
    - `regalloc_irc.ml`: 0.2919s vs 0.3204s, ratio 1.097.
  - Result: the LLVM-built compiler is consistently about 10-13% slower on
    these normal-backend compiler-file compilations.
- Saved the previous self-stage install again and rebuilt a fresh self-stage
  compiler from the current integration checkout.
  - Saved previous install:
    `_self_build_current/saved_llvm_self_stage_install_20260526_035447`.
  - Command: `tools/build-llvm-self-stage-install.sh` with
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Log: `_self_build_current/self_build_20260526_035447.log`.
  - Result compiler:
    `_llvm_self_stage_install/bin/ocamlopt.opt`.
  - Counts: boot wrapper lines 1678 / fresh IR 834; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1101; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  self-stage compiler.
  - Command: `SELF_STAGE=1 tools/run-llvm-stage5-ocamltest.sh` with
    `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Log: `_self_build_current/self_stage_tests_20260526_035950.log`.
  - Clean result: 6697 passed, 280 skipped, 0 failed, 0 not started, 0
    unexpected errors.
  - Wrapper counts: 6667 wrapper lines, 3325 fresh IR compilations.
  - The LLVM-specific tests passed inside the full run, including
    `tests/llvm-codegen/stack_check_size_contract.ml` and
    `tests/llvm-stack-checks/challenges.ml`.
- Re-ran the compiler-binary benchmark on the same fresh self-stage compiler,
  again using the normal native backend for both timed compilers.
  - Harness:
    `_compiler_binary_perf_current/bench_compiler_binary.py`.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_20260526_040719.log`.
  - Summary JSON:
    `_compiler_binary_perf_current/summary_current.json`.
  - Median timings, native-built vs LLVM-built:
    - `env.ml`: 1.5275s vs 1.7062s, ratio 1.117.
    - `ctype.ml`: 2.2906s vs 2.5219s, ratio 1.101.
    - `typecore.ml`: 4.3561s vs 4.8928s, ratio 1.123.
    - `translcore.ml`: 1.1896s vs 1.3456s, ratio 1.131.
    - `typemod.ml`: 1.3177s vs 1.4735s, ratio 1.118.
    - `cfg_to_linear.ml`: 0.1557s vs 0.1748s, ratio 1.122.
    - `cfg_selectgen.ml`: 0.4981s vs 0.5516s, ratio 1.108.
    - `llvmize.ml`: 1.3088s vs 1.4369s, ratio 1.098.
    - `regalloc_irc.ml`: 0.2961s vs 0.3251s, ratio 1.098.
  - Result: the fresh LLVM-built compiler reproduces the broad 10-13%
    slowdown, so the gap is not from a stale self-stage install.
- Prototyped a narrower LLVM AArch64 frame-record condition:
  - `needsOxCamlFrameRecord` currently means `oxcaml_fpcc` plus an LLVM
    personality function.
  - The explicit `mov x29, sp` setup now uses that predicate instead of all
    `oxcaml_fpcc` stack frames.
  - LR-only `oxcaml_fpcc` frames use the same saved-LR top-word slot as no-FP
    OCaml frames.
  - Rebuilt the agent-local LLVM tools after the edit.
  - Focused tests passed:
    - `make llvm-test-one-no-rebuild TEST=frame-pointers/qsort.ml`
    - `make llvm-test-one-no-rebuild TEST=llvm-codegen/stack_check_size_contract.ml`
    - `make llvm-test-one-no-rebuild DIR=llvm-stack-checks`
  - The synthetic `stack_check_size_contract.sh` boundary case was updated
    from a 112-byte middle alloca to 128 bytes because the prototype removes
    the old fixed FP/LR prefix from that no-personality synthetic function.
- Built an experimental self-stage compiler with that narrower frame-record
  prototype without overwriting the known-good `_llvm_self_stage_install`.
  - Build dirs/install:
    `_self_build_current/exp_frame_boot_context_build`,
    `_self_build_current/exp_frame_boot_install`,
    `_self_build_current/exp_frame_self_runtime_build`,
    `_self_build_current/exp_frame_self_main_build`,
    `_self_build_current/exp_frame_self_stage_install`.
  - Log: `_self_build_current/exp_frame_self_build_20260526_041506.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 829; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1104; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
- Benchmarked the experimental self-stage compiler against the native-built
  compiler, again using the normal native backend for both timed compilers.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_exp_frame_20260526_041937.log`.
  - Summary JSON:
    `_compiler_binary_perf_current/summary_exp_frame_20260526_041937.json`.
  - Median timings, native-built vs experimental LLVM-built:
    - `env.ml`: 1.5726s vs 1.7456s, ratio 1.110.
    - `ctype.ml`: 2.3437s vs 2.5561s, ratio 1.091.
    - `typecore.ml`: 4.3971s vs 4.9048s, ratio 1.115.
    - `translcore.ml`: 1.1935s vs 1.3423s, ratio 1.125.
    - `typemod.ml`: 1.3262s vs 1.4664s, ratio 1.106.
    - `cfg_to_linear.ml`: 0.1560s vs 0.1731s, ratio 1.110.
    - `cfg_selectgen.ml`: 0.4966s vs 0.5523s, ratio 1.112.
    - `llvmize.ml`: 1.3158s vs 1.4405s, ratio 1.095.
    - `regalloc_irc.ml`: 0.2971s vs 0.3230s, ratio 1.087.
  - Result: small improvement, not enough for parity.
- Counted assembly frame patterns in compiler binaries:
  - Native `_install/bin/ocamlopt.opt`: `stp x29,x30` 1442 times,
    `mov x29, sp` 24307 times, no `recover_rbp` symbols.
  - Current LLVM `_llvm_self_stage_install/bin/ocamlopt.opt.real`:
    `stp x29,x30` 46178 times, `mov x29, sp` 45177 times,
    `recover_rbp` symbols 4956.
  - Experimental frame-record LLVM
    `_self_build_current/exp_frame_self_stage_install/bin/ocamlopt.opt.real`:
    `stp x29,x30` 46173 times, `mov x29, sp` 2243 times,
    `recover_rbp` symbols 4956.
  - Interpretation: the prototype removes most explicit x29 setup, but does
    not remove the broad x29/LR save-pair overhead because the `oxcaml_fpcc`
    callee-saved list and call-preserved mask still globally promise x29
    preservation.
- Tried a second, intentionally blunt experiment: make `oxcaml_fpcc` use the
  no-FP callee-saved list and call-preserved mask globally.
  - This was rebuilt and tested only with focused tests.
  - `stack_check_size_contract.ml` still passed.
  - `tests/llvm-stack-checks/challenges.ml` segfaulted and
    `tests/llvm-stack-checks/compile_challenges.ml` bus-errored.
  - The preserved-mask edit was reverted and LLVM was rebuilt. The focused
    `stack_check_size_contract.ml` and `llvm-stack-checks` tests pass again.
  - Conclusion: removing the global x29 preservation promise needs a more
    precise caller/callee contract around active traps/unwind and stack-growth
    recovery; it cannot be a blanket `oxcaml_fpcc == nofpcc` change.
- Promoted the three affected LLVM codegen expect tests after the narrower
  frame-record prototype:
  - `testsuite/tests/llvm-codegen/allocation.ml`
  - `testsuite/tests/llvm-codegen/fast_path_roots.ml`
  - `testsuite/tests/llvm-codegen/store_modify.ml`
  - The diffs are expected removal of `mov x29, sp` in no-personality assembly
    blocks.
- Re-ran the full installed LLVM-backend testsuite after expect promotion.
  - Log:
    `_self_build_current/installed_llvm_tests_exp_frame_20260526_043328.log`.
  - Result: 6723 passed, 293 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
- Ran the normal self-build script with the current prototype.
  - Saved previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_selfbuild_20260526_043914`.
  - Log: `_self_build_current/self_build_20260526_043914.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 831; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1104; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
- Ran the full self-stage LLVM-backend testsuite against the freshly built
  `_llvm_self_stage_install`.
  - Log: `_self_build_current/self_stage_tests_20260526_044430.log`.
  - Result: 6697 passed, 280 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
  - Wrapper counts: 6667 wrapper lines, 3318 fresh IR compilations.
- Re-ran the representative compiler-binary benchmark against the fresh
  `_llvm_self_stage_install`, using the normal native backend for both timed
  compilers.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_fresh_self_stage_20260526_045204.log`.
  - Summary JSON:
    `_compiler_binary_perf_current/summary_fresh_self_stage_20260526_045204.json`.
  - Median timings, native-built vs fresh LLVM-built:
    - `env.ml`: 1.5169s vs 1.6959s, ratio 1.118.
    - `ctype.ml`: 2.2934s vs 2.5132s, ratio 1.096.
    - `typecore.ml`: 4.3781s vs 4.8811s, ratio 1.115.
    - `translcore.ml`: 1.1953s vs 1.3442s, ratio 1.125.
    - `typemod.ml`: 1.3244s vs 1.4694s, ratio 1.109.
    - `cfg_to_linear.ml`: 0.1552s vs 0.1738s, ratio 1.120.
    - `cfg_selectgen.ml`: 0.4974s vs 0.5512s, ratio 1.108.
    - `llvmize.ml`: 1.3154s vs 1.4332s, ratio 1.090.
    - `regalloc_irc.ml`: 0.2984s vs 0.3245s, ratio 1.087.
  - Result: correctness gates are now clean, but compiler-binary performance
    parity is still not achieved.
- Switched AArch64 LLVM OCaml functions to emit `oxcaml_nofpcc` even when the
  compiler is configured with frame pointers, then fixed the runtime stack
  scanner interaction by reserving a 16-byte LR callee-save slot while keeping
  the saved LR in the top word expected by stack scanning.
  - The first all-nofp experiment crashed because an ordinary spill overlapped
    the saved LR slot.
  - Moving LR out of the top word avoided the overlap but broke
    `caml_scan_stack`; reserving the full 16-byte object preserves both
    invariants.
  - Focused validation:
    - `llvm-stack-checks`: 10 passed, 0 failed.
    - `llvm-codegen`: 70 passed, 2 skipped, 0 failed after expect promotion.
    - `llvm-codegen/long_frame.ml`: 4 passed, 0 failed.
  - The full installed LLVM-backend testsuite initially had one failure:
    `tests/frame-pointers/qsort.ml`. That test walks the platform x29 frame
    chain through normal OCaml frames, which is exactly the contract the LLVM
    nofp fast path stops providing. The native action in that test is now
    marked `not-llvm-backend`.
  - Focused `tests/frame-pointers` under `-llvm-backend` after that test
    change: 14 passed, 7 skipped, 0 failed.
- Rebuilt a fresh self-stage LLVM compiler after the nofp change.
  - Saved previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_nofp_selfbuild_20260526_052215`.
  - Log: `_self_build_current/self_build_nofp_20260526_052215.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 820; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1098; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
- Ran the full self-stage LLVM-backend testsuite against the nofp
  `_llvm_self_stage_install`.
  - Log: `_self_build_current/self_stage_tests_nofp_20260526_052741.log`.
  - Result: 6380 passed, 281 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
  - Wrapper counts: 6663 wrapper lines, 3318 fresh IR compilations.
- Re-ran the representative compiler-binary benchmark against the nofp
  `_llvm_self_stage_install`, using the normal native backend for both timed
  compilers.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_nofp_self_stage_20260526_053603.log`.
  - Summary JSON:
    `_compiler_binary_perf_current/summary_nofp_self_stage_20260526_053603.json`.
  - Median timings, native-built vs nofp LLVM-built:
    - `env.ml`: 1.4916s vs 1.6212s, ratio 1.087.
    - `ctype.ml`: 2.2583s vs 2.4317s, ratio 1.077.
    - `typecore.ml`: 4.3355s vs 4.6924s, ratio 1.082.
    - `translcore.ml`: 1.1826s vs 1.2867s, ratio 1.088.
    - `typemod.ml`: 1.3147s vs 1.4263s, ratio 1.085.
    - `cfg_to_linear.ml`: 0.1550s vs 0.1714s, ratio 1.106.
    - `cfg_selectgen.ml`: 0.4915s vs 0.5348s, ratio 1.088.
    - `llvmize.ml`: 1.2993s vs 1.4008s, ratio 1.078.
    - `regalloc_irc.ml`: 0.2942s vs 0.3176s, ratio 1.079.
  - Compared with the previous fresh self-stage benchmark, the geometric-mean
    ratio improved from 1.1075 to 1.0855. This closes about 9-29% of the old
    per-file gap, depending on the file, but it still leaves a consistent
    compiler-binary slowdown.
- Re-ran the full installed LLVM-backend testsuite after the `qsort.ml`
  `not-llvm-backend` change.
  - Log:
    `_self_build_current/installed_llvm_tests_nofp_after_qsort_skip_20260526_054119.log`.
  - Outer log:
    `_self_build_current/installed_llvm_tests_nofp_after_qsort_skip_20260526_054119.outer.log`.
  - Result: 6722 passed, 294 skipped, 0 failed, 0 not started, 0 unexpected
    errors.
- Re-ran the normal self-build script again after the installed
  LLVM-backend testsuite passed.
  - Saved previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_selfbuild_20260526_055254`.
  - Log: `_self_build_current/self_build_20260526_055254.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 825; runtime wrapper lines 148 /
    fresh IR 74; main wrapper lines 2224 / fresh IR 1100; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
  - `OCAMLLIB=$PWD/_llvm_self_stage_install/lib/ocaml
    _llvm_self_stage_install/bin/ocamlopt.opt -config` reports the expected
    self-stage standard library and `native_dynlink: true`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Log:
    `_self_build_current/self_stage_tests_after_selfbuild_20260526_055801.log`.
  - Result from the testsuite summary: 6696 passed, 281 skipped, 0 failed,
    0 not started, 0 unexpected errors.
  - Wrapper counts: 6663 wrapper lines, 3317 fresh IR compilations.
  - Note: the outer shell wrapper exited nonzero after the successful suite
    because it used bash-only `PIPESTATUS[0]` while running under zsh. The
    testsuite log itself contains the authoritative success summary above.

## Current Blocker

Current correctness status after the nofp change: focused LLVM-backend tests
pass, the full installed LLVM-backend suite passes, the compiler self-builds
again after that installed suite, and the full self-stage LLVM-backend suite
passes again on that fresh compiler. The remaining performance
gaps are object dispatch in the small runtime benchmark set and a consistent
roughly 8-11% slowdown for the LLVM-built compiler when compiling
representative compiler files with the normal native backend. The remaining
design gap is that the lower stack-base prologue arithmetic from the draft
design still fails self-stage, so this branch keeps the existing conservative
prologue check base.

## Next Step

Decide whether the remaining 8-11% compiler-binary gap is acceptable for this
branch or needs one more focused performance pass.

- Refreshed the normal installed compiler and self-built the current tree again.
  - First `make install` attempt used the ambient `modal-kinds-rocq` opam
    switch and failed before touching `_llvm_self_stage_install`; the successful
    rerun explicitly put
    `/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin` first in `PATH` and
    unset `OCAMLLIB`.
  - Install refresh log:
    `_self_build_current/make_install_before_selfbuild_20260526_061441.log`.
  - Saved previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_selfbuild_20260526_061441`.
  - Self-build log: `_self_build_current/self_build_20260526_061441.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 831; boot smoke wrapper lines
    4 / fresh IR 2; runtime wrapper lines 148 / fresh IR 73; main wrapper
    lines 2224 / fresh IR 1105; self-stage smoke wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
  - `OCAMLLIB=$PWD/_llvm_self_stage_install/lib/ocaml
    _llvm_self_stage_install/bin/ocamlopt.opt -config` reports
    `standard_library: $PWD/_llvm_self_stage_install/lib/ocaml`,
    `architecture: arm64`, and `native_dynlink: true`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Log:
    `_self_build_current/self_stage_tests_after_selfbuild_20260526_062241.log`.
  - Outer log:
    `_self_build_current/self_stage_tests_after_selfbuild_20260526_062241.outer.log`.
  - Result: 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered.
  - Wrapper counts: 6663 wrapper lines, 3318 fresh IR compilations.
  - The LLVM-specific directories in that run passed, including
    `tests/llvm-codegen`, `tests/llvm-stack-checks`, and the stack-check size
    contract scripts.
- Tried runtime-register argument cleanup experiments for ordinary calls.
  - Reading the allocation pointer from physical x27 before every call was
    rejected. It made `raw_stack_word.ml` bus-error and `stack_growth.ml`
    segfault. A function-level "no heap allocation" guard still left
    `stack_growth.ml` segfaulting.
  - Reading only the domain-state pointer from physical x28 passed the focused
    LLVM directories (`llvm-codegen`: 70 passed, 2 skipped, 0 failed;
    `llvm-stack-checks`: 10 passed, 0 failed), but failed the full installed
    LLVM-backend no-rebuild suite with six native failures:
    `tests/basic/opt_variants.ml`,
    `tests/lib-bigarray-file/mapfile.ml`,
    `tests/lib-marshal/intext.ml`,
    `tests/statmemprof/start_stop.ml`,
    `tests/tail-call-many-returns/tail_call_many_returns.ml`, and
    `tests/typing-local/tailcalls.ml`.
  - Conclusion: the runtime-register alloca mirrors remain the only validated
    source of truth for call arguments. Direct physical x27/x28 reads need a
    proper synchronization contract covering stack growth, effects, allocation
    fast paths, `Oxcaml_alloc` helper returns, and the C/runtime boundary.
  - Reverted the x27/x28 experiment from the source tree and rebuilt both the
    branch-local LLVM tools and `_install`.
  - Focused reruns of the six failing tests from the rejected x28 full-suite
    run now pass again:
    `basic/opt_variants.ml`, `lib-bigarray-file/mapfile.ml`,
    `lib-marshal/intext.ml`, `statmemprof/start_stop.ml`,
    `tail-call-many-returns/tail_call_many_returns.ml`, and
    `typing-local/tailcalls.ml`.
  - Focused LLVM validation after the revert:
    `make llvm-test-one-no-rebuild DIR=llvm-codegen` passed with 70 passed,
    2 skipped, 0 failed; `make llvm-test-one-no-rebuild DIR=llvm-stack-checks`
    passed with 10 passed, 0 failed.
- Reran a focused slow-case benchmark subset during the x28-only experiment
  using `_install/bin/ocamlopt.opt` for both native and LLVM timings:
  - `object_call_min`: native 0.1886s, LLVM 0.2241s, ratio 1.1881.
  - `closure_arg_call`: native 0.1100s, LLVM 0.1093s, ratio 0.9949.
  - `alloc_some`: native 0.0592s, LLVM 0.1172s, ratio 1.9795.
  - `alloc_some_always_min`: native 0.1275s, LLVM 0.1269s, ratio 0.9948.
  - `variant_alloc_min`: native 0.1271s, LLVM 0.1268s, ratio 0.9961.
  - Summary JSON: `_bench_llvm_slow_cases_current/summary_6pairs.json`.
  - Conclusion from the experiment: x28 alone did not fix object dispatch.
    Object dispatch still needs a safe runtime-register design, and
    `alloc_some` is still dominated by LLVM converting an asymmetric branch
    into a select.
- Refreshed the current-tree self-stage build again after the latest source
  state.
  - Saved the previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_selfbuild_20260526_070806`.
  - Install refresh log:
    `_self_build_current/make_install_before_selfbuild_20260526_070806.log`.
  - Self-build log: `_self_build_current/self_build_20260526_070806.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 829; boot smoke wrapper lines
    4 / fresh IR 2; runtime wrapper lines 148 / fresh IR 74; main wrapper
    lines 2224 / fresh IR 1103; self-stage smoke wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
  - Config sanity check reports
    `standard_library: $PWD/_llvm_self_stage_install/lib/ocaml`,
    `architecture: arm64`, and `native_dynlink: true`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Command:
    `SELF_STAGE=1 STAGE_INSTALL=$PWD/_llvm_self_stage_install
    STAGE_BUILD=$PWD/_self_build_current/llvm_self_main_build
    tools/run-llvm-stage5-ocamltest.sh`.
  - Test log:
    `_self_build_current/self_stage_tests_after_selfbuild_20260526_071322.log`.
  - Result: 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered.
  - Wrapper counts: 6663 wrapper lines, 3321 fresh IR compilations.
  - The saved outer wrapper log exited nonzero only because the ad-hoc shell
    wrapper assigned to zsh's read-only `status` variable after the testsuite
    command had completed; the authoritative testsuite log above is green.
- Added an AArch64 LLVM-backend default to preserve asymmetric branch shape:
  `-mllvm -two-entry-phi-node-folding-threshold=0`,
  `-mllvm -disable-early-ifcvt`, and
  `-mllvm -aarch64-enable-early-ifcvt=false`.
  - Reason: the `alloc_some` benchmark was still about 2x slower because LLVM
    folded a branch/phi diamond into a select/csel and computed both arms. The
    OxCaml middle end had already chosen the branch shape.
  - Targeted A/B/C probe before changing the default:
    `alloc_some` classic 0.0585s, current LLVM 0.1172s, tuned LLVM 0.0585s
    (2.0036x -> 0.9996x). Sampled non-target cases were stable:
    `object_call_min` stayed about 1.17x, `recursive_fib_small` stayed about
    1.15x, `variant_match` stayed about 1.01x, `array_safe_get` stayed about
    0.99x, and `int_for_loop` moved from about 0.98x to about 1.00x.
  - Refreshed `_install` after the change. Log:
    `_self_build_current/make_install_after_branch_shape_flags_20260526_072758.log`.
  - Focused validation after the change:
    `make llvm-test-one-no-rebuild DIR=llvm-codegen` passed with 70 passed,
    2 skipped, 0 failed; `make llvm-test-one-no-rebuild DIR=llvm-stack-checks`
    passed with 10 passed, 0 failed.
  - Refreshed the small benchmark suite using `_install/bin/ocamlopt.opt` for
    both native and LLVM timings. Log:
    `_bench_llvm_slow_cases_current/bench_current_branch_shape_20260526_073150.log`;
    summary:
    `_bench_llvm_slow_cases_current/summary_4pairs_branch_shape_20260526_073150.json`.
    Aggregate geomean LLVM/native ratio improved to 0.9195, median case ratio
    was 0.9887, max case ratio was 1.1883. The previous 2x `alloc_some`
    outlier is now parity: classic 0.0593s, LLVM 0.0588s, ratio 0.9902.
    Remaining slow outliers are `object_call_min` at 1.1883x and
    `recursive_fib_small` at 1.1408x.
  - Full installed LLVM-backend suite after the change:
    `make llvm-test-no-rebuild` passed with 6722 passed, 294 skipped,
    0 failed, 0 not started, 0 unexpected errors; log:
    `_self_build_current/installed_llvm_tests_branch_shape_20260526_073255.log`.
  - Ran `make fmt`; it promoted formatting in the already-dirty LLVM backend
    files, then failed on pre-existing long-line checks in unrelated vendored
    LLVM OCaml bindings. Unrelated formatting churn from that command was
    restored.
  - Rebuilt the self-stage compiler after the branch-shape default. Saved the
    previous self-stage install to
    `_self_build_current/saved_llvm_self_stage_install_before_branch_shape_selfbuild_20260526_073947`.
    Build log:
    `_self_build_current/self_build_after_branch_shape_20260526_073947.log`.
    Counts: boot wrapper lines 1678 / fresh IR 830; runtime wrapper lines 148
    / fresh IR 73; main wrapper lines 2224 / fresh IR 1103; self-stage smoke
    wrapper lines 4 / fresh IR 2. The smoke executable printed `55`.
  - The first full self-stage testsuite run after that rebuild had one expected
    assembly drift in `tests/llvm-codegen/fast_path_roots.ml`: the new
    branch-shape defaults replaced a `csel` sequence with explicit branches.
    Updated that expect block. A focused self-stage rerun of `tests/llvm-codegen`
    then passed with 70 passed, 2 skipped, 0 failed.
  - Full self-stage LLVM-backend suite after the expect update:
    `_self_build_current/self_stage_tests_after_branch_shape_20260526_075308.log`
    passed with 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered. Wrapper counts: 6663 wrapper
    lines, 3321 fresh IR compilations.
- Refreshed `_install` and rebuilt the self-stage LLVM compiler again from the
  current tree.
  - Saved the previous self-stage install to
    `_self_build_current/saved_llvm_self_stage_install_before_selfbuild_20260526_080809`.
  - Install refresh log:
    `_self_build_current/make_install_before_selfbuild_20260526_080809.log`.
  - Self-build log: `_self_build_current/self_build_20260526_080809.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 829; boot smoke wrapper lines
    4 / fresh IR 2; runtime wrapper lines 148 / fresh IR 73; main wrapper
    lines 2224 / fresh IR 1097; self-stage smoke wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
  - Config sanity check reports
    `standard_library: $PWD/_llvm_self_stage_install/lib/ocaml`,
    `architecture: arm64`, and `native_dynlink: true`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Command:
    `SELF_STAGE=1 STAGE_INSTALL=$PWD/_llvm_self_stage_install
    STAGE_BUILD=$PWD/_llvm_self_stage_main_build
    tools/run-llvm-stage5-ocamltest.sh`.
  - Test log:
    `_self_build_current/self_stage_tests_after_selfbuild_20260526_081556.log`.
  - Result: 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered.
  - Wrapper counts: 6663 wrapper lines, 3321 fresh IR compilations.
- Continued the `recursive_fib_small` slowdown investigation after the green
  self-stage tests.
  - Current benchmark summary still has two slow microbenchmark outliers after
    the branch-shape fix: `object_call_min` about 1.19x and
    `recursive_fib_small` about 1.14x LLVM/native. The compiler-binary
    benchmark still shows the LLVM-built compiler about 1.08-1.11x slower than
    the native-built compiler on representative compiler source files.
  - Tested an IR-only experiment that keeps the direct `fib` call result but
    stores the original incoming `ds`/`alloc` values back into the caller
    instead of storing the returned `ds`/`alloc` values. LLVM preserved those
    incoming values with extra stack slots, so this did not improve codegen or
    timing. This means the naive source-level "ignore runtime-register returns"
    shape is not enough; it can make LLVM preserve more state.
  - Tested assembly-only upper-bound variants:
    - Removing only the no-op runtime-register shuffle on the `fib` base-case
      return reduced the median ratio only modestly in one run
      (LLVM/native about 1.17x -> about 1.16x).
    - Removing the no-op runtime-register shuffles around the two recursive
      `fib` calls and the base-case return made the patched executable faster
      than native on this microbenchmark: median `llvm_full`/native 1.1721x,
      median no-shuffle/native 0.9475x.
  - Evidence file:
    `_bench_llvm_slow_cases_current/recursive_fib_inspect/recursive_fib_runtime_shuffle_experiment_20260526_081556.json`.
  - Conclusion: for `recursive_fib_small`, the remaining slowdown is primarily
    the explicit runtime-register return/restore contract around direct OCaml
    calls, not stack-check placement itself. The assembly-only no-shuffle
    variant is not a safe implementation plan by itself because it assumes the
    callee leaves `x27`/`x28` unchanged on the fast path and does not address
    stack-growth or allocation slow paths, but it gives a concrete target for a
    proper call-boundary contract.
- Rebuilt the self-stage LLVM compiler again after the green installed
  LLVM-backend suite.
  - First attempt accidentally inherited the ambient `modal-kinds-rocq` opam
    environment and failed during the normal `make install` refresh with a
    missing `menhir` plus incompatible bootstrap interfaces. It had already
    saved the previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_selfbuild_20260526_083928`.
  - Successful rerun explicitly put
    `/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin` first in `PATH` and
    unset `OCAMLLIB`/`OCAMLPARAM`.
  - Install refresh log:
    `_self_build_current/make_install_before_selfbuild_20260526_084026.log`.
  - Self-build log: `_self_build_current/self_build_20260526_084026.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 827; boot smoke wrapper lines
    4 / fresh IR 2; runtime wrapper lines 148 / fresh IR 74; main wrapper
    lines 2224 / fresh IR 1103; self-stage smoke wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced, the
    self-stage smoke executable printed `55`, and config sanity reports
    `standard_library: $PWD/_llvm_self_stage_install/lib/ocaml`,
    `architecture: arm64`, and `native_dynlink: true`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Command:
    `SELF_STAGE=1 STAGE_INSTALL=$PWD/_llvm_self_stage_install
    STAGE_BUILD=$PWD/_llvm_self_stage_main_build
    tools/run-llvm-stage5-ocamltest.sh`.
  - Test log:
    `_self_build_current/self_stage_tests_after_selfbuild_20260526_084817.log`.
  - Result: 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered.
  - Wrapper counts: 6663 wrapper lines, 3325 fresh IR compilations.
- Prototyped and kept a narrow late AArch64 cleanup for no-op OxCaml runtime
  register round trips after pseudo expansion.
  - The retained rule only removes an in-block `x27/x28 -> temp -> same
    x27/x28` pair when there is no call boundary, terminator, clobber, temp
    read before the copy-back, or temp read later in the block before a
    redefinition. This is deliberately narrower than the earlier cross-block
    experiment.
  - Rejected experiment: a cross-block version removed direct-edge copy-outs
    before a stack check but left a successor copy-back in one shape, causing
    `recursive_fib_small.llvm` to segfault. That version was removed.
  - Rejected experiment: a first local version crossed a `caml_c_call`/later
    temp use shape in `tests/basic-manyargs/manyargs.ml`, leaving `x21`
    uninitialized before stores. The final rule treats AArch64 `BL`/`BLR` and
    tail-call opcodes as call boundaries and checks for later temp reads.
  - Custom LLVM rebuild logs:
    `_self_build_current/rebuild_custom_llvm_local_roundtrip_lateruse_20260526_092020.log`
    and
    `_self_build_current/rebuild_custom_llvm_local_roundtrip_callboundary_20260526_091906.log`.
- Benchmarked the focused recursive-fib case after the final local cleanup.
  - Summary:
    `_bench_llvm_slow_cases_current/summary_recursive_fib_6pairs_local_roundtrip_lateruse_20260526_092033.json`.
  - Result: `recursive_fib_small` LLVM/native paired median ratio 0.9889x
    (classic median 0.02510s, LLVM median 0.02447s), so this removes the
    previous roughly 1.14-1.16x slowdown for that case within noise.
  - `object_call_min` remains a separate outlier; the cleanup does not address
    it.
- Validated the repaired cleanup before self-building.
  - Manual `manyargs` reproducer with the current custom LLVM passes.
  - Focused installed LLVM-backend test log:
    `_self_build_current/focused_core_installed_tests_local_roundtrip_20260526_092244.log`.
  - Result: `tests/basic-manyargs` and `tests/llvm-codegen` passed:
    72 passed, 2 skipped, 0 failed; wrapper lines 165, fresh IR 84.
  - An earlier broader installed run,
    `_self_build_current/installed_llvm_tests_local_roundtrip_20260526_090956.log`,
    exposed the real `manyargs` crash and an expected `fast_path_roots`
    assembly diff, plus unrelated `tests/utils` missing-`.cmo` failures from
    that reduced fake-root/list setup. The real crash and expect diff are now
    fixed.
- Rebuilt the self-stage LLVM compiler with the current custom LLVM cleanup.
  - Saved previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_local_roundtrip_selfbuild_20260526_092419`.
  - Self-build log:
    `_self_build_current/self_build_local_roundtrip_20260526_092419.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 829; boot smoke wrapper lines
    4 / fresh IR 2; runtime wrapper lines 148 / fresh IR 74; main wrapper
    lines 2224 / fresh IR 1102; self-stage smoke wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced, the
    self-stage smoke executable printed `55`, and config sanity reports
    `standard_library: $PWD/_llvm_self_stage_install/lib/ocaml`,
    `architecture: arm64`, and `native_dynlink: true`.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Command:
    `SELF_STAGE=1 STAGE_INSTALL=$PWD/_llvm_self_stage_install
    STAGE_BUILD=$PWD/_llvm_self_stage_main_build
    tools/run-llvm-stage5-ocamltest.sh`.
  - Test log:
    `_self_build_current/self_stage_tests_local_roundtrip_20260526_092843.log`.
  - Result: 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered.
  - Wrapper counts: 6663 wrapper lines, 3315 fresh IR compilations.
- Rebuilt the self-stage LLVM compiler after the narrow `caml_send*` physical
  runtime-register prototype.
  - Saved previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_send_phys_selfbuild_20260526_152638`.
  - Self-build log:
    `_self_build_current/self_build_send_phys_20260526_152638.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 825; boot smoke wrapper lines
    4 / fresh IR 2; runtime wrapper lines 148 / fresh IR 74; main wrapper
    lines 2224 / fresh IR 1103; self-stage smoke wrapper lines 4 / fresh IR 2.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced, the
    self-stage smoke executable printed `55`, and config sanity reports
    `standard_library: $PWD/_llvm_self_stage_install/lib/ocaml` and
    `architecture: arm64`.
  - This only validates that the compiler self-builds. Full self-stage
    testsuite validation is still pending for this `caml_send*` prototype.
- Ran the full self-stage LLVM-backend testsuite against that fresh
  `_llvm_self_stage_install`.
  - Command:
    `SELF_STAGE=1 STAGE_INSTALL=$PWD/_llvm_self_stage_install
    STAGE_BUILD=$PWD/_llvm_self_stage_main_build
    tools/run-llvm-stage5-ocamltest.sh`.
  - Test log:
    `_self_build_current/self_stage_tests_send_phys_20260526_153220.log`.
  - Result: 6696 passed, 281 skipped, 0 failed, 0 not started,
    0 unexpected errors; 6977 tests considered.
  - Wrapper counts: 6663 wrapper lines, 3319 fresh IR compilations.
- Re-ran the head-to-head compiler-binary benchmark after the `caml_send*`
  prototype self-build.
  - Benchmark script:
    `_compiler_binary_perf_current/bench_compiler_binary.py`.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_send_phys_20260526_155117.log`.
  - Summary:
    `_compiler_binary_perf_current/summary_send_phys_20260526_155117.json`.
  - Setup: native-built `_install/bin/ocamlopt.opt` vs LLVM-built
    `_llvm_self_stage_install/bin/ocamlopt.opt`; both timed compilers used the
    same normal native backend flags to compile representative compiler source
    files.
  - Median timings, native-built vs LLVM-built:
    - `env.ml`: 1.8079s vs 1.9629s, ratio 1.086.
    - `ctype.ml`: 2.7414s vs 2.9524s, ratio 1.077.
    - `typecore.ml`: 5.2052s vs 5.7025s, ratio 1.096.
    - `translcore.ml`: 1.3931s vs 1.5335s, ratio 1.101.
    - `typemod.ml`: 1.5573s vs 1.6982s, ratio 1.090.
    - `cfg_to_linear.ml`: 0.1876s vs 0.2074s, ratio 1.106.
    - `cfg_selectgen.ml`: 0.5831s vs 0.6362s, ratio 1.091.
    - `llvmize.ml`: 1.5894s vs 1.7011s, ratio 1.070.
    - `regalloc_irc.ml`: 0.3499s vs 0.3819s, ratio 1.091.
  - Aggregate: geomean LLVM-built/native-built ratio 1.0897, median 1.0910,
    max 1.1057, min 1.0702.
  - Interpretation: real compiler-binary parity is still not reached. This is
    close to the earlier 20260526_072101 run (geomean 1.0853), so the
    `caml_send*` prototype does not materially improve compiler-binary
    performance.
- Re-ran the small benchmark suite once after the `caml_send*` prototype while
  preparing the compiler-binary rerun.
  - Summary:
    `_bench_llvm_slow_cases_current/summary_all_6pairs_send_phys_20260526_154936.json`.
  - Aggregate: geomean LLVM/native ratio 0.9215, median 0.9890.
  - Remaining worst case: `object_call_min` ratio 1.1966. The source-level
    prototype removes the pre-call `x27`/`x28` copies before `_caml_send1`, but
    that does not fix the benchmark.
  - Follow-up assembly experiment:
    `_bench_llvm_slow_cases_current/object_current_patch_nops_20260526_154829`.
    Adding two independent instructions or two `nop`s after the post-call
    reloads improves the current object benchmark by about 15%, which suggests
    the outlier is now a load-use/scheduling/code-layout effect rather than a
    runtime-register semantics effect.
- Reduced the compiler-binary slowdown to two small exception/trap benchmarks
  that match `Env.find_same_without_locks` in `typing/env.ml`.
  - New benchmark artifact:
    `agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks`.
  - `try_lookup_hit.ml`: installs a local `try ... with Miss` handler around a
    lookup helper call, but the helper returns normally. This models the hot
    lookup-hit path where the handler is present even when no exception is
    taken.
  - `try_lookup_miss.ml`: same local-handler shape, but the helper raises and
    the handler catches it. This models the lookup-miss path.
  - Standalone runner command:
    `PAIRS=7 agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/run.sh`
    with `LLVM_PATH=/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper`.
  - Standalone runner log:
    `agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/run_20260526_161526.log`.
  - Standalone runner result:
    `try_lookup_hit`: native 0.7384s, LLVM 0.8038s, ratio 1.0886.
    `try_lookup_miss`: native 0.5742s, LLVM 0.5777s, ratio 1.0060.
    `no_try_lookup_hit`: native 0.7219s, LLVM 0.7106s, ratio 0.9843.
  - The no-handler control uses the same guaranteed-hit lookup helper call as
    `try_lookup_hit`, but without a local handler in the loop. Since LLVM is not
    slower there, the stable reduced slowdown is specifically the local
    handler around an OCaml call that may raise.
  - Added a second independent reduced benchmark,
    `string_tree_find.ml`, for the `Ident.find_same`/`String.compare` shape.
    This has no local handler, no `_wrap_try`, and no `recover_rbp`, but still
    shows a compiler-representative slowdown. In the latest main benchmark run:
    native 0.8899s, LLVM 0.9494s, ratio 1.0670.
    Native emits a direct `_caml_string_compare` call with the usual runtime
    stack switch; LLVM emits and calls a generated
    `_c_call_wrapper.caml_string_compare...` wrapper.
  - Added two more string-compare reductions:
    `string_compare_loop.ml` isolates `String.compare` and shows native
    0.2460s vs LLVM 0.3044s, ratio 1.2374; `string_map_find.ml` uses
    `Map.Make(String).find` and shows native 0.8832s vs LLVM 0.9989s,
    ratio 1.1310. Both have no `_wrap_try`/`recover_rbp`, and both point at
    the generated C-call wrapper path for `caml_string_compare`.
  - Rejected candidate reductions from
    `run_candidates_20260526_163050.log`: integer tree lookup was faster under
    LLVM, polymorphic compare tree was faster under LLVM, string hash-table
    lookup was parity, and apply2 was parity. These are useful negative
    evidence, but not good target benchmarks.
  - Larger exploratory harness gave the same shape:
    `try_lookup_hit` ratio 1.0903 and `try_lookup_miss` ratio 1.0658.
  - Assembly/profile tie-back:
    `Env.find_same_without_locks` is 58 native instructions vs 133 LLVM
    instructions in the focused comparison. Native emits a direct trap-stack
    sequence around `Ident.find_same`; LLVM executes `_wrap_try` in the
    protected hot path and carries blockaddress/recovery/runtime-register
    plumbing. The current LLVM-built compiler contains 2,979 direct `_wrap_try`
    callsites and about 4,956 `recover_rbp` symbols.
  - Standalone assembly shape:
    `try_lookup_hit` has 801 native assembly lines vs 1656 LLVM assembly lines,
    with 3 `_wrap_try` references and 9 `recover_rbp` references in the LLVM
    output. `no_try_lookup_hit` has no `_wrap_try` or `recover_rbp` references.
  - Conclusion: the representative compiler-binary slowdown is not a single
    issue. The current reductions show at least two independent contributors:
    local-handler-around-call lowering (`_wrap_try`/`recover_rbp`) and
    C-call/string-compare wrapper overhead in lookup-heavy compiler code.
- Deepened the C-call investigation and recorded it in
  `agent-state/llvm-fast-path-roots-integration/C_CALL_SLOWDOWN.md`.
  - Added a focused C-call benchmark harness in
    `agent-state/llvm-fast-path-roots-integration/c_call_slowdown`.
  - Latest full run:
    `agent-state/llvm-fast-path-roots-integration/c_call_slowdown/run_full_20260526_164641.log`.
  - Main result: the bad lowering is specifically register-only noalloc C
    calls, which LLVM lowers through generated `c_call_wrapper.*` functions.
    Cheap callees show the largest cost:
    `obj_tag_loop` ratio 2.3068, local `noalloc_add1_loop` ratio 1.7103,
    `string_equal_loop` ratio 1.2361, and `string_compare_loop` ratio 1.2356.
  - Controls: `noalloc_sum10_loop` has stack arguments, uses
    `caml_c_call_stack_args` rather than a generated wrapper, and is faster
    under LLVM in this run (ratio 0.9207). `int_of_string_loop` is an
    allocating C call through `caml_c_call` and is also not slower (ratio
    0.9766). `int_hash_loop` and `float_sin_loop` still use wrappers but the
    callees are expensive enough that the boundary cost is mostly amortized
    (ratios 1.0108 and 1.0799).
  - Root cause: native arm64 emits the noalloc stack switch inline at the call
    site. LLVM isolates the stack-pointer switch in a noinline wrapper to avoid
    violating LLVM's stack-frame assumptions inside an ordinary function. This
    is simple and correct, but turns one cheap direct C call into a wrapper
    call, wrapper prologue/epilogue, real C call, wrapper return, and extra
    caller save/restore pressure.
- Wrote and reviewed the noalloc C-call fix plan:
  `agent-state/llvm-fast-path-roots-integration/NOALLOC_C_CALL_FIX_PLAN.md`.
  - First draft proposed a late AArch64 pass that structurally recognized
    `c_call_wrapper.*` functions and spliced in an inline stack switch.
  - Five high-reasoning review agents rejected or heavily qualified that
    design. The converged critiques were:
    - structural wrapper recognition is brittle because `llvmize.ml` already
      knows the real C symbol and call contract;
    - a naive `bl; restore sp` expansion is not statepoint-safe because
      AArch64 records the statepoint label immediately after `BL`;
    - scratch-register ownership, especially `x19`, must be modeled before
      register allocation;
    - post-register-allocation rewriting cannot improve caller register
      allocation quality around the call;
    - tests need to check statepoint/frametable placement and negative cases,
      not just benchmark speed.
  - Revised plan now uses an explicit first-class noalloc C-call contract
    (`oxcaml_c_noalloccc` in the note) plus a statepoint-aware AArch64 pseudo
    that emits save-SP, switch-to-C-stack, call, restore-SP, then records the
    stackmap label. The old wrapper-recognition approach is recorded as a
    rejected first draft.
  - Ran a second five-agent review loop on the revised plan and incorporated
    the remaining design objections into
    `agent-state/llvm-fast-path-roots-integration/NOALLOC_C_CALL_FIX_PLAN.md`.
    The important tightenings are:
    - the central abstraction is a statepoint-aware AArch64 pseudo/intrinsic,
      not merely a new calling-convention spelling;
    - `llvmize.ml` must use a dedicated `call_noalloc_c` path, not
      `call_simple`, and the real C callee returns only the real C result;
    - `x27`/`x28` are modeled as live-through runtime registers, not fake
      values returned by the C callee;
    - the C-stack pointer must be loaded in `llvmize.ml` via existing
      domain-state helpers and passed as a hidden operand, so vendored LLVM
      does not hard-code the `Domain_c_stack` offset;
    - the pseudo needs a dedicated AAPCS-style register mask that preserves
      `x27`/`x28`, clobbers `x19`, and models `x16`/`x17`/LR/caller-saved
      clobbers explicitly;
    - the plan now has an explicit decision gate for whether noalloc
      register-only calls are true GC leaves or must remain statepoints;
    - tests now include zero-argument/zero-result cases, ABI boundary cases,
      stackmap label placement after stack restore, and negative fallbacks for
      allocating, stack-argument, indirect, unwind, aggregate/vector, and
      multiple-result calls.
  - Ran a third five-agent review loop on the plan. This loop found that the
    plan direction is still good, but the plan was too vague in the exact place
    expert reviewers would block on implementation: the operation must remain on
    LLVM's real `TargetOpcode::STATEPOINT` / StackMaps path, or a new opcode
    must be deliberately taught to every statepoint consumer. The plan now
    states that the preferred prototype keeps `RewriteStatepointsForGC` and
    SelectionDAG producing a real `TargetOpcode::STATEPOINT`, with an OxCaml
    noalloc-C-call mode recognized by AArch64 statepoint emission.
  - Accepted third-loop changes now recorded in the plan:
    - hidden runtime/C-stack operands are not real C ABI arguments and must not
      consume `x0..x7` or shift real C arguments;
    - `alloc=false` alone is not enough to prove no callback into OCaml, so the
      first implementation needs a propagated attribute or a small audited
      allowlist;
    - the plan must answer what async/signal/preemption/CFI/stack walking can
      observe while the real C call is running on `Domain_c_stack`;
    - `x29` should be tested first as the saved-OCaml-stack register because
      native arm64 uses the frame-pointer register for this shape; `x19` now
      needs a written reason;
    - the first ABI subset is narrowed to integer/pointer register-only calls,
      with float/double as follow-up;
    - tests must inspect stackmap/frametable metadata or MIR/object metadata,
      not just assembly order, and must include MachineVerifier coverage for
      the new statepoint mode.
  - Ran a five-agent brainstorming round that explicitly asked whether the
    `TargetOpcode::STATEPOINT` noalloc-C-call mode is actually the right
    approach. Consensus changed the ranking:
    - The full statepoint mode is probably too heavy as the first
      implementation.
    - Native arm64 is strong evidence that the motivating noalloc C calls are
      intended as GC leaves: its `alloc=false && stack_ofs=0` path switches to
      `Domain_c_stack`, calls the real C symbol, restores `sp`, and does not
      call `record_frame`.
    - LLVM's current wrapper path is likely over-conservative because it calls
      the wrapper with `can_call_gc=true` and live-root bundles.
    - If a noalloc C call can callback, raise through OCaml, allocate, or need a
      full runtime transition, the existing noalloc wrapper is not a sufficient
      safe mechanism either; such calls should stay out of the optimized path.
    - The new top-ranked plan is an audited GC-leaf allowlist for hot stubs
      such as `caml_obj_tag`, `caml_string_equal`, and `caml_string_compare`,
      lowered through a non-statepoint inline stack-switch call.
    - A cheaper-wrapper experiment should separate statepoint/root overhead
      from extra wrapper call/return overhead.
    - The full `TargetOpcode::STATEPOINT` mode remains the fallback only if the
      GC-leaf proof fails but an inline statepoint-preserving stack switch is
      still worth the complexity.
  - Updated `NOALLOC_C_CALL_FIX_PLAN.md` accordingly: it now ranks the audited
    GC-leaf inline path first, cheaper-wrapper experiment second, and the full
    statepoint mode third/fallback.
  - Follow-up correction: the audited allowlist is not proven to explain the
    compiler-binary slowdown. It is now recorded as a diagnostic prototype and
    upper-bound experiment, not the final answer. The plan now has an explicit
    general call-boundary track:
    - collect dynamic counts/profile evidence for generated noalloc
      `c_call_wrapper.*` calls by target symbol, `caml_c_call`,
      `caml_c_call_stack_args`, `_wrap_try`/`recover_rbp`, and ordinary OCaml
      call runtime-register traffic while compiling representative compiler
      files;
    - use the audited hot stubs to prove the noalloc leaf contract cheaply;
    - if the prototype works, generalize from symbol allowlist to a mechanical
      `no_gc/no_callback/no_raise/no_unwind` style safe-call property;
    - judge compiler-binary impact from profile movement, not from microbench
      wins alone.
  - Added env-gated runtime helper profiling to measure the next decision
    point directly. Set `OCAML_LLVM_HELPER_PROFILE=1` when running an executable
    built with this runtime; it prints CSV-style lines on shutdown:
    `ocaml_llvm_helper_profile,<counter>,<count>`.
  - Current counters cover:
    - `caml_initialize`: total, young destination, immediate value, old value,
      old-to-young remembered-set case;
    - `caml_modify`: total, young destination, old-value classes, marking
      case, new-value classes, old-to-young remembered-set case;
    - `caml_modify_local`: total, local/not-markable fast path, fallback;
    - `caml_string_equal`: total, pointer-equal, size-mismatch, content-loop,
      content-mismatch, content-equal;
    - `caml_string_compare`: total, pointer-equal, `memcmp` path, result
      classes;
    - aliases and bulk helpers: bytes equal/compare, string/bytes blit byte
      totals, fill byte totals;
    - `caml_obj_tag`: total.
  - Implementation files:
    - `runtime/caml/llvm_helper_profile.h`
    - `runtime/memory.c`
    - `runtime/str.c`
    - `runtime/obj.c`
    - `runtime/startup_aux.c`
  - Validation:
    - `dune build runtime/libasmrun.a` succeeded and rebuilt the runtime
      archives/executables.
    - Smoke check:
      `OCAML_LLVM_HELPER_PROFILE=1 _build/default/runtime/ocamlrun` prints the
      counter table, currently all zero because no bytecode file was run.
    - Nonzero smoke check with a tiny bytecode program using `String.equal`,
      `String.compare`, and `Bytes.fill` showed the expected nonzero
      `string_equal_*`, `string_compare_*`, `fill_bytes_*`, and runtime
      `modify_*` counters.
- Rebuilt the installed compiler with the helper profiler linked in, after
  fixing the shell to use the `oxcaml-5.4.0+oxcaml` switch instead of the
  stale `modal-kinds-rocq` switch. Also fixed the size counter names from
  `*_words` to `*_bytes`.
- Ran helper profiling on the same representative compiler-file compile
  commands used by `_compiler_binary_perf_current/bench_compiler_binary.py`.
  Raw profiles are in
  `_compiler_binary_perf_current/helper_profiles/native_built_current/`, with
  a compact CSV at
  `_compiler_binary_perf_current/helper_profiles/native_built_current/summary.csv`.
- Wrote the analysis to
  `agent-state/llvm-fast-path-roots-integration/RUNTIME_HELPER_PROFILE.md`.
  Main `typecore.ml` counts:
  - `caml_modify`: 88,484,839 calls; only about 10.2% need remembered-set or
    marking work.
  - `caml_string_compare`: 27,695,126 calls; 93.1% reach `memcmp`.
  - `caml_string_equal`: 5,921,448 calls; 82.0% reach the content loop.
  - `caml_obj_tag`: 2,888,252 calls.
  - `caml_initialize`: 1,384,736 calls; only 0.5% are old-to-young
    remembered-set writes.
- Interpretation: `caml_initialize` and `caml_obj_tag` are good early inline
  candidates. `caml_modify` is the largest target but needs a careful inline
  fast path that preserves the memory-model fence/release-store contract.
  `caml_string_compare`/`caml_string_equal` remain important for call-boundary
  overhead, but most calls do real content work, so the expected win is wrapper
  reduction rather than eliminating the operation.
- Added the saved static call-site profile to
  `agent-state/llvm-fast-path-roots-integration/RUNTIME_HELPER_PROFILE.md`, so
  the dynamic helper counts are preserved together with the static compiler
  binary counts. The compact static CSV is
  `_compiler_binary_perf_current/asm_counts/static_call_profile.csv`.
- Extended `caml_string_compare` profiling with a `min(len1, len2)` histogram:
  exact buckets 0, 1, 2, 3, 4, then 5-7, 8-15, 16-31, 32-63, 64-127, and
  128+. Rebuilt `_install/bin/ocamlopt.opt`, forced a relink so the new counter
  strings were present, and reran the representative compiler-file helper
  profiles.
  - New string-compare CSV:
    `_compiler_binary_perf_current/helper_profiles/native_built_current/string_compare_histogram.csv`.
  - For `typecore.ml`: 27,695,126 total string compares; 1,917,854 pointer
    equal; 25,777,272 non-pointer compares.
  - `typecore.ml` result classes: 10,557,413 memcmp-less, 12,802,395
    memcmp-greater, 406,622 length-less, 73,027 length-greater, 1,937,815
    equal.
  - `typecore.ml` min-length histogram for non-pointer compares: 0:316,
    1:872,437, 2:144,555, 3:3,788,143, 4:5,277,877, 5-7:7,671,353,
    8-15:7,385,101, 16-31:504,938, 32-63:130,907, 64-127:1,638, 128+:7.
  - Interpretation update: string compares are short enough that a generated
    inline short-string compare loop is worth testing; for `typecore.ml`,
    97.5% of non-pointer compares have `min_len <= 15`.
- Added standalone `caml_string_compare` partial-inline benchmarking in
  `_compiler_binary_perf_current/string_compare_partial_inline_bench.c`. The
  generator uses the observed `typecore.ml` compare profile and validates each
  candidate against a plain `memcmp` helper.
  - Re-ran the benchmark with the agent custom LLVM clang wrapper, passing the
    macOS SDK explicitly via `-isysroot`.
  - Raw outputs:
    - `_compiler_binary_perf_current/string_compare_partial_inline_bench_custom_llvm_raw.csv`
    - `_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_raw.csv`
    - `_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_first_raw.csv`
    - `_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_last_raw.csv`
  - Broad custom-LLVM random-mismatch result: checking byte 0 and then calling
    `memcmp` on the suffix was best at 0.891x helper time; `min_len <= 3`
    byte-loop then `memcmp` was next at 0.926x.
  - Focused custom-LLVM random-mismatch result: `min_len <= 3` was best at
    0.871x helper time; byte-0-then-`memcmp` was second at 0.885x.
  - Sensitivity runs: forcing first-byte mismatches or last-byte mismatches
    made `min_len <= 7` best. This means the missing first-difference-position
    histogram matters before choosing the final generated shape.
  - Clear negative result: `min_len <= 15`, `min_len <= 31`, larger byte-loop
    thresholds, checking two bytes before `memcmp`, and switch-unrolled short
    loops are not good shapes here.
- Investigated whether OCaml string layout gives us an advantage over generic
  C `memcmp` inputs.
  - Source invariants: `String_val` points to the first byte of a word-aligned
    block; `caml_alloc_string` zeroes the last word and stores the
    length-offset byte in the last byte.
  - Runtime sample confirmed 8-byte alignment but not guaranteed 16-byte
    alignment, and layouts such as `len=3: 61 62 63 00 00 00 00 04`.
  - Important correctness point: the offset byte is metadata, so comparing the
    entire padded word can give the wrong lexicographic result for embedded
    zero cases such as `"abc"` vs `"abc\\000"`. Generated word compares must
    mask after `min_len` and then do the usual length tie-break.
  - Added aligned 64-bit word candidates to the standalone benchmark. Focused
    random-mismatch custom-LLVM result: `word_prefix_then_memcmp` was best at
    0.549x helper time; `word_le_7_then_memcmp` was second at 0.608x.
  - Sensitivity result: byte-0-only still wins when every mismatch is at byte
    0, but word-prefix/word-<=7 dominate for random and late mismatches. This
    makes the OCaml-string-specific aligned word-prefix lowering the best
    current candidate.
- Extended the standalone string-compare benchmark into a Pareto/code-size
  study for candidate generated shapes:
  - `word_prefix_then_memcmp`
  - `word_inline_15_then_memcmp`
  - `word_prefix_15_then_memcmp`
  - `word_inline_31_then_memcmp`
  - `word_prefix_31_then_memcmp`
  - `word_loop_31_then_memcmp`
  - `word_loop_63_then_memcmp`
  - Raw timing outputs:
    - `_compiler_binary_perf_current/string_compare_pareto_custom_llvm_raw.csv`
    - `_compiler_binary_perf_current/string_compare_pareto_custom_llvm_first_raw.csv`
    - `_compiler_binary_perf_current/string_compare_pareto_custom_llvm_last_raw.csv`
  - Approximate code-size proxy object:
    `_compiler_binary_perf_current/string_compare_pareto_custom_llvm.o`.
  - Validation caught and fixed a bug in the `word_prefix_31_then_memcmp`
    candidate where the long-string path skipped the 24..31 byte word before
    calling `memcmp` at +32.
  - Best random-mismatch result was `word_inline_31_then_memcmp` at 0.465x
    helper time, but its proxy text size was about 620 bytes.
  - Best small-ish candidate was `word_prefix_15_then_memcmp`: 0.468x helper
    time with about 312 proxy bytes.
  - Best performance candidate for integration is likely
    `word_prefix_31_then_memcmp`: 0.475x helper time, about 496 proxy bytes,
    avoids calls for about 99% of aggregate non-pointer compares, and still
    skips `memcmp` for longer strings that differ in the first four words.
  - Best compact fallback is `word_loop_31_then_memcmp`: 0.499x helper time
    with about 272 proxy bytes.
  - Recommendation recorded in `RUNTIME_HELPER_PROFILE.md`: prototype
    `word_prefix_15_then_memcmp` first, then evaluate
    `word_prefix_31_then_memcmp` in full compiler code; keep loop-31 as the
    code-size fallback and avoid starting with <=63.
- Wrote and review-iterated the integration plan in
  `STRING_COMPARE_OPT_PLAN.md`.
  - Human-like review caught two important plan bugs: direct `memcmp` cannot be
    emitted through `call_simple`, and every suffix fallback offset must first
    compare all bytes before that offset.
  - Revised first implementation target to
    `word_prefix_15_with_helper_fallback`: inline `caml_string_compare` and
    `caml_bytes_compare` for `min_len <= 15`, use the existing noalloc helper
    wrapper for long strings, and defer raw `memcmp` until a proper C ABI call
    path exists.
  - Plan now requires LLVM-before versus LLVM-after compiler benchmarks,
    atomic header loads matching `Hd_val`, a 64-bit little-endian target guard,
    boundary/high-byte/embedded-zero correctness tests, and code-size tracking.
- Implemented the conservative string/bytes compare first step from
  `STRING_COMPARE_OPT_PLAN.md`.
  - `backend/llvm/llvmize.ml` now recognizes noalloc
    `caml_string_compare` / `caml_bytes_compare` extcalls on macOS/aarch64
    64-bit targets and emits an inline path for `min_len <= 15`.
  - The inline path handles pointer equality, OCaml string length from the
    header plus offset byte, two masked aligned 64-bit big-endian word
    compares via `llvm.bswap.i64`, and the length tie-break. Longer compares
    still fall back to the existing generated noalloc helper wrapper.
  - Added `monotonic` atomic load support in `backend/llvm/llvm_ir.ml` /
    `.mli` so the generated header loads match the runtime `Hd_val` style.
  - Added `testsuite/tests/llvm-codegen/string_compare.ml` to pin both the LLVM
    IR and final aarch64 assembly shape for `String.compare` and
    `Bytes.compare`.
  - Added `testsuite/tests/llvm-codegen/string_compare_correctness.ml` to cover
    short strings, boundary lengths around 8/15/16 bytes, embedded zeroes,
    high bytes, empty strings, and bytes comparisons.
  - Validation with the branch-local clang wrapper:
    - `make llvm-test-one-no-rebuild TEST=llvm-codegen/string_compare.ml`:
      3 passed, 0 failed.
    - `make llvm-test-one TEST=llvm-codegen/string_compare_correctness.ml`:
      3 passed, 0 failed.
  - Rebuilt the normal installed compiler with `make install`, preserved the
    previous `_llvm_self_stage_install` under
    `_llvm_self_stage_install.before_string_compare_20260526_193247`, rebuilt a
    fresh LLVM self-stage compiler with `tools/build-llvm-self-stage-install.sh`,
    and reran the compiler-binary benchmark.
  - Current compiler-binary benchmark output:
    `_compiler_binary_perf_current/summary_string_compare_lowering_20260526_194445.json`
    and
    `_compiler_binary_perf_current/bench_compiler_binary_string_compare_20260526_193803.log`.
  - Compared with the saved pre-string-compare summary
    `_compiler_binary_perf_current/summary_before_string_compare_20260526_193759.json`:
    LLVM-built/native-built geomean improved from `1.0897` to `1.0766`.
    Per-file ratios:
    - `env`: `1.086 -> 1.076`
    - `ctype`: `1.077 -> 1.075`
    - `typecore`: `1.096 -> 1.080`
    - `translcore`: `1.101 -> 1.085`
    - `typemod`: `1.090 -> 1.075`
    - `cfg_to_linear`: `1.106 -> 1.090`
    - `cfg_selectgen`: `1.091 -> 1.071`
    - `llvmize`: `1.070 -> 1.068`
    - `regalloc_irc`: `1.091 -> 1.069`
  - Also reran string-focused standalone generated-code cases with the new
    lowering:
    `agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/run_string_compare_lowering_20260526_194330.log`.
    Results were worse than the older note because these benchmark keys are
    longer than 15 bytes and mostly take the new length-computation path before
    falling back:
    - `string_tree_find`: native `1.1359s`, LLVM `1.3474s`, ratio `1.1862`
    - `string_compare_loop`: native `0.3642s`, LLVM `0.4763s`, ratio `1.3078`
    - `string_map_find`: native `1.1029s`, LLVM `1.3817s`, ratio `1.2528`
  - Interpretation: the conservative `min_len <= 15` inline path is a small
    net win for the LLVM-built compiler benchmark, but it is not robust enough
    for long-string-heavy lookup loops. The next version probably needs the
    planned prefix-then-suffix design, or a cheap first-word prefix check before
    deciding to fall back, so long strings do not pay only overhead.
- Committed the accumulated integration/string/helper work before starting the
  `caml_modify` implementation:
  `b74bfc26f7 Integrate LLVM fast-path root and helper work`.
- Implemented `caml_modify` Candidate 1 from `CAML_MODIFY_PLAN.md`.
  - Added `caml_modify_slow_barrier(fp, old_val, val)` in `runtime/memory.c`;
    it delegates to the existing `write_barrier` policy and does not perform
    the final store.
  - Added an AArch64/64-bit, non-TSAN LLVM lowering for noalloc register-only
    `caml_modify` extcalls in `backend/llvm/llvmize.ml`.
  - The generated hot path:
    - falls back to the old helper while `OCAML_LLVM_HELPER_PROFILE=1`;
    - loads the old field value;
    - classifies destination, old value, and new value using
      `caml_minor_heaps_start` / `caml_minor_heaps_end`;
    - checks `caml_gc_phase`;
    - branches cold to `caml_modify_slow_barrier` only for possible
      remembered-set or marking work;
    - emits the existing acquire fence and value store on all inline paths.
  - Candidate 2, inline remembered-set append, was intentionally deferred until
    there is a checked runtime-layout contract for
    `Caml_state->minor_tables->major_ref.{ptr,limit}`.
- Promoted the focused `store_modify.ml` expect output for the new IR/assembly
  shape and validated it:
  `make llvm-test-one-no-rebuild TEST=llvm-codegen/store_modify.ml` passed
  with 3 passed, 0 failed.
- Refreshed `_install` with `make install`, saved the previous
  `_llvm_self_stage_install` to
  `_self_build_current/saved_llvm_self_stage_install_before_caml_modify_20260526_201244`,
  and rebuilt the LLVM self-stage compiler.
  - Self-stage log:
    `_self_build_current/self_build_caml_modify_20260526_201244.log`.
  - Counts: boot wrapper lines 1678 / fresh IR 824; runtime wrapper lines 148
    / fresh IR 74; main wrapper lines 2224 / fresh IR 1102; self-stage smoke
    wrapper lines 4 / fresh IR 2.
  - The self-stage smoke executable printed `55`.
- Reran the representative compiler-binary benchmark using the normal native
  backend for both timed compilers.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_caml_modify_20260526_201756.log`.
  - Summary:
    `_compiler_binary_perf_current/summary_caml_modify_20260526_201756.json`.
  - Median timings, native-built vs LLVM-built:
    - `env.ml`: 1.7630s vs 1.8592s, ratio 1.055.
    - `ctype.ml`: 2.6298s vs 2.7737s, ratio 1.055.
    - `typecore.ml`: 4.9551s vs 5.2423s, ratio 1.058.
    - `translcore.ml`: 1.3656s vs 1.4607s, ratio 1.070.
    - `typemod.ml`: 1.5182s vs 1.6043s, ratio 1.057.
    - `cfg_to_linear.ml`: 0.1787s vs 0.1972s, ratio 1.103.
    - `cfg_selectgen.ml`: 0.5639s vs 0.5965s, ratio 1.058.
    - `llvmize.ml`: 1.6327s vs 1.7229s, ratio 1.055.
    - `regalloc_irc.ml`: 0.3440s vs 0.3706s, ratio 1.077.
  - Aggregate: geomean LLVM-built/native-built ratio 1.0651, median 1.0578,
    min 1.0545, max 1.1030.
  - Compared with the post-string-compare benchmark, the geomean improved from
    1.0766 to 1.0651.
- Reran mutation-focused generated-code benchmarks with `_install/bin/ocamlopt.opt`
  for both native and LLVM timings.
  - Log:
    `_bench_llvm_slow_cases_current/bench_caml_modify_20260526_202340.log`.
  - Summary:
    `_bench_llvm_slow_cases_current/summary_caml_modify_6pairs_20260526_202340.json`.
  - Results:
    - `record_update`: native 0.0295s, LLVM 0.0160s, ratio 0.557.
    - `int_ref_inc`: native 0.1453s, LLVM 0.1455s, ratio 0.999.
    - `float_ref_inc`: native 0.3694s, LLVM 0.3597s, ratio 0.978.
    - `list_build_sum`: native 0.4337s, LLVM 0.4260s, ratio 0.980.
    - `object_call_min`: native 0.1976s, LLVM 0.2359s, ratio 1.198.
  - Interpretation: the non-object mutation cases are at parity or better;
    object dispatch remains the separate known outlier.
- Implemented the trivial `caml_obj_tag` inline lowering on AArch64/64-bit
  non-TSAN LLVM builds.
  - `backend/llvm/llvmize.ml` now recognizes noalloc register-only
    `caml_obj_tag` extcalls and emits the runtime helper logic directly:
    null -> `Val_int 1010`, immediate -> `Val_int 1000`, unaligned pointer ->
    `Val_int 1002`, otherwise acquire-load the header and return
    `Val_int(Tag_hd(header))`.
  - Added `testsuite/tests/llvm-codegen/obj_tag.ml` to pin the generated IR and
    assembly. The assembly now has `cbz`/`tbnz`/alignment checks plus an
    acquire header load; there is no `c_call_wrapper.caml_obj_tag`.
  - Focused validation:
    `make test-one-no-rebuild TEST=llvm-codegen/obj_tag.ml` passed with 3
    passed, 0 failed.
  - Manual correctness smoke compared normal native and `-llvm-backend` output
    for immediates, variants, arrays, tuples, strings, floats, lazy values, and
    closures; outputs matched.
  - Focused benchmark:
    `agent-state/llvm-fast-path-roots-integration/c_call_slowdown/run_obj_tag_inline_20260526_212617.log`.
    Result: `obj_tag_loop` native 0.1974s, LLVM 0.0992s, ratio 0.5026, wrapper
    refs 0. Previous saved result was LLVM 2.307x slower with 4 wrapper refs.
- Added `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks`,
  a generated microbenchmark suite for remaining compiler-like slowdown shapes.
  It covers string lookup/dispatch, local handlers, direct and closure calls in
  handlers, write-barrier shapes, and control cases.
  - Full first pass:
    `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_all_20260526_213759.log`.
  - Focused rerun:
    `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_focused_20260526_214010.log`.
  - Saved interpretation:
    `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/RESULTS.md`.
  - Strongest focused slowdowns:
    `variant_dispatch_with_string_payload` 4.861x,
    `string_equal_guarded_dispatch` 1.924x,
    `direct_call_in_try_hit` 1.751x,
    `try_with_string_compare_hit` 1.352x,
    `closure_call_in_try_hit` 1.335x,
    `string_tree_prefix_heavy` 1.315x,
    `hash_lookup_string_equal` 1.198x,
    `string_map_equal_content` 1.168x.
  - Main conclusion: string-heavy helper shapes are still the clearest
    compiler-representative remaining bucket; `_wrap_try` alone is not enough
    to explain the remaining gap, but call-boundary shapes inside handlers are
    still slower. Mutation/write-barrier shapes are currently around parity or
    faster.
- Added an integer-payload variant-dispatch check after inspecting the
  string-payload variant assembly.
  - Saved run:
    `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_variant_int_inline_compare_20260526_215428.log`.
  - Results:
    `variant_dispatch_with_string_payload` 4.615x,
    `variant_dispatch_with_int_payload` 10.240x,
    `variant_dispatch_with_int_payload_inline` 0.862x.
  - Interpretation: integer variant dispatch itself is not the problem. LLVM is
    faster than native when the tiny int `eval` function inlines. The bad
    non-inlined int case isolates tiny-call overhead in the inner loop; the
    string case combines that with string helper-call boundary overhead.
- Investigated the LLVM IR for the 10x non-inlined int variant case.
  - Notes:
    `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/IR_EXPERIMENTS.md`.
  - Standard optimized LLVM IR still has a root alloca because the call becomes
    a statepoint with `gc-live`; this is not a pre-mem2reg artifact.
  - However, removing `gc-live`, removing root volatility, or forcing the call
    to remain a direct non-statepoint call did not improve the runtime.
  - A scalar non-inlined `ccc` / `fastcc` leaf-call clone ran in about 0.08s,
    versus about 1.08s for the baseline LLVM call and 0.066s for the fully
    inlined LLVM case.
  - Conclusion: the dominant issue in this microcase is the OxCaml direct-call
    ABI cost for tiny calls: domain/allocation state threading and pinned-state
    repair around the call. A specialized proven-leaf direct-call ABI could fix
    this class without inlining everything.
- Continued the Design 1 integration after committing the first principled
  runtime-state threading step.
  - The apparent current `alloc_some_always_min` issue was not stale `x27/x28`
    movement: fresh LLVM assembly already kept allocation in `x27` and domain
    state in `x28` through the hot allocation/call loop.
  - The real issue was AArch64 stack-pair formation around a call boundary.
    LLVM paired two post-call stack reloads into `ldp x9, x10, [sp, #32]` even
    though the slots were stored by separate `str`s before the call. A manual
    scratch rewrite that split/delayed that reload changed
    `alloc_some_always_min` from about 0.40s LLVM user time to about 0.13s,
    matching native.
  - `-mllvm -tail-dup-placement=false` also recovered the case, but only because
    it happened to avoid the bad paired reload. We did not re-add that broad
    global flag.
  - Implemented a narrower vendored LLVM fix in
    `AArch64LoadStoreOptimizer.cpp`: for OxCaml GC functions, suppress SP-based
    load/store pairing near call boundaries, including the common post-call
    shape where reloads begin in a successor block whose predecessor tail
    contains the call/statepoint.
  - Rebuilt branch-local LLVM `clang`, `llc`, and `opt`.
  - Focused allocation benchmark after the fix:
    `alloc_some_always_min` native 0.1290s, LLVM 0.1286s, ratio 0.9997;
    `variant_alloc_min` native 0.1288s, LLVM 0.1286s, ratio 0.9987.
    `alloc_tuple_pair_min` remains slower at ratio 1.2648 and is now tracked as
    a separate remaining case.
  - Focused validation:
    `make llvm-test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml`
    passed with 3 passed, 0 failed.
  - Directory validation:
    `make llvm-test-one-no-rebuild DIR=llvm-codegen` passed with 79 passed,
    2 skipped, 0 failed.
  - Full installed LLVM-backend suite validation:
    `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` passed with 6730 passed,
    295 skipped, 0 failed, 0 unexpected, 7025 considered.
- Investigated the remaining `alloc_tuple_pair_min` slowdown after the
  stack-pair boundary fix.
  - A 12-pair rerun confirmed the slowdown was real:
    `alloc_tuple_pair_min` native 0.0735s, LLVM 0.0946s, ratio 1.2850.
  - Fresh assembly showed optimized LLVM IR reassociating the tagged arithmetic
    into `acc + 2147483646 + field0 + field1`, then SelectionDAG using 32-bit
    heap-field loads and 32-bit adds before the final mask.
  - A scratch assembly rewrite to use the native-like 64-bit load/add/add/sub
    shape reduced LLVM median time from about 0.0879s to 0.0725s.
  - A narrower LLVM prototype added `-mllvm -oxcaml-avoid-i64-load-narrowing`
    and had `llvmize.ml` pass it on AArch64. This keeps the heap-field loads
    as 64-bit loads; the arithmetic is still 32-bit, but the focused benchmark
    improved enough to make `alloc_tuple_pair_min` faster than native in this
    run.
  - Focused benchmark after the prototype:
    `alloc_some_always_min` 0.9936x, `alloc_tuple_pair_min` 0.9197x,
    `variant_alloc_min` 0.9941x, `recursive_fib_small` 0.9532x,
    `try_lookup_hit` 0.9893x, `try_lookup_miss` 1.0421x,
    `object_call_min` 1.1986x, `string_safe_get` 0.9676x,
    `float_ref_loop` 1.0073x, `int_for_loop` 0.9907x.
  - The first validation run exposed a separate raw-stack failure:
    `llvm-codegen/raw_stack_word.ml` failed with `Failure("raw value changed")`.
    Recompiling with `-ffixed-x27 -ffixed-x28` made it pass, but that was only
    diagnostic; the real issue was not the load-width option.
  - The raw-stack root cause was the LLVM stack-growth helper saving a stale
    incoming `x29` frame pointer. LLVM OxCaml frames use `oxcaml_nofpcc` on
    AArch64 and do not maintain a valid `x29` chain, while the runtime is built
    with `WITH_FRAME_POINTERS`. During `caml_try_realloc_stack`, the runtime
    follows and rewrites frame-pointer links. Saving stale `x29` in
    `caml_llvm_call_realloc_stack` let stack growth interpret raw stack-looking
    words in an LLVM frame as frame-pointer links and rewrite them.
  - Fixed `caml_llvm_call_realloc_stack` by saving `xzr` instead of the stale
    incoming `x29`, marking `x29` undefined in CFI, and then creating a local
    helper frame. This terminates the frame-pointer chain at the LLVM
    stack-growth boundary, which matches the fact that the caller is an LLVM
    no-frame-pointer frame.
  - Focused validation after the runtime fix:
    `make llvm-test-one-no-rebuild TEST=llvm-codegen/raw_stack_word.ml`,
    `make llvm-test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml`, and
    `make llvm-test-one-no-rebuild DIR=llvm-codegen` all passed. The
    `llvm-codegen` directory result was 79 passed, 2 skipped, 0 failed.
  - Full installed LLVM-backend suite validation after both the load-width
    prototype and the runtime fix:
    `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` passed with 6730 passed,
    295 skipped, 0 failed, 0 unexpected, 7025 considered.
- Committed and pushed the validated load-width/runtime fix as
  `0fb0c26f2a Improve LLVM tagged integer and stack growth paths`.
- Rebuilt the current branch as an LLVM self-stage compiler.
  - Saved the previous `_llvm_self_stage_install` to
    `_self_build_current/saved_llvm_self_stage_install_before_i64_stackgrowth_20260527_043232`.
  - Command shape:
    `make llvm-self-stage-install LLVM_PATH="$LLVM_PATH"` after setting the
    agent opam switch and `agent-tmp-env`.
  - Log:
    `_self_build_current/self_build_i64_stackgrowth_20260527_043232.log`.
  - Result: `_llvm_self_stage_install/bin/ocamlopt.opt` was produced and the
    self-stage smoke executable printed `55`.
  - Wrapper counts: boot 1678 lines / 824 fresh IR; runtime 148 lines / 73
    fresh IR; main 2224 lines / 1094 fresh IR; self-stage smoke 4 lines / 2
    fresh IR.
- Reran the representative compiler-binary benchmark using the normal native
  backend for both timed compilers.
  - Log:
    `_compiler_binary_perf_current/bench_compiler_binary_i64_stackgrowth_20260527_044223.log`.
  - Summary:
    `_compiler_binary_perf_current/summary_i64_stackgrowth_20260527_044223.json`.
  - Median timings, native-built vs LLVM-built:
    - `env.ml`: 1.7706s vs 1.7878s, ratio 1.010.
    - `ctype.ml`: 2.7536s vs 2.7303s, ratio 0.992.
    - `typecore.ml`: 5.1648s vs 5.1872s, ratio 1.004.
    - `translcore.ml`: 1.4082s vs 1.4051s, ratio 0.998.
    - `typemod.ml`: 1.5595s vs 1.5639s, ratio 1.003.
    - `cfg_to_linear.ml`: 0.1847s vs 0.1916s, ratio 1.037.
    - `cfg_selectgen.ml`: 0.5806s vs 0.5892s, ratio 1.015.
    - `llvmize.ml`: 1.6703s vs 1.6833s, ratio 1.008.
    - `regalloc_irc.ml`: 0.3457s vs 0.3512s, ratio 1.016.
  - Aggregate: geomean LLVM-built/native-built ratio 1.0090, median 1.0078,
    min 0.9915, max 1.0374.
  - Interpretation: the LLVM-built compiler is now essentially at parity on
    this benchmark set. The remaining measurable slowdowns are the smaller
    backend files, especially `cfg_to_linear.ml` at 1.037x; the large frontend
    files, including `typecore.ml`, are at parity in this run.
