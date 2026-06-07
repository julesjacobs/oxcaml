# Progress

Last updated: 2026-06-02.

## Current Claim

Validation ran on an agent branch whose parent is
`29e4cd42102c2c71c4fe140309c725f13cf8c639`.

## Evidence

- OxCaml branch: `jujacobs/test-suite-29e4cd`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/32
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/test-suite-29e4cd`
- `HEAD`: `0fb6cf718e21d48860560fb6d34d5167804965ed`
- `HEAD^`: `29e4cd42102c2c71c4fe140309c725f13cf8c639`
- Agent-local clang built at `../llvm-build/bin/clang`; wrapper at
  `../clang-wrapper`.
- Normal suite command:
  `LLVM_PATH="$PWD/../clang-wrapper" make LLVM_BOOT_BACKEND=0 LLVM_BACKEND=0 OCAMLPARAM= BUILD_OCAMLPARAM= test`
  - Result: failed, 6760 passed, 294 skipped, 20 failed, 7074 considered.
  - The failures were LLVM-codegen/GC-root tests where expectnat did not see
    `llvm-path` from `LLVM_PATH`; clang rejected
    `-oxcaml-avoid-i64-load-narrowing` because those invocations did not use
    the agent-local clang wrapper.
- Self-stage1 command:
  `STAGE0_INSTALL="$PWD/_install" LLVM_WRAPPER="$PWD/../clang-wrapper" tools/build-llvm-self-stage-install.sh`
  - Result: passed; produced `_llvm_self_stage_install`.
- Self-stage2 command:
  `STAGE0_INSTALL="$PWD/_llvm_self_stage_install" BOOT_BUILD="$PWD/_llvm_self_stage2_boot_context_build" BOOT_INSTALL="$PWD/_llvm_self_stage2_boot_install" SELF_RUNTIME_BUILD="$PWD/_llvm_self_stage2_runtime_build" SELF_MAIN_BUILD="$PWD/_llvm_self_stage2_main_build" SELF_STAGE_INSTALL="$PWD/_llvm_self_stage2_install" LLVM_WRAPPER="$PWD/../clang-wrapper" tools/build-llvm-self-stage-install.sh`
  - Result: passed; produced `_llvm_self_stage2_install`.
- Self-stage2 tests command:
  `SELF_STAGE=2 LLVM_WRAPPER="$PWD/../clang-wrapper" tools/run-llvm-stage5-ocamltest.sh`
  - Result: failed, 6746 passed, 284 skipped, 2 failed, 7032 considered.
  - Failed tests:
    - `tests/llvm-codegen/'long_frame.ml' with line 7 (script)`
    - `tests/llvm-codegen/'stack_check_size_contract.ml' with line 8 (script)`
  - Both failures were `No such file or directory` for
    `/tmp/oxcaml-clang-wrapper`; the scripts use
    `${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}` while the stage5 runner sets
    `LLVM_WRAPPER`, not `LLVM_PATH`.
- Representative microbenchmarks command:
  `PAIRS=3 LLVM_PATH="$PWD/../clang-wrapper" agent-state/test-suite-29e4cd/representative_microbenchmarks/run.sh`
  - Result: passed; summary at
    `agent-state/test-suite-29e4cd/representative_microbenchmarks/.build/summary.json`.
  - Aggregate: 44 cases, geomean LLVM/native `0.8245`, median `0.9357`,
    min `0.2600`, max `1.3784`. Lower is faster for LLVM.
  - Slowest LLVM/native ratios: `closure_call_in_nested_try_hit` `1.3784`,
    `closure_call_in_try_hit` `1.2773`, `nested_scope_lookup` `1.2436`,
    `string_equal_guarded_dispatch` `1.1659`,
    `direct_call_in_try_hit` `1.1507`.
  - Fastest LLVM/native ratios: `string_compare_short_equal_loop` `0.2600`,
    `ident_find_same_short_names` `0.3103`,
    `env_find_same_current_hit` `0.3385`,
    `string_compare_short_late_diff_loop` `0.3439`,
    `env_find_same_mini` `0.4414`.
  - Wrapper reference totals: `c_call_wrapper_refs=0`, `wrap_try_refs=0` for
    both native and LLVM assemblies.
- Reduced compiler slowdown benchmarks command:
  `PAIRS=5 LLVM_PATH="$PWD/../clang-wrapper" agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/run.sh`
  - Result: passed; summary at
    `agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/.build/summary.json`.
  - Aggregate: 6 cases, geomean LLVM/native `1.0090`, median `1.0001`.
    Lower is faster for LLVM.
  - Slowest LLVM/native ratios: `string_map_find` `1.1074`,
    `string_compare_loop` `1.0576`, `no_try_lookup_hit` `1.0034`.
  - Fastest LLVM/native ratios: `try_lookup_hit` `0.9489`,
    `try_lookup_miss` `0.9497`, `string_tree_find` `0.9967`.
- Compiler binary benchmark command:
  `python3 agent-state/test-suite-29e4cd/compiler_binary_bench/bench_compiler_binary.py --pairs 7 --out-dir agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1 --summary agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary.json --native-build _build --native-install _install --llvm-build _llvm_self_stage_main_build --llvm-install _llvm_self_stage_install`
  - Result: passed; summary at
    `agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary.json`.
  - Timed native compiler: `_install/bin/ocamlopt.opt`.
  - Timed LLVM compiler: `_llvm_self_stage_install/bin/ocamlopt.opt.real`.
  - Aggregate: 9 modules, geomean LLVM-built/native-built `1.0051`,
    median `0.9983`, min `0.9506`, max `1.0730`. Lower is faster for the
    LLVM-built compiler.
  - Slowest LLVM-built/native-built ratios: `ctype.ml` `1.0730`,
    `typecore.ml` `1.0459`, `env.ml` `1.0158`,
    `translcore.ml` `1.0111`.
  - Fastest LLVM-built/native-built ratios: `cfg_selectgen.ml` `0.9506`,
    `regalloc_irc.ml` `0.9834`, `llvmize.ml` `0.9841`,
    `typemod.ml` `0.9891`.
- Compiler benchmark rerun:
  - Reduced compiler slowdown command:
    `PAIRS=5 LLVM_PATH="$PWD/../clang-wrapper" agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/run.sh`
    - Result: passed; summary at
      `agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/.build/summary.json`.
      This overwrote the previous reduced benchmark JSON.
    - Aggregate: 6 cases, geomean LLVM/native `0.9883`, median `0.9993`.
      Lower is faster for LLVM.
    - Slowest LLVM/native ratios: `string_tree_find` `1.0588`,
      `string_compare_loop` `1.0376`, `no_try_lookup_hit` `1.0043`.
    - Fastest LLVM/native ratios: `try_lookup_hit` `0.9187`,
      `try_lookup_miss` `0.9246`, `string_map_find` `0.9943`.
  - Compiler binary command:
    `python3 agent-state/test-suite-29e4cd/compiler_binary_bench/bench_compiler_binary.py --pairs 7 --out-dir agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1 --summary agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary.json --native-build _build --native-install _install --llvm-build _llvm_self_stage_main_build --llvm-install _llvm_self_stage_install`
    - Result: passed; summary at
      `agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary.json`.
      This overwrote the previous compiler binary benchmark JSON.
    - Aggregate: 9 modules, geomean LLVM-built/native-built `0.9538`,
      median `0.9843`, min `0.7070`, max `1.1056`. Lower is faster for the
      LLVM-built compiler.
    - Slowest LLVM-built/native-built ratios: `regalloc_irc.ml` `1.1056`,
      `cfg_to_linear.ml` `1.0093`, `env.ml` `0.9936`.
    - Fastest LLVM-built/native-built ratios: `llvmize.ml` `0.7070`,
      `translcore.ml` `0.9347`, `typemod.ml` `0.9381`,
      `ctype.ml` `0.9789`.
- Compiler benchmark third run:
  - Archived previous rerun summaries before overwriting:
    `agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/.build/summary_before_run_20260602_133416.json`
    and
    `agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary_before_run_20260602_133416.json`.
  - Reduced compiler slowdown command:
    `PAIRS=5 LLVM_PATH="$PWD/../clang-wrapper" agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/run.sh`
    - Result: passed; summary at
      `agent-state/test-suite-29e4cd/compiler_slowdown_benchmarks/.build/summary.json`.
    - Aggregate: 6 cases, geomean LLVM/native `1.0024`, median `0.9840`.
      Lower is faster for LLVM.
    - Slowest LLVM/native ratios: `string_compare_loop` `1.1133`,
      `string_tree_find` `1.0455`, `no_try_lookup_hit` `0.9871`.
    - Fastest LLVM/native ratios: `try_lookup_hit` `0.9405`,
      `try_lookup_miss` `0.9571`, `string_map_find` `0.9810`.
  - Compiler binary command:
    `python3 agent-state/test-suite-29e4cd/compiler_binary_bench/bench_compiler_binary.py --pairs 7 --out-dir agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1 --summary agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary.json --native-build _build --native-install _install --llvm-build _llvm_self_stage_main_build --llvm-install _llvm_self_stage_install`
    - Result: passed; summary at
      `agent-state/test-suite-29e4cd/compiler_binary_bench/.build/stage1_summary.json`.
    - Aggregate: 9 modules, geomean LLVM-built/native-built `0.9905`,
      median `0.9834`, min `0.9594`, max `1.0748`. Lower is faster for the
      LLVM-built compiler.
    - Slowest LLVM-built/native-built ratios: `translcore.ml` `1.0748`,
      `typemod.ml` `1.0022`, `cfg_to_linear.ml` `0.9914`.
    - Fastest LLVM-built/native-built ratios: `env.ml` `0.9594`,
      `typecore.ml` `0.9653`, `cfg_selectgen.ml` `0.9750`,
      `llvmize.ml` `0.9811`.

## Current Blocker

Two self-stage2 script tests still hardcode the old fallback wrapper path and
do not consume `LLVM_WRAPPER`.

## Next Step

Fix the two test scripts to use the wrapper variable passed by the stage5
runner, then rerun the focused LLVM-codegen tests.
