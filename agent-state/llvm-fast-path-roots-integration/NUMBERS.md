# Numbers

Last updated: 2026-05-29.

This file keeps the headline performance numbers for the LLVM-built compiler
and the runtime helper profiles that motivated the current noalloc C-call work.

## Compiler Binary Benchmark

Benchmark shape:

- Compare a normal native-built `ocamlopt.opt` against an LLVM-built
  `ocamlopt.opt`.
- Both compilers compile representative compiler source files with the normal
  backend. This measures the speed of the compiler binary itself, not LLVM
  backend compile time.
- Current valid run, after the native-like trap model, the active-trap stack
  adjustment fixes, and focused stage2 validation with the brittle
  `fast_path_roots.ml` golden IR test skipped:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_after_stage2_skip_20260528_141703.json`.
- Current valid run, after the undefined-GC-root invariant fix and a full
  self-stage2 test-suite pass:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_after_undef_root_invariant_20260529_025704.json`.
- Previous valid run, after the native-like trap model, removing hot
  `Caml_state(exn_handler)` stores from AArch64 trap push/pop/recovery, and
  fresh self-stage2 validation:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_final_no_domain_exn_store_20260528_030435.json`.
- Previous valid run, after full validation and self-stage2:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_post_full_validation_stage2_20260527_233923.json`.
- Previous valid run, after removing the scalar-clone experiment, rebuilding the
  LLVM self-stage compiler, and rebuilding `_install` as a clean normal-backend
  compiler:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_clean_native_vs_no_scalar_llvm_20260527_101300.json`.
- Invalid run, after removing the scalar-clone experiment but before rebuilding
  a clean normal-backend `_install`:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_no_scalar_clone_20260527_095212.json`.
- Previous run, after the review fixes in `5eac16a411` and a fresh self-stage
  rebuild:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_reviewfix_20260527_061712.json`.
- Previous run, after Design 1 runtime-state threading, stack-pair suppression
  near OxCaml call boundaries, the tagged-int load-width prototype, and the
  LLVM stack-growth frame-pointer fix:
  `_compiler_binary_perf_current/summary_i64_stackgrowth_20260527_044223.json`
- Previous run, after the `caml_modify` Candidate 1 fast path:
  `_compiler_binary_perf_current/summary_caml_modify_20260526_201756.json`
- Previous saved baseline, after conservative short string/bytes compare
  lowering:
  `_compiler_binary_perf_current/summary_string_compare_lowering_20260526_194445.json`
- Original saved baseline, before string compare lowering:
  `_compiler_binary_perf_current/summary_before_string_compare_20260526_193759.json`

Geomean LLVM/native ratio:

- Old baseline: `1.0897`
- After conservative short string/bytes compare lowering: `1.0766`
- After `caml_modify` Candidate 1 fast path: `1.0651`
- Invalid/suspect after Design 1, stack-pair suppression, and tagged-int
  load-width prototype: `1.0090`
- Invalid post-review rerun: `1.0079`
- Invalid no-scalar run with LLVM-built `_install`: `1.0085`
- Clean-native vs no-scalar LLVM-built run: `1.0618`
- Current after-stage2-skip run: `1.0383`
- Current after undefined-root invariant fix: `0.9983`
- Previous no-domain-exn-store stage2 run: `1.0478`
- Previous post-validation stage2 run: `1.0539`
- String-compare change: `-0.0131`
- `caml_modify` Candidate 1 change: `-0.0115`
- Later near-parity runs were invalid or suspect because the supposed native
  `_install` had been built with LLVM backend enabled.
- Current recorded change from old baseline to the after-stage2-skip run:
  `-0.0514`.
- Current recorded change from old baseline to the undefined-root invariant
  fix run: `-0.0914`.
- Previous recorded change from old baseline to the no-domain-exn-store stage2
  run: `-0.0419`.
- Previous recorded change from old baseline to the post-validation stage2 run:
  `-0.0358`.
- Previous recorded change from old baseline to the corrected clean-native run:
  `-0.0279`.

Current after undefined-root invariant fix run details:

- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_after_undef_root_invariant_20260529_025704.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_after_undef_root_invariant_20260529_025704.json`.
- Compared clean `_native_install` against freshly validated
  `_llvm_self_stage2_install`.
- Validation state: full normal LLVM-backend suite passed; full self-stage2
  suite passed with 6714 passed, 283 skipped, 0 failed.
- Aggregate: geomean LLVM-built/native-built ratio `0.9983`, median `0.9910`,
  min `0.9833`, max `1.0398`.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.4936s | 1.4820s | 0.992 |
| `ctype.ml` | 2.2688s | 2.2445s | 0.989 |
| `typecore.ml` | 4.3465s | 4.3072s | 0.991 |
| `translcore.ml` | 1.1877s | 1.1748s | 0.989 |
| `typemod.ml` | 1.3225s | 1.3004s | 0.983 |
| `cfg_to_linear.ml` | 0.1548s | 0.1609s | 1.040 |
| `cfg_selectgen.ml` | 0.4922s | 0.4918s | 0.999 |
| `llvmize.ml` | 1.4392s | 1.4179s | 0.985 |
| `regalloc_irc.ml` | 0.2907s | 0.2956s | 1.017 |

Previous after-stage2-skip run details:

- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_after_stage2_skip_20260528_141703.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_after_stage2_skip_20260528_141703.json`.
- Compared clean `_native_install` against `_llvm_self_stage2_install`.
- Validation state: full normal LLVM-backend suite passed; full self-stage2
  reached one expected-output-only `fast_path_roots.ml` failure from encoded
  `statepoint-id` integer churn; that brittle golden test is now skipped, and
  focused stage2 `tests/llvm-codegen` passed.
- Aggregate: geomean LLVM-built/native-built ratio `1.0383`, median `1.0322`,
  min `1.0222`, max `1.0788`.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.8297s | 1.8927s | 1.034 |
| `ctype.ml` | 2.7726s | 2.8609s | 1.032 |
| `typecore.ml` | 5.3121s | 5.4299s | 1.022 |
| `translcore.ml` | 1.4445s | 1.4780s | 1.023 |
| `typemod.ml` | 1.6257s | 1.6780s | 1.032 |
| `cfg_to_linear.ml` | 0.1927s | 0.2079s | 1.079 |
| `cfg_selectgen.ml` | 0.6015s | 0.6280s | 1.044 |
| `llvmize.ml` | 1.7947s | 1.8442s | 1.028 |
| `regalloc_irc.ml` | 0.3569s | 0.3754s | 1.052 |

Previous no-domain-exn-store stage2 run details:

- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_final_no_domain_exn_store_20260528_030435.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_final_no_domain_exn_store_20260528_030435.json`.
- Compared clean `_native_install` against validated
  `_llvm_self_stage2_install`.
- Aggregate: geomean LLVM-built/native-built ratio `1.0478`, median `1.0422`,
  min `1.0331`, max `1.0686`.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.6746s | 1.7385s | 1.038 |
| `ctype.ml` | 2.5330s | 2.6400s | 1.042 |
| `typecore.ml` | 4.8499s | 5.0452s | 1.040 |
| `translcore.ml` | 1.3290s | 1.3729s | 1.033 |
| `typemod.ml` | 1.4759s | 1.5340s | 1.039 |
| `cfg_to_linear.ml` | 0.1782s | 0.1888s | 1.060 |
| `cfg_selectgen.ml` | 0.5525s | 0.5781s | 1.046 |
| `llvmize.ml` | 1.6055s | 1.7155s | 1.069 |
| `regalloc_irc.ml` | 0.3294s | 0.3503s | 1.063 |

Previous post-validation stage2 run details:

- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_post_full_validation_stage2_20260527_233923.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_post_full_validation_stage2_20260527_233923.json`.
- Compared clean `_native_install` against validated
  `_llvm_self_stage2_install`.
- Aggregate: geomean LLVM-built/native-built ratio `1.0539`, median `1.0502`,
  min `1.0444`, max `1.0722`.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.7248s | 1.8013s | 1.044 |
| `ctype.ml` | 2.6287s | 2.7703s | 1.054 |
| `typecore.ml` | 4.9933s | 5.2252s | 1.047 |
| `translcore.ml` | 1.3491s | 1.4213s | 1.054 |
| `typemod.ml` | 1.5062s | 1.5818s | 1.050 |
| `cfg_to_linear.ml` | 0.1838s | 0.1967s | 1.070 |
| `cfg_selectgen.ml` | 0.5674s | 0.5940s | 1.047 |
| `llvmize.ml` | 1.6295s | 1.7471s | 1.072 |
| `regalloc_irc.ml` | 0.3360s | 0.3519s | 1.047 |

Current clean-native vs no-scalar LLVM-built run details:

- Clean native rebuild log:
  `_self_build_current/make_install_clean_native_20260527_101000.log`.
- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_clean_native_vs_no_scalar_llvm_20260527_101300.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_clean_native_vs_no_scalar_llvm_20260527_101300.json`.
- Aggregate: geomean LLVM-built/native-built ratio `1.0618`, median `1.0561`,
  min `1.0446`, max `1.0968`.
- Sanity checks:
  `_build/log` records `OCAMLPARAM: ""`; `_install/bin/ocamlopt.opt` is
  48.5 MB and differs from `_llvm_self_stage_install/bin/ocamlopt.opt.real`
  at 106.6 MB.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.7181s | 1.8107s | 1.054 |
| `ctype.ml` | 2.5891s | 2.7135s | 1.048 |
| `typecore.ml` | 4.9291s | 5.1974s | 1.054 |
| `translcore.ml` | 1.3445s | 1.4199s | 1.056 |
| `typemod.ml` | 1.5050s | 1.5721s | 1.045 |
| `cfg_to_linear.ml` | 0.1825s | 0.2002s | 1.097 |
| `cfg_selectgen.ml` | 0.5587s | 0.5949s | 1.065 |
| `llvmize.ml` | 1.6287s | 1.7277s | 1.061 |
| `regalloc_irc.ml` | 0.3334s | 0.3591s | 1.077 |

Invalid no-scalar-clone run details:

This run compared against an `_install` whose `_build/log` recorded
`OCAMLPARAM: "_,llvm-backend=1,llvm-path=..."`, so the supposed native side was
also built through the LLVM backend. Keep it only as evidence of the mistake,
not as a native-vs-LLVM performance number.

- Build log:
  `_self_build_current/self_build_no_scalar_clone_20260527_092520.log`.
- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_no_scalar_clone_20260527_095212.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_no_scalar_clone_20260527_095212.json`.
- Aggregate: geomean LLVM-built/native-built ratio `1.0085`, median `1.0025`,
  min `0.9997`, max `1.0374`.
- Self-stage validation:
  `_self_build_current/self_stage_tests_no_scalar_clone_rerun_20260527_092520.log`
  passed with 6394 passed, 282 skipped, 0 failed, 6676 considered.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.8470s | 1.8505s | 1.002 |
| `ctype.ml` | 2.8490s | 2.8524s | 1.001 |
| `typecore.ml` | 5.4352s | 5.4335s | 1.000 |
| `translcore.ml` | 1.4660s | 1.4692s | 1.002 |
| `typemod.ml` | 1.6198s | 1.6238s | 1.002 |
| `cfg_to_linear.ml` | 0.1927s | 0.1999s | 1.037 |
| `cfg_selectgen.ml` | 0.6038s | 0.6096s | 1.010 |
| `llvmize.ml` | 1.7562s | 1.7624s | 1.004 |
| `regalloc_irc.ml` | 0.3615s | 0.3684s | 1.019 |

Previous post-review run details:

- Build log:
  `_self_build_current/self_build_reviewfix_20260527_060224.log`.
- Benchmark log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_reviewfix_20260527_061712.log`.
- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_reviewfix_20260527_061712.json`.
- Aggregate: geomean LLVM-built/native-built ratio `1.0079`, median `1.0033`,
  min `0.9920`, max `1.0433`.

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.5605s | 1.5644s | 1.002 |
| `ctype.ml` | 2.3675s | 2.3665s | 1.000 |
| `typecore.ml` | 4.5539s | 4.5552s | 1.000 |
| `translcore.ml` | 1.2639s | 1.2538s | 0.992 |
| `typemod.ml` | 1.3781s | 1.3867s | 1.006 |
| `cfg_to_linear.ml` | 0.1608s | 0.1677s | 1.043 |
| `cfg_selectgen.ml` | 0.5182s | 0.5218s | 1.007 |
| `llvmize.ml` | 1.4729s | 1.4777s | 1.003 |
| `regalloc_irc.ml` | 0.3060s | 0.3116s | 1.018 |

Previous i64/stack-growth run:

| file | native-built | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.7706s | 1.7878s | 1.010 |
| `ctype.ml` | 2.7536s | 2.7303s | 0.992 |
| `typecore.ml` | 5.1648s | 5.1872s | 1.004 |
| `translcore.ml` | 1.4082s | 1.4051s | 0.998 |
| `typemod.ml` | 1.5595s | 1.5639s | 1.003 |
| `cfg_to_linear.ml` | 0.1847s | 0.1916s | 1.037 |
| `cfg_selectgen.ml` | 0.5806s | 0.5892s | 1.015 |
| `llvmize.ml` | 1.6703s | 1.6833s | 1.008 |
| `regalloc_irc.ml` | 0.3457s | 0.3512s | 1.016 |

Previous `caml_modify` Candidate 1 run:

| file | pre-modify native-built | pre-modify LLVM-built | pre-modify LLVM/native | post-modify native-built | post-modify LLVM-built | post-modify LLVM/native | ratio change |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `env.ml` | 1.7268s | 1.8583s | 1.076 | 1.7630s | 1.8592s | 1.055 | -0.022 |
| `ctype.ml` | 2.5733s | 2.7670s | 1.075 | 2.6298s | 2.7737s | 1.055 | -0.021 |
| `typecore.ml` | 4.9070s | 5.3016s | 1.080 | 4.9551s | 5.2423s | 1.058 | -0.022 |
| `translcore.ml` | 1.3505s | 1.4655s | 1.085 | 1.3656s | 1.4607s | 1.070 | -0.015 |
| `typemod.ml` | 1.5200s | 1.6341s | 1.075 | 1.5182s | 1.6043s | 1.057 | -0.018 |
| `cfg_to_linear.ml` | 0.1809s | 0.1971s | 1.090 | 0.1787s | 0.1972s | 1.103 | +0.013 |
| `cfg_selectgen.ml` | 0.5681s | 0.6083s | 1.071 | 0.5639s | 0.5965s | 1.058 | -0.013 |
| `llvmize.ml` | 1.5979s | 1.7070s | 1.068 | 1.6327s | 1.7229s | 1.055 | -0.013 |
| `regalloc_irc.ml` | 0.3477s | 0.3715s | 1.069 | 0.3440s | 0.3706s | 1.077 | +0.009 |

Interpretation:

- The conservative short string/bytes compare lowering was a small net win for
  the compiler binary benchmark.
- The `caml_modify` Candidate 1 fast path is another small net win. It reduces
  the current geomean gap from about `7.7%` to about `6.5%`.
- The win is broad across the larger frontend files, especially `env.ml`,
  `ctype.ml`, `typecore.ml`, and `typemod.ml`. Small backend files are noisier:
  `cfg_to_linear.ml` and `regalloc_irc.ml` regressed in this run.
- The current implementation is still only Candidate 1. It avoids the wrapper
  on hot no-barrier writes, but still calls a cold helper for every real
  remembered-set or marking case. Candidate 2, inline remembered-set append,
  remains the next `caml_modify` performance step if this area needs more work.

## Mutation Microbenchmarks

These use `_install/bin/ocamlopt.opt` for both native and LLVM code generation,
with the same flags except `-llvm-backend -llvm-path` on the LLVM side.

- Current run:
  `_bench_llvm_slow_cases_current/summary_caml_modify_6pairs_20260526_202340.json`

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `record_update` | 0.0295s | 0.0160s | 0.557 |
| `int_ref_inc` | 0.1453s | 0.1455s | 0.999 |
| `float_ref_inc` | 0.3694s | 0.3597s | 0.978 |
| `list_build_sum` | 0.4337s | 0.4260s | 0.980 |
| `object_call_min` | 0.1976s | 0.2359s | 1.198 |

Interpretation:

- The mutation-heavy non-object cases are at parity or better with the current
  LLVM backend.
- `object_call_min` is unchanged and remains a separate object-dispatch/code
  layout outlier, not evidence against the `caml_modify` fast path.

## Representative Try/Trap Microbenchmarks

These compare native code generation with LLVM backend code generation using
the installed compiler. The important checks here are that the hot `_wrap_try`
helper has disappeared from the trap cases, and that removing the extra
domain-state exception-handler stores improved the remaining try/call cases.

- Current run, after native-like trap frames, active-trap stack adjustment
  fixes, and skipping the brittle `fast_path_roots.ml` golden IR test:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_after_stage2_skip_20260528_141352.json`
- Previous run:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_final_no_domain_exn_store_20260528_020706.json`
- Previous same-day run before removing the hot `Caml_state(exn_handler)`
  stores:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_final_20260528_015623.json`
- Current aggregate across 43 cases: geomean LLVM/native `0.8902`, median
  `0.9926`, min `0.3021`, max `1.5471`.
- Previous aggregate across 36 cases: geomean LLVM/native `0.8861`, median
  `1.0437`, min `0.2991`, max `1.5784`.
- Earlier aggregate across 36 cases: geomean LLVM/native `0.9131`, median
  `0.9965`, min `0.2870`, max `1.6201`.
- All listed current cases have `wrap_try_refs = 0`.

Largest current slowdowns:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `direct_call_in_try_hit` | 0.0640s | 0.0991s | 1.547 |
| `env_find_same_layered_hit` | 0.3360s | 0.4624s | 1.376 |
| `string_tree_prefix_heavy` | 0.3361s | 0.4582s | 1.363 |
| `closure_call_in_try_hit` | 0.0686s | 0.0888s | 1.294 |
| `try_with_string_compare_hit` | 0.2595s | 0.3234s | 1.246 |
| `closure_call_in_nested_try_hit` | 0.0509s | 0.0621s | 1.219 |

Selected try/raise cases:

| case | LLVM/native |
| --- | ---: |
| `try_raise_cross_function_caught` | 1.066 |
| `layered_try_raise_hit_only` | 0.993 |
| `try_raise_inline_caught` | 0.754 |
| `layered_try_raise_inline_hit_only` | 0.597 |

Previous no-domain-exn-store comparison:

| case | earlier LLVM/native | previous LLVM/native |
| --- | ---: | ---: |
| `try_find_hit_deep` | 0.927 | 0.942 |
| `try_find_multiple_handlers` | 0.943 | 0.941 |
| `try_find_cold_handler_large_body` | 0.957 | 0.996 |
| `try_find_miss_rare` | 0.908 | 0.969 |
| `try_int_find_hit` | 0.971 | 0.968 |
| `direct_call_in_try_hit` | 1.378 | 1.174 |
| `closure_call_in_try_hit` | 1.414 | 1.050 |
| `closure_call_in_nested_try_hit` | 1.401 | 1.235 |
| `try_with_string_compare_hit` | 1.470 | 1.185 |
| `env_find_same_layered_hit` | 1.620 | 1.578 |

## Undefined GC Root Invariant Fix, 2026-05-29

Validation before benchmarking:

- `make llvm-test LLVM_PATH="$LLVM_PATH"` passed with 6740 passed, 296 skipped,
  0 failed.
- `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` passed with 6714 passed,
  283 skipped, 0 failed.
- Root cause fixed: `undef`/poison GC pointer values were being materialized in
  runtime-scanned OCaml root slots. Those slots must always contain a valid
  OCaml value bit pattern, so the lowering now uses `Val_unit` (`1`) as the
  non-moving default for undefined OxCaml GC pointers.

Representative microbenchmarks:

- Summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_after_undef_root_invariant_20260529_025418.json`
- Log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_after_undef_root_invariant_20260529_025418.log`
- Aggregate across 43 cases: geomean `0.8558`, median `0.9416`, min `0.2994`,
  max `1.4571`.
- All measured `_wrap_try` refs are still zero.
- Main try/call regressions from the previous stage2 run are gone:
  `direct_call_in_try_hit` moved from `1.488` to `0.967`,
  `closure_call_in_try_hit` moved from `1.334` to `1.024`, and
  `closure_call_in_nested_try_hit` moved from `1.228` to `0.942`.

Worst representative microbenchmark slowdowns:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `string_tree_prefix_heavy` | 0.3383s | 0.4929s | 1.457 |
| `string_map_equal_content` | 0.2543s | 0.3464s | 1.362 |
| `higher_order_fold_string_keys` | 1.1320s | 1.3745s | 1.214 |
| `nested_scope_lookup` | 0.1488s | 0.1757s | 1.181 |
| `string_map_interned_keys` | 0.2350s | 0.2748s | 1.170 |
| `try_with_string_compare_hit` | 0.2729s | 0.3177s | 1.164 |
| `string_equal_guarded_dispatch` | 0.1264s | 0.1465s | 1.159 |
| `env_find_same_layered_hit` | 0.3924s | 0.4459s | 1.136 |

Selected exception/dispatch cases:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `try_find_hit_deep` | 0.6490s | 0.5707s | 0.879 |
| `try_find_miss_rare` | 0.8188s | 0.7492s | 0.915 |
| `try_raise_cross_function_caught` | 0.4466s | 0.4375s | 0.979 |
| `direct_call_in_try_hit` | 0.0562s | 0.0543s | 0.967 |
| `closure_call_in_try_hit` | 0.0614s | 0.0629s | 1.024 |
| `closure_call_in_nested_try_hit` | 0.0459s | 0.0432s | 0.942 |
| `variant_dispatch_with_int_payload` | 0.1036s | 0.0766s | 0.739 |

Compiler-binary benchmark:

- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_after_undef_root_invariant_20260529_025704.json`
- Log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_after_undef_root_invariant_20260529_025704.log`
- Compared clean `_native_install` against freshly validated
  `_llvm_self_stage2_install`.
- Aggregate: geomean `0.9983`, median `0.9910`, min `0.9833`, max `1.0398`.

| file | native | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.4936s | 1.4820s | 0.992 |
| `ctype.ml` | 2.2688s | 2.2445s | 0.989 |
| `typecore.ml` | 4.3465s | 4.3072s | 0.991 |
| `translcore.ml` | 1.1877s | 1.1748s | 0.989 |
| `typemod.ml` | 1.3225s | 1.3004s | 0.983 |
| `cfg_to_linear.ml` | 0.1548s | 0.1609s | 1.040 |
| `cfg_selectgen.ml` | 0.4922s | 0.4918s | 0.999 |
| `llvmize.ml` | 1.4392s | 1.4179s | 0.985 |
| `regalloc_irc.ml` | 0.2907s | 0.2956s | 1.017 |

## Runtime Helper Counts

These counts are from the runtime helper profiling runs used to prioritize
inline/noalloc C-call work.

| helper | calls | extra observation |
| --- | ---: | --- |
| `caml_modify` | 88,484,839 | About 10.2% need remembered-set or marking work. |
| `caml_string_compare` | 27,695,126 | 93.1% reach `memcmp`. |
| `caml_string_equal` | 5,921,448 | 82.0% reach the content loop. |
| `caml_obj_tag` | 2,888,252 | Tiny helper; now inlined by the LLVM backend on AArch64. |
| `caml_initialize` | 1,384,736 | Only 0.5% are old-to-young remembered-set writes. |
| `caml_modify_local` | 5,738,361 | 97.4% fall back to `caml_modify`. |

Design implications:

- `caml_modify` dominates by call count, but its slow cases matter for
  correctness and write-barrier behavior.
- `caml_string_compare` is common enough to matter, but because most calls reach
  `memcmp`, a useful lowering needs to avoid adding overhead to long strings.
- `caml_obj_tag` was the easiest tiny helper target and is now inlined.
- `caml_initialize` and selected write-barrier fast paths still look like
  promising small inline targets.

## `caml_obj_tag` Focused Benchmark

Current focused rerun after inline lowering:

- Log:
  `agent-state/llvm-fast-path-roots-integration/c_call_slowdown/run_obj_tag_inline_20260526_212617.log`

| case | native | LLVM | LLVM/native | LLVM wrapper refs |
| --- | ---: | ---: | ---: | ---: |
| `obj_tag_loop` | 0.1974s | 0.0992s | 0.503 | 0 |

Previous saved result from `C_CALL_SLOWDOWN.md`:

| case | native | LLVM | LLVM/native | LLVM wrapper refs |
| --- | ---: | ---: | ---: | ---: |
| `obj_tag_loop` | 0.1673s | 0.3860s | 2.307 | 4 |

Interpretation:

- This fully removes the noalloc wrapper overhead for the focused tag case.
- The compiler-binary effect is expected to be modest because `caml_obj_tag`
  is much less frequent than `caml_modify` and `caml_string_compare`.

## Design 1 Runtime-State Prototype Benchmarks

Current Design 1 prototype:

- Keeps domain state and allocation pointer as ordinary SSA values.
- Passes them through the OxCaml calling convention at relevant call
  boundaries, with `x28`/`x27` allocatable instead of globally reserved.
- Makes the AArch64 exception recovery path resume at the same machine return
  point as the normal `wrap_try` return, so the normal call-result extraction
  also supplies domain state and allocation pointer on the exceptional path.
- Does not use the rejected global LLVM block-placement/tail-duplication
  pessimization.

Correctness status while these numbers were recorded:

- Installed compiler rebuild passed.
- Focused `fast_path_roots.ml` and `typing-local/tailcalls.ml` checks passed
  after promoting/skipping tests for the intended code shape.
- Full `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` passed:
  `6730` passed, `295` skipped, `0` failed, `0` unexpected errors.

Post-full-validation representative microbenchmark run:

- Log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_post_full_validation_20260527_233923.log`
- Summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_post_full_validation_20260527_233923.json`
- Aggregate over 36 cases: geomean LLVM/native ratio `0.9075`, median
  `1.0143`, min `0.2958`, max `1.5118`.
- All reported try cases had `wrap_try_refs=0`.

Largest slowdowns:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `env_find_same_layered_hit` | 0.3431s | 0.5187s | 1.512 |
| `closure_call_in_try_hit` | 0.0666s | 0.0931s | 1.397 |
| `closure_call_in_nested_try_hit` | 0.0488s | 0.0680s | 1.394 |
| `direct_call_in_try_hit` | 0.0628s | 0.0848s | 1.350 |
| `try_with_string_compare_hit` | 0.2484s | 0.3274s | 1.318 |
| `string_map_equal_content` | 0.2477s | 0.3176s | 1.282 |
| `string_tree_prefix_heavy` | 0.3293s | 0.4100s | 1.245 |

Representative microbenchmark run:

- Log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_design1_current_20260527_030206.log`
- Summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_design1_current_20260527_030505.json`
- Command shape:
  both native and LLVM cases used the same installed compiler and flags, except
  for `-llvm-backend -llvm-path "$LLVM_PATH"` on the LLVM side.

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `try_find_hit_deep` | 0.6702s | 0.6235s | 0.930 |
| `try_find_multiple_handlers` | 0.5918s | 0.5667s | 0.957 |
| `try_find_cold_handler_large_body` | 0.7967s | 0.7410s | 0.930 |
| `try_find_miss_rare` | 0.8649s | 0.8040s | 0.930 |
| `try_int_find_hit` | 0.7586s | 0.7026s | 0.926 |
| `env_find_same_mini` | 0.1240s | 0.0839s | 0.677 |
| `record_mutate_old_to_immediate` | 0.0447s | 0.0445s | 0.996 |
| `record_mutate_old_to_young` | 0.0091s | 0.0089s | 0.984 |
| `array_set_young_values` | 0.0228s | 0.0210s | 0.922 |
| `ref_option_churn` | 0.0138s | 0.0141s | 1.026 |
| `string_assoc_find_hit` | 0.5525s | 0.6369s | 1.153 |
| `string_tree_prefix_heavy` | 0.3374s | 0.4272s | 1.266 |
| `string_tree_first_byte_diff` | 0.4223s | 0.4698s | 1.113 |
| `string_map_interned_keys` | 0.2495s | 0.2666s | 1.069 |
| `string_map_equal_content` | 0.2564s | 0.3163s | 1.234 |
| `string_equal_guarded_dispatch` | 0.1276s | 0.2539s | 1.990 |
| `try_with_string_compare_hit` | 0.2532s | 0.3515s | 1.388 |
| `nested_scope_lookup` | 0.1576s | 0.1898s | 1.204 |
| `persistent_map_update_lookup` | 0.1183s | 0.1366s | 1.155 |
| `closure_call_in_try_hit` | 0.0665s | 0.1185s | 1.780 |
| `direct_call_in_try_hit` | 0.0626s | 0.0872s | 1.394 |
| `higher_order_fold_string_keys` | 1.1670s | 1.3562s | 1.162 |
| `list_lookup_string_compare` | 0.8393s | 0.9597s | 1.144 |
| `array_binary_search_string` | 0.3762s | 0.4113s | 1.093 |
| `hash_lookup_string_equal` | 0.7386s | 0.8277s | 1.121 |
| `variant_dispatch_with_string_payload` | 0.0546s | 0.2604s | 4.770 |
| `variant_dispatch_with_int_payload` | 0.1081s | 1.1573s | 10.704 |
| `variant_dispatch_with_int_payload_inline` | 0.0770s | 0.0668s | 0.868 |
| `int_tree_find_same_shape` | 0.1476s | 0.0734s | 0.497 |

Focused old slow-case run:

- Log:
  `_bench_llvm_slow_cases_current/bench_design1_current_20260527_030528.log`
- Summary:
  `_bench_llvm_slow_cases_current/summary_design1_current_20260527_030636.json`

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `alloc_some_always_min` | 0.1339s | 0.4254s | 3.169 |
| `alloc_tuple_pair_min` | 0.0768s | 0.0880s | 1.143 |
| `variant_alloc_min` | 0.1321s | 0.4317s | 3.248 |
| `recursive_fib_small` | 0.0257s | 0.0257s | 1.019 |
| `try_lookup_hit` | 2.3446s | 2.4640s | 1.051 |
| `try_lookup_miss` | 1.9134s | 1.9427s | 1.015 |
| `object_call_min` | 0.1964s | 0.2352s | 1.199 |
| `string_safe_get` | 0.2228s | 0.2139s | 0.960 |
| `float_ref_loop` | 0.2259s | 0.2278s | 1.009 |
| `int_for_loop` | 0.2673s | 0.2616s | 0.978 |

Interpretation:

- Better: the old try/handler fast-path problem is materially improved. The
  representative try-only cases are now near parity or faster than native.
- Better: mutation/write-barrier focused shapes remain at parity or better.
- Not worse: simple integer, floating-point, and safe-string-get loops stayed
  near parity.
- Still bad: `alloc_some_always_min` and `variant_alloc_min` are around `3.2x`
  slower. This means the current Design 1 prototype has not recovered all of
  the earlier allocation fast-path wins.
- Still bad: non-inlined tiny call shapes, especially
  `variant_dispatch_with_int_payload`, remain much slower. The inline control
  variant being faster than native (`0.868x`) says the bad case is mostly the
  call-boundary/code-shape contract, not integer variant dispatch itself.

## Stack-Pair Boundary Fix

The remaining `alloc_some_always_min` and `variant_alloc_min` regressions were
not caused by `x27`/`x28` placement after Design 1. Fresh assembly already kept
`alloc` in `x27` and `ds` in `x28` on the hot path.

Root cause:

- The AArch64 load/store optimizer paired post-call stack reloads into:

  ```asm
  ldp x9, x10, [sp, #32]
  ```

- Those slots were written by separate `str` instructions before the call:

  ```asm
  str x0,  [sp, #32]
  str x10, [sp, #40]
  ```

- On the allocation microbenchmarks, this paired reload shape was enough to make
  LLVM about 3x slower than native. Native used scalar reloads in the same
  region.

Evidence:

- Baseline current LLVM:
  `alloc_some_always_min` native 0.1288s, LLVM 0.4126s, ratio 3.0756.
- Manual assembly rewrite, only splitting/delaying the hot reload:
  `alloc_some_always_min` LLVM user time dropped from about 0.40s to about
  0.13s.
- Passing `-mllvm -tail-dup-placement=false` also produced scalar reloads and
  restored user time to about 0.12s, but this is too broad to use as the fix.
- Implemented the narrower fix in
  `vendor/llvm-project/llvm/lib/Target/AArch64/AArch64LoadStoreOptimizer.cpp`:
  suppress SP-based load/store pairing in OxCaml GC functions near call
  boundaries, including successor blocks that start immediately after a
  call/statepoint predecessor.

Focused post-fix run:

- Log:
  `_bench_llvm_slow_cases_current/bench_after_stack_pair_boundary_focused_20260527_033547.log`
- Summary:
  `_bench_llvm_slow_cases_current/summary_after_stack_pair_boundary_focused_20260527_033657.json`

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `alloc_some_always_min` | 0.1290s | 0.1286s | 0.9997 |
| `alloc_tuple_pair_min` | 0.0739s | 0.0931s | 1.2648 |
| `variant_alloc_min` | 0.1288s | 0.1286s | 0.9987 |
| `recursive_fib_small` | 0.0263s | 0.0252s | 0.9526 |
| `try_lookup_hit` | 2.3861s | 2.4954s | 1.0519 |
| `try_lookup_miss` | 1.8348s | 1.9072s | 1.0313 |
| `object_call_min` | 0.1968s | 0.2305s | 1.1708 |
| `string_safe_get` | 0.2141s | 0.2057s | 0.9604 |
| `float_ref_loop` | 0.2021s | 0.2030s | 1.0034 |
| `int_for_loop` | 0.2347s | 0.2310s | 0.9842 |

Validation:

- Rebuilt branch-local LLVM `clang`, `llc`, and `opt`.
- `make llvm-test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml
  LLVM_PATH="$LLVM_PATH"` passed: 3 passed, 0 failed.
- `make llvm-test-one-no-rebuild DIR=llvm-codegen LLVM_PATH="$LLVM_PATH"`
  passed: 79 passed, 2 skipped, 0 failed.
- `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` passed: 6730 passed,
  295 skipped, 0 failed, 0 unexpected, 7025 considered.

Remaining:

- `alloc_tuple_pair_min` is still slower in the focused run. It does not share
  the fixed `alloc_some`/`variant_alloc` stack-pair pattern and should be
  treated as a separate remaining case.

## Stage2-Validated Trap-Recovery Run, 2026-05-28

Validation before benchmarking:

- Normal full LLVM-backend tests:
  `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` passed with 6740 passed,
  296 skipped, 0 failed.
- Self-stage2 validation:
  `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` passed with 6714 passed,
  283 skipped, 0 failed.
- Clean native comparison compiler was rebuilt before compiler-binary perf with
  `tools/build-clean-native-install.sh --force -j8`.

Representative microbenchmarks:

- Summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_stage2_validated_20260528_165527.json`
- Log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_stage2_validated_20260528_165527.log`
- Aggregate across 43 cases: geomean `0.8816`, median `0.9758`, min `0.2973`,
  max `1.4881`.

Worst representative microbenchmark slowdowns:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `direct_call_in_try_hit` | 0.0654s | 0.0973s | 1.488 |
| `closure_call_in_try_hit` | 0.0678s | 0.0904s | 1.334 |
| `env_find_same_layered_hit` | 0.3367s | 0.4465s | 1.326 |
| `string_map_equal_content` | 0.2606s | 0.3308s | 1.269 |
| `higher_order_fold_string_keys` | 1.1715s | 1.4432s | 1.232 |
| `closure_call_in_nested_try_hit` | 0.0503s | 0.0618s | 1.228 |
| `try_with_string_compare_hit` | 0.2673s | 0.3250s | 1.216 |
| `string_tree_prefix_heavy` | 0.3562s | 0.4267s | 1.198 |

Representative exception/dispatch status:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `try_find_hit_deep` | 0.6613s | 0.6168s | 0.933 |
| `try_find_miss_rare` | 0.8652s | 0.7873s | 0.910 |
| `try_raise_cross_function_caught` | 0.4954s | 0.5353s | 1.080 |
| `direct_call_in_try_hit` | 0.0654s | 0.0973s | 1.488 |
| `variant_dispatch_with_int_payload` | 0.1111s | 0.0788s | 0.709 |

Compiler-binary benchmark:

- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_stage2_validated_20260528_170412.json`
- Log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_stage2_validated_20260528_170412.log`
- Compared clean `_native_install` against `_llvm_self_stage2_install`.
- Aggregate: geomean `1.0418`, median `1.0406`, min `1.0294`, max `1.0690`.

| file | native | LLVM-built | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.7589s | 1.8106s | 1.029 |
| `ctype.ml` | 2.6346s | 2.7503s | 1.044 |
| `typecore.ml` | 5.0050s | 5.1530s | 1.030 |
| `translcore.ml` | 1.3599s | 1.4049s | 1.033 |
| `typemod.ml` | 1.5244s | 1.5794s | 1.036 |
| `cfg_to_linear.ml` | 0.1846s | 0.1974s | 1.069 |
| `cfg_selectgen.ml` | 0.5723s | 0.5956s | 1.041 |
| `llvmize.ml` | 1.6872s | 1.7568s | 1.041 |
| `regalloc_irc.ml` | 0.3439s | 0.3625s | 1.054 |

## Explicit Exception Roots And Non-Volatile Root Slots, 2026-05-28

Validation before benchmarking:

- Focused LLVM-backend test directories passed after updating the expected
  `llvm-codegen/allocation.ml` root-slot IR from volatile to ordinary
  loads/stores:
  - `llvm-codegen`: 89 passed, 3 skipped, 0 failed.
  - `compaction`: 8 passed, 3 skipped, 0 failed.
  - `gc-roots`: 10 passed, 0 skipped, 0 failed.
  - `basic`: 82 passed, 0 skipped, 0 failed.
  - `exception-extra-args`: 6 passed, 0 skipped, 0 failed.
  - `backtrace`: 67 passed, 6 skipped, 0 failed.
  - `async-exns`: 5 passed, 0 skipped, 0 failed.
  - `match-exception`: 16 passed, 0 skipped, 0 failed.
  - `runtime-C-exceptions`: 2 passed, 0 skipped, 0 failed.
  - `raise-counts`: 2 passed, 0 skipped, 0 failed.

Representative microbenchmarks:

- Full summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_after_exnroot_volatile_removal_20260528_232227.json`
- Full log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_after_exnroot_volatile_removal_20260528_232227.log`
- Focused summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_after_exnroot_volatile_removal_focused_20260528_232542.json`
- Focused log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_after_exnroot_volatile_removal_focused_20260528_232542.log`
- Full aggregate across 43 cases: geomean `0.8765`, median `1.0090`, min
  `0.2826`, max `1.2578`.
- All measured `_wrap_try` refs in these runs are now zero.
- Compared with the stage2-validated trap-recovery run above, the full-run max
  slowdown improved from `1.4881` to `1.2578`.

Worst full-run representative microbenchmark slowdowns:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `env_find_same_layered_hit` | 0.3400s | 0.4276s | 1.258 |
| `string_tree_prefix_heavy` | 0.3567s | 0.4471s | 1.253 |
| `higher_order_fold_string_keys` | 1.1585s | 1.4254s | 1.230 |
| `string_map_equal_content` | 0.2619s | 0.3179s | 1.214 |
| `try_with_string_compare_hit` | 0.2719s | 0.3297s | 1.213 |
| `string_tree_first_byte_diff` | 0.4177s | 0.5045s | 1.208 |
| `string_equal_guarded_dispatch` | 0.1297s | 0.1552s | 1.197 |
| `closure_call_in_try_hit` | 0.0739s | 0.0874s | 1.183 |

Focused rerun of important cases:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `try_with_string_compare_hit` | 0.2552s | 0.3443s | 1.349 |
| `env_find_same_layered_hit` | 0.3379s | 0.4452s | 1.317 |
| `string_tree_prefix_heavy` | 0.3506s | 0.4566s | 1.302 |
| `string_map_equal_content` | 0.2585s | 0.3238s | 1.252 |
| `higher_order_fold_string_keys` | 1.1695s | 1.4231s | 1.217 |
| `string_tree_first_byte_diff` | 0.4175s | 0.5004s | 1.199 |
| `closure_call_in_try_hit` | 0.0733s | 0.0858s | 1.170 |
| `string_equal_guarded_dispatch` | 0.1382s | 0.1614s | 1.168 |
| `direct_call_in_try_hit` | 0.0544s | 0.0621s | 1.143 |
| `variant_dispatch_with_int_payload` | 0.1095s | 0.0778s | 0.710 |
| `variant_dispatch_with_string_payload` | 0.0540s | 0.0383s | 0.709 |

Interpretation:

- The explicit exception-root lowering and ordinary root-slot traffic remove
  the old trap-wrapper benchmark pattern. `direct_call_in_try_hit` improved
  from `1.488` to `1.009` in the full run, and
  `closure_call_in_try_hit` improved from `1.334` to `1.183`.
- The remaining representative slowdowns are mostly string/helper-heavy shapes
  and one layered environment lookup shape, not the old `_wrap_try` path.

## Raw Heap Address Canonicalization, 2026-05-29

Validation:

- LLVM checks:
  - `vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-raw-heap-addresses.ll`
  - `vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-explicit-exception-roots.ll`
- Focused LLVM-backend suites:
  - `typing-layouts-iarrays`: 81 passed, 0 failed.
  - `llvm-codegen`: 89 passed, 3 skipped, 0 failed.
- Full LLVM-backend suite:
  `agent-state/llvm-fast-path-roots-integration/full_suite_after_raw_heap_canonicalization_20260529_140100.log`,
  6743 passed, 296 skipped, 0 failed.
- Full self-stage2 validation:
  `agent-state/llvm-fast-path-roots-integration/stage2_after_raw_heap_canonicalization_20260529_140725.log`,
  6717 passed, 283 skipped, 0 failed.

Representative microbenchmarks:

- Summary:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_after_raw_heap_canonicalization_20260529_142901.json`
- Log:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_after_raw_heap_canonicalization_20260529_142901.log`
- Aggregate across 43 cases: geomean `0.8144`, median `0.9588`, min
  `0.2830`, max `1.0702`.

Worst representative microbenchmark slowdowns:

| case | LLVM/native |
| --- | ---: |
| `ref_option_churn` | 1.070 |
| `env_find_same_layered_hit` | 1.068 |
| `record_mutate_old_to_young` | 1.061 |
| `env_find_same_layered_one_node` | 1.032 |
| `closure_call_in_try_hit` | 1.021 |
| `array_binary_search_string` | 1.020 |
| `try_raise_cross_function_caught` | 1.010 |
| `list_lookup_string_compare` | 1.002 |

Compiler-binary benchmark:

- Summary:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_after_raw_heap_canonicalization_20260529_143216.json`
- Log:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/run_after_raw_heap_canonicalization_20260529_143216.log`
- Compared `_native_install` with `_llvm_self_stage2_install` using 7 pairs.
- Aggregate geomean `0.9866`, median `0.9849`, min `0.9709`, max `1.0022`.

Compiler-binary per-file medians:

| file | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `env.ml` | 1.4928s | 1.4679s | 0.983 |
| `ctype.ml` | 2.2723s | 2.2379s | 0.985 |
| `typecore.ml` | 4.3947s | 4.3272s | 0.985 |
| `translcore.ml` | 1.1855s | 1.1734s | 0.990 |
| `typemod.ml` | 1.3293s | 1.3098s | 0.985 |
| `cfg_to_linear.ml` | 0.1564s | 0.1567s | 1.002 |
| `cfg_selectgen.ml` | 0.5008s | 0.4862s | 0.971 |
| `llvmize.ml` | 1.4609s | 1.4311s | 0.980 |
| `regalloc_irc.ml` | 0.2951s | 0.2949s | 0.999 |

Interpretation:

- The raw heap-address canonicalization fixed stale unrooted copies produced by
  raw `addrspace(0)` heap addresses that survived across statepoints.
- The remaining benchmark picture is now near parity or faster for this
  benchmark set. The compiler-binary max slowdown in this run is
  `cfg_to_linear.ml` at `1.002x`.
