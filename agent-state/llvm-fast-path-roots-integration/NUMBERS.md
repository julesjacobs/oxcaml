# Numbers

Last updated: 2026-05-27.

This file keeps the headline performance numbers for the LLVM-built compiler
and the runtime helper profiles that motivated the current noalloc C-call work.

## Compiler Binary Benchmark

Benchmark shape:

- Compare a normal native-built `ocamlopt.opt` against an LLVM-built
  `ocamlopt.opt`.
- Both compilers compile representative compiler source files with the normal
  backend. This measures the speed of the compiler binary itself, not LLVM
  backend compile time.
- Current run, after the `caml_modify` Candidate 1 fast path:
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
- String-compare change: `-0.0131`
- `caml_modify` Candidate 1 change: `-0.0115`
- Total recorded change: `-0.0246`

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
