# Numbers

Last updated: 2026-05-26.

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
| `caml_obj_tag` | 2,888,252 | Tiny helper; good inline candidate. |
| `caml_initialize` | 1,384,736 | Only 0.5% are old-to-young remembered-set writes. |
| `caml_modify_local` | 5,738,361 | 97.4% fall back to `caml_modify`. |

Design implications:

- `caml_modify` dominates by call count, but its slow cases matter for
  correctness and write-barrier behavior.
- `caml_string_compare` is common enough to matter, but because most calls reach
  `memcmp`, a useful lowering needs to avoid adding overhead to long strings.
- `caml_initialize`, `caml_obj_tag`, and selected write-barrier fast paths still
  look like promising small inline targets.
