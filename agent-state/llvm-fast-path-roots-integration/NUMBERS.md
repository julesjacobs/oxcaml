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
- Current run:
  `_compiler_binary_perf_current/summary_string_compare_lowering_20260526_194445.json`
- Previous saved baseline:
  `_compiler_binary_perf_current/summary_before_string_compare_20260526_193759.json`

Geomean LLVM/native ratio:

- Old baseline: `1.0897`
- After conservative short string/bytes compare lowering: `1.0766`
- Change: `-0.0131`

| file | old native-built | old LLVM-built | old LLVM/native | new native-built | new LLVM-built | new LLVM/native | ratio change |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `env.ml` | 1.8079s | 1.9629s | 1.086 | 1.7268s | 1.8583s | 1.076 | -0.010 |
| `ctype.ml` | 2.7414s | 2.9524s | 1.077 | 2.5733s | 2.7670s | 1.075 | -0.002 |
| `typecore.ml` | 5.2052s | 5.7025s | 1.096 | 4.9070s | 5.3016s | 1.080 | -0.016 |
| `translcore.ml` | 1.3931s | 1.5335s | 1.101 | 1.3505s | 1.4655s | 1.085 | -0.016 |
| `typemod.ml` | 1.5573s | 1.6982s | 1.090 | 1.5200s | 1.6341s | 1.075 | -0.015 |
| `cfg_to_linear.ml` | 0.1876s | 0.2074s | 1.106 | 0.1809s | 0.1971s | 1.090 | -0.016 |
| `cfg_selectgen.ml` | 0.5831s | 0.6362s | 1.091 | 0.5681s | 0.6083s | 1.071 | -0.020 |
| `llvmize.ml` | 1.5894s | 1.7011s | 1.070 | 1.5979s | 1.7070s | 1.068 | -0.002 |
| `regalloc_irc.ml` | 0.3499s | 0.3819s | 1.091 | 0.3477s | 0.3715s | 1.069 | -0.022 |

Interpretation:

- The conservative short string/bytes compare lowering is a small net win for
  the compiler binary benchmark.
- It does not close the remaining compiler-binary gap. The current LLVM-built
  compiler is still about `7.7%` slower by geomean.
- String-heavy standalone cases with keys longer than 15 bytes regressed,
  because they mostly compute lengths and then fall back to the helper. This is
  evidence for the next planned shape: compare an early word prefix before
  falling back, or implement the prefix-then-suffix design.

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
