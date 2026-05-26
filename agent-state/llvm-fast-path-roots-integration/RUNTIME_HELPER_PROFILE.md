# Runtime helper profile

This records the env-gated helper profiling added while investigating whether
hot C/runtime helpers explain the remaining LLVM-built compiler slowdown.

## Mechanism

Set:

```sh
OCAML_LLVM_HELPER_PROFILE=1
```

The runtime then prints CSV-style lines on shutdown:

```text
ocaml_llvm_helper_profile,<counter>,<count>
```

The counters are in:

- `runtime/caml/llvm_helper_profile.h`
- `runtime/memory.c`
- `runtime/str.c`
- `runtime/obj.c`
- `runtime/startup_aux.c`

The profiling is intentionally helper-side. It measures actual runtime behavior
without changing LLVM lowering at each call site.

## Workload

I rebuilt `_install/bin/ocamlopt.opt` with the instrumented runtime using:

```sh
eval "$(opam env --switch=oxcaml-5.4.0+oxcaml --set-switch)" && make _install
```

Then I ran the same compile commands used by
`_compiler_binary_perf_current/bench_compiler_binary.py`, once per
representative compiler source file, with `OCAML_LLVM_HELPER_PROFILE=1`.

Raw profiles are under:

```text
_compiler_binary_perf_current/helper_profiles/native_built_current/
```

The compact CSV is:

```text
_compiler_binary_perf_current/helper_profiles/native_built_current/summary.csv
```

The string-compare-focused CSV is:

```text
_compiler_binary_perf_current/helper_profiles/native_built_current/string_compare_histogram.csv
```

## Static Profile

This is a static call-site count from the saved full compiler-binary objdump
snapshots:

```text
_compiler_binary_perf_current/asm_counts/native_objdump.txt
_compiler_binary_perf_current/asm_counts/current_llvm_objdump.txt
```

The compact CSV is:

```text
_compiler_binary_perf_current/asm_counts/static_call_profile.csv
```

These are static counts, so they answer "how many call sites exist in the
compiler binary", not "how often each call runs".

| symbol | native direct call sites | LLVM direct call sites | LLVM wrapper call sites |
| --- | ---: | ---: | ---: |
| `caml_initialize` | 0 | 0 | 11,092 |
| `caml_modify` | 0 | 0 | 4,807 |
| `caml_modify_local` | 1,328 | 7 | 1,325 |
| `caml_obj_tag` | 177 | 3 | 177 |
| `caml_string_compare` | 409 | 7 | 412 |
| `caml_string_equal` | 628 | 51 | 628 |
| `caml_string_notequal` | 1,809 | 31 | 1,807 |
| `caml_blit_bytes` | 444 | 19 | 444 |
| `caml_blit_string` | 1,937 | 75 | 1,937 |
| `caml_fill_bytes` | 72 | 12 | 72 |
| `caml_c_call` | 5,974 | 5,987 | 0 |
| `_wrap_try` | 0 | 2,979 | 0 |
| all `c_call_wrapper.*` targets | 0 | 0 | 23,854 |

The static profile explains why the noalloc C-call boundary is suspicious:
the native-built compiler mostly calls these helpers directly, while the
LLVM-built compiler has a generated wrapper call site for nearly every
high-level helper call site. The dynamic profile below then tells us which of
those static targets are actually hot while compiling representative compiler
files.

## Summary

`typecore.ml` is the most important representative case:

| helper | count | useful detail |
| --- | ---: | --- |
| `caml_modify` | 88,484,839 | only 10.2% need remembered-set or marking work |
| `caml_string_compare` | 27,695,126 | 93.1% reach `memcmp` |
| `caml_string_equal` | 5,921,448 | 82.0% reach the content loop |
| `caml_blit_bytes` | 4,565,412 | 28,778,695 bytes total |
| `caml_blit_string` | 3,886,104 | 17,241,148 bytes total |
| `caml_obj_tag` | 2,888,252 | tiny helper, pure tag load |
| `caml_initialize` | 1,384,736 | only 0.5% are old-to-young remembered-set writes |
| `caml_modify_local` | 5,738,361 | 97.4% fall back to `caml_modify` |

For `typecore.ml`, the `caml_string_compare` result and length breakdown is:

| outcome | count | share of all compares |
| --- | ---: | ---: |
| pointer equal | 1,917,854 | 6.9% |
| `memcmp` less-than | 10,557,413 | 38.1% |
| `memcmp` greater-than | 12,802,395 | 46.2% |
| equal contents, shorter left string | 406,622 | 1.5% |
| equal contents, shorter right string | 73,027 | 0.3% |
| equal strings | 1,937,815 | 7.0% |

For non-pointer-equal `typecore.ml` compares, the `min(len1, len2)` histogram
is:

| `min(len1, len2)` | count | share of non-pointer compares |
| --- | ---: | ---: |
| 0 | 316 | 0.0% |
| 1 | 872,437 | 3.4% |
| 2 | 144,555 | 0.6% |
| 3 | 3,788,143 | 14.7% |
| 4 | 5,277,877 | 20.5% |
| 5-7 | 7,671,353 | 29.8% |
| 8-15 | 7,385,101 | 28.6% |
| 16-31 | 504,938 | 2.0% |
| 32-63 | 130,907 | 0.5% |
| 64-127 | 1,638 | 0.0% |
| 128+ | 7 | 0.0% |

Across the nine representative files, the same pattern holds:

| case | `caml_modify` | modify record/mark % | `caml_string_compare` | compare memcmp % | `caml_obj_tag` |
| --- | ---: | ---: | ---: | ---: | ---: |
| `typecore` | 88,484,839 | 10.2% | 27,695,126 | 93.1% | 2,888,252 |
| `llvmize` | 41,601,573 | 16.2% | 3,528,708 | 87.2% | 632,882 |
| `ctype` | 33,106,214 | 13.0% | 5,296,672 | 87.0% | 853,333 |
| `translcore` | 22,064,914 | 38.4% | 6,813,085 | 88.0% | 1,272,000 |
| `env` | 19,378,794 | 9.2% | 8,448,737 | 89.2% | 932,832 |
| `typemod` | 17,851,571 | 10.1% | 6,278,412 | 91.3% | 911,730 |
| `cfg_selectgen` | 6,031,623 | 12.7% | 1,988,765 | 91.5% | 321,926 |
| `regalloc_irc` | 4,069,179 | 8.3% | 443,269 | 89.1% | 136,394 |
| `cfg_to_linear` | 1,497,106 | 9.9% | 390,085 | 91.1% | 77,943 |

For string compare length specifically, the representative files mostly compare
short strings. Shares below are out of non-pointer-equal compares:

| case | `min_len <= 7` | `min_len <= 15` | `min_len >= 32` |
| --- | ---: | ---: | ---: |
| `typecore` | 68.9% | 97.5% | 0.5% |
| `env` | 85.7% | 97.0% | 0.7% |
| `ctype` | 77.8% | 96.2% | 1.7% |
| `typemod` | 75.5% | 96.0% | 1.3% |
| `translcore` | 66.0% | 93.9% | 0.9% |
| `llvmize` | 87.1% | 95.7% | 2.1% |
| `cfg_selectgen` | 73.2% | 93.5% | 2.3% |
| `regalloc_irc` | 54.9% | 75.7% | 10.0% |
| `cfg_to_linear` | 58.4% | 78.5% | 8.8% |

## Interpretation

`caml_modify` is the biggest call-count target. Most calls do not need the
expensive GC-barrier work, but they still pay for the helper call and the
memory-model fence/store sequence. This is a strong argument for a fast inline
classification path with a call only for the real barrier cases, if the memory
model contract can be preserved.

`caml_initialize` is much easier: almost every call is either a young
destination or a non-young value. Only a tiny fraction needs the remembered-set
slow path. This looks like a good early inline-helper candidate.

`caml_obj_tag` is tiny and hot enough to keep on the allowlist for audited
direct lowering.

`caml_string_compare` and `caml_string_equal` are very frequent, but most calls
do real content work. For `caml_string_compare`, the new length histogram makes
full inlining more plausible than it looked from call counts alone: in
`typecore.ml`, 97.5% of non-pointer-equal compares have `min_len <= 15`.
That supports testing a generated short-string compare loop. A hybrid design
could inline small compares and keep a direct helper or `memcmp` path for the
rare larger compares.

`caml_modify_local` mostly falls back to `caml_modify`, so optimizing
`caml_modify` matters more than optimizing the local helper itself.

## Standalone string compare partial-inline benchmark

I added a standalone benchmark at:

```text
_compiler_binary_perf_current/string_compare_partial_inline_bench.c
```

It generates deterministic string pairs using the observed `typecore.ml`
`caml_string_compare` profile above: pointer-equal share, result classes, and
`min(len1, len2)` buckets. The benchmark validates every candidate against a
plain `memcmp` helper before timing it.

The broad custom-LLVM run used:

```sh
SDKROOT=$(xcrun --show-sdk-path)
eval "$(../../../scripts/agent-tmp-env)"
"$LLVM_PATH" -isysroot "$SDKROOT" -O3 -march=armv8.5-a \
  -Wall -Wextra -Werror -DDATASET_SIZE=65536 -DITERS=40 \
  _compiler_binary_perf_current/string_compare_partial_inline_bench.c \
  -o _compiler_binary_perf_current/string_compare_partial_inline_bench_custom_llvm
```

Raw outputs are:

```text
_compiler_binary_perf_current/string_compare_partial_inline_bench_custom_llvm_raw.csv
_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_raw.csv
_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_first_raw.csv
_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_last_raw.csv
```

Custom LLVM, broad random-mismatch run, median ns/compare:

| candidate | median ns | ratio vs helper |
| --- | ---: | ---: |
| check byte 0, then `memcmp` suffix | 14.712 | 0.891 |
| byte loop for `min_len <= 3`, else `memcmp` | 15.287 | 0.926 |
| byte loop for `min_len <= 2`, else `memcmp` | 15.631 | 0.947 |
| byte loop for `min_len <= 7`, else `memcmp` | 15.853 | 0.960 |
| inline same shape but always call `memcmp` | 16.345 | 0.990 |
| plain helper-shaped `memcmp` | 16.505 | 1.000 |
| check two bytes, then `memcmp` suffix | 17.913 | 1.085 |
| unrolled byte loop for `min_len <= 7`, else `memcmp` | 19.284 | 1.168 |
| byte loop for `min_len <= 15`, else `memcmp` | 20.949 | 1.269 |

Custom LLVM, focused random-mismatch run with more iterations:

| candidate | median ns | ratio vs helper |
| --- | ---: | ---: |
| byte loop for `min_len <= 3`, else `memcmp` | 14.118 | 0.871 |
| check byte 0, then `memcmp` suffix | 14.343 | 0.885 |
| byte loop for `min_len <= 7`, else `memcmp` | 15.732 | 0.971 |
| plain helper-shaped `memcmp` | 16.208 | 1.000 |
| inline same shape but always call `memcmp` | 16.343 | 1.008 |

Sensitivity runs changed only the first-different-byte placement for the
`memcmp` less/greater outcomes:

| mismatch placement | best candidate | median ns | ratio vs helper |
| --- | --- | ---: | ---: |
| always byte 0 | byte loop for `min_len <= 7`, else `memcmp` | 6.472 | 0.419 |
| always last byte | byte loop for `min_len <= 7`, else `memcmp` | 17.471 | 0.875 |

Interpretation:

- Do not inline a byte loop up to `min_len <= 15`; it is clearly worse even
  though the length histogram makes it tempting.
- The best robust candidates are:
  - check byte 0, then call `memcmp` on the suffix; and
  - byte loop for `min_len <= 3`, otherwise call `memcmp`.
- `min_len <= 7` can be better when the workload has many very short compares
  or first-byte mismatches, but it was less stable in the random-mismatch
  focused run.
- Checking two bytes before `memcmp` and switch-unrolled short loops are bad
  shapes in this benchmark.
- The current profile does not record the first differing byte. Before locking
  in a generated lowering, add a short-lived first-difference histogram to
  `caml_string_compare`; that decides between the `byte 0 + memcmp suffix`,
  `min_len <= 3`, and `min_len <= 7` shapes.

## OCaml string layout advantage

The runtime gives OCaml strings two useful properties:

- `String_val(s)` is word-aligned. Locally sampled strings were always
  8-byte aligned, but not always 16-byte aligned.
- `caml_alloc_string` allocates whole words, zeroes the last word, and stores
  the length-offset byte in the last byte of that word.

Relevant source:

```text
runtime/alloc.c
runtime/str.c
runtime/caml/mlvalues.h
```

For a 64-bit runtime:

```text
len=3  bytes: 61 62 63 00 00 00 00 04
len=7  bytes: 61 62 63 64 65 66 67 00
len=8  bytes: 61 62 63 64 65 66 67 68 00 00 00 00 00 00 00 07
```

This means:

- The byte at logical index `len` is always zero.
- Padding bytes before the final offset byte are zero for normally allocated
  strings/bytes.
- The final offset byte is not generally zero. It encodes
  `Bsize_wsize(wosize) - 1 - len`.

The offset byte matters. We cannot blindly compare the full padded word as the
lexicographic tie-break. Example:

```text
"abc"      -> 61 62 63 00 00 00 00 04
"abc\000" -> 61 62 63 00 00 00 00 03
```

The full word would order the shorter string after the longer string because
`04 > 03`, which is wrong for OCaml string comparison. Any word-at-a-time
generated compare must mask off bytes after `min_len` and then use the normal
length comparison when the masked prefix is equal.

I added two word-load candidates to the standalone benchmark:

- `word_le_7_then_memcmp`: for `min_len <= 7`, load one aligned 64-bit word,
  byte-swap to big-endian order, mask to `min_len`, compare, otherwise call
  `memcmp`.
- `word_prefix_then_memcmp`: always compare the first aligned 64-bit word
  masked/byte-swapped when possible; for `min_len > 8`, call `memcmp` on the
  suffix only if the first 8 bytes match.

Raw outputs:

```text
_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_words_raw.csv
_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_words_first_raw.csv
_compiler_binary_perf_current/string_compare_partial_inline_bench_focused_custom_llvm_words_last_raw.csv
```

Custom LLVM, focused random-mismatch run:

| candidate | median ns | ratio vs helper |
| --- | ---: | ---: |
| aligned word prefix, then `memcmp` suffix | 9.393 | 0.549 |
| aligned word compare for `min_len <= 7`, else `memcmp` | 10.400 | 0.608 |
| check byte 0, then `memcmp` suffix | 15.802 | 0.923 |
| plain helper-shaped `memcmp` | 17.115 | 1.000 |

Sensitivity:

| mismatch placement | best candidate | median ns | ratio vs helper |
| --- | --- | ---: | ---: |
| always byte 0 | check byte 0, then `memcmp` suffix | 3.854 | 0.321 |
| always last byte | aligned word compare for `min_len <= 7`, else `memcmp` | 10.336 | 0.539 |

Updated interpretation:

- Yes, OCaml strings have a real layout advantage over arbitrary C buffers.
- The most promising general generated shape is an aligned 64-bit prefix
  compare, with masking for `min_len < 8`, followed by `memcmp` for the suffix
  only if the first word is equal.
- This should be generated as OCaml-string-specific lowering, not as a generic
  `memcmp` replacement.
- The lowering must preserve the exact OCaml semantics for embedded zero bytes
  and length tie-breaks; the final offset byte is metadata, not string content.

## String compare Pareto candidates

I extended the standalone benchmark with lowering-shape candidates that look
closer to what we would generate in the LLVM backend:

- `word_prefix_then_memcmp`: compare the first aligned 64-bit word; if equal,
  call `memcmp` on the suffix.
- `word_inline_15_then_memcmp`: avoid `memcmp` for `min_len <= 15`; otherwise
  call `memcmp` on the whole string.
- `word_prefix_15_then_memcmp`: avoid `memcmp` for `min_len <= 15`; for longer
  strings, compare the first two words and call `memcmp` on the suffix only if
  those words match.
- `word_inline_31_then_memcmp`: avoid `memcmp` for `min_len <= 31`; otherwise
  call `memcmp` on the whole string.
- `word_prefix_31_then_memcmp`: avoid `memcmp` for `min_len <= 32`; for longer
  strings, compare the first four words and call `memcmp` on the suffix only if
  those words match.
- `word_loop_31_then_memcmp` / `word_loop_63_then_memcmp`: compact loop forms
  that avoid `memcmp` up to the threshold.

Raw outputs:

```text
_compiler_binary_perf_current/string_compare_pareto_custom_llvm_raw.csv
_compiler_binary_perf_current/string_compare_pareto_custom_llvm_first_raw.csv
_compiler_binary_perf_current/string_compare_pareto_custom_llvm_last_raw.csv
```

Approximate function text sizes are computed from adjacent Mach-O local symbol
addresses in:

```text
_compiler_binary_perf_current/string_compare_pareto_custom_llvm.o
```

Custom LLVM, typecore-weighted random mismatch positions:

| candidate | approx bytes | median ns | ratio vs helper |
| --- | ---: | ---: | ---: |
| `word_inline_31_then_memcmp` | 620 | 7.614 | 0.465 |
| `word_prefix_15_then_memcmp` | 312 | 7.671 | 0.468 |
| `word_prefix_31_then_memcmp` | 496 | 7.782 | 0.475 |
| `word_loop_31_then_memcmp` | 272 | 8.185 | 0.499 |
| `word_inline_15_then_memcmp` | 308 | 8.285 | 0.505 |
| `word_loop_63_then_memcmp` | 272 | 8.494 | 0.518 |
| `word_prefix_then_memcmp` | 240 | 8.511 | 0.519 |
| `word_le_7_then_memcmp` | 208 | 9.472 | 0.578 |
| plain helper-shaped `memcmp` | 136 | 16.391 | 1.000 |

Sensitivity medians, shown as `ns / ratio-vs-helper`:

| candidate | random | first-byte mismatch | last-byte mismatch |
| --- | ---: | ---: | ---: |
| `word_inline_31_then_memcmp` | 7.614 / 0.465 | 7.271 / 0.532 | 8.214 / 0.408 |
| `word_prefix_15_then_memcmp` | 7.671 / 0.468 | 6.454 / 0.472 | 7.910 / 0.393 |
| `word_prefix_31_then_memcmp` | 7.782 / 0.475 | 7.261 / 0.531 | 7.857 / 0.391 |
| `word_loop_31_then_memcmp` | 8.185 / 0.499 | 7.964 / 0.583 | 9.763 / 0.485 |
| `word_loop_63_then_memcmp` | 8.494 / 0.518 | 8.236 / 0.603 | 10.102 / 0.502 |
| `word_prefix_then_memcmp` | 8.511 / 0.519 | 7.567 / 0.554 | 11.124 / 0.553 |
| `first_byte_then_memcmp` | 14.219 / 0.867 | 3.862 / 0.283 | 16.887 / 0.840 |

Pareto interpretation:

- `word_prefix_15_then_memcmp` is the best small-ish candidate. It is almost
  tied with the fastest random-mismatch result while being much smaller than
  the straight-line `<=31` shape. It also covers 96.3% of aggregate
  non-pointer compares without calling `memcmp`.
- `word_prefix_31_then_memcmp` is the best performance-oriented candidate. It
  covers about 99.0% of aggregate non-pointer compares without calling
  `memcmp`, still avoids the `memcmp` call for long strings that differ in the
  first four words, and is smaller than the straight-line `word_inline_31`
  proxy.
- `word_loop_31_then_memcmp` is the best compact candidate. It is slower, but
  it has a small proxy code footprint and still removes almost all runtime
  helper calls.
- `word_loop_63_then_memcmp` may become attractive only if the real OCaml
  noalloc C-call boundary is extremely expensive, because the histogram says
  it only saves the last roughly 1.0% of non-pointer compares beyond `<=31`.
- Fully byte-loop-based shapes should not be used for this lowering.

Recommended integration order:

1. Prototype `word_prefix_15_then_memcmp` first. It is the lowest-risk
   high-payoff lowering: two aligned 64-bit words, correct masking, normal
   length tie-break, suffix `memcmp` for longer strings.
2. If code size is acceptable, promote to `word_prefix_31_then_memcmp`.
   This is the likely final performance target for compiler workloads.
3. Keep `word_loop_31_then_memcmp` as the fallback design if straight-line
   expansion causes unacceptable code-size growth in generated compiler code.
4. Do not start with `<=63`; collect a real full-compiler code-size/perf
   result first.

## Next experiments

1. Prototype inline `caml_initialize` fast paths with a slow call for
   old-to-young writes.
2. Prototype inline `caml_obj_tag`.
3. Prototype a guarded `caml_modify` fast path that preserves the required
   acquire fence/release store semantics.
4. Separately test noalloc C-call boundary overhead for `caml_string_compare`
   and `caml_string_equal`, because the content work remains real but the call
   frequency is high.
