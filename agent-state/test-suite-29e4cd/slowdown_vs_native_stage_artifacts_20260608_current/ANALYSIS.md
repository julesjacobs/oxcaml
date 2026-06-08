# Remaining Slowdowns Analysis

This note reflects the current `jujacobs/llvm-backend` checkout after the
string compare fallback change and the follow-up string equality specialization.

The numbers below use the installed compiler at `_install/bin/ocamlopt.opt` and
the agent-local LLVM at `../llvm-build/bin/clang`.

During the benchmark refresh, self-stage caught a bug in the equality
specialization for strings of length 17 through 24: the second word compare was
given more than 8 bytes. That is fixed in the current numbers below. A fresh
native comparison install and a fresh LLVM self-stage install were rebuilt after
that fix.

## Current Microbench Summary

Broad representative microbench run, `PAIRS=3`:

- Cases: `44`
- Geomean LLVM/native: `0.660604x`
- Median LLVM/native: `0.681666x`
- Best ratio: `0.266189x`
- Worst ratio: `1.076832x`

Largest remaining slowdowns:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `try_raise_cross_function_caught` | 0.4794s | 0.5162s | 1.0768x |
| `ref_option_churn` | 0.0134s | 0.0143s | 1.0710x |
| `env_find_same_layered_int_key` | 0.3718s | 0.3945s | 1.0610x |
| `layered_try_raise_hit_only` | 0.5969s | 0.6281s | 1.0522x |
| `record_mutate_old_to_young` | 0.0100s | 0.0104s | 1.0385x |

Important non-slowdowns:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `closure_call_in_try_hit` | 0.0702s | 0.0680s | 0.9694x |
| `direct_call_in_try_hit` | 0.0672s | 0.0443s | 0.6584x |
| `try_with_string_compare_hit` | 0.2509s | 0.1340s | 0.5340x |
| `higher_order_fold_string_keys` | 1.1001s | 0.8182s | 0.7438x |

## String Compare Result

The old string-search slowdown bucket is resolved by the current string compare
fallback change.

Current broad run numbers, `PAIRS=3`:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `string_tree_first_byte_diff` | 0.3965s | 0.1461s | 0.3685x |
| `array_binary_search_string` | 0.3498s | 0.3238s | 0.9257x |
| `string_map_equal_content` | 0.2610s | 0.1418s | 0.5434x |
| `string_compare_short_equal_loop` | 0.3365s | 0.0896s | 0.2662x |
| `string_compare_short_late_diff_loop` | 0.2950s | 0.0917s | 0.3107x |
| `string_tree_prefix_heavy` | 0.3531s | 0.1791s | 0.5072x |
| `try_with_string_compare_hit` | 0.2509s | 0.1340s | 0.5340x |

The compare specialization strategy is:

- for `min_len <= 16`, stay in the inline word compare path;
- for `min_len > 16`, compare the first word inline;
- if the first word matches, call direct `memcmp` on `s1 + 8`, `s2 + 8`,
  `min_len - 8`;
- apply the OCaml length tiebreak after `memcmp` returns zero.

This fixes the previous bad pattern where the LLVM path decoded both string
lengths, missed the `min_len <= 15` inline cutoff, and then called
`caml_string_compare`, which decoded the lengths again before doing `memcmp`.

## String Equality Result

String equality now gets the same broad treatment, but with a larger no-call
inline window because equality does not need lexicographic ordering:

- pointer equality returns true;
- unequal lengths return false;
- zero length returns true;
- equal lengths through 24 bytes use inline word equality only;
- longer equal-length strings compare the first word inline and then call direct
  `memcmp` on the suffix.

The 24-byte inline window matters for compiler-like keys such as
`fold_shared_key_17`. With only first-word-plus-`memcmp`, the common-prefix case
paid a `memcmp` call in the inner loop. With the 24-byte path, those 18-byte
keys stay fully inline.

Focused equality run, `PAIRS=5`:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `higher_order_fold_string_keys` | 1.1197s | 0.8268s | 0.7384x |
| `hash_lookup_string_equal` | 0.7017s | 0.3074s | 0.4381x |
| `string_equal_guarded_dispatch` | 0.1206s | 0.0520s | 0.4310x |
| `string_assoc_find_hit` | 0.5655s | 0.2425s | 0.4288x |
| `string_map_interned_keys` | 0.2509s | 0.1794s | 0.7148x |

Broad run equality-related cases, `PAIRS=3`:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `higher_order_fold_string_keys` | 1.1001s | 0.8182s | 0.7438x |
| `hash_lookup_string_equal` | 0.6893s | 0.3079s | 0.4466x |
| `string_equal_guarded_dispatch` | 0.1353s | 0.0497s | 0.3669x |
| `string_assoc_find_hit` | 0.5688s | 0.2380s | 0.4184x |
| `string_map_interned_keys` | 0.2522s | 0.1758s | 0.6972x |

The LLVM assembly summary reports `0` `caml_string_equal` refs for the current
top equality cases, so the helper fallback is no longer on the hot path.

## Current Minibench Summary

Minibench run, `SAMPLES=3`, `WARMUPS=1`.

Raw results:

- `minibench-results-20260608-current.json`

Aggregate:

- Cases: `10`
- Geomean LLVM/native runtime: `0.933475x`
- Median LLVM/native runtime: `0.945591x`
- Total LLVM/native runtime: `0.942936x`
- Total runtime speedup: `6.05%`
- Total native runtime: `2.4793s`
- Total LLVM runtime: `2.3378s`
- Total LLVM/native compile-time ratio: `1.494018x`

Per-case runtime ratios:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `almabench` | 0.6860s | 0.5926s | 0.8638x |
| `bdd` | 0.1016s | 0.1007s | 0.9913x |
| `boyer` | 0.0863s | 0.0903s | 1.0463x |
| `boyer_no_exc` | 0.0673s | 0.0649s | 0.9635x |
| `fft` | 0.9704s | 0.9847s | 1.0147x |
| `kb` | 0.1293s | 0.1143s | 0.8834x |
| `kb_no_exc` | 0.1316s | 0.1134s | 0.8618x |
| `nucleic` | 0.0981s | 0.0803s | 0.8180x |
| `raytrace` | 0.1588s | 0.1473s | 0.9277x |
| `splay` | 0.0498s | 0.0494s | 0.9927x |

The remaining minibench slowdowns are small:

- `boyer`: `1.0463x`
- `fft`: `1.0147x`

Follow-up targeted rerun, `CASES=boyer,fft`, `SAMPLES=9`, `WARMUPS=2`:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `boyer` | 0.0860s | 0.0900s | 1.0473x |
| `fft` | 0.9704s | 0.9269s | 0.9552x |

This makes `fft` look like noise in the original 3-sample run, not a stable
slowdown. `boyer` reproduces as a real small slowdown.

`boyer` is specifically an exception-modeling slowdown:

- `boyer` uses exceptions for failed unification and catches them in
  `rewrite_with_lemmas`.
- `boyer_no_exc` is the same benchmark shape with the hot failure path encoded
  as `option`, and LLVM is faster there: `0.9635x`.
- The LLVM IR for `boyer` has two landingpads and three
  `llvm.aarch64.oxcaml.trap.recover` uses. `boyer_no_exc` has none.
- In assembly, the two relevant exception helpers grow substantially under
  LLVM:

| function | LLVM lines | native lines | notes |
|---|---:|---:|---|
| `Boyer.unify1` | 235 | 166 | pushtrap/recover plus invoke path around `caml_equal` |
| `Boyer.rewrite_with_lemmas` | 216 | 110 | pushtrap/recover plus three invokes under the handler |
| `Boyer_no_exc.unify1` | 110 | 153 | no exception handler; LLVM is smaller |
| `Boyer_no_exc.rewrite_with_lemmas` | 86 | 92 | no exception handler; LLVM is slightly smaller |

The evidence file
`minibench_suite/inspect/boyer_exception_functions.txt` contains the extracted
LLVM IR plus LLVM/native assembly for those helper functions.

## Current Compiler Benchmark Summary

Compiler benchmark run, `REPETITIONS=5`, no extra `ocamlopt` flags.

Raw results:

- `compiler-bench-current-vs-native-20260608-173120.json`

Setup:

- Native compiler: `_native_install/bin/ocamlopt.opt`
- LLVM-built compiler: `_llvm_self_stage_install/bin/ocamlopt.opt`
- Native build log checked clean before benchmarking.
- LLVM self-stage rebuild completed successfully and had fresh wrapper IR
  activity.

Round-total median:

- Native total: `14.2102s`
- LLVM total: `13.8403s`
- LLVM/native: `0.973969x`
- LLVM speedup: `2.67%`

Per-module medians:

| module | native | llvm | llvm/native | LLVM speedup |
|---|---:|---:|---:|---:|
| `cfg_selectgen` | 0.5543s | 0.5318s | 0.9594x | 4.24% |
| `llvmize` | 1.7339s | 1.7314s | 0.9986x | 0.14% |
| `translcore` | 1.3407s | 1.3319s | 0.9934x | 0.66% |
| `ctype` | 2.5302s | 2.4791s | 0.9798x | 2.06% |
| `env` | 1.6620s | 1.6413s | 0.9876x | 1.26% |
| `typecore` | 4.9016s | 4.7169s | 0.9623x | 3.92% |
| `typemod` | 1.4915s | 1.4375s | 0.9638x | 3.76% |

## Remaining Slowdown Buckets

### 1. Handler-Live Root Traffic

Affected cases:

- `layered_try_raise_hit_only`: `1.0522x`
- `env_find_same_layered_int_key`: `1.0610x`
- minibench class from previous runs: `boyer`

These are now the clearest non-noisy slowdowns. They are independent of string
handling and still fit the handler-live-root bucket. The LLVM path is correct
enough for validation, but handler-live values can still force explicit root
slots and extra stack traffic compared with native.

Likely next check: inspect whether current exnroot placement still creates
duplicate stores or redundant reloads in `layered_try_raise_hit_only` and
`env_find_same_layered_int_key`, then compare with the Boyer functions from the
minibench evidence.

### 2. Exception Path Without Root Traffic

Affected case:

- `try_raise_cross_function_caught`: `1.0768x`

Previous stage inspection showed the relevant function has statepoints but no
GC-live operands, relocates, or exnroots. That points at pushtrap/recover and
frame setup cost, not root placement.

Known local shape from the earlier artifact:

- LLVM uses a bulkier trap/frame sequence.
- Native's taken exception path is tighter.
- The difference persists even when no root traffic is involved.

Likely next check: keep this as the no-root exception-control-transfer
representative. Compare pushtrap setup, recover path, and stack frame layout
against native after the current pushtrap lowering changes.

### 3. Small Mutation/Allocation Cases

Affected cases:

- `ref_option_churn`: `1.0710x`
- `record_mutate_old_to_young`: `1.0385x`

`ref_option_churn` is now the second-worst ratio, but the absolute runtime is
very small (`~14ms`), so noise is a real risk. It should be scaled up or rerun
with more pairs before spending time on assembly-level diagnosis.

## Current Takeaways

The string compare and string equality slowdowns have turned into wins:

- compare avoids the failed-inline-then-generic-helper shape;
- equality avoids the helper entirely for the common compiler-key lengths;
- the broad microbench geomean improved to `0.660604x` LLVM/native.
- minibenches are `6.05%` faster by total runtime;
- the LLVM-built compiler is `2.67%` faster by round-total median.

The next slowdown worth facing is no longer string-related. The most useful
next target is `layered_try_raise_hit_only`, because it is the largest stable
remaining micro slowdown and belongs to the handler-live/exception machinery.
For larger programs, the stable remaining slowdown is `boyer`; `fft` flipped to
a win in a 9-sample targeted rerun.
