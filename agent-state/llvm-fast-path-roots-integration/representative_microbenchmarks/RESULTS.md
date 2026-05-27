# Representative Microbenchmark Results

These microbenchmarks probe remaining native-vs-LLVM generated-code
differences using small compiler-like shapes. They are intended to complement
the larger compiler-binary benchmark and the older focused slow-case suites.

The suite was run with the installed compiler from this checkout, compiling
each case twice: once with the normal native backend and once with
`-llvm-backend`. The timed binaries were then run with the same benchmark
arguments and interleaved native/LLVM timing pairs.

## Commands

Full first pass:

```sh
eval "$(../../../scripts/agent-tmp-env)"
PAIRS=3 LLVM_PATH="$LLVM_PATH" \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run.sh \
  2>&1 | tee agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_all_20260526_213759.log
cp agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/.build/summary.json \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_all_20260526_213759.json
```

Focused rerun for the larger or more interesting gaps:

```sh
eval "$(../../../scripts/agent-tmp-env)"
CASES=string_tree_prefix_heavy,string_map_equal_content,string_equal_guarded_dispatch,try_with_string_compare_hit,closure_call_in_try_hit,direct_call_in_try_hit,hash_lookup_string_equal,variant_dispatch_with_string_payload \
PAIRS=7 LLVM_PATH="$LLVM_PATH" \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run.sh \
  2>&1 | tee agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_focused_20260526_214010.log
cp agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/.build/summary.json \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_focused_20260526_214010.json
```

## Focused Results

| Case | Native median | LLVM median | LLVM/native | Wrapper refs | `_wrap_try` refs |
| --- | ---: | ---: | ---: | ---: | ---: |
| `variant_dispatch_with_string_payload` | 0.0542s | 0.2633s | 4.861x | 13 | 0 |
| `string_equal_guarded_dispatch` | 0.1350s | 0.2598s | 1.924x | 13 | 0 |
| `direct_call_in_try_hit` | 0.0567s | 0.0994s | 1.751x | 13 | 4 |
| `try_with_string_compare_hit` | 0.2671s | 0.3611s | 1.352x | 39 | 4 |
| `closure_call_in_try_hit` | 0.0733s | 0.0979s | 1.335x | 13 | 4 |
| `string_tree_prefix_heavy` | 0.3533s | 0.4647s | 1.315x | 31 | 0 |
| `hash_lookup_string_equal` | 0.7130s | 0.8542s | 1.198x | 25 | 0 |
| `string_map_equal_content` | 0.2715s | 0.3172s | 1.168x | 43 | 0 |

## Full First-Pass Results

| Case | Native median | LLVM median | LLVM/native | Wrapper refs | `_wrap_try` refs |
| --- | ---: | ---: | ---: | ---: | ---: |
| `variant_dispatch_with_string_payload` | 0.0553s | 0.2626s | 4.749x | 13 | 0 |
| `string_equal_guarded_dispatch` | 0.1342s | 0.2551s | 1.901x | 13 | 0 |
| `direct_call_in_try_hit` | 0.0612s | 0.1014s | 1.657x | 13 | 4 |
| `closure_call_in_try_hit` | 0.0729s | 0.0980s | 1.345x | 13 | 4 |
| `try_with_string_compare_hit` | 0.2668s | 0.3548s | 1.330x | 39 | 4 |
| `string_tree_prefix_heavy` | 0.3589s | 0.4423s | 1.232x | 31 | 0 |
| `string_map_equal_content` | 0.2697s | 0.3299s | 1.223x | 43 | 0 |
| `hash_lookup_string_equal` | 0.7101s | 0.8441s | 1.189x | 25 | 0 |
| `array_binary_search_string` | 0.3761s | 0.4390s | 1.167x | 33 | 0 |
| `higher_order_fold_string_keys` | 1.1811s | 1.3461s | 1.140x | 31 | 0 |
| `string_tree_first_byte_diff` | 0.4271s | 0.4828s | 1.130x | 36 | 0 |
| `string_assoc_find_hit` | 0.5978s | 0.6702s | 1.121x | 33 | 0 |
| `list_lookup_string_compare` | 0.8953s | 0.9734s | 1.087x | 25 | 0 |
| `nested_scope_lookup` | 0.1693s | 0.1840s | 1.086x | 58 | 0 |
| `string_map_interned_keys` | 0.2484s | 0.2661s | 1.071x | 38 | 0 |
| `record_mutate_old_to_young` | 0.0088s | 0.0092s | 1.053x | 5 | 0 |
| `persistent_map_update_lookup` | 0.1325s | 0.1390s | 1.049x | 38 | 0 |
| `try_find_hit_deep` | 0.6290s | 0.6400s | 1.017x | 25 | 4 |
| `try_int_find_hit` | 0.7018s | 0.7101s | 1.012x | 25 | 4 |
| `record_mutate_old_to_immediate` | 0.0434s | 0.0435s | 1.001x | 5 | 0 |
| `ref_option_churn` | 0.0141s | 0.0139s | 0.989x | 5 | 0 |
| `try_find_miss_rare` | 0.8277s | 0.8093s | 0.978x | 25 | 4 |
| `try_find_multiple_handlers` | 0.5859s | 0.5690s | 0.971x | 27 | 4 |
| `try_find_cold_handler_large_body` | 0.7727s | 0.7448s | 0.964x | 25 | 4 |
| `array_set_young_values` | 0.0267s | 0.0204s | 0.764x | 13 | 0 |
| `env_find_same_mini` | 0.1258s | 0.0877s | 0.697x | 41 | 4 |
| `int_tree_find_same_shape` | 0.1477s | 0.0682s | 0.461x | 5 | 0 |

## Interpretation

String-heavy shapes remain the strongest compiler-representative remaining
bucket. The focused string cases are 1.17x to 1.92x slower under LLVM, and the
synthetic variant-dispatch string payload case is 4.86x slower.

The prefix-heavy string tree is worse than the first-byte-different string
tree, 1.32x vs 1.13x. That confirms that long shared prefixes matter, but the
first-byte-different case is still slower, so helper-call and wrapper overhead
also matters.

`String.equal` guarded dispatch is a good target case because compiler code has
many attribute, primitive-name, and mode-name checks of that form. The
`variant_dispatch_with_string_payload` case should be treated as a stress test,
not a direct compiler estimate.

Plain local-handler lookup shapes are not currently the main slowdown source.
`try_find_hit_deep`, `try_find_multiple_handlers`, `try_find_miss_rare`, and
`try_int_find_hit` are around parity or faster. `_wrap_try` by itself is not
enough to explain the remaining compiler-binary gap.

Direct and closure calls inside a local handler are still slower. The focused
direct-call case is 1.75x slower and the closure-call case is 1.34x slower.
This points at call-boundary cost in a handler context, especially when combined
with other helper-heavy code.

Mutation/write-barrier shapes are not the next obvious target. Old-to-immediate
stores are at parity, old-to-young stores are only 1.05x slower, ref option
churn is at parity, and young array stores are faster under LLVM.

The control cases are useful guardrails. `int_tree_find_same_shape` and the
small `env_find_same_mini` are faster under LLVM, which means the suite is not
just measuring a blanket LLVM penalty. The `env_find_same_mini` shape should not
be over-interpreted as a model of real `Env` code.

## Assembly Counters

The focused run also saved assembly under `.build/inspect`. A few high-signal
counts:

| Case | Backend | Assembly lines | Wrapper refs | String compare refs | String equal refs | `_wrap_try` refs | Recover refs |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `variant_dispatch_with_string_payload` | native | 877 | 0 | 2 | 1 | 0 | 0 |
| `variant_dispatch_with_string_payload` | LLVM | 1306 | 13 | 6 | 5 | 0 | 0 |
| `string_equal_guarded_dispatch` | native | 911 | 0 | 0 | 5 | 0 | 0 |
| `string_equal_guarded_dispatch` | LLVM | 1306 | 13 | 0 | 9 | 0 | 0 |
| `direct_call_in_try_hit` | native | 751 | 0 | 0 | 0 | 0 | 0 |
| `direct_call_in_try_hit` | LLVM | 1207 | 13 | 0 | 0 | 4 | 10 |
| `try_with_string_compare_hit` | native | 1504 | 0 | 1 | 0 | 0 | 0 |
| `try_with_string_compare_hit` | LLVM | 3094 | 39 | 5 | 0 | 4 | 10 |

The assembly counters match the benchmark story: string helper references and
generic wrapper references remain visible in the LLVM output, while handler
wrapping only becomes costly in some call-boundary shapes.

## Integer Variant Dispatch Check

After inspecting `variant_dispatch_with_string_payload`, we added two integer
payload variants:

- `variant_dispatch_with_int_payload`: same structure, but `eval` compares int
  payloads and remains `[@inline never]`.
- `variant_dispatch_with_int_payload_inline`: same int-payload structure, but
  `eval` is allowed to inline into the loop.

Command:

```sh
eval "$(../../../scripts/agent-tmp-env)"
CASES=variant_dispatch_with_string_payload,variant_dispatch_with_int_payload,variant_dispatch_with_int_payload_inline \
PAIRS=9 LLVM_PATH="$LLVM_PATH" \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run.sh \
  2>&1 | tee agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run_variant_int_inline_compare_20260526_215428.log
cp agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/.build/summary.json \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_variant_int_inline_compare_20260526_215428.json
```

Results:

| Case | Native median | LLVM median | LLVM/native | Wrapper refs | `_wrap_try` refs |
| --- | ---: | ---: | ---: | ---: | ---: |
| `variant_dispatch_with_string_payload` | 0.0536s | 0.2472s | 4.615x | 13 | 0 |
| `variant_dispatch_with_int_payload` | 0.1074s | 1.0998s | 10.240x | 5 | 0 |
| `variant_dispatch_with_int_payload_inline` | 0.0764s | 0.0659s | 0.862x | 5 | 0 |

Interpretation:

The non-inlined int-payload variant is not evidence that integer variant
dispatch is intrinsically bad in LLVM. The `eval` body itself is slightly
better in LLVM assembly than native: LLVM uses conditional selects for the
integer comparisons, while native uses branches. The slowdown appears when that
tiny body is kept as a separate call in the inner loop.

When `eval` is allowed to inline, LLVM becomes faster than native on the same
integer variant dispatch shape. So the string-payload benchmark is mixing two
effects: string helper-call boundary overhead and tiny non-inlined function-call
overhead. The int-payload check isolates the second effect.
