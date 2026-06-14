# Numbers

Last compacted: 2026-05-31.

This keeps the benchmark values previously recorded in this file, without the running history. Ratios are LLVM/native; lower than `1.0` means LLVM was faster.

## Compiler Binary

Compiler-binary runs compare a normal native-built `ocamlopt.opt` against an LLVM-built `ocamlopt.opt` while both compile representative compiler source files with the normal backend.

| run | geomean | median | min | max | note |
| --- | ---: | ---: | ---: | ---: | --- |
| Old baseline before string compare lowering | 1.0897 | | | | valid |
| After conservative short string/bytes compare lowering | 1.0766 | | | | valid |
| After `caml_modify` Candidate 1 fast path | 1.0651 | | | | valid |
| Clean native vs no-scalar LLVM-built, 2026-05-27 | 1.0618 | 1.0561 | 1.0446 | 1.0968 | valid |
| Post-validation stage2, 2026-05-27 | 1.0539 | 1.0502 | 1.0444 | 1.0722 | valid |
| No-domain-exn-store stage2, 2026-05-28 | 1.0478 | 1.0422 | 1.0331 | 1.0686 | valid |
| Stage2 validated trap recovery, 2026-05-28 | 1.0418 | 1.0406 | 1.0294 | 1.0690 | valid |
| After stage2 skip, 2026-05-28 | 1.0383 | 1.0322 | 1.0222 | 1.0788 | valid |
| After undefined-root invariant fix, 2026-05-29 | 0.9983 | 0.9910 | 0.9833 | 1.0398 | valid; full normal LLVM-backend and self-stage2 passed |
| After raw heap-address canonicalization, 2026-05-29 | 0.9866 | 0.9849 | 0.9709 | 1.0022 | valid; 7 pairs |
| Post-review rerun | 1.0079 | 1.0033 | 0.9920 | 1.0433 | invalid/suspect |
| No-scalar clone with LLVM-built `_install` | 1.0085 | 1.0025 | 0.9997 | 1.0374 | invalid; supposed native side used LLVM backend |
| Design 1 / stack-growth prototype | 1.0090 | | | | invalid/suspect |

Recorded changes from old baseline:

| comparison | geomean change |
| --- | ---: |
| String compare lowering | -0.0131 |
| `caml_modify` Candidate 1 | -0.0115 |
| Clean-native vs no-scalar LLVM-built | -0.0279 |
| Post-validation stage2 | -0.0358 |
| No-domain-exn-store stage2 | -0.0419 |
| After stage2 skip | -0.0514 |
| Undefined-root invariant fix | -0.0914 |
| Raw heap-address canonicalization | -0.1031 |

Latest compiler-binary per-file medians, after raw heap-address canonicalization:

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

After undefined-root invariant fix:

| file | native | LLVM | LLVM/native |
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

Stage2 validated trap recovery:

| file | native | LLVM | LLVM/native |
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

After stage2 skip:

| file | native | LLVM | LLVM/native |
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

No-domain-exn-store stage2:

| file | native | LLVM | LLVM/native |
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

Post-validation stage2:

| file | native | LLVM | LLVM/native |
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

Clean-native vs no-scalar LLVM-built:

| file | native | LLVM | LLVM/native |
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

## Representative Microbenchmarks

| run | cases | geomean | median | min | max | note |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| Design 1 post-full-validation | 36 | 0.9075 | 1.0143 | 0.2958 | 1.5118 | all reported try cases had `wrap_try_refs=0` |
| Earlier no-domain-exn-store comparison | 36 | 0.9131 | 0.9965 | 0.2870 | 1.6201 | before removing hot exn-handler stores |
| No-domain-exn-store | 36 | 0.8861 | 1.0437 | 0.2991 | 1.5784 | previous current run |
| Stage2 validated trap recovery | 43 | 0.8816 | 0.9758 | 0.2973 | 1.4881 | full normal and self-stage2 passed |
| Explicit exception roots and non-volatile slots | 43 | 0.8765 | 1.0090 | 0.2826 | 1.2578 | all measured `_wrap_try` refs zero |
| After stage2 skip | 43 | 0.8902 | 0.9926 | 0.3021 | 1.5471 | all listed current cases had `wrap_try_refs=0` |
| After undefined-root invariant fix | 43 | 0.8558 | 0.9416 | 0.2994 | 1.4571 | full normal LLVM-backend and self-stage2 passed |
| After raw heap-address canonicalization | 43 | 0.8144 | 0.9588 | 0.2830 | 1.0702 | full normal LLVM-backend and self-stage2 passed |

Worst slowdowns after raw heap-address canonicalization:

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

Worst slowdowns after undefined-root invariant fix:

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

Selected exception/dispatch cases after undefined-root invariant fix:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `try_find_hit_deep` | 0.6490s | 0.5707s | 0.879 |
| `try_find_miss_rare` | 0.8188s | 0.7492s | 0.915 |
| `try_raise_cross_function_caught` | 0.4466s | 0.4375s | 0.979 |
| `direct_call_in_try_hit` | 0.0562s | 0.0543s | 0.967 |
| `closure_call_in_try_hit` | 0.0614s | 0.0629s | 1.024 |
| `closure_call_in_nested_try_hit` | 0.0459s | 0.0432s | 0.942 |
| `variant_dispatch_with_int_payload` | 0.1036s | 0.0766s | 0.739 |

Selected after-stage2-skip slowdowns:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `direct_call_in_try_hit` | 0.0640s | 0.0991s | 1.547 |
| `env_find_same_layered_hit` | 0.3360s | 0.4624s | 1.376 |
| `string_tree_prefix_heavy` | 0.3361s | 0.4582s | 1.363 |
| `closure_call_in_try_hit` | 0.0686s | 0.0888s | 1.294 |
| `try_with_string_compare_hit` | 0.2595s | 0.3234s | 1.246 |
| `closure_call_in_nested_try_hit` | 0.0509s | 0.0621s | 1.219 |

Selected no-domain-exn-store comparison:

| case | earlier LLVM/native | later LLVM/native |
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

## Focused Microbenchmarks

Mutation microbenchmarks after `caml_modify` Candidate 1:

| case | native | LLVM | LLVM/native |
| --- | ---: | ---: | ---: |
| `record_update` | 0.0295s | 0.0160s | 0.557 |
| `int_ref_inc` | 0.1453s | 0.1455s | 0.999 |
| `float_ref_inc` | 0.3694s | 0.3597s | 0.978 |
| `list_build_sum` | 0.4337s | 0.4260s | 0.980 |
| `object_call_min` | 0.1976s | 0.2359s | 1.198 |

`caml_obj_tag` focused benchmark:

| run | native | LLVM | LLVM/native | LLVM wrapper refs |
| --- | ---: | ---: | ---: | ---: |
| Before inline lowering | 0.1673s | 0.3860s | 2.307 | 4 |
| After inline lowering | 0.1974s | 0.0992s | 0.503 | 0 |

Focused stack-pair boundary fix:

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

Design 1 focused old slow cases:

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

## Runtime Helper Counts

| helper | calls | observation |
| --- | ---: | --- |
| `caml_modify` | 88,484,839 | About 10.2% need remembered-set or marking work. |
| `caml_string_compare` | 27,695,126 | 93.1% reach `memcmp`. |
| `caml_string_equal` | 5,921,448 | 82.0% reach the content loop. |
| `caml_obj_tag` | 2,888,252 | Tiny helper; inlined by the LLVM backend on AArch64. |
| `caml_initialize` | 1,384,736 | Only 0.5% are old-to-young remembered-set writes. |
| `caml_modify_local` | 5,738,361 | 97.4% fall back to `caml_modify`. |

## Validation Counts

| checkpoint | result |
| --- | --- |
| Design 1 full `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` | 6730 passed, 295 skipped, 0 failed |
| Stack-pair boundary fix full `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` | 6730 passed, 295 skipped, 0 failed, 7025 considered |
| Stage2-validated trap-recovery normal suite | 6740 passed, 296 skipped, 0 failed |
| Stage2-validated trap-recovery self-stage2 | 6714 passed, 283 skipped, 0 failed |
| Explicit exception roots focused suites | `llvm-codegen` 89/3/0, `compaction` 8/3/0, `gc-roots` 10/0/0, `basic` 82/0/0, `exception-extra-args` 6/0/0, `backtrace` 67/6/0, `async-exns` 5/0/0, `match-exception` 16/0/0, `runtime-C-exceptions` 2/0/0, `raise-counts` 2/0/0 |
| Undefined-root invariant fix normal suite | 6740 passed, 296 skipped, 0 failed |
| Undefined-root invariant fix self-stage2 | 6714 passed, 283 skipped, 0 failed |
| Raw heap-address canonicalization focused `typing-layouts-iarrays` | 81 passed, 0 failed |
| Raw heap-address canonicalization focused `llvm-codegen` | 89 passed, 3 skipped, 0 failed |
| Raw heap-address canonicalization full normal suite | 6743 passed, 296 skipped, 0 failed |
| Raw heap-address canonicalization full self-stage2 | 6717 passed, 283 skipped, 0 failed |

## Regmask Remainder Splitting

2026-06-01 local experiment on `jujacobs/llvm-backend-stage2`, using the
representative microbenchmark harness from commit `2b789140bd` and compiler
binary benchmark harness against installed compilers. The original PR #30
variant left OxCaml regmask-crossing region-split remainders at `RS_New`; this
preserved the `direct_call_in_try_hit` speedup but caused a compile-time
pathology (`misc.ll` timed out after 60s, versus about 1s without the flag).

Changing the experiment to mark those remainders `RS_Split2` instead preserves
the speedup while avoiding the full region-splitting retry loop. The cleaned-up
version adds an explicit `RS_CallSplit` stage with the same bounded behavior,
so the reason is visible in the stage lattice instead of overloading
`RS_Split2`:

| measurement | baseline | PR #30 `RS_New` | bounded `RS_Split2` | explicit `RS_CallSplit` |
| --- | ---: | ---: | ---: | ---: |
| micro geomean LLVM/native | 0.8428 | 0.8287 | 0.8315 | 0.8265 |
| micro median LLVM/native | 0.9534 | 0.9637 | 0.9643 | 0.9621 |
| micro max LLVM/native | 1.7968 | 1.3484 | 1.3434 | 1.3752 |
| `direct_call_in_try_hit` | 1.7968 | 1.0143 | 1.0142 | 1.0285 |
| `closure_call_in_try_hit` | 1.2591 | 1.2225 | 1.2124 | 1.2315 |
| `closure_call_in_nested_try_hit` | 1.3665 | 1.3484 | 1.3434 | 1.3752 |
| `higher_order_fold_string_keys` | 1.0682 | 0.9897 | 0.9463 | 1.0719 |

Focused compile-time check on generated `.ocamlcommon.objs/native/misc.ll`:

| clang mode | time |
| --- | ---: |
| no flag | 2.354s |
| PR #30 `RS_New` flag | timed out after 60s |
| bounded `RS_Split2` flag | 1.136s |

Compiler-binary benchmark, native-built compiler versus LLVM-built compiler:

| compiler build | geomean | median | min | max |
| --- | ---: | ---: | ---: | ---: |
| existing self-stage2 LLVM compiler | 1.0251 | 1.0234 | 0.9898 | 1.0672 |
| rebuilt with bounded `RS_Split2` flag | 0.9829 | 0.9849 | 0.9580 | 1.0053 |
| rebuilt with explicit `RS_CallSplit` flag | 0.9782 | 0.9732 | 0.9624 | 1.0154 |

Per-module compiler-binary ratios, existing LLVM-built compiler to explicit
`RS_CallSplit` compiler:

| module | before | after |
| --- | ---: | ---: |
| `env.ml` | 1.0099 | 0.9624 |
| `ctype.ml` | 1.0672 | 0.9731 |
| `typecore.ml` | 1.0420 | 0.9772 |
| `translcore.ml` | 1.0183 | 0.9866 |
| `typemod.ml` | 1.0234 | 0.9670 |
| `cfg_to_linear.ml` | 1.0516 | 1.0154 |
| `cfg_selectgen.ml` | 0.9898 | 0.9696 |
| `llvmize.ml` | 1.0266 | 0.9718 |
| `regalloc_irc.ml` | 0.9996 | 0.9819 |

Regmask child-classification follow-up, tested and rejected:

| measurement | strict regmask-progress child classification | call-free child `RS_New`, call-crossing child `RS_CallSplit` |
| --- | ---: | ---: |
| micro geomean LLVM/native | not run full suite | 0.8307 |
| micro median LLVM/native | not run full suite | 0.9702 |
| micro max LLVM/native | not run full suite | 1.3456 |
| `direct_call_in_try_hit` | 1.6607 | 1.0170 |
| `closure_call_in_try_hit` | 1.2413 | 1.2329 |
| `closure_call_in_nested_try_hit` | 1.3521 | 1.3456 |

Conclusion: the strict rule is wrong at the current `splitAroundRegion` hook,
because the ordinary region split may not reduce call-regmask crossings before
the later block/local split gets a chance to extract call-free pieces. The
weaker child classification is neutral to slightly worse than plain
`RS_CallSplit`, so the source was restored to plain `RS_CallSplit`.
