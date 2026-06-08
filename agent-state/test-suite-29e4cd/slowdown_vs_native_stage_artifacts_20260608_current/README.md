# Slowdown vs Native Stage Artifacts (Current)

- agent checkout: `/Users/julesjacobs/git/oxcaml-llvm/agents/test-suite-29e4cd/oxcaml`
- git head: `0d68a665d2f89920bc656f495a73c75e84e4617a`
- generated at: `2026-06-08 16:17:09 -0400`
- benchmark source: fresh micro/minibench rerun from current `_install/bin/ocamlopt.opt` and agent-local LLVM wrapper

## Covered Slowdowns
| case | suite | native s | llvm s | llvm/native | case dir |
| --- | --- | ---: | ---: | ---: | --- |
| `hash_lookup_string_equal` | micro | 0.7277 | 0.8134 | 1.1178x | `cases/hash_lookup_string_equal` |
| `try_raise_cross_function_caught` | micro | 0.4865 | 0.5396 | 1.1093x | `cases/try_raise_cross_function_caught` |
| `layered_try_raise_hit_only` | micro | 0.6244 | 0.6798 | 1.0888x | `cases/layered_try_raise_hit_only` |
| `env_find_same_layered_int_key` | micro | 0.3770 | 0.4084 | 1.0833x | `cases/env_find_same_layered_int_key` |
| `array_binary_search_string` | micro | 0.4145 | 0.4471 | 1.0786x | `cases/array_binary_search_string` |
| `string_tree_first_byte_diff` | micro | 0.4069 | 0.4275 | 1.0505x | `cases/string_tree_first_byte_diff` |
| `string_map_equal_content` | micro | 0.2551 | 0.2665 | 1.0448x | `cases/string_map_equal_content` |
| `try_find_miss_rare` | micro | 0.9558 | 0.9952 | 1.0412x | `cases/try_find_miss_rare` |
| `boyer` | mini | 0.0879 | 0.0913 | 1.0387x | `cases/boyer` |

## Aggregate Results
- micro geomean LLVM/native: `0.809764`
- micro median LLVM/native: `0.955866`
- minibench total LLVM/native: `0.936220`
- minibench total LLVM speedup: `6.81%`

## Files
- `micro-summary.json` and `minibench-results.json`: benchmark results used to select cases.
- `summary.json`: extracted per-case/per-function counts and paths.
- `ANALYSIS.md`: root-cause analysis of the remaining slowdowns.
- `cases/<case>/functions/<symbol>/`: before RS4GC IR, after RS4GC IR, post-isel MIR, LLVM asm, native asm.
