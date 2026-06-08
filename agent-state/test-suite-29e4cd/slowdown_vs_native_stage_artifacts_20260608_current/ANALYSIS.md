# Remaining Slowdowns Analysis

This directory was regenerated from the current `jujacobs/llvm-backend` checkout at
`0d68a665d2f89920bc656f495a73c75e84e4617a`.

The benchmark inputs are fresh runs using the installed compiler in
`_install/bin/ocamlopt.opt` and the agent-local LLVM wrapper.

## Benchmark Summary

Microbenchmarks:

- Cases: 44
- Geomean LLVM/native: `0.809764`
- Median LLVM/native: `0.955866`
- Worst slowdown: `hash_lookup_string_equal`, `1.1178x`

Minibenches:

- Cases: 10
- Total runtime LLVM/native: `0.936220`
- Total runtime LLVM speedup: `6.81%`
- Only runtime slowdown: `boyer`, `1.0387x`

`closure_call_in_try_hit`, which used to be the target try-body slowdown, is no
longer a slowdown in this run: LLVM/native is about `0.9705x`.

## Slowdown Buckets

### 1. String Search Loops

Affected cases:

- `array_binary_search_string`: `1.0786x`
- `string_tree_first_byte_diff`: `1.0505x`
- `string_map_equal_content`: `1.0448x`
- partly `hash_lookup_string_equal`: `1.1178x`

The hot leaf lookup functions for the first three cases have no statepoints, no
GC-live operands, no relocates, and no exnroots. The local regression is not
RS4GC.

The shape is the LLVM string compare partial inline. LLVM emits a
short-string fast path with length/header decoding, masked 64-bit loads, byte
reversal, masks, comparisons, and a fallback call. Native mostly keeps a compact
call to the runtime compare/equal helper.

Concrete counts:

- `array_binary_search_string__find`: LLVM `107` asm instructions, native `43`;
  no statepoints in either before or after RS4GC.
- `string_tree_first_byte_diff__find`: LLVM `87`, native `28`; no statepoints.
- `string_map_equal_content__find`: LLVM `90`, native `30`; no statepoints.
- `string_map_equal_content__find_opt`: LLVM `103`, native `38`; one ordinary
  statepoint, but the large local difference is still mostly compare/equal code.

The source of this shape is `backend/llvm/llvmize.ml`:

- `supports_inline_string_compare` returns true unconditionally for AArch64
  64-bit.
- `maybe_emit_specialized_string_compare_extcall` handles
  `caml_string_compare` and `caml_bytes_compare`.
- It expands pointer equality, two string length computations, a min-length
  check, up to two masked 64-bit big-endian word compares, a length tiebreak,
  and a fallback call for `min_len > 15`.

That expansion is profitable for some short-string compare workloads, but it is
too expensive in recursive tree/search loops where each iteration wants a small
control-flow body. Native's helper call keeps the loop body much smaller.

The decisive detail for the slow string-search cases is that their keys usually
miss the inline fast path. The LLVM fast path handles only `min_len <= 15`; for
`min_len > 15`, it computes both OCaml string lengths and then falls back to
`caml_string_compare`, which computes the lengths again before calling `memcmp`.
The slow cases use keys just above that cutoff:

- `string_tree_first_byte_diff`: `A_compiler_key_0` has length `16`, and
  two-digit keys have length `17`.
- `array_binary_search_string`: `array_binary_key_0` has length `18`, and
  two-digit keys have length `19`.
- `string_map_equal_content`: `compiler_equal_key_0` has length `20`, and
  two-digit keys have length `21`.

So these cases do not demonstrate a slow short-string helper. They demonstrate
an expensive failed inline attempt: the caller pays the length/header decoding,
branches around the inline body, and then still calls the runtime helper.

This also explains the opposite result for the intentionally short compare
microbenchmarks:

- `string_compare_short_equal_loop`: keys are length `5` or `6`, so the inline
  path is used and LLVM is much faster (`0.2710x` LLVM/native).
- `string_compare_short_late_diff_loop`: keys are length `7` or `8`, so the
  inline path is used and LLVM is much faster (`0.3044x` LLVM/native).

`hash_lookup_string_equal` should be treated separately. Its hot `find` function
uses `caml_string_equal`, and the LLVM leaf is close to native: LLVM `25`
instructions vs native `24`. The equality specialization only adds a pointer
equality check before falling back to the helper. The slowdown for this case is
in the larger `run` setup/loop shape and the three samples are noisy enough that
it should not drive the compare-inline decision by itself.

Likely fix direction: make string compare/equal partial inlining more selective.
The current fast path is very good when it actually handles the compare, but bad
when it frequently falls back. Good candidates are extending the inline path to
cover the common 16-byte case, or making the fallback decision cheaper. A plain
threshold/cost-model knob would avoid the regression but would also give up the
large wins for genuinely short-string loops.

### 2. Exception Path Without Root Traffic

Affected case:

- `try_raise_cross_function_caught`: `1.1093x`

`try_raise_cross_function_caught__find` has two statepoints after RS4GC, but no
GC-live operands, no relocates, and no exnroots. LLVM is still bulkier:

- LLVM `45` asm instructions, `21` stack-pointer references.
- Native `32` asm instructions, `15` stack-pointer references.

This points at the remaining pushtrap/landingpad/frame setup cost, not root
placement. LLVM still builds a larger frame shape and has a more verbose taken
exception path.

The local asm difference is concrete:

- LLVM splits the frame adjustment into two `sub sp` operations and uses a
  separate 16-byte area before pushing the trap record.
- Native uses a tighter trap setup: one ordinary frame area, then `stp x26, x16,
  [sp, #-16]!` for the trap record.
- LLVM calls `_caml_llvm_call_realloc_stack`; native calls
  `_caml_call_realloc_stack` with the frame size saved in the temporary stack
  record.

Likely fix direction: keep improving the pushtrap/recover lowering itself. The
useful target is smaller trap setup and direct exception control transfer where
the handler is statically known. Root optimizations will not fix this case.

### 3. Handler-Live Root Traffic

Affected cases:

- `layered_try_raise_hit_only`: `1.0888x`
- `env_find_same_layered_int_key`: `1.0833x`
- `boyer`: `1.0387x`

These still have handler-live values materialized through exnroot slots.

Concrete counts:

- `layered_try_raise_hit_only__find`: two statepoints, one relocate, one exnroot
  allocation pattern; LLVM `48` asm instructions vs native `40`.
- `env_find_same_layered_int_key__find_same_without_locks`: two statepoints, two
  relocates, two exnroot allocation patterns; LLVM `51` vs native `47`.
- `boyer__rewrite_with_lemmas`: five statepoints, ten relocates, four exnroot
  allocation patterns; LLVM `95` vs native `64`, with `45` stack-pointer
  references vs native `21`.
- `boyer__unify1`: two statepoints, three relocates, three exnroot allocation
  patterns; LLVM `110` vs native `103`, with `40` stack-pointer references vs
  native `29`.

The current exnroot strategy is correct enough for validation, but it still
turns handler-live values into explicit volatile stack traffic. That is visible
in Boyer because it has exception-heavy control flow and enough live values to
make the root slots dominate the local diff.

In `boyer__rewrite_with_lemmas`, after RS4GC starts with four exnroot allocas,
all initialized to tagged `1`. Before the first invoke it stores four roots:

- the current lemma list cursor,
- the current subject term,
- the same handler-visible value through two different handler paths,
- the original argument value used after recovery.

After the `unify` invoke returns normally, it reloads some exnroot values and
then stores another group before the `as_rec` invoke; after `as_rec`, it stores
again before the `rewrite` invoke. Native uses stack homes too, but has fewer
separate homes and fewer repeated stores in the same logical path.

Likely fix direction: reduce exnroot formation before trying to optimize stores
afterward. The remaining duplicate-looking stores should be traced to their
source values and handler uses. If a value is not truly handler-live, it should
not become an exnroot. If it is handler-live, the chosen slot should be reused
consistently and stores should be placed at the value definition or at the
cheapest dominating point, not repeated at every statepoint by default.

### 4. Unrolled Setup Code

Affected case:

- `try_find_miss_rare`: `1.0412x`

`try_find_miss_rare__run` is much larger in LLVM:

- LLVM `389` asm instructions, native `273`.
- After RS4GC: three statepoints, one relocate, one exnroot allocation pattern.

The large local count is not only exception mechanics. The generated setup code
contains a fully unrolled array initialization shape with many repeated
`caml_modify` calls and header checks. Native keeps this setup more compact.

This is visible before RS4GC: the LLVM IR already has a sequence of
`L189`, `L189.1`, ..., each calling `caml_modify` and rechecking the array tag.
Native emits a compact loop over the same array initialization. RS4GC is not the
origin of the unrolling.

Likely fix direction: inspect the optimization path that unrolls the array setup
under `-O3`, and either prevent unrolling for this shape or make the inlined
`caml_modify`/header-check sequence cheaper. This is lower priority than the
string compare and Boyer root issues because the benchmark is intentionally
rare-miss/setup-heavy.

## String Compare Follow-up

Implemented the string compare fallback experiment in `backend/llvm/llvmize.ml`:

- short inline compare now covers `min_len <= 16` instead of `min_len <= 15`;
- long compare first checks the first word inline;
- if the first word is equal, the fallback calls `memcmp` directly on the
  suffix starting at offset 8 and then applies the length tiebreak;
- the old generic `caml_string_compare` helper is no longer used on the hot LLVM
  assembly path for the representative string cases.

Focused microbench results after rebuild, `PAIRS=5`:

| case | native | llvm | llvm/native |
|---|---:|---:|---:|
| `string_tree_first_byte_diff` | 0.4271s | 0.1507s | 0.3530x |
| `array_binary_search_string` | 0.3568s | 0.3327s | 0.9325x |
| `string_map_equal_content` | 0.2615s | 0.1478s | 0.5654x |
| `string_compare_short_equal_loop` | 0.3432s | 0.0910s | 0.2651x |
| `string_compare_short_late_diff_loop` | 0.3105s | 0.0929s | 0.2991x |
| `string_tree_prefix_heavy` | 0.3455s | 0.1822s | 0.5274x |

The retained IR for `array_binary_search_string` shows the expected shape:
`min_len > 16` branches to the long path; the long path compares the first word,
then calls direct `memcmp` on `s1 + 8`, `s2 + 8`, `min_len - 8`. The LLVM
assembly for the same case contains `bl _memcmp` and no hot
`_caml_string_compare` call.

The broad representative microbench run has geomean `0.710x` LLVM/native.
The largest remaining slowdowns are now:

- `higher_order_fold_string_keys`: `1.1237x`;
- `layered_try_raise_hit_only`: `1.1144x`;
- `env_find_same_layered_int_key`: `1.0768x`;
- `try_raise_cross_function_caught`: `1.0644x`;
- `ref_option_churn`: `1.0617x`;
- `record_mutate_old_to_young`: `1.0449x`.

The remaining slowdowns are no longer explained by generic string compare
fallbacks.

## Concrete Next Experiments

1. Add a control knob for `maybe_emit_specialized_string_compare_extcall`, then
   rerun the string-search microbenchmarks with compare inlining disabled while
   keeping string equality's pointer-equality fast path. Expected result:
   `array_binary_search_string`, `string_tree_first_byte_diff`, and
   `string_map_equal_content` should move toward native.

2. Inspect `boyer__rewrite_with_lemmas` handler-live classification before
   exnroot creation. The first question is whether the two slots that carry the
   same handler-visible value are semantically distinct or an artifact of the
   transformation. Expected result: either remove a redundant exnroot, or prove
   the duplicate stores correspond to genuinely distinct live values.

3. For `try_raise_cross_function_caught`, isolate a no-root direct raise/catch
   micro test and compare just pushtrap/recover lowering. Expected result:
   quantify the fixed cost of the LLVM trap frame independent of roots.

4. For `try_find_miss_rare`, find whether the unrolled array init comes from
   Flambda/Cfg before LLVM or from LLVM IR optimization. Expected result: decide
   whether this is an OCaml optimization knob or an LLVM pass/cost-model knob.

## Current Main Takeaways

The biggest remaining micro slowdowns are now mostly not the original
`closure_call_in_try_hit` problem. They split into:

1. Non-RS4GC string compare/equal partial inlining that is too large in lookup
   loops.
2. Exception frame/trap overhead even when no GC roots are involved.
3. Handler-live exnroot traffic in exception-heavy functions, especially Boyer.

The next highest-value investigation is probably the string compare/equal cost
model, because several slowdowns have hot leaf functions with zero statepoint
involvement. The next exception-specific investigation is Boyer
`rewrite_with_lemmas`, because the evidence still shows large exnroot-driven
stack traffic there.
