# LLVM string compare optimization plan

## Goal

Lower hot calls to the three-way OCaml string/bytes compare helpers directly in
the LLVM backend for the common short-string cases, while preserving exact OCaml
string comparison semantics.

The first patch should cover `caml_string_compare` and `caml_bytes_compare`,
which use the same `String_tag` representation. It should not try to cover the
relational helpers (`caml_string_lessthan`, `caml_string_lessequal`, etc.) in
the same patch; those are separate helper symbols with different result shapes.
This optimization is not a generic `memcmp` replacement.

## Evidence

Dynamic helper profiling on representative compiler files showed
`caml_string_compare` is hot:

- `typecore.ml`: 27,695,126 calls.
- Aggregate across representative compiler files: 55,293,907 non-pointer
  string compares.

The non-pointer `min(len1, len2)` histogram is short-string heavy:

| threshold | aggregate coverage | `typecore.ml` coverage |
| --- | ---: | ---: |
| `<= 7` | 73.3% | 68.9% |
| `<= 15` | 96.3% | 97.5% |
| `<= 31` | 99.0% | 99.5% |
| `<= 63` | ~100.0% | ~100.0% |

Standalone custom-LLVM benchmarking of generated-shape proxies found:

| candidate | approx bytes | random mismatch ratio vs helper |
| --- | ---: | ---: |
| `word_prefix_15_then_memcmp` | 312 | 0.468x |
| `word_prefix_31_then_memcmp` | 496 | 0.475x |
| `word_loop_31_then_memcmp` | 272 | 0.499x |
| plain helper-shaped `memcmp` | 136 | 1.000x |

In the real LLVM backend, the win should be larger than this standalone proxy
because the current noalloc C-call path still pays generated wrapper overhead
and GC statepoint/root machinery.

## OCaml String Contract

The lowering may rely on these runtime representation facts:

- `String_val(s)` points at the first byte of a word-aligned block.
- On supported 64-bit targets, `String_val` pointers are word-aligned by the
  OCaml block representation. The first implementation should only emit this
  lowering on 64-bit little-endian targets where that invariant is true.
- `caml_alloc_string` allocates a whole number of words.
- The byte at logical index `String.length s` is zero.
- The padding bytes before the final offset byte are zero for normally
  allocated strings/bytes.
- The final byte of the allocated block is the length-offset byte, not string
  content.

The lowering must not compare the final offset byte as content. For example:

```text
"abc"      -> 61 62 63 00 00 00 00 04
"abc\000" -> 61 62 63 00 00 00 00 03
```

A full padded-word comparison gives the wrong result here. The generated code
must mask bytes after `min_len` and use the normal length tie-break when the
masked compared prefix is equal.

## Integration Design

Add an LLVM-backend-only direct lowering for non-allocating extcalls whose
symbol is exactly `caml_string_compare` or `caml_bytes_compare`.

Initial hook:

- `backend/llvm/llvmize.ml`
- In `extcall`, before the current `alloc = false` wrapper path.
- Match:
  - `func_symbol = "caml_string_compare"` or `"caml_bytes_compare"`
  - `alloc = false`
  - two arguments
  - one integer result
  - no stack arguments
  - 64-bit little-endian target with word-aligned OCaml block payloads

If the shape does not match, fall back to the existing wrapper path.

Keep the lowering local to `backend/llvm/llvmize.ml` at first. Add a small
helper group rather than a new module until a second string helper needs the
same machinery. Suggested helper boundaries:

- `emit_load_ocaml_header_word`
- `emit_ocaml_string_length`
- `emit_compare_u64_be_masked`
- `emit_string_compare_fast_path`
- `maybe_emit_specialized_noalloc_extcall`

Use existing IR builder functions and local style:

- `load_reg_to_temp`
- `store_into_reg`
- `emit_ins`
- `emit_ins_no_res`
- `I.load`, `I.binary`, `I.icmp`, `I.select`, `I.convert`
- `call_simple` only for the existing generated wrapper fallback, not for a raw
  C ABI `memcmp` call.

Add external symbol references only for fallback calls that remain:

- In the first patch, keep the original helper wrapper only on the long-string
  fallback path.
- Do not add the original helper wrapper when the short specialized lowering
  succeeds.

A direct suffix `memcmp` is a separate design decision. `call_simple` is the
wrong abstraction for that call: it emits the OxCaml call shape, threads runtime
registers, and expects an OCaml-call return convention. A raw `memcmp` suffix
would need a small direct C ABI helper using the default C calling convention,
the right C stack state, raw pointer/size arguments, raw `int` result handling,
and no GC statepoint/root semantics. Until that exists, long strings should use
the existing noalloc wrapper fallback.

Keep the hook in `extcall` as a thin detector/dispatcher. The compare algorithm
should live in a compact helper section so `extcall` does not become a large
string-specialization body.

## Generated Shape

Prototype `word_prefix_15_with_helper_fallback` first. This shape avoids adding
a new raw C-call path while still covering about 96% of measured non-pointer
string compares:

1. If `s1 == s2`, return `Val_int(0)`.
2. Compute `len1 = caml_string_length(s1)` inline:
   - atomically load the header word at `s1 - 8` with relaxed ordering, matching
     `Hd_val`
   - if `backend/llvm/llvm_ir.ml` only supports `seq_cst` atomic loads today,
     add relaxed load support rather than silently using a plain load
   - `wosize = (header & HEADER_WOSIZE_MASK) >> HEADER_WOSIZE_SHIFT`
   - on this 64-bit OxCaml configuration, `HEADER_WOSIZE_SHIFT = 10`
   - do not rely on reserved/color/tag bits being zero
   - `bosize = wosize * 8`
   - `offset_index = bosize - 1`
   - `offset = load i8 s1[offset_index]`
   - `len1 = offset_index - offset`
3. Compute `len2` similarly.
4. `min_len = min(len1, len2)`.
5. If `min_len == 0`, skip content compare and use length tie-break.
6. If `min_len > 15`, call the existing helper through the current noalloc
   wrapper fallback. Do not compare a prefix first in the first patch; let the
   existing helper handle the whole long compare.
7. Otherwise compare the first aligned 64-bit word masked to
   `min(min_len, 8)`.
8. If equal and `min_len > 8`, compare the second aligned 64-bit word masked to
   `min_len - 8`.
9. Convert the raw compare result to OCaml compare result:
   - negative -> `Val_int(-1)`
   - positive -> `Val_int(1)`
   - zero -> length tie-break

Use 64-bit loads with explicit alignment 8. Byte-swap each loaded word before
unsigned comparison so the integer comparison follows byte lexicographic order
on little-endian targets. For masked words, after byte-swap keep the high
`count` bytes and clear the rest. The masked-word helper must have a total
contract:

- It is called only with `count` in `1..8`.
- `count = 8` produces an all-ones mask without shifting by 64.
- `count = 0` is handled by the caller and never reaches the helper.

Emit the returned OCaml integers as immediate values: `Val_int(-1) = -1`,
`Val_int(0) = 1`, and `Val_int(1) = 3`.

Second phase after the first patch is correct and measured:

- `word_prefix_15_then_raw_memcmp`
- Compare word offsets `0` and `8` using the general rule:
  `count = min(8, min_len - offset)` under `min_len > offset`.
- Only after all bytes up to offset `15` have been covered may the suffix path
  start at `s + 16`.
- Use raw `memcmp(s1 + 16, s2 + 16, min_len - 16)` only after the direct C ABI
  call design is implemented and tested.

Second candidate after the first prototype passes:

- `word_prefix_31_then_memcmp`
- Same raw-`memcmp` phase structure, but compare up to four aligned 64-bit
  words before falling back to `memcmp(s1 + 32, s2 + 32, min_len - 32)`.
- The same no-gap rule applies at every offset: compare bytes `0..31` before
  starting a suffix at `32`.
- This is the likely performance target if code size is acceptable.

Code-size fallback:

- `word_loop_31_then_memcmp`
- Use a compact loop for up to four words.
- Keep this option if the straight-line `<=31` shape grows generated compiler
  code too much.

Do not start with `<=63`; the histogram only leaves about 1.0% of aggregate
non-pointer compares above `<=31`.

## Small Benchmarks

Create small executable benchmarks that can be compiled by native and by the
LLVM backend with identical OCaml optimization flags.

Each benchmark should run enough iterations to be stable and print one integer
or checksum so the loop is not optimized away. Build strings at runtime or use
`Sys.opaque_identity` so the compiler cannot constant-fold the comparisons.

Cases:

1. Equal short strings:
   - lengths 3, 7, 8, 15, 16, 31.
2. First-byte mismatch:
   - same lengths.
3. Last-byte mismatch:
   - same lengths.
4. Embedded-zero correctness/performance:
   - `"abc"` vs `"abc\000"`
   - `"abc\000"` vs `"abc"`
   - equal strings containing interior zeros.
5. Long prefix-equal strings:
   - length 64 and 128, differing after byte 32.
   - verifies the long fallback path.
6. Long equal strings:
   - length 64 and 128.
   - checks that the fallback is not made worse for common equal-prefix cases.
7. Boundary and unsigned-byte cases:
   - lengths/min lengths 8, 9, 15, 16, 17, 31, 32, 33.
   - first difference at bytes 7, 8, 15, 16, 31, 32.
   - byte values `'\128'`, `'\255'`, `'\000'`.
8. Mixed compiler-like distribution:
   - generated fixed array of string pairs matching the saved `typecore.ml`
     histogram and result classes.

Measure:

- native backend runtime
- LLVM backend before patch
- LLVM backend after short-only patch
- LLVM backend after raw-`memcmp` suffix patch, if that phase exists
- benchmark object/text size with a named tool such as `size -m`

Expected result:

- Large improvement for LLVM backend on short and compiler-like distributions.
- Smaller but still positive improvement for long strings that differ early.
- No regression for long strings that use the helper fallback.
- If the raw-`memcmp` suffix phase is implemented, long prefix-equal cases
  should improve relative to the helper fallback.

## Large-Scale Compiler Benchmark

Use the existing compiler-binary benchmarking harness under
`_compiler_binary_perf_current/`.

Build three separate compilers from clean, separate directories:

1. Native-built compiler from `make install`, as context.
2. LLVM-built stage2 compiler from the unpatched LLVM-backend baseline.
3. LLVM-built stage2 compiler from the patched backend.

Benchmark all compilers with the normal backend, not `-llvm-backend`, because
the workload we care about is how fast the compiler binary itself runs. The
primary comparison is LLVM baseline versus patched LLVM. Native is context for
how close the LLVM-built compiler is to parity.

Representative files:

- `typing/typecore.ml`
- `typing/ctype.ml`
- `typing/env.ml`
- `typing/typemod.ml`
- `lambda/translcore.ml`
- `backend/llvm/llvmize.ml`
- `asmcomp/cfg_selectgen.ml`
- `asmcomp/regalloc_irc.ml`
- `asmcomp/cfg_to_linear.ml`

Use the same flags for all compilers. Run multiple repetitions and report:

- median wall time
- min/max or interquartile spread
- per-file speedup
- aggregate/geomean speedup
- compiler binary text size with the same tool for all binaries

Also rerun the helper profile once after the patch. The expected profile change
is that dynamic calls to `caml_string_compare` from LLVM-built compiler code
and `caml_bytes_compare` should drop sharply for inlined short sites. Long-site
fallback calls may remain in the short-only patch.

Record code-size data:

- static count of specialized call sites
- representative per-site assembly size
- installed compiler `.text` delta
- benchmark executable `.text` deltas

## Codegen Tests

Add focused expect tests under `testsuite/tests/llvm-codegen/`, likely in a new
`string_compare.ml`.

Test source cases:

- direct `String.compare a b`
- direct `Bytes.compare a b`
- result used as int
- pointer-equal fast path present
- embedded-zero examples to force correct length tie-break
- long string fallback case that still emits the original helper wrapper in the
  short-only patch
- raw suffix case that emits `memcmp`, if the raw-C-call phase is implemented

Expect checks should verify:

- LLVM IR no longer calls a generated `c_call_wrapper.caml_string_compare` or
  `c_call_wrapper.caml_bytes_compare` for the specialized short shape.
- IR contains relaxed atomic header loads, aligned `i64` content loads,
  `bswap.i64`, masking, and length tie-break control flow.
- Assembly contains the expected load/compare/branch shape and does not call
  `_c_call_wrapper.caml_string_compare...` or
  `_c_call_wrapper.caml_bytes_compare...` for short cases.
- The long fallback case calls the original helper wrapper in the first patch.
- The raw suffix phase, if implemented, calls `_memcmp` with a raw C-call shape
  rather than through `call_simple`.

Keep the expect test small. Avoid blessing huge IR if a smaller function can
prove the lowering. Prefer targeted checks for key operations and absence of
the wrapper over matching the full function body.

## Correctness Tests

Add runtime tests outside the codegen expect file for semantics:

- exhaustive strings over a tiny alphabet for lengths 0..4, including `'\000'`
- selected lengths around boundaries: 7, 8, 9, 15, 16, 17, 31, 32, 33
- pairs that differ before, at, and after each boundary
- pairs that differ at bytes 7, 8, 15, 16, 31, and 32
- unsigned-byte ordering cases such as `"\255"` versus `"\000"` and
  `"\128"` versus `"\127"`
- equal content but different lengths
- empty/prefix cases such as `""` versus `"\000"` and `"\000"` versus `""`
- pointer-equal same string
- physically distinct but equal strings
- long equal strings and long strings equal through the optimized prefix but
  differing near the final byte

For each pair, compare:

- `String.compare`
- `Bytes.compare`
- expected result from a simple OCaml reference comparator

Run under:

- native backend
- LLVM backend
- bytecode/native runtime only as a sanity reference when useful

## Test Suite Strategy

Fast validation during development:

1. Build the compiler component needed for `ocamlopt.opt`.
2. Run the new `llvm-codegen/string_compare.ml` expect test.
3. Run the small semantic string compare test with `-llvm-backend`.
4. Run the standalone benchmark to catch obvious performance regressions.

Before proposing the PR:

1. `make install`
2. LLVM-codegen tests.
3. Relevant runtime/stdlib string tests.
4. The small benchmark suite.
5. The representative compiler benchmark.
6. A broader LLVM backend test-suite run if time permits.

## Risks And Mitigations

Risk: encoding OCaml string layout incorrectly.

- Mitigation: keep a direct comment near the lowering explaining the atomic
  header load, the offset byte, and why masking is required; add embedded-zero
  tests.

Risk: using 8-byte loads where alignment or bounds are not valid.

- Mitigation: emit this lowering only on supported 64-bit little-endian targets
  where OCaml string payloads are word-aligned; use `align 8`; do not use
  16-byte vector loads in this first patch.

Risk: generated code size grows too much.

- Mitigation: start with `word_prefix_15_with_helper_fallback`; measure
  compiler binary text size; only move to raw-`memcmp` or `<=31` variants if the
  real benchmark justifies them.

Risk: fallback C call reintroduces expensive wrapper machinery.

- Mitigation: accept the existing wrapper for long strings in the first patch.
  The histogram says short strings carry most of the expected win. Treat direct
  raw `memcmp` as a follow-up only after the C ABI and stack/root contract is
  explicit.

Risk: platform assumptions leak to non-AArch64 targets.

- Mitigation: guard the first implementation to known-supported 64-bit
  little-endian targets, initially AArch64/macOS if that is the only target
  covered by the expect tests.

## Open Questions

- Should the first patch implement only `word_prefix_15_with_helper_fallback`,
  or also add the direct raw `memcmp` suffix path?
- Can the suffix `memcmp` be emitted as a direct noalloc C call without the
  current wrapper/statepoint overhead?
- Is this optimization better represented as a special extcall lowering in
  `llvmize.ml`, or should the compiler introduce a more explicit Cmm/CFG
  operation for OCaml string compare?
- Should relational helpers share any machinery in a later patch, or should this
  stay strictly scoped to three-way compare?

## Human-Like Review Iteration

Five independent review agents reviewed the first draft. Accepted changes:

- The main compiler benchmark must compare patched LLVM-built compiler versus
  unpatched LLVM-built compiler. Native-built compiler is only context.
- `call_simple` must not be used for direct `memcmp`; the first patch should
  use the existing wrapper fallback for long strings unless a raw C-call helper
  is designed and tested.
- The generated prefix rule must not skip bytes. Every suffix fallback offset
  must be preceded by comparing all bytes before that offset.
- Scope must be explicit: first patch covers `caml_string_compare` and
  `caml_bytes_compare`; relational helpers are out.
- Header loads must preserve the runtime's atomic `Hd_val` semantics.
- The target guard must be mandatory for the first patch, not a vague follow-up.
- Correctness tests need boundary-byte, high-byte, embedded-zero, and long
  fallback cases.
- Code size measurement is required, including static specialized-site count
  and compiler/benchmark `.text` deltas.

Rejected or deferred changes:

- Do not move the optimization into a new Cmm/CFG operation for the first patch.
  The value is specifically in bypassing this backend's noalloc C-call wrapper
  on short paths, and a local `llvmize.ml` hook is the smaller first step.
- Do not include relational helpers in the first patch. They are worth a later
  patch, but they add different return shapes and extra test surface.
- Do not require direct raw `memcmp` in the first patch. It is promising, but it
  needs a separate ABI/stack/rooting design.

## Current Recommendation

Implement `word_prefix_15_with_helper_fallback` first. It avoids the unsafe
direct-`memcmp` design question, removes wrapper calls for roughly 96.3% of
aggregate non-pointer compares, and has a modest code-size footprint. Then
benchmark the full compiler binary against an unpatched LLVM-built baseline. If
string compare remains visible and code size is acceptable, add the raw C-call
design and evaluate `word_prefix_15_then_raw_memcmp` and
`word_prefix_31_then_raw_memcmp` as follow-ups.

## 2026-05-28 Env Benchmark Follow-Up

While investigating the remaining exception slowdown after the AArch64
`raise_notrace` pseudo, `env_find_same_layered_hit` showed a separate string
compare issue:

- The benchmark keys are length 17.
- LLVM emits an inline short-string compare prelude in `ident_find_same`, but
  because `min_len >= 16` it falls back to the helper anyway.
- Native calls `caml_string_compare` directly at that site.
- Manual assembly patch: branch directly to the existing helper fallback from
  the non-pointer string path, skipping the LLVM inline prelude.
- Measured result for `env_find_same_layered_hit`:
  - native: about `0.361s`
  - LLVM original: about `0.524s`
  - LLVM with direct helper at this site: about `0.445s`

Conclusion: this does not explain all of the layered exception slowdown, but it
is a real independent cost. The first string-compare patch should avoid emitting
a short-string prelude that immediately falls back for common length-17 strings.
Either raise the direct compare threshold beyond 15 or make the threshold choice
aware of measured compiler-like string lengths.
