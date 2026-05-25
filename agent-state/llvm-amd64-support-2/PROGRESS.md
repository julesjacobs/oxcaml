# Progress

Last updated: 2026-05-25.

## Current Claim

Independent AMD64 LLVM-backend implementation is in progress. The compiler
builds on AMD64, and simple scalar programs compile and run with
`-llvm-backend` through this agent's own LLVM wrapper.

## Evidence

- OxCaml branch: `jujacobs/llvm-amd64-support-2`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/12
- Initial state commit: `c206e2f15041`
- PR-record commit: `b1cde046ea`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-amd64-support-2`
- Goal: build AMD64 support for `-llvm-backend` until simple programs run,
  full `llvm-test` passes, and `llvm-self-stage2-test` passes.
- Per-agent temp setup:
  - `eval "$(../../../scripts/agent-tmp-env)"`
  - `OXCAML_AGENT_TMP=/tmp/oxcaml-agent-llvm-amd64-support-2`
  - `LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support-2/clang-wrapper`
  - `LLVM_WRAPPER_LOG=/tmp/oxcaml-agent-llvm-amd64-support-2/clang-wrapper.log`
- Built vendored LLVM `llc` and `opt` for this agent checkout under
  `$OXCAML_AGENT_TMP/llvm-build` with `LLVM_TARGETS_TO_BUILD=X86`.
- Implemented AMD64 lowering in `backend/llvm/llvmize.ml` for the AMD64
  `Arch.specific_operation` constructors needed by non-SIMD code:
  `Ilea`, `Istore_int`, `Ioffset_loc`, `Ifloatarithmem`, `Ibswap`,
  `Isextend32`, `Izextend32`, `Irdtsc`, `Irdpmc`, fences, and
  `Illvm_intrinsic`.
- Added AMD64 LLVM-backend selection/lowering for target-only `Cpackf32` and
  `Cprefetch` so they no longer fall into the generic selector fatal path.
  `Icldemote` is selected but lowered as a no-op because it is only a cache hint
  and `llvm.x86.cldemote` aborts in this LLVM X86 configuration without the
  matching selectable target feature.
- SIMD AMD64 specifics are intentionally still reported as not implemented;
  this keeps the first implementation scalar and avoids importing ARM64-specific
  lowering into AMD64.
- Fixed a normal AMD64 compiler build break in
  `backend/amd64/simd_selection.ml` by selecting the generated
  `vpmulhrsw_Y_Y_Ym256` AVX2 instruction helper.
- `opam exec -- ./configure --enable-frame-pointers` completed.
- `opam exec -- make compiler` completed after the source changes.
- Manual scalar smoke after the rebuild:
  - compiled with `_build/install/main/bin/ocamlopt.opt -nostdlib -I
    _build/runtime_stdlib_install/lib/ocaml_runtime_stdlib -llvm-backend
    -llvm-path "$LLVM_PATH"`
  - program used recursion, allocation, exceptions, `Printf`, and `sqrt`
  - output: `42 24 3.0`
  - wrapper log contained 2 `-x ir` invocations.
- Manual prefetch/cldemote smoke:
  - used builtin externals for `caml_prefetch_read_high`,
    `caml_prefetch_write_low`, and `caml_cldemote`, plus temporary C stubs for
    primitive table symbols
  - compiled with the same `-llvm-backend -llvm-path "$LLVM_PATH"` setup
  - output: `7`
  - wrapper log contained 2 `-x ir` invocations.
- Local testsuite setup works when run under opam with a make-time local prefix:
  `opam exec -- make prefix="$PWD/_local-test-install" install_for_test`.
- Direct testsuite run of `tests/llvm-codegen/arithmetic.ml` completed through
  the harness but skipped because the test is currently gated by `macos` and
  `arch_arm64`.
- Added `testsuite/tests/llvm-codegen/amd64_smoke.ml` plus
  `amd64_smoke.sh`, an AMD64-gated testsuite smoke that builds and runs three
  `-llvm-backend` programs:
  - scalar recursion/allocation/exception/`Printf`/`sqrt` smoke, expected
    output `42 24 3.0`
  - float32 conversion smoke, expected output `1.5 2.5`
  - prefetch/cldemote builtin smoke with local C primitive-table stubs,
    expected output `7`
- The smoke script passes `_runtest/stdlib` explicitly with `-nostdlib -I`
  because the local `ocamlopt.opt` can otherwise look at its configured install
  prefix and fail with `Unbound module "Stdlib"` in this harness setup.
- Focused testsuite run passed:
  `env -u DIR -u LIST opam exec -- make --trace one TEST=tests/llvm-codegen/amd64_smoke.ml`
  from `_runtest/testsuite`, with `OCAMLSRCDIR` and
  `CAML_LD_LIBRARY_PATH` pointed at `_runtest`.
- Started broader `llvm-test` validation with a local prefix and
  `LLVM_BOOT_BACKEND=0` to keep normal backend testing separate from self-stage:
  `opam exec -- make prefix="$PWD/_local-llvm-test-install" llvm-test LLVM_BOOT_BACKEND=0`.
  The run reached the testsuite and was stopped after the first repeated
  failures were reduced.
- Fixed AMD64 LLVM object files for Linux PIE/shared linking by passing `-fPIC`
  to clang when `Clflags.pic_code` or `Clflags.dlcode` is set. This fixed the
  repeated `R_X86_64_32S against .text` link failures seen in native tests.
- Fixed `-internal-assembler` interactions with `-llvm-backend` by forcing
  `Asmgen.compile_unit` to create an assembly file for LLVM even when
  `Emitaux.binary_backend_available` has been enabled by the internal
  assembler flag. This fixed the missing `.s` failure in AMD64 asmcomp tests.
- Verified after both fixes:
  - `opam exec -- make compiler`
  - local `-shared` smoke using `_runtest/ocamlopt.opt` with
    `OCAMLPARAM=_,llvm-backend=1,llvm-path=$LLVM_PATH`
  - `tests/array-functions/test.ml` through `_runtest/testsuite`
  - `tests/asmcomp/movsx_small_ints.ml` through `_runtest/testsuite`
  - `tests/llvm-codegen/amd64_smoke.ml` through `_runtest/testsuite`
- Added independent AMD64 exception/runtime fixes in
  `backend/llvm/llvmize.ml`:
  - empty `deopt` operand bundles are emitted for non-leaf calls, while
    `Gc_leaf_function` calls skip operand bundles;
  - `Compare_exchange` now returns the old loaded value from LLVM `cmpxchg`;
  - AMD64 runtime exception recovery uses PIC-safe GOTPCREL addressing and
    moves the exception bucket through `%r11`;
  - frame-pointer attributes are attached on AMD64 when configured, including
    generated C-call wrappers and `wrap_try`.
- Added AMD64 LLVM lowering for the subset of SIMD builtins needed by layout
  tests: `caml_int64x2_*`, `caml_simd_int64x2_{add,sub}`,
  64-bit vec128 interleaves, low vec256 extraction, and AVX vec256
  extract/insert 128 operations. Added the corresponding AMD64 selection and
  purity table entries.
- Added limited LLVM lowering for vector reinterpret casts between vec128 and
  vec256 low lanes.
- Added minimal probe lowering: `Probe` terminators branch to their continuation
  and `Probe_is_enabled` returns the static `enabled_at_init` value when known
  (otherwise false). This is enough for the focused probe tests but does not yet
  emit real USDT probe metadata.
- Focused tests that now pass:
  - `tests/exception-extra-args/exception_extra_args.ml`
  - `tests/match-exception/nested_handlers.ml`
  - `tests/lib-atomic/test_atomic_cmpxchg.ml`
  - `tests/callback/test3.ml`
  - `tests/frame-pointers/stack_realloc.ml`
  - `tests/frame-pointers/stack_realloc2.ml`
  - `tests/typing-layouts-block-indices/block_indices_native.ml`
  - `tests/typing-layouts-or-null/probe.ml`
  - `tests/templates/basic/probe.ml`
- Rebuilt and refreshed the local testsuite install after the latest source
  changes:
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
- Full `llvm-test` with `LLVM_BOOT_BACKEND=0` completed:
  - `6632 passed`, `280 skipped`, `43 failed`, `6955 considered`
  - This improves from the previous full run's `6605 passed`, `280 skipped`,
    `67 failed`, `6952 considered`.
  - Fixed failures include `exception_extra_args`, `nested_handlers`,
    `test_atomic_cmpxchg`, block-index SIMD compilation, and probe tests.
  - Remaining notable failures include native segfaults in
    `tests/async-exns/async_exns_2.ml`, `tests/basic/patmatch.ml`,
    small-int tests, `tests/misc-kb/kbmain.ml`, and several layout array/iarray
    semantic assertions; quotation linking, native CFI stepping, and unboxed
    primitive argument ABI failures remain.
- Attempted a simpler AMD64 exception recovery path that avoided moving `%r14`
  and `%r15` into LLVM's landing registers; it immediately regressed
  `exception_extra_args`, so that experiment was backed out before this handoff.
- Fixed the remaining AMD64 LLVM exception recovery corruption independently:
  - AMD64 trap frames now save `%rbp` as `rbp - trap_block`, matching the
    relocation-stable AArch64 model instead of storing an absolute stack
    pointer that can go stale across stack relocation.
  - The recovery shim reconstructs `%rbp` from the current trap block and seeds
    both `%rax` and `%rcx` from the domain-state register `%r14`; this avoids
    corrupting the domain state after exception recovery.
- Focused tests that now pass after the recovery fix:
  - `tests/async-exns/async_exns_2.ml`
  - `tests/exception-extra-args/exception_extra_args.ml`
  - `tests/match-exception/nested_handlers.ml`
  - `tests/basic/patmatch.ml`
  - `tests/lib-smallint/test_int16_u.ml`
  - `tests/lib-smallint/test_int8_u.ml`
  - `tests/misc-kb/kbmain.ml`
  - `tests/typing-layouts/unboxed_int_stringlike_indexing.ml`
  - `tests/typing-layouts-arrays/test_or_null_product_array.ml`
- Rebuilt and refreshed the local testsuite install after the recovery fix:
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
- Full `llvm-test` with `LLVM_BOOT_BACKEND=0` completed again:
  - `6641 passed`, `280 skipped`, `34 failed`, `6955 considered`
  - This improves from the previous full run's `6632 passed`, `280 skipped`,
    `43 failed`, `6955 considered`.
  - The previous native segfault failures in `async_exns_2`, `patmatch`,
    `test_int16_u`, `test_int8_u`, `kbmain`,
    `unboxed_int_stringlike_indexing`, and `test_or_null_product_array` are now
    fixed.
  - Remaining notable failures are native CFI stepping, quotation native/linker
    tests with missing stub libraries, product array/iarray semantic assertion
    failures, `tool-toplevel/dwarf_binary_emitter.ml`, and the AMD64
    unboxed-primitive argument ABI mismatch.
- Began an independent reduction of
  `tests/unboxed-primitive-args/test.ml` rather than using earlier agent code.
  Isolated single generated externals such as `test_v_ssfls`,
  `test_v_sfIxI`, `test_v_sIsf`, `test_v_fsLI`, `test_v_Ixff`, and
  `test_v_xll` pass when run alone through the generated `Common.run_tests`
  harness, but fail in the full generated list after prior calls have changed
  register state.
- Added LLVM AMD64 calling-convention coverage for vec128/vec256/vec512
  values in the normal OxCaml convention, with stack fallback for float and
  vector values. The normal argument vector register set is capped at
  XMM/YMM/ZMM0-9 to match `backend/amd64/proc.ml`.
- Changed noalloc external calls with C stack arguments to use private C-call
  wrappers instead of the allocating-call `caml_c_call_stack_args` path. This
  avoids relying on the runtime stack-copy helper for noalloc calls and lets
  LLVM form the real C ABI call from the C stack.
- Rebuilt after the ABI work:
  - `cmake --build "$OXCAML_AGENT_TMP/llvm-build" --target llc opt`
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
- The focused unboxed primitive test still fails at run time:
  `env -u DIR -u LIST opam exec -- sh -c 'export CAML_LD_LIBRARY_PATH="$TEST_CAML_LD_LIBRARY_PATH"; make one TEST=tests/unboxed-primitive-args/test.ml'`
  from `_runtest/testsuite`.
  Remaining failures are now concentrated on order/register-state-dependent
  float32 and vec128/int64x2 arguments, for example `test_v_ssfls`,
  `test_v_sfIxI`, `test_v_xll`, and related mixed signatures. Large C-stack
  argument reductions that previously failed early are no longer the leading
  reduced failure.
- Corrected the noalloc C-stack-argument experiment above: private noalloc
  wrappers switch to the C stack before LLVM has reserved an outgoing C call
  area, so generated code wrote stack arguments directly at `%rsp`. Calls with
  C stack arguments now use `caml_c_call_stack_args` regardless of `alloc`.
- Independently reduced the unboxed primitive failure to boxed `float32`
  corruption in `Common.Buffer.get_float32`: the raw `int32` bits were correct,
  but the converted double in `%xmm0` crossed an allocation slow path and the
  LLVM backend always called plain `caml_call_gc`, which saves no SIMD
  registers. The allocated `float32` box then received zero after GC clobbered
  `%xmm0`.
- Added LLVM AMD64 GC-call selection for allocation and poll safepoints:
  choose `caml_call_gc_sse`, `caml_call_gc_avx`, or
  `caml_call_gc_avx512` when liveness shows Float/Float32/Vec128/Vec256/Vec512
  values across the safepoint.
- Rebuilt and refreshed the local testsuite install after the GC-call fix:
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
- Focused unboxed primitive test now passes:
  `env -u DIR -u LIST opam exec -- sh -c 'export CAML_LD_LIBRARY_PATH="$TEST_CAML_LD_LIBRARY_PATH"; make one TEST=tests/unboxed-primitive-args/test.ml'`
  from `_runtest/testsuite` completed with `8 tests passed`, `1 skipped`,
  `0 failed`.
- Full `llvm-test` with `LLVM_BOOT_BACKEND=0` completed after the SIMD-saving
  GC fix:
  - `6656 passed`, `280 skipped`, `20 failed`, `6956 considered`
  - This improves from the previous full run's `6641 passed`, `280 skipped`,
    `34 failed`, `6955 considered`.
  - `tests/unboxed-primitive-args/test.ml` now passes in the full run.
  - Remaining failures are native CFI stepping; quotation native/linker tests
    that fail linking due to missing stub libraries; `tool-toplevel` native
    toplevel output for `dwarf_binary_emitter.ml`; and product/vector array
    semantic assertions in `tests/typing-layouts-arrays`.
- Independently reduced the remaining vec128/product-array semantic failures
  to non-root raw values live across GC slow paths. In particular, boxed
  int64x2 comparison could print equal lanes but return nonzero with a small
  minor heap because an extracted raw scalar crossed a GC path without being
  kept in its stack slot.
- Changed LLVM AMD64 safepoint preservation so every Cfg register live across a
  GC-capable instruction gets a preserved alloca slot. Only OCaml values are
  still reported as `gc-live` roots.
- Rebuilt and refreshed the local testsuite install after this preservation
  fix:
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
- Focused tests now passing with `OCAMLSRCDIR="$PWD/.."` from
  `_runtest/testsuite` and `OCAMLPARAM="_,llvm-backend=1,llvm-path=$LLVM_PATH"`:
  - `tests/typing-layouts-arrays/test_vec128_u_array.ml`
  - `tests/typing-layouts-arrays/test_ignorable_product_array_3.ml`
  - `tests/typing-layouts-arrays/test_ignorable_product_array_4.ml`
  - `tests/typing-layouts-arrays/test_ignorable_product_array_with_uninit_3.ml`
  - `tests/typing-layouts-arrays/test_ignorable_product_array_with_uninit_4.ml`
  - `tests/unboxed-primitive-args/test.ml`
- `tests/typing-layouts-arrays/test_vec256_u_array.ml` now compiles and passes
  with `OCAMLRUNPARAM=s=256M`, but with `OCAMLRUNPARAM=s=32k` it aborts in
  `caml_scan_stack` with a missing frame descriptor while exercising
  `caml_call_gc_avx`. This is the next reduced vector-array issue.
- Fixed the vec256 small-stack scan abort independently by preventing LLVM from
  dynamically realigning AMD64 OxCaml stack frames. LLVM's `andq $-32, %rsp`
  prologue creates a call-site-dependent gap that the OCaml frame table cannot
  encode as one static frame size; the AMD64 LLVM backend now emits
  `"no-realign-stack"` on generated OxCaml functions while keeping frame
  pointers.
- Verified after the no-realign-stack fix:
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
  - direct reduced `test_vec256_u_array.ml` compile/run with
    `OCAMLRUNPARAM=s=32k,b`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" llvm-test-one
    TEST=typing-layouts-arrays/test_vec256_u_array.ml LLVM_PATH="$LLVM_PATH"
    LLVM_BOOT_BACKEND=0`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" llvm-test-one
    TEST=llvm-codegen/amd64_smoke.ml LLVM_PATH="$LLVM_PATH"
    LLVM_BOOT_BACKEND=0`

## Current Blocker

No immediate source blocker. Full `llvm-test` has not yet been rerun after the
non-root safepoint preservation and AMD64 no-realign-stack fixes. Other known
failures from the last full run include native CFI stepping, quotation
native/linker tests with missing stub libraries, and
`tool-toplevel/dwarf_binary_emitter.ml`.

## Next Step

Rerun full `llvm-test`, then address `tool-toplevel/dwarf_binary_emitter.ml`
and native CFI stepping before attempting `llvm-self-stage2-test`.
