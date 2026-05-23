# LLVM Backend Correctness Areas

Checklist for correctness issues that could make the LLVM backend unsafe as a
replacement for the native backend. Mark an area complete only after either
finding and covering a concrete issue, or auditing enough source/tests to be
confident it is not currently a correctness issue.

The ten audit areas are:

- Quick reference:
  1. Atomic loads and stores
  2. Load mutability
  3. Exception control-flow edges
  4. Effect handlers
  5. C calls that can allocate
  6. Non-allocating C-call wrappers
  7. Local allocation and stack allocation
  8. Copied-stack growth
  9. SIMD and vector memory operations
  10. DWARF, frame tables, and statepoint metadata

- [x] Atomic loads and stores. Check whether OCaml atomic primitives preserve the
   same ordering as the native backend.
  - Found and fixed: AArch64 atomic field loads used to lower to plain `ldr`.
    They now emit `fence acquire` plus a `seq_cst` LLVM atomic load, producing
    `dmb ishld; ldar` like the native backend. Covered by
    `testsuite/tests/llvm-codegen/atomic_load.ml`.
  - Audited for current macOS/arm64 LLVM builds: `Intop_atomic` is not reachable
    through the native-pointer/bigstring builtin path because the arm64 target
    reports `Catomic` unsupported, so those operations remain external calls.
    The dormant LLVM `Intop_atomic` lowering should be re-audited before any
    target enables `Catomic`.
- [x] Load mutability. LLVM lowering ignores final-load `mutability`, but this
  matches the native final emitters. `mutability` is already consumed by CFG CSE
  and vectorization before final lowering; final assembly selection does not need
  a separate mutable-vs-immutable instruction.
- [x] Exception control-flow edges. Real potentially-raising calls under a trap
  lower to `invoke` plus `landingpad`; AArch64 trap setup uses `wrap_try`
  marked `returns_twice`, records a recovery block address, and final assembly
  contains the recovery path. A focused smoke test with `call_catch`,
  `alloc_catch`, and `bounds_catch` compiled through `-llvm-backend` and ran
  correctly, and inspection with `-keep-llvmir -dllvmir` showed `invoke`,
  `landingpad`, statepoint bundles, `wrap_try`, and recovery assembly for real
  call/handler paths.
- [ ] Effect handlers. Effects stress stack switching, saved continuations, and
   non-local control flow.
  - Non-preemption effects passed under real LLVM use:
    `make llvm-test-one DIR=effects LLVM_PATH=/tmp/oxcaml-clang-wrapper`
    reported `127` passed, `28` skipped, `0` failed, with `2073` wrapper
    invocations containing `-x ir` and the fixed-register flags.
  - The skipped tests are the preemption subdirectory, because this checkout's
    `ocamltest_config` has `POLL_INSERTION=false`. A manual preemption attempt
    with `-enable-poll-insertion` timed out on both LLVM and the native backend,
    so it is not useful LLVM-specific evidence. Need either a poll-insertion
    configured build or to cover preemption under the copied-stack/preemption
    audit before marking this complete.
  - Added `testsuite/tests/llvm-codegen/effect_preemption.ml`, a focused
    LLVM smoke test that uses signal-driven preemption in two ways: allocation
    across a preemption with major GC in the handler, and an explicit `%poll`
    in a non-allocating loop. Both resume the continuation and check live data.
    `make llvm-test-one DIR=llvm-codegen
    LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed (`40` passed, `0` failed),
    with `2056` wrapper invocations containing `-x ir`. This gives real LLVM
    coverage for the preemption runtime path and LLVM `Poll` lowering, but does
    not replace a poll-insertion configured run of the full preemption suite.
- [x] C calls that can allocate. LLVM lowering sends allocating externals
  through `caml_c_call` or `caml_c_call_stack_args`, marks them as primitive
  calls, attaches `statepoint-id`, passes `gc-live` roots from
  `load_live_gc_roots_across`, and extracts the returned domain/allocation
  registers. A focused `-keep-llvmir -dllvmir` compile showed both
  `caml_c_call` with `gc-live` and `caml_c_call_stack_args` with a statepoint.
  Existing runtime coverage also passed under real LLVM use: `c-api` (`11`
  passed), `callback` (`38` passed, `1` skipped), and `statmemprof` (`48`
  passed, `3` skipped), with wrapper logs confirming `-x ir` and fixed-register
  flags.
- [x] Non-allocating C-call wrappers. Noalloc externals lower to private
  `c_call_wrapper.*` functions using the OCaml calling convention. The wrapper
  loads `Domain_c_stack`, switches to the C stack for the direct C call,
  switches back, and returns the original domain/allocation registers with the C
  result. Focused IR inspection of a noalloc external showed that shape, with
  the outer call represented as a statepoint after LLVM's rewrite. Existing C
  API coverage passed under real LLVM use. A noalloc C stub that raises is not a
  useful LLVM-specific test: the native backend does not catch that exception
  either, so that behavior is outside the supported noalloc contract.
- [x] Local allocation and stack allocation. `Begin_region` loads
  `Domain_local_sp`, `End_region` restores it, and local allocations update the
  domain local stack fields before computing the local block address. The slow
  path calls `caml_call_local_realloc` as a GC leaf; runtime inspection shows it
  grows the local arena with stat allocation rather than OCaml heap GC, and the
  assembly wrapper saves/restores all OCaml registers. A focused
  `-keep-llvmir -dllvmir` compile showed `caml_call_local_realloc` on the local
  slow path and a later `Gc.minor` statepoint with the local value live. The
  existing `typing-local` suite passed under real LLVM use (`87` passed, `6`
  skipped, `0` failed), including stack-allocation variants, local mutation,
  exceptions, effects, region loops, and local-GC regression tests.
- [ ] Copied-stack growth. Look for missed saved pointers, false positives, stale
   frame-pointer recovery, and interaction with LLVM spills.
  - Added `testsuite/tests/llvm-codegen/stack_growth.ml`, which compiles with
    `-llvm-backend`, runs an effect-handled non-tail-recursive computation, and
    checks that copied-stack growth completes with the expected result.
    `make llvm-test-one DIR=llvm-codegen
    LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed (`27` passed, `0` skipped,
    `0` failed), and the wrapper log recorded `2051` `-x ir` invocations with
    the fixed-register flags.
  - Source audit: on arm64, `gc_regs` points at saved `x0`; the rewrite helper
    scans the saved OCaml value GPR slots through `x25`, matching
    `runtime/arm64.S`. The raw copied-stack scan runs after exception and C
    stack-link rewrites, so it does not hide old-stack links from those typed
    rewrites.
  - Found and covered: the false-positive rewrite can be triggered. A raw
    `nativeint#` initialized to the current stack base can be kept live across
    stack growth; the arm64 LLVM fallback rewrites that raw word to the
    corresponding address in the new stack, even though it is not a pointer.
    Coverage is in `testsuite/tests/llvm-codegen/raw_stack_word.ml`; `make
    llvm-test-one DIR=llvm-codegen LLVM_PATH=/tmp/oxcaml-clang-wrapper`
    passed (`40` passed, `0` failed), with `2056` wrapper invocations
    containing `-x ir`.
  - Needed fix: replace the all-word copied-stack rewrite with precise
    metadata for stack-address-bearing slots/registers, or otherwise arrange
    that LLVM never preserves OCaml stack addresses in unreported raw locations
    across stack growth.
- [x] SIMD and vector memory operations. Check alignment, truncation/extension,
   calling convention, and unsupported sizes.
  - Source audit: LLVM lowering maps unaligned 128/256/512-bit memory chunks to
    LLVM loads/stores with `align 1`, and aligned chunks to normal vector
    loads/stores. On the current arm64 path, 256-bit vectors are split into two
    128-bit vectors before Cmm (`Lambda.split_vectors = true`), so the
    unimplemented LLVM 256/512 scalar static-cast paths are not reachable for
    the supported arm64 SIMD operations audited here.
  - A manual `-llvm-backend` SIMD smoke test passed with real LLVM use
    (`2` `-x ir` wrapper invocations). It covered unaligned 128-bit
    bytes loads/stores, scalar-to/from-`int64x2#` builtin casts, and unboxed
    vector calls through the existing SIMD C stubs.
  - `make llvm-test-one DIR=typing-layouts-vec128
    LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed (`5` passed, `0` skipped,
    `0` failed), with `2025` `-x ir` wrapper invocations with fixed-register
    flags.
- [ ] DWARF, frame tables, and statepoint metadata. Verify that LLVM-generated
    frame layout, live roots, return addresses, and debug info agree with the
    runtime and debugger.
  - Ordinary frame-table emission works in a focused manual smoke test: a
    small allocating program compiled with `-g -O3 -llvm-backend -S`, ran
    successfully, and emitted a `caml...__frametable` with descriptor count,
    live-root offsets, allocation metadata, and debug strings. The wrapper log
    confirmed real LLVM use.
  - Found and fixed: the LLVM frametable printer emitted only the short
    frame-descriptor format. A function with a large static frame aborted in
    LLVM codegen instead of using the native backend's long-frame descriptor
    path. `OxCamlGCPrinter.cpp` now emits `FRAME_LONG_MARKER`, 32-bit frame
    data, 32-bit live count, and 32-bit live offsets when the short format is
    too small. Coverage is in `testsuite/tests/llvm-codegen/long_frame.ml`;
    `make llvm-test-one DIR=llvm-codegen LLVM_PATH=/tmp/oxcaml-clang-wrapper`
    passed (`40` passed, `0` failed), and the wrapper log recorded `2057`
    `-x ir` invocations. A manual `-S` check showed `.short 32767` followed by
    `.long 67793`, `.long 4300`, and 32-bit live offsets in the frametable.
