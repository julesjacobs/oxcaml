# LLVM Backend Correctness Areas

Potential areas to audit next:

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
- [ ] Local allocation and stack allocation. Check `Begin_region`, `End_region`,
   `caml_modify_local`, and local roots under LLVM optimization.
- [ ] Copied-stack growth. Look for missed saved pointers, false positives, stale
   frame-pointer recovery, and interaction with LLVM spills.
- [ ] SIMD and vector memory operations. Check alignment, truncation/extension,
   calling convention, and unsupported sizes.
- [ ] DWARF, frame tables, and statepoint metadata. Verify that LLVM-generated
    frame layout, live roots, return addresses, and debug info agree with the
    runtime and debugger.
