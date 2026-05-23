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
- [ ] Exception control-flow edges. Verify that raise paths are visible enough to
   LLVM, statepoints, and liveness.
- [ ] Effect handlers. Effects stress stack switching, saved continuations, and
   non-local control flow.
- [ ] C calls that can allocate. Check statepoint roots and runtime register
   preservation around `caml_c_call`, `caml_c_call_stack_args`, callbacks, and
   blocking sections.
- [ ] Non-allocating C-call wrappers. Verify stack switching, register
   preservation, unwind behavior, and GC root handling.
- [ ] Local allocation and stack allocation. Check `Begin_region`, `End_region`,
   `caml_modify_local`, and local roots under LLVM optimization.
- [ ] Copied-stack growth. Look for missed saved pointers, false positives, stale
   frame-pointer recovery, and interaction with LLVM spills.
- [ ] SIMD and vector memory operations. Check alignment, truncation/extension,
   calling convention, and unsupported sizes.
- [ ] DWARF, frame tables, and statepoint metadata. Verify that LLVM-generated
    frame layout, live roots, return addresses, and debug info agree with the
    runtime and debugger.
