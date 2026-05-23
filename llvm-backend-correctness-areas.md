# LLVM Backend Correctness Areas

Potential areas to audit next:

- [ ] Atomic loads and stores. Check whether OCaml atomic primitives preserve the
   same ordering as the native backend.
  - Found and fixed: AArch64 atomic field loads used to lower to plain `ldr`.
    They now emit `fence acquire` plus a `seq_cst` LLVM atomic load, producing
    `dmb ishld; ldar` like the native backend. Covered by
    `testsuite/tests/llvm-codegen/atomic_load.ml`.
  - Still to audit: `Intop_atomic` lowering for native-pointer/bigstring-style
    atomics. Standard `Atomic.set`/`exchange`/CAS on arm64 currently goes
    through runtime helper calls because `Catomic` is unsupported by the arm64
    native backend.
- [ ] Load mutability. LLVM lowering still ignores `mutability` and `is_atomic` on
   loads, which may matter for TSAN, CSE assumptions, or memory-model behavior.
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
