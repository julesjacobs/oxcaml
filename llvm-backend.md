# OxCaml on LLVM

This LLVM backend currently only works for arm64.

| Benchmark | Speedup | Compile-time increase |
| --- | ---: | ---: |
| `almabench` | 16.5% | 115% |
| `bdd` | 1.2% | 82% |
| `boyer` | -3.5% | 75% |
| `boyer_no_exc` | 3.2% | 95% |
| `fft` | 4.9% | 88% |
| `kb` | 12.9% | 119% |
| `kb_no_exc` | 12.6% | 122% |
| `nucleic` | 17.1% | 92% |
| `raytrace` | 12.3% | 70% |
| `splay` | 0.5% | 86% |
| `matmul` | 47.0% | 62% |
| `matmul_transposed` | 199.3% | 89% |
| `xxhash` | 28.9% | 86% |
| compiler itself | 4.1% | 68% |

Benchmarks are from jsoo except `matmul`, `matmul_transposed`, `xxhash`, and
compiler itself. 

## What changed

This required changes to the OxCaml compiler as well as changes to LLVM itself:

- fit OxCaml into LLVM's GC model, where GC pointers live in SSA values and
  frame tables are derived from their types; this required preserving the
  distinction between GC pointers and integers through the compiler pipeline;
- rematerialized interior pointers (which LLVM optimizations can create) as a
  base GC pointer plus a non-GC offset, because the OxCaml GC does not support
  arbitrary interior pointers;
- added LLVM intrinsics and lowering for `pushtrap`, `poptrap`, and trap
  handlers - LLVM makes assumptions about its exception handler model that
  are violated by O(x)Caml's `pushtrap`/`poptrap` mechanism, for example,
  its RS4GC pass has to be changed to take this into account;
- changed register allocation to aggressively split live ranges at call sites, 
  because LLVMs register allocator does poorly with OxCaml's no-callee-saves;
- added OxCaml calling conventions for normal calls, allocation calls, C calls,
  GC/runtime calls, with fixed runtime-state registers;
- avoid pinning registers like the trap and domain registers so that regalloc
  can use them freely except on call boundaries;
- improved statepoint lowering to reduce redundant stack stores for GC roots;
- made llvm not emit `stp` (store pair) and `ldp` (load pair) because those
  turned out to be much slower (in some cases 10x slower)
- adapted stack-check sinking to LLVM, taking into account the fact that LLVM
  may already use stack space in its prelude before the body runs;
- and more.

Currently the compiler is able to self-build and pass the test suite, but there
are bound to be bugs.

## What's still slow

The main problem is that LLVMs GC support is not ideal, because  spilling around
safepoints is not decided by the register allocator, but by ad-hoc safepoint 
lowering. This results in lower code quality than the register allocator would 
produce for non-GC values. Fixing this is an invasive change because it would 
require tracking GC-pointerness all the way through register allocation. Instead, 
I improved safepoint lowering, which helps but does not solve the whole problem.

`boyer` is the main slowdown and it is caused by that problem. It uses exceptions 
for failed unification, and the LLVM lowering still spills too much around exception handlers. Exceptions themselves are fast, but GC values that are live over safepoints
and subsequently live into the exception handler sometimes get redundant stack slots
and redundant stack stores. This is primarily caused by LLVM thinking that a GC
pointer can change when it goes over a safepoint. That is in fact true, but this 
pessimizes spilling in an unnecessary way. For example, when a value has previously
been spilled and then goes over another safepoint, we don't need to spill it again:
it is true that its value may change at the safepoint, but the GC will *also* update
the spill slot in the same consistent way, so we don't need to overwrite the spill
slot with the "new" value: the GC has already placed the new value there.

Representative LLVM sequence:

```asm
str x1, [sp, #64]
str x0, [sp, #56] (* Redundant slots *)
str x0, [sp, #48]
str x9, [sp, #72]
str x10, [sp, #40]
str x9, [sp, #32]
bl _camlBoyer__unify_9_29_code
...
ldr x0, [sp, #40]
ldr x11, [sp, #64]
ldr x10, [sp, #56]
ldr x12, [sp, #32]
...
str x11, [sp, #64] (* Redundant stores *)
str x10, [sp, #56]
str x12, [sp, #48]
str x12, [sp, #72]
str x10, [sp, #40]
bl _camlBoyer__as_rec_8_34_code
```

Some of these stack stores are entirely redundant: the exact same value gets put into 
multiple spill slots, and subsequent stores overwrite those slots redundantly as well.
Fixing this properly requires changing the register allocator to understand safepoints.