# loop_invariant_gc_across_call ARM64 evidence

This directory records the `loop_invariant_gc_across_call` case from
`agent-state/test-suite-29e4cd/loop_invariant_microbench`.

The artifacts are intentionally ARM64/Darwin outputs.  They are meant as a
known-good reference for the ARM backend behavior while comparing the AMD64
bring-up work against ARM.

Files:

- `loop_invariant_gc_across_call.ml`: source program.
- `loop_invariant_gc_across_call.llvm.ll`: LLVM IR from the executable compile.
- `loop_invariant_gc_across_call.asm.llvm.ll`: LLVM IR from the `-S -c`
  assembly compile.
- `loop_invariant_gc_across_call.llvm.s`: LLVM backend assembly.
- `loop_invariant_gc_across_call.native.s`: native backend assembly.

The loop shape is:

```ocaml
let x = Sys.opaque_identity "loop_invariant_payload" in
...
loop x n 0
```

where the loop calls `tick`, then uses `String.length x`.  The interesting
question is how the backend treats the loop-invariant GC value `x` across the
call.
