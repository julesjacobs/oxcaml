# Background

This branch is moving the OxCaml LLVM backend toward a no-frontend-roots
model.

The intended model is:

- The frontend emits LLVM IR with the right value representation.
- Every OCaml value that may be a GC-managed value is represented as
  `ptr addrspace(1)` in LLVM IR.
- Later, our customized `RewriteStatepointsForGC` pass discovers live
  `addrspace(1)` values at statepoints and materializes the GC data needed by
  the OxCaml runtime.
- The frontend should not be responsible for explicitly spelling ordinary
  register-slot GC roots in the IR.

This is the model LLVM's statepoint pipeline is built around: the IR carries
typed GC pointer values, and RS4GC rewrites statepoints so the backend can
lower precise GC metadata.

## Cfg Registers and SSA

The frontend initially translates Cfg registers through stack slots: each Cfg
register is represented by an LLVM `alloca`, with Cfg reads and writes
translated to loads and stores. In the normal pipeline, LLVM's `mem2reg`
promotion then turns those allocas into SSA values.

This is also the intended path for OCaml values represented as
`ptr addrspace(1)`. If a Cfg register holds an `addrspace(1)` value, its
alloca is still just the temporary lowering form. After `mem2reg`, the value
should be a normal SSA `addrspace(1)` value that RS4GC can see at statepoints.

## OxCaml Value Model

An OxCaml `ptr addrspace(1)` value is an OCaml value. It can be either:

- a true GC heap pointer; or
- an immediate, such as an OCaml integer.

This distinction matters. The LLVM type alone says "this is an OCaml value",
not "this is definitely a heap object pointer".

The opposite direction also matters: an `i64` value may be an ordinary integer,
an OCaml immediate, a static symbol address, an allocation pointer, or a raw
copy of a heap pointer. After LLVM optimization, a local late pass usually
cannot prove which semantic case an arbitrary integer represents.

## What Is Dangerous

The dangerous pattern is:

1. Start with a true GC heap pointer.
2. Cast it to an integer.
3. Carry that integer across a statepoint.
4. Cast the integer back to `ptr addrspace(1)` and use it as an OCaml value.

That is unsafe with a moving GC. While the value is carried as an integer,
RS4GC does not see it as a GC pointer and does not make the frametable update
it. If the GC moves the object at the statepoint, the integer still contains
the old address. Casting that stale integer back to `ptr addrspace(1)` gives a
stale heap pointer.

The same IR shape is not automatically wrong when the original value was an
immediate. For example, an OCaml `int` can cross a statepoint as an integer and
later be cast to `ptr addrspace(1)` for a callee expecting a `val`. There is no
moved heap object in that case, so there is no pointer for the frametable to
update.

This is why a late checker cannot simply reject every integer that crosses a
statepoint and later becomes `ptr addrspace(1)`. That pattern is a useful
debugging heuristic, not a correctness invariant by itself.

## Derived Pointers

The OxCaml GC runtime cannot handle interior pointers in its metadata. A
statepoint cannot expose a live derived heap pointer and expect the runtime to
fix it up directly.

The intended RS4GC behavior is:

- identify the base object pointer for each live derived `addrspace(1)` value;
- root or relocate the base pointer;
- rematerialize the derived pointer from the relocated base after the
  statepoint.

If RS4GC cannot prove and rematerialize the base relation, it should fail
closed before stackmap lowering rather than silently producing metadata the
runtime cannot interpret.

## Allocation Pointers

Allocation-pointer arithmetic is a special boundary case. Fresh object
construction naturally starts from the allocation pointer, which is an integer
runtime register. It is expected to compute a fresh object address from that
integer and then introduce a `ptr addrspace(1)` value for the newly allocated
object.

That is different from taking an existing heap object pointer, hiding it in an
integer across a statepoint, and later reconstructing it.

## Practical Diagnostic Rule

The current useful diagnostic is heuristic:

- find integer values live across a statepoint;
- find those that later contribute to `inttoptr ... to ptr addrspace(1)`;
- filter obvious allocation-pointer-derived fresh-object construction;
- report the remaining candidates for human investigation.

The report should not claim the candidate is definitely wrong. The human
question is: did this integer represent a true GC heap pointer, or was it an
immediate/static/allocation-derived value?

The important correctness invariant remains representation-driven: true live
GC heap values must be visible to RS4GC as `addrspace(1)` values at
statepoints, or must be rematerializable from visible relocated bases.
