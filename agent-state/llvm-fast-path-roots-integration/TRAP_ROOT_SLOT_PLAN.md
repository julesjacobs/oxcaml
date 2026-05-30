# Trap root slot plan

## Goal

Make LLVM AArch64 `try` code use the explicit exceptional control-flow edges
that we now emit, instead of forcing ordinary live values through volatile
memory slots.

The native-like model we want is:

- calls that can raise have an explicit exceptional edge to the active handler;
- ordinary OCaml values live across the call are normal LLVM values;
- GC root metadata still describes where live roots are at safepoints;
- trap recovery only reconstructs runtime ABI state;
- `Stack (Domainstate _)` locations are treated as call-boundary argument
  storage, not as stable local storage.

## Current compiler shape

The current emitter still emits volatile root-slot traffic in hot `try` code.
For example, `direct_call_in_try_hit` has this shape:

```llvm
store volatile i64 %arg0, ptr %slot0
store volatile i64 %arg1, ptr %slot1
%r = invoke ... to label %normal unwind label %recovery_landingpad
...
%arg0_reload = load volatile i64, ptr %slot0
%arg1_reload = load volatile i64, ptr %slot1
```

It also emits a volatile save/restore around a domain-state extra-parameter
slot:

```llvm
%p = inttoptr i64 (ds + 64) to ptr
%old = load i64, ptr %p
store volatile i64 %old, ptr %slot
...
%old_reload = load volatile i64, ptr %slot
store i64 %old_reload, ptr %p
```

That slot is a `Stack (Domainstate _)` location in `Domain_extra_params`, not a
general "exception handler" slot. `reg.mli` says these locations are shared
between all functions in a domain. The caller stores them immediately before a
call, and the callee copies them immediately.

## Critique round 1

Naive idea: remove `across_traps` from `preserved_reg_slots`.

Problem: `preserved_reg_slots` currently has two different meanings:

1. keep an alloca address available so `gc-live` operand bundles can name the
   root location;
2. make all loads and stores for that register volatile.

Those are not the same requirement. GC metadata may still need an addressable
root slot at safepoints, but that does not mean every access to the slot should
be volatile. Removing the register from `preserved_reg_slots` can also remove it
from `gc-live` bundles, which is not a safe performance change.

Result: do not remove `preserved_reg_slots` wholesale.

## Critique round 2

Better idea: split "addressable root slot" from "volatile slot".

The addressable root slot is needed when the stackmap/frametable path needs a
stable memory location for a live root. Passing the alloca pointer in the
`gc-live` operand bundle should keep the slot addressable at the safepoint.

The volatile slot is only needed when LLVM cannot see a control-flow or memory
dependency and might otherwise promote away a value that hidden runtime control
flow reads. With explicit AArch64 exceptional edges, ordinary values no longer
need this. The one case that still needs conservatism is `Stack (Domainstate _)`
because the current emitter maps it directly to shared domain-state memory.

Result: keep `preserved_reg_slots` for `gc-live` bundles, but use a separate
`volatile_reg_slots` set for volatile loads and stores.

## Critique round 3

Too-aggressive implementation idea: on AArch64, make only
`Stack (Domainstate _)` slots volatile.

Problem: the save of a domain-state extra-parameter value is not itself a
`Stack (Domainstate _)` register by the time the handler reloads it. The
compiler first loads from `Domain_extra_params` into an ordinary local pseudo,
then that local pseudo is live across the trap. Filtering volatile slots by
`Reg.is_domainstate` misses this case and lets LLVM fold the recovery shape into
an invalid block.

Result: on AArch64, the volatile set should be the values live across the trap
edge, not the values whose own location is `Stack (Domainstate _)`.

## Critique round 4

That still does not solve the hot case.

The focused `direct_call_in_try_hit` check showed that the ordinary loop values
are themselves live across the trap edge, so keeping all trap-edge values
volatile preserves the current slowdown. Removing all trap-edge volatility made
LLVM optimize to the shape we want for ordinary values, but the current
LLVM-side recovery-block recognizer rejected the result.

The important observation is that this rejection is a property of our current
prototype, not a semantic reason to keep volatile slots.

There are two distinct classes of values at the handler entry:

- recovery ABI values: exception bucket, previous trap pointer, allocation
  pointer, and domain-state pointer. These come from the raising path and must
  be snapped from x0/x26/x27/x28 by `trap.recover`;
- caller lexical values: loop variables, saved domain-state extra-parameter
  values, and other source variables captured by the handler. These are the
  caller's values at the raising call. LLVM should model them as ordinary SSA
  values live across the `invoke` exceptional edge. Codegen must then spill them
  across the call if the runtime-entered edge clobbers their registers.

The current LLVM-side shape checker forbids protected-path SSA operands in the
recovery block. That was too strict. It forced us toward volatile slots even for
values LLVM can handle normally.

The better invariant is:

- `trap.recover` must be the first real instruction in the runtime-entered
  recovery block;
- no caller lexical value may be used before `trap.recover`;
- after `trap.recover` has snapped x0/x26/x27/x28, ordinary handler code may use
  normal SSA values from the `invoke` exceptional edge;
- the AArch64 runtime-entry machine pass must make calls on edges to
  runtime-entered blocks clobber allocatable registers, forcing those ordinary
  values to stack slots when needed.

This matches the user's intended model: the handler gets `ds`, `alloc`, and the
exception bucket from the raising path, while other variables remain ordinary
locals that LLVM can promote and spill normally.

## Stabilized implementation plan

1. Change the LLVM trap-recovery recognizer so protected-path SSA operands are
   allowed after the first `trap.recover` instruction, but not before it.
2. Keep the machine-level runtime-entered block invariant:
   - required ABI live-ins are x0, x26, x27, and x28;
   - no PHIs in the runtime-entered block;
   - call predecessors clobber allocatable registers on runtime-entered edges.
3. Remove AArch64 trap-edge volatility for ordinary values in the OCaml LLVM
   emitter.
4. Rebuild enough of the compiler to regenerate focused IR and check that:
   - ordinary hot-path values in `direct_call_in_try_hit` are no longer volatile;
   - `maybe_raise_in_try` and `nested_handler_raise` compile after `opt -O3`;
   - MIR shows x0/x26/x27/x28 as recovery ABI live-ins;
   - ordinary caller lexical values are spilled/reloaded as needed, not treated
     as physical ABI live-ins;
   - focused exception tests still pass.

## Remaining proper fix

The full native-like cleanup is to stop mapping `Stack (Domainstate _)`
directly to domain-state memory for the whole function. Instead:

- copy incoming domain-state extra arguments into local temporaries at function
  entry;
- use those temporaries inside the function;
- flush outgoing domain-state extra arguments immediately before calls and
  tailcalls that need them;
- load domain-state extra results immediately after calls that produce them.

That is a larger calling-convention change. It is separate from the LLVM
recovery recognizer prototype here.

Also audit and remove the recovery-side `write_trap_pointer_register` path.
For AArch64 runtime-entered recovery, this is not native-shaped: the runtime
raise sequence has already popped the trap frame and entered recovery with the
previous trap pointer in x26. The recovery intrinsic should model that x26 value
directly. Emitting a side-effect inline asm `mov x26, ...` in the recovery
block is redundant in the intended model and prevents LLVM/regalloc from making
the recovery handoff disappear.

## LLVM-side prototype result

The current prototype changes the LLVM recognizer, not the OCaml emitter.

Old recognizer rule:

- the whole runtime-entered recovery block could only use values defined in the
  recovery block or materializable entry-block expressions.

New recognizer rule:

- before `trap.recover`, the old restriction still applies;
- after `trap.recover`, ordinary protected-path SSA values are allowed.

This matches the semantic model:

- `trap.recover` snapshots the recovery ABI values from x0, x26, x27, and x28;
- normal handler code after that may use source variables from the throwing call
  as ordinary SSA values;
- machine codegen treats the runtime-entered edge as clobbering allocatable
  registers, so values needed by the handler are spilled across the call if
  needed.

Prototype tests:

- `oxcaml-trap-recovery-invoke-protected-ssa.ll`
  - protected SSA value used after `trap.recover`;
  - alloca/store/load shape like the OCaml LLVM backend emits;
  - `opt -O2` promotes the alloca away;
  - `llc -stop-after=greedy` spills/reloads the protected value across the
    exceptional call edge.
- `oxcaml-trap-recovery-invoke-protected-ssa-before-recover.ll`
  - protected SSA value used before `trap.recover`;
  - still rejected.
- `oxcaml-trap-recovery-invoke-recover-not-first.ll`
  - fixed to use a real protected value before `trap.recover`, so it still tests
    the intended invalid shape.

Manual validation with the rebuilt LLVM tools:

```text
ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc opt
opt -S -O2 protected-ssa.ll | llc -mtriple=arm64-apple-macosx -verify-machineinstrs
opt -S -O2 protected-ssa.ll | llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=greedy
llc -mtriple=arm64-apple-macosx -verify-machineinstrs selected trap recovery tests
```

The current compiler-emitted all-nonvolatile IR prototypes for
`maybe_raise_in_try`, `nested_handler_raise`, and `direct_call_in_try_hit` also
pass `opt -O3` followed by `llc -verify-machineinstrs` with this LLVM-side
change.
