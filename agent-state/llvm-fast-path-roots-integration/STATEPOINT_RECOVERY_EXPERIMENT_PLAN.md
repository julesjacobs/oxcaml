# Statepoint recovery experiment plan

## Goal

Find the correct LLVM IR representation for GC pointers that are live into an
OxCaml trap handler.

Do this before changing the OCaml emitter. The first question is what LLVM
already supports for `invoke` statepoints, `landingpad token`, and
`gc.relocate`. The second question is how to combine that valid shape with the
OxCaml-specific `runtime-entered` recovery block and `trap.recover`.

## Order

Run experiments in two layers:

1. Stock/current LLVM statepoint experiments.
2. OxCaml-modified LLVM recovery experiments.

Do not start by modifying LLVM for a statepoint shape that stock LLVM already
rejects or handles differently than expected.

## Layer 1: stock/current LLVM IR experiments

These experiments should avoid OxCaml `runtime-entered` machinery where
possible. They are about LLVM's existing statepoint model.

### Experiment 1: unique invoke statepoint

Shape:

```llvm
%tok = invoke token @llvm.experimental.gc.statepoint(...)
       ["gc-live"(%obj)]
       to label %normal
       unwind label %recover

recover:
  %lp = landingpad token cleanup
  %obj2 = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %lp, ...)
```

Questions:

- Does the verifier accept exceptional `gc.relocate` tied to the landingpad
  token?
- Does `opt -O2` preserve the shape?
- Does `llc` lower it without losing the GC pointer?
- What stackmap/root location is produced?

Expected useful result:

- If this works, use exceptional `gc.relocate` for one invoke statepoint to one
  recovery landingpad.

### Experiment 2: two invoke statepoints sharing one landingpad

Shape:

```llvm
%tok_a = invoke token @llvm.experimental.gc.statepoint(...)
         unwind label %recover

%tok_b = invoke token @llvm.experimental.gc.statepoint(...)
         unwind label %recover

recover:
  %lp = landingpad token cleanup
  %obj2 = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %lp, ...)
```

Questions:

- Does LLVM accept this?
- If accepted, which statepoint does the landingpad token identify?
- Does optimization split, reject, or miscompile the shape?

Expected useful result:

- If this is rejected or ambiguous, shared recovery cannot use one exceptional
  `gc.relocate` directly.

### Experiment 3: separate recovery landingpads, then shared handler

Shape:

```llvm
%tok_a = invoke token @llvm.experimental.gc.statepoint(...)
         unwind label %recover_a

%tok_b = invoke token @llvm.experimental.gc.statepoint(...)
         unwind label %recover_b

recover_a:
  %lp_a = landingpad token cleanup
  %obj_a = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %lp_a, ...)
  br label %handler

recover_b:
  %lp_b = landingpad token cleanup
  %obj_b = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %lp_b, ...)
  br label %handler

handler:
  %obj = phi ptr addrspace(1) [ %obj_a, %recover_a ], [ %obj_b, %recover_b ]
```

Questions:

- Does this verify and optimize?
- Does `llc` lower both exceptional relocates correctly?
- Do PHIs after per-edge relocate behave normally?
- Does any pass try to merge the recovery blocks and break the association?

Expected useful result:

- If this works, prefer this shape for shared OCaml handlers when GC values are
  live into the handler.

### Experiment 4: explicit root-slot fallback

Shape:

```llvm
store ptr addrspace(1) %obj, ptr %handler_root_slot

%tok = invoke token @llvm.experimental.gc.statepoint(...)
       ["gc-live"(ptr %handler_root_slot)]
       to label %normal
       unwind label %recover

recover:
  %lp = landingpad token cleanup
  %obj2 = load ptr addrspace(1), ptr %handler_root_slot
```

Questions:

- Does the stackmap/frame-table path report the root slot correctly?
- Does the slot need to be volatile, or is addressability through `gc-live`
  enough?
- Does `mem2reg` keep the slot when it must remain a root location?
- What assembly cost does this introduce?

Expected useful result:

- Use this only when exceptional `gc.relocate` cannot represent the shape.
- Keep it narrow: only handler-live GC pointers that need the slot.

### Experiment 5: non-GC alloca/SSA control

Shape:

- same control flow as the GC cases;
- handler-live integer or non-GC value;
- no volatile slot.

Questions:

- Does `mem2reg` produce PHIs?
- Does machine PHI elimination place copies before the invoke/call?
- Does regalloc spill/reload across the exceptional edge?

Expected useful result:

- Confirms non-GC handler-live values should stay on the SSA/PHI path.

## Layer 2: OxCaml-modified LLVM experiments

After Layer 1 identifies valid statepoint shapes, combine them with OxCaml
trap recovery.

### Experiment 6: direct recovery landingpad with `trap.recover`

Shape:

```llvm
call void @llvm.aarch64.oxcaml.trap.publish(..., blockaddress(@f, %recover))

%tok = invoke token @llvm.experimental.gc.statepoint(...)
       to label %normal
       unwind label %recover

recover:
  %lp = landingpad token cleanup
  %rec = call @llvm.aarch64.oxcaml.trap.recover()
```

Questions:

- Does the OxCaml recognizer classify the direct landingpad as
  `runtime-entered`?
- Does the block have exactly `x0`, `x26`, `x27`, `x28` live-ins?
- Does final assembly avoid skipped trampoline work?

### Experiment 7: runtime-entered PHI lowering

Shape:

- two invokes unwind to one `runtime-entered` recovery landingpad;
- recovery has scalar PHIs before `landingpad token`;
- `trap.recover` snapshots ABI state.

Questions:

- Does verifier allow runtime-entered PHIs before PHI elimination?
- Does PHI elimination lower copies before the possibly-raising call?
- Does runtime-entry clobber insertion after PHI elimination force correct
  spilling?

### Experiment 8: combine GC handling with `trap.recover`

Run two variants based on Layer 1 results:

- per-recovery exceptional `gc.relocate`, then `trap.recover`, then shared
  handler;
- explicit root-slot fallback, then `trap.recover`, then load/use in handler.

Questions:

- Which instruction order is legal for `landingpad`, `gc.relocate`, and
  `trap.recover`?
- Does `trap.recover` still count as first required runtime-entry operation if
  exceptional `gc.relocate` must appear immediately after `landingpad`?
- If not, do we need to define the invariant as "no ordinary handler code before
  `trap.recover`" rather than "`trap.recover` is literally first after
  landingpad"?

This is the most important design-deciding experiment.

## Decision criteria

Prefer a design if:

- LLVM verifier accepts it without special casing invalid IR;
- `opt -O2` and `opt -O3` preserve the intended statepoint association;
- `llc -verify-machineinstrs` passes;
- final assembly has no skipped block work;
- non-GC values are optimized through SSA/PHI rather than volatile slots;
- GC values are represented precisely enough for frame tables and handler use;
- the shape can be generated systematically from the OCaml LLVM backend.

Fall back to explicit root slots for handler-live GC values if:

- shared recovery makes exceptional `gc.relocate` ambiguous;
- LLVM requires unique landingpad/statepoint association;
- optimization merges or splits blocks in a way that loses the association.

Do not fall back to broad volatile trap-live slots unless a narrower
representation is proven unsound.
