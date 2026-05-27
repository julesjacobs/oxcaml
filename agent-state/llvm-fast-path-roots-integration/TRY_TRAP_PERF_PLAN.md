# Try/trap performance plan

## Current problem

LLVM AArch64 `try` lowering is much slower than native lowering in hot
non-raising code and in expected-control-flow exception paths.

Representative repros from the current benchmark set:

- `closure_call_in_nested_try_hit`: about 2.0x slower
- `closure_call_in_try_hit`: about 2.0x slower
- `env_find_same_layered_hit`: about 1.7x slower
- `direct_call_in_try_hit`: about 1.6x slower
- `try_with_string_compare_hit`: about 1.3x slower

The direct string-compare controls are not the problem. The cost comes from
active trap setup and recovery modeling.

## Native shape

Native AArch64 pushes a two-word trap block on the OCaml stack:

```asm
adr x16, handler
stp x26, x16, [sp, #-16]!
mov x26, sp
```

Normal exit restores x26:

```asm
ldr x26, [sp], #16
```

`Raise_notrace` jumps through the trap chain inline:

```asm
mov sp, x26
ldp x26, x16, [sp], #16
br x16
```

## Current LLVM shape

LLVM AArch64 `Pushtrap` calls `_wrap_try` as a `returns_twice` call. The helper
body is trivial, but the call remains on the hot path and forces extra work:

- save domain state and allocation pointer around the call;
- store previous trap pointer;
- store restore-SP delta;
- store saved frame pointer when frame pointers are enabled;
- store resume and recovery PCs;
- publish x26;
- reload and pop the trap block on normal exit.

`Raise_notrace` also calls `caml_raise_notrace` instead of emitting the native
trap jump inline.

## Patch 1: inline `Raise_notrace`

This is independent of the `_wrap_try` removal.

Lower AArch64 `Raise_notrace` to inline assembly equivalent to:

```asm
mov x0, $bucket
mov sp, x26
ldp x26, x16, [sp], #16
br x16
```

Expected effect:

- improves expected-control-flow exceptions, especially `env_find_same_layered_hit`;
- does not address non-raising `try` overhead;
- keeps the current LLVM trap-block layout and recovery target.

Checks:

- no `bl _caml_raise_notrace` in focused expected assembly;
- bucket reaches x0;
- caught `raise_not_trace`, nested handlers, reraise, and uncaught exception
  sanity tests still pass.

## Patch 2: remove `_wrap_try`

The current viable direction is a target-owned AArch64 trap-region design. Do
not implement inline-asm `callbr` as the final owner of x26 publication.

Update after the latest spike: use a generic `runtime-entered` MachineBasicBlock
classification for the recovery block, not `landing-pad`, unless a later audit
finds a blocker.

The modeled construct is a dynamic handler registration with a hidden runtime
transfer into a recovery entry. It is not LLVM EH.

### Required invariants

- The trap block is not published until all fields needed by the runtime
  recovery path are initialized.
- The stored recovery target and the modeled Machine CFG recovery successor
  are derived from the same machine block target. Do not let an arbitrary symbol
  drift away from the modeled successor.
- x26 publication is visible in MachineInstr semantics.
- The hidden runtime transfer is represented by an explicit codegen concept that
  survives verifier, PHI elimination, live interval construction, pseudo
  expansion, register allocation, and final assembly.
- The recovery entry starts by snapshotting the recovery ABI values:
  - x0: exception bucket;
  - x26: previous trap pointer;
  - x27: allocation pointer;
  - x28: domain state pointer.
- Recovery rethreads `domainstate_ptr`, `allocation_ptr`, and the exception
  bucket exactly where `_wrap_try`/`returns_twice` currently does it.
- AArch64 stack repair happens before ordinary handler code runs. Runtime
  `JUMP_TO_TRAP_PTR` enters with `sp = trap_block + 16`; current LLVM lowering
  repairs `sp` and optionally x29 in module asm before branching to the real
  handler.
- The recovery entry does not use ordinary protected-path SSA values or caller
  scratch registers such as x1 unless they are explicitly reloaded or otherwise
  modeled.
- The recovery block is represented with a generic block classification that
  makes allocatable physical live-ins legal. The spike used `landing-pad`; the
  real patch must either add a generic runtime-entered ABI block concept or
  deliberately audit and test EH-pad reuse.
- Active-trap root handling stays conservative until a separate patch proves a
  narrower root-map rule is safe.

### Current experiment result

The LLVM-modifying spike proved the recovery-copy mechanism, not the whole
hidden edge.

Experimental changes:

- Add one AArch64 intrinsic:
  `llvm.aarch64.oxcaml.trap.recover`.
- Select it to ordered copies from x0, x26, x27, and x28.
- Add those physical registers as live-ins on the recovery block.
- Temporarily mark `gc "ocaml"` `callbr` indirect targets as landing-pad-like
  so MachineVerifier permits allocatable physical live-ins.

The spike passes:

- `opt -passes=verify`;
- `opt -O2`;
- `llc -verify-machineinstrs -stop-after=finalize-isel`;
- `llc -verify-machineinstrs -stop-after=greedy`;
- full `llc -verify-machineinstrs`.

The stronger ABI-only test is
`try_trap_spikes/callbr_truth_test/callbr_target_recover_intrinsic_abi_only.ll`.
Its recovery block has only recovery ABI live-ins:

```mir
bb.3.exn_entry (machine-block-address-taken, landing-pad, inlineasm-br-indirect-target):
  liveins: $x0, $x26, $x27, $x28
  %14:gpr64 = COPY $x0
  %15:gpr64 = COPY $x26
  %16:gpr64all = COPY $x27
  %17:gpr64all = COPY $x28
```

Final recovery assembly is:

```asm
LBB0_2:
  add x0, x0, x26
  ret
```

This shows the recovery ABI values can be modeled explicitly and then disappear
when they already match the calling convention. It does not prove that real
raising transfers always have current x27/x28 state; that remains a proof
obligation for the target-owned trap-region operation.

A follow-up spike replaced the temporary `landing-pad` classification with a
generic `runtime-entered` MachineBasicBlock bit. That bit is printed and parsed
in MIR, is accepted by MachineVerifier for allocatable physical live-ins, and is
treated by LiveIntervals as an ABI-entry block. The combined
publish-plus-recover test verifies on macOS and Linux without `landing-pad`.
Removing `runtime-entered` from the same MIR makes MachineVerifier reject the
x0/x27/x28 live-ins, which is the intended negative check.

Devil's advocate tests found that `runtime-entered` is necessary but not
sufficient:

- it permits extra allocatable live-ins such as x1 unless OxCaml adds an exact
  recovery ABI check;
- it does not by itself require the recovery snapshot to be first;
- it does not prevent protected-path SSA values from being used in recovery;
- most importantly, the current `callbr` carrier models a recovery edge only at
  trap publication time, not from calls or other runtime-entering operations
  inside the protected body.

Follow-up prototypes addressed the first three with validation:

- classify a recovery block as `runtime-entered` only when the publish/recover
  block shape is exact;
- reject non-x0/x26/x27/x28 live-ins on `runtime-entered` blocks;
- reject PHIs in `runtime-entered` blocks.

The active-body call issue was addressed only as a MIR prototype: adding a
hidden successor from the call block to the `runtime-entered` recovery block
verifies through `none` and `greedy`. This means the approach remains viable,
but the final Patch 2 design is an active trap-region design, not a single
publication-edge design.

### Current Patch 2 design

Implement one target-owned trap-entry/publish operation that owns, in order:

1. store previous trap pointer;
2. store or materialize the exact recovery label;
3. store any remaining trap-block fields needed by the current ABI;
4. publish x26;
5. expose the normal successor and recovery successor to Machine CFG.

Implement one target-owned recovery operation:

1. valid only as the first non-debug operation in a dedicated trap recovery
   block;
2. snapshots x0/x26/x27/x28 into values used by the OCaml handler lowering;
3. adds explicit physical live-ins to the recovery block;
4. rejects or asserts if used outside the matching runtime-entered recovery
   block.

`callbr` may remain useful as a short-lived IR reachability and label-carrier
mechanism while prototyping. It should not be the final owner of x26 publication,
because:

- with an x26 clobber, LLVM warns about clobbering a reserved register;
- without the clobber, final assembly writes x26 but MIR does not know.

### Next experiments

The next experiment should answer one question: can a target-owned operation own
publication and recovery-block classification tightly enough that the broad
`gc "ocaml"` `callbr` to `EHPad` hack can be removed?

#### Stage 2A: edge-owned publish pseudo

First decide the IR-to-MIR edge owner. A successor edge by itself is not enough:
LLVM has passes that special-case EH pads and `INLINEASM_BR` indirect targets
when placing PHI copies, checking CFG successors, and building live intervals.

Viable spike paths:

1. Keep inline-asm `callbr` only as a temporary IR label/successor carrier, but
   lower the trap publication itself to an AArch64 pseudo with visible effects.
2. Add a real non-inline-asm lowering path that creates a terminator-like or
   otherwise pass-audited MachineInstr owning the normal and recovery edges.

Do not proceed until the chosen path says which MachineInstr or lowering hook
owns the recovery edge after instruction selection.

Devil's advocate update: a single `callbr` edge at the publication point is not
enough for the real protected region. Calls, allocation slow paths, polls, stack
checks, and inline raises inside the active trap region can all enter the runtime
while the trap is active. The final design must model those as recovery-transfer
points too, or introduce a region-level codegen concept that makes liveness and
root handling conservative across the whole protected region.

The current preferred shape is therefore:

- one publish operation starts the active trap region;
- one pop operation ends it on normal exit;
- every runtime-entering operation inside the region gets a hidden successor to
  the same `runtime-entered` recovery block;
- the recovery block consumes only the recovery ABI values and explicit reloads.

Add an AArch64 machine pseudo that represents trap publication. The pseudo is not
a normal call and is not the final inline-asm owner of x26 publication.

Inputs:

- trap block pointer;
- previous trap pointer value;
- recovery target machine block;
- any extra values needed for the current trap-block ABI, such as restore-SP
  delta or saved frame-pointer state;
- either the domain `exn_handler` address, or an explicit proof that the domain
  copy is refreshed at every runtime transition that can observe it.

Effects:

- stores the previous trap pointer into the trap block;
- materializes the recovery label from the same machine block that is the modeled
  recovery successor, and stores that label into the trap block;
- stores any remaining current-ABI trap-block fields;
- defines physical x26 as the trap block pointer;
- updates `Caml_state->exn_handler`, or leaves a documented and tested refresh
  rule for runtime transitions;
- has memory side effects;
- has an explicit Machine CFG normal successor and recovery successor, through
  the edge owner chosen above.

Expected final assembly for the minimal spike should be equivalent to:

```asm
str <previous-trap>, [<trap-block>]
adr x9, <recovery-block>
str x9, [<trap-block>, #8]
mov x26, <trap-block>
```

But MIR must know about the x26 definition. This is the main difference from the
inline-asm `callbr` spike.

Likely LLVM files:

- `llvm/lib/Target/AArch64/AArch64InstrInfo.td`
- `llvm/lib/Target/AArch64/AArch64ExpandPseudoInsts.cpp`
- `llvm/lib/Target/AArch64/AArch64ISelDAGToDAG.cpp`
- possibly `llvm/lib/CodeGen/SelectionDAG/SelectionDAGBuilder.cpp` only for
  temporary IR-to-MIR plumbing.
- if a new generic block classification is chosen:
  `llvm/include/llvm/CodeGen/MachineBasicBlock.h`, MIR parser/printer,
  `llvm/lib/CodeGen/MachineVerifier.cpp`, `llvm/lib/CodeGen/LiveIntervals.cpp`,
  and `llvm/lib/CodeGen/PHIEliminationUtils.cpp`.

Acceptance checks:

- MIR before pseudo expansion contains a named pseudo with an explicit x26 def.
- MIR after pseudo expansion still has a visible x26 def or an equivalent
  verifier-visible physical-register effect.
- MIR after instruction selection, after pseudo expansion, and after register
  allocation still lists the normal and recovery `successors:`.
- PHI copies on the recovery edge are placed before the trapping transfer point,
  not at the ordinary terminator position.
- Final assembly stores the recovery label before publishing x26.
- No reserved-register inline-asm warning.
- No hidden x26 write.

#### Stage 2B: narrow recovery-block classification

Status: initial `runtime-entered` spike passed.

The broad:

```cpp
if (I.getFunction()->hasGC() && I.getFunction()->getGC() == "ocaml")
  Target->setIsEHPad();
```

has been replaced in the spike with a narrow match that sets `runtime-entered`
only when the immediately preceding trap-publish intrinsic names the same
recovery block as the `callbr` indirect target.

The current `runtime-entered` wiring covers:

- MIR printer/parser;
- `MachineBasicBlock`;
- `MachineVerifier`;
- `LiveIntervals::computeLiveInRegUnits`;
- CFG successor verification;
- PHI copy placement;
- generic critical-edge splitting.

This avoids the AArch64 Linux EH-pad preserved-mask issue because the recovery
block is not an EH pad.

Remaining audit work: check whether any other generic passes need to know that
`runtime-entered` is an ABI-entry block, and decide whether begin-block
clobber-mask handling needs a target hook for runtime-entered blocks. The
current spike deliberately does not add EH unwinder clobber masks.

The recovery intrinsic must also be guarded:

- it should only lower in a block marked by the generic runtime-entered ABI block
  concept and tied to the matching publish operation, or in the audited EH-pad
  fallback;
- it should be the first non-debug operation in that block;
- the block should have exactly the target-defined recovery ABI live-ins, not
  arbitrary allocatable live-ins;
- the block should not contain PHIs or uses of ordinary protected-path SSA
  values unless those values are explicit reloads;
- otherwise lowering should fail loudly in an assertion build.

Acceptance checks:

- an ordinary `gc "ocaml"` `callbr` indirect target is not automatically
  classified as recovery-like;
- the minimal trap-publish test gets recovery live-ins legally;
- a misplaced recovery intrinsic is rejected or asserts in a focused negative
  test;
- a recovery block with non-ABI allocatable physical live-ins is rejected;
- a recovery block not tied to the matching publish operation is rejected;
- an ordinary protected-path SSA value cannot be used in recovery unless it is
  explicitly reloaded or passed through a modeled recovery value.

#### Stage 2C: combined publish-plus-recovery test

Create one minimal hand-written IR/MIR test where:

1. normal entry lowers to the target-owned publish pseudo;
2. the publish pseudo stores the exact recovery target and publishes x26;
3. the recovery block is marked recovery-like by the same mechanism;
4. `llvm.aarch64.oxcaml.trap.recover` snapshots x0/x26/x27/x28;
5. the recovery block computes only from recovery ABI values, constants, or
   explicit reloads;
6. all four recovery ABI values are observably consumed so dead code elimination
   cannot hide a missing live-in.

Inspect four points:

- after instruction selection;
- after pseudo expansion;
- after register allocation;
- final assembly.

Do not proceed to `llvmize.ml` until this combined test passes.

#### Stage 2D: wire AArch64 lowering behind a fallback

Once Stage 2C passes, wire AArch64 `Pushtrap` lowering in
`backend/llvm/llvmize.ml` behind a fallback to the existing `_wrap_try` path. The
first runnable case can be small, but the production path is not a partial
`Pushtrap`: if a required invariant is not modeled, use the fallback instead.

Constraints:

- keep the current trap-block layout unless the generated assembly shows a
  remaining cost that justifies changing it;
- explicitly preserve the current stack-repair rule: recovery entry must restore
  `sp` from the trap block restore delta and restore x29 when frame pointers are
  enabled before ordinary handler code;
- explicitly replace the `_wrap_try` runtime-register rethreading: recovery
  outputs update `domainstate_ptr`, `allocation_ptr`, and the exception bucket
  before `trap_handler_entry` consumes them;
- choose and test the rule for `Caml_state->exn_handler`: either publish it with
  the trap block or prove every runtime transition under an active trap refreshes
  it from x26 before observation;
- keep active-trap root handling conservative;
- keep `Raise_regular` and `Raise_reraise` on the existing path until the
  notrace path and non-raising path are validated;
- inspect generated assembly before benchmarking.

Focused validation:

- expected assembly has no hot `bl _wrap_try`;
- x26 publication is visible in MIR;
- recovery entry uses only x0/x26/x27/x28 or explicit reloads;
- x27/x28 are current after calls, polls, allocation slow paths, stack checks,
  and inline `Raise_notrace` under an active trap;
- nested handlers and normal pop restore x26 correctly;
- calls, polls, allocation slow paths, and stack checks under active traps still
  have conservative root maps;
- stack maps and frame descriptors preserve the current active-trap-depth stack
  adjustment, or the plan documents the deliberate replacement.

Only after this should the focused repro benchmarks and compiler-binary
benchmark be rerun. The benchmark gate must use compilers produced by
`make install`, a clean native comparison build, logs proving the native build
does not use `llvm-backend=1`, fresh LLVM wrapper or IR activity for the LLVM
build, hashes and sizes for both timed compiler executables, repeated runs, and
neutral controls such as the direct string-compare cases.

## Patch 3: native-shaped dynamic push/pop

If Patch 2 remains too heavy, investigate the native dynamic `sp` push/pop
shape later:

```asm
adr x16, handler
stp x26, x16, [sp, #-16]!
mov x26, sp
...
ldr x26, [sp], #16
```

This is likely fastest but riskier for LLVM stack maps, statepoints, frame
lowering, and stack checks. Do not make it the first `_wrap_try` removal patch.
