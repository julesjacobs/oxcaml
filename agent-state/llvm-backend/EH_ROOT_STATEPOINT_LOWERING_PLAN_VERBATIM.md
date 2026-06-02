# Design: OxCaml EH Roots for Shared Pushtrap Handlers in LLVM

## Status

Proposed staged design.

The converged plan is:

```text
Stage 1:
  Add OxCaml-specific EH-root support in statepoint lowering.

Stage 2:
  Move the same abstraction into MIR/register allocation/spiller support.
```

Stage 1 is the implementation to do first. Stage 2 is the final architecture for native-quality code generation. The uploaded notes converge on this exact split: use the SelectionDAG/statepoint EH-root plan first, while treating MIR/RA stack-root constraints as the long-term endpoint. 

---

## 1. Problem statement

OxCaml/OCaml uses an exact moving GC. At every GC safepoint, the frame table must identify all live OCaml values that may be heap pointers. If the collector moves an object, the compiled code must use the updated value after the safepoint.

LLVM’s `gc.statepoint` model normally represents this by producing a statepoint token and then using `gc.relocate` values tied to that token. LLVM’s statepoint documentation explicitly says that `gc.relocate` and `gc.result` are tied to the statepoint token and together form a statepoint relocation sequence. ([LLVM][1])

That token-tied model becomes awkward for OxCaml’s pushtrap/poptrap exception model. In OxCaml, several protected invokes may unwind to the same runtime-entered recovery block or source handler. A single shared handler cannot contain one ordinary set of `gc.relocate`s, because there is no single statepoint token that dominates the handler.

The current workaround materializes volatile exception-root slots:

```llvm
store volatile ptr addrspace(1) %f, ptr %f.exnroot

%tok = invoke ...
       [ "gc-live"(ptr addrspace(1) %f,
                   ptr %f.exnroot) ]

normal:
  %f.normal = gc.relocate(%tok, %f, %f)

handler:
  %f.exn = load volatile ptr addrspace(1), ptr %f.exnroot
```

This is correct-ish, but it represents one logical root as two mechanisms: ordinary statepoint relocation plus an explicit volatile exception root. It also prevents the normal register allocator and spiller from reasoning naturally about the root home.

The replacement design is:

```text
A value live into a runtime recovery block has one logical GC root.
That root has a stack-recoverable home.
The normal continuation and the runtime-entered handler recover through the same home.
```

---

## 2. Goals

The design must satisfy these correctness goals:

```text
1. At every protected statepoint, every EH-live OCaml base value is reported
   in the frame table / stack map.

2. The reported root location is updateable by the GC.

3. The shared recovery block can reload the relocated value without knowing
   which invoke threw.

4. Normal continuation uses and exceptional continuation uses observe the same
   relocated value.

5. Frame-table roots remain base OCaml values, not arbitrary interior addresses.

6. The system never treats a frame index as containing the current value unless
   that fact has been proved or a store has just been emitted.
```

The design has these performance goals:

```text
1. Remove the duplicate normal-root + volatile-exnroot preservation mechanism.

2. Let normal gc.relocate and handler recovery share one physical root home.

3. Reuse existing statepoint spill-slot machinery where possible in Stage 1.

4. Eventually let the real register allocator/spiller place stores, reloads,
   and root-home updates in Stage 2.
```

---

## 3. Non-goals

Stage 1 does **not** attempt to produce native-quality loop code in all cases.

Stage 1 should remove the duplicate exnroot mechanism and may remove repeated stores in simple cases, but it will not globally solve questions like:

```text
Should the root-home store be hoisted to the loop preheader?
Can a reload be delayed until the actual use?
Can an ordinary spill slot become the EH root home?
Can stack slot coloring merge several root homes?
```

Those are allocator/spiller questions. The uploaded notes explicitly call out that Stage 1 is a correctness and coalescing repair, while store placement and root-home currentness across loops belong to the allocator/spiller layer. 

---

## 4. Key abstraction

The central abstraction is:

```text
EH-live GC value:
  A base OCaml value live into a runtime-entered recovery block.

EH root id:
  A stable identifier for one logical handler live-in of one runtime recovery
  continuation.

EH root home:
  A canonical frame index assigned to that EH root id.

EH root recovery:
  A token-independent reload from the EH root home.
```

An EH root home is **not** a second logical root. It is the stack home of the same logical root that may also be live on the normal continuation.

This is the conceptual replacement for volatile exnroot allocas:

```text
For statepoint S:
  value v is live into runtime recovery block R;
  therefore base(v) must be in a stack-map-visible stack location;
  recovery block R reloads that location without referencing S's token.
```

The uploaded notes state this explicitly: the exception-root slot should not be a second logical root; if a value is live on both normal and exceptional continuations, there should be one logical GC root with one stack-home constraint, not a normal `gc.relocate` root plus a volatile exnroot slot. 

---

## 5. Stage 1 design: statepoint EH-root extension

### 5.1 IR representation

Add an OxCaml-only token-independent recovery intrinsic:

```llvm
declare ptr addrspace(1)
@llvm.oxcaml.gc.eh.recover.p1(i32 immarg %recovery_id,
                              i32 immarg %root_id)
```

The intrinsic means:

```text
Reload the relocated base OCaml value for (recovery_id, root_id)
from its canonical EH root home.
```

It must not be `readnone`, `speculatable`, or freely movable. It reads implicit root memory that may have been updated by the GC during the throwing call.

Then add an EH-live statepoint operand or operand bundle:

```llvm
"oxcaml-eh-live"(i32 %recovery_id,
                 i32 %root_id,
                 ptr addrspace(1) %base_value)
```

The `base_value` must be a real operand/use of the statepoint, not only metadata, so that instruction selection and statepoint lowering see the value that must be materialized.

The transformed shape should be:

```llvm
%tok = invoke token @llvm.experimental.gc.statepoint(...)
         [ "gc-live"(ptr addrspace(1) %f.base),
           "oxcaml-eh-live"(i32 %recovery_id,
                             i32 %root_id,
                             ptr addrspace(1) %f.base) ]
       to label %normal
       unwind label %recovery

normal:
  %f.normal =
    call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %tok, i32 ..., i32 ...)

recovery:
  %f.exn =
    call ptr addrspace(1)
      @llvm.oxcaml.gc.eh.recover.p1(i32 %recovery_id, i32 %root_id)
```

The uploaded plan uses this exact replacement pattern: remove the volatile exnroot store/load and use `oxcaml-eh-live` plus token-independent `eh.recover`, while keeping the actual EH-live value as a statepoint operand. 

### 5.2 Root identity

The root id must be stable for the runtime recovery continuation.

Use a key like:

```c++
struct EHRootKey {
  FunctionId Function;
  RecoveryEntryId RecoveryEntry;
  HandlerLiveInId RootId;
  RootKind Kind; // initially BaseOCamlValue
};
```

Do **not** key only by source handler basic block. The runtime-entered recovery continuation is the entity that reloads the root. Later CFG cleanup may split or merge source-level handler blocks, and multiple runtime recovery entries may branch to the same source handler.

Required invariant:

```text
For one runtime-entered recovery block and one handler live-in,
all invokes that can enter that recovery block use the same frame index.
```

If invoke S1 uses `FI#10` for handler-live `f` and invoke S2 uses `FI#18`, the shared handler cannot know which slot to reload unless the runtime passes callsite identity or performs dynamic edge copies. The intended OxCaml design does neither. The uploaded notes make this handler-stability requirement explicit. 

### 5.3 Base-only EH roots

EH root homes contain base OCaml values only:

```text
allowed:
  base OCaml value

not allowed by default:
  arbitrary ptr addrspace(1) derived/interior address
```

If the handler needs a derived or interior address:

```llvm
%base.recovered = call ptr addrspace(1)
  @llvm.oxcaml.gc.eh.recover.p1(i32 %recovery_id, i32 %base_root_id)

%derived.recovered = rematerialize_from_base(%base.recovered, offset/path)
```

LLVM’s statepoint documentation distinguishes base and derived pointers and says `gc.relocate` carries the allocation/base operand needed to relocate derived pointers correctly. It also notes that explicit stack-region allocas have no way to indicate a corresponding base pointer, which is a bad fit for this base-root invariant. ([LLVM][1])

The uploaded notes also require EH homes to contain base OCaml values and to rematerialize derived values from the recovered base. 

---

## 6. Stage 1 lowering model

### 6.1 MachineFunctionInfo state

Add OxCaml-specific MachineFunction state:

```c++
struct OxcamlEHRootInfo {
  EHRootKey Key;
  MachineBasicBlock *RecoveryMBB;
  const Value *BaseValue; // debug/verification only
  int FrameIndex;
};

DenseMap<EHRootKey, OxcamlEHRootInfo> EHRootHomes;
```

For every `EHRootKey`, allocate one stable fixed frame object:

```c++
int FI = MFI.CreateSpillStackObject(PtrSize, Align(PtrAlign));
MFI.markAsStatepointSpillSlotObjectIndex(FI);
EHRootHomes[Key].FrameIndex = FI;
```

Do not use the ordinary rotating statepoint spill-slot pool for these roots. A shared recovery block emits a fixed reload from a fixed frame index, so the frame index must be stable across every protected invoke that can enter that recovery block. 

### 6.2 Stable frame addressing

EH root homes must be fixed function-frame objects, not outgoing call-frame slots.

If OxCaml pushtrap/poptrap code adjusts `sp`, functions with traps or EH root homes should force a stable frame pointer or base pointer:

```text
if function has OxCaml traps or EH root homes:
  force hasFP / stable base pointer
```

This keeps EH root accesses stable across trap pushes/pops, outgoing call-frame adjustments, and runtime-entered recovery. The uploaded notes call out this frame-index stability requirement directly. 

---

## 7. Known versus preferred locations

This is the most important correctness rule.

Do **not** treat:

```text
root_id -> FI
```

as proof that `FI` already contains the current value.

A canonical EH root home is a placement constraint. It says where the value must be put if it is needed at the statepoint. It does not prove the slot is current.

Use three states:

```text
KnownLocation(v) = FI
  FI is proved to contain the current relocated value of v.
  Emit no store.

PreferredLocation(v) = FI
  FI is the required EH root home for v.
  Emit a store unless KnownLocation has been proved.

NoLocation(v)
  Existing statepoint lowering chooses an ordinary spill slot.
```

Lowering rule:

```c++
int FI = getEHRootHome(RecoveryId, RootId);

if (findPreviousSpillSlot(V) == FI) {
  setKnownLocation(V, FI);       // no store
} else {
  setPreferredLocation(V, FI);   // emit store V -> FI
}
```

Then:

```text
KnownLocation:
  stack map reports FI
  no store emitted

PreferredLocation:
  store current base(v) -> FI
  stack map reports FI
  relocated result is now known to come from FI
```

The uploaded notes identify this as the “non-negotiable correction” and warn that preassigning a frame index as if it already contained the value would be unsound: the GC or handler could read a stale slot. 

---

## 8. Deduplicating normal-live and EH-live roots

If the same base value appears in both normal-live and EH-live sets at the same statepoint:

```llvm
"gc-live"(ptr addrspace(1) %f.base)
"oxcaml-eh-live"(i32 %recovery_id,
                 i32 %root_id,
                 ptr addrspace(1) %f.base)
```

lower it as:

```text
one logical GC root
with required/preferred stack home FI(recovery_id, root_id)
```

not as:

```text
one ordinary normal root
plus one EH root
```

The normal `gc.relocate` and the handler `eh.recover` should both lower through the same frame index:

```text
normal gc.relocate(%f)  -> load/recover from FI_f
handler eh.recover(%f)  -> load from FI_f
stack map               -> reports FI_f once for the logical root
```

The target machine shape should become:

```asm
; only if FI_f is not already current
str xF, [FI_f]

bl callee
; stack map reports FI_f

normal:
    ldr xF, [FI_f]

handler:
    ldr xF, [FI_f]
```

This removes the current duplicated preservation mechanism: normal relocation preservation plus volatile exception-root preservation. 

LLVM stack maps are a good fit for this representation because they record the locations of runtime-required live values at a particular instruction address, rather than all LLVM values live across that point. ([LLVM][2])

---

## 9. SelectionDAG statepoint lowering changes

Extend `StatepointLoweringInfo` with EH root information:

```c++
struct OxcamlEHStackRoot {
  EHRootKey Key;
  const Value *Base;
};

SmallVector<OxcamlEHStackRoot, 8> EHStackRoots;
```

During `lowerStatepointMetaArgs`, build:

```c++
DenseSet<SDValue> ForceStackGCValues;
DenseMap<SDValue, int> PreferredEHRootFI;
DenseMap<SDValue, int> KnownLocations;
DenseMap<SDValue, int> PreferredLocations;
DenseSet<int> ReservedEHRootFIs;
```

For every EH-live base value:

```c++
ForceStackGCValues.insert(BaseSDValue);
PreferredEHRootFI[BaseSDValue] = FI(Key);
```

EH-live base values:

```text
must not lower as vreg-only roots;
must appear as stack-map GC pointer operands;
must prefer/require FI(root_id).
```

Modify the spill path:

```c++
if (KnownLocations.contains(V)) {
  FI = KnownLocations[V];
  // no store
} else if (PreferredLocations.contains(V)) {
  FI = PreferredLocations[V];
  reserveEHRootFI(FI);
  store V -> FI;
} else {
  FI = allocateOrdinaryStatepointSpillSlot();
  store V -> FI;
}
```

The uploaded notes propose this exact change: build `ForceStackGCValues` and `PreferredEHRootFI`, distinguish known and preferred locations, and change `spillIncomingStatepointValue` so preferred EH homes are spill targets rather than assumed-current locations. 

---

## 10. Lowering `eh.recover`

Lower:

```llvm
%v = call ptr addrspace(1)
  @llvm.oxcaml.gc.eh.recover.p1(i32 %recovery_id, i32 %root_id)
```

to a real fixed-stack load from `FI(EHRootKey)`.

SelectionDAG sketch:

```c++
void SelectionDAGBuilder::visitOxcamlGCEHRecover(const CallInst &CI) {
  EHRootKey Key = getEHRootKey(CI);
  int FI = FuncInfo.OxcamlEHRootHomes.lookup(Key).FrameIndex;

  SDValue Slot = DAG.getFrameIndex(FI, getFrameIndexTy());

  MachinePointerInfo PtrInfo =
      MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI);

  MachineMemOperand *MMO = MF.getMachineMemOperand(
      PtrInfo,
      MachineMemOperand::MOLoad,
      MFI.getObjectSize(FI),
      MFI.getObjectAlign(FI));

  SDValue Load = DAG.getLoad(VT, getCurSDLoc(), DAG.getRoot(), Slot, MMO);

  PendingLoads.push_back(Load.getValue(1));
  setValue(&CI, Load);
}
```

Required ordering semantics:

```text
1. eh.recover is a real memory read.
2. It must not be CSE’d with a pre-statepoint value.
3. It must not be hoisted before the runtime recovery entry.
4. It must not be moved above any recovery-entry setup needed for frame access.
```

A target-specific pseudo such as `OXCAML_EHROOT_RELOAD` is also acceptable if it makes verifier and scheduling constraints clearer.

---

## 11. Previous-spill-slot discovery

Extend `findPreviousSpillSlot` so it can prove currentness through:

```text
1. ordinary gc.relocate lowered through a spill FI;
2. eh.recover(root_id) lowered through FI(root_id);
3. bitcasts / addrspace casts that preserve the underlying root;
4. PHIs where all incoming values are known to come from the same FI.
```

If a PHI has incoming values from different locations, or one incoming value is the original pre-statepoint value with no known current FI, the result is not known-current. Use `PreferredLocation` and emit a store.

This prevents stale-root unsoundness and explains why Stage 1 may still emit repeated stores in some loops. The uploaded notes give the common loop case:

```llvm
%f.loop = phi [ %f0, %preheader ],
              [ %f.relocated, %normal ]
```

Even if `%f.relocated` came from `FI_f`, the `%f0` incoming edge may prevent the existing heuristic from proving that the PHI is already current in `FI_f`; then statepoint lowering may still emit repeated stores. 

---

## 12. Frame table / stack map emission

For each protected statepoint, emit a frame-table / stack-map entry for each logical root.

For a base-only EH root:

```text
root: %f.base
location: FI_f
base location: FI_f
derived location: FI_f
```

For a derived value needed by the handler:

```text
rooted value: base(%derived)
location: FI_base
handler rematerializes derived from recovered base
```

Do not put arbitrary derived/interior values in EH root homes unless the frame-table format explicitly encodes the base/derived relationship and the runtime understands it.

LLVM statepoint stack map records contain relocation records with locations for base and derived pointers, and the docs note that the same physical location may appear in more than one role. ([LLVM][1])

---

## 13. Verifiers

Add verifier checks early. These should be enabled in asserts builds at minimum, with a stricter mode available in CI.

### 13.1 IR / RS4GC verifier

Check:

```text
1. A shared runtime recovery block does not contain ordinary gc.relocate
   values tied to one invoke token.

2. Every eh.recover(recovery_id, root_id) is in a runtime-entered recovery
   block or a block dominated by one.

3. Every protected statepoint that can unwind to that recovery block has a
   matching oxcaml-eh-live(recovery_id, root_id, base).

4. EH-live values are base OCaml values, or have an associated base plus
   rematerialization plan.

5. If a value appears in both gc-live and oxcaml-eh-live, the representation
   records one logical root with an EH stack-home constraint.
```

### 13.2 Machine verifier

Check:

```text
1. Every EHRootKey has exactly one stable FI.

2. All protected statepoints that can unwind to the same recovery/root id use
   the same FI.

3. Every stack map for such a statepoint reports FI(root_id).

4. The value stored/reported for root_id is a base OCaml value.

5. No runtime-entered recovery block assumes ordinary predecessor vreg live-ins.

6. EH root homes are fixed frame objects, not outgoing call-frame slots.

7. Functions with EH root homes use stable FP/base-pointer addressing if SP may
   be dynamically adjusted by trap operations.
```

The uploaded patch plan includes the core MIR verifier requirements: every `ehroot.reload` must be in a runtime-entered recovery block; every protected statepoint must provide the root id; every root id has one stable FI; every relevant stack map reports that FI; the value is a base OCaml value; and the handler must not assume ordinary virtual-register live-ins. 

---

## 14. Stage 1 patch plan

### Patch 1: Add IR surface

Add:

```llvm
@llvm.oxcaml.gc.eh.recover.p1(i32 immarg %recovery_id,
                              i32 immarg %root_id)
```

Add an operand bundle or equivalent statepoint side channel:

```llvm
"oxcaml-eh-live"(i32 %recovery_id,
                 i32 %root_id,
                 ptr addrspace(1) %base)
```

Keep the volatile exnroot lowering behind a fallback/debug flag, but remove it from the optimized path.

### Patch 2: Extend custom RS4GC

For each protected invoke:

```text
1. Compute EHLiveRoots(S):
     base OCaml values needed by the runtime-entered recovery block.

2. Exclude fixed recovery-state registers/values if they are already handled
   by the raise ABI.

3. Assign stable (recovery_id, root_id) identities.

4. Attach oxcaml-eh-live entries to the statepoint.

5. Replace shared-handler gc.relocate uses with eh.recover.

6. For derived handler values, record enough information to rematerialize from
   the recovered base.
```

### Patch 3: Add `EHRootHomes` to MachineFunction state

Allocate one stable FI per `EHRootKey`.

Mark it as a statepoint spill/root object.

Force stable frame addressing for functions with OxCaml traps or EH root homes.

### Patch 4: Extend SelectionDAG statepoint lowering

Implement:

```text
ForceStackGCValues
PreferredEHRootFI
KnownLocations
PreferredLocations
ReservedEHRootFIs
```

Prevent EH-live roots from lowering as vreg-only roots.

Use preferred FI as the spill target unless the same FI is known-current.

### Patch 5: Deduplicate normal and EH roots

If a base appears in both ordinary `gc-live` and `oxcaml-eh-live`, emit one logical relocation record constrained to the EH FI.

Lower normal `gc.relocate` and handler `eh.recover` through the same FI.

### Patch 6: Lower `eh.recover`

Lower to fixed-stack load or target pseudo.

Give it real memory-read ordering.

Do not allow it to be hoisted before the recovery entry.

### Patch 7: Add verifier coverage

Implement the IR and MIR checks listed above.

### Patch 8: Tests and runtime validation

Add lit tests and runtime stress tests before removing the fallback flag.

---

## 15. Stage 2 design: MIR/RA stack-root constraints

Stage 1 still contains a specialized mini-spiller inside statepoint lowering. That is acceptable for the first patch, but it is not the final architecture.

LLVM’s code generator documentation places register allocation as the phase that transforms virtual registers to the concrete register file, introduces spill code, and eliminates virtual register references. ([LLVM][3]) LLVM’s own statepoint documentation also notes that current statepoint lowering quality is poor and that long-term integration with the register allocator is desirable. ([LLVM][1])

The final representation should be MIR-level:

```text
STATEPOINT_INVOKE @callee
  gc-live          %vreg_acc.base
  gc-stack-live    %vreg_f.base, root_id_f
  gc-stack-live    %vreg_x.base, root_id_x
  unwind           %recovery
```

Semantics of `gc-stack-live %vreg, root_id`:

```text
At this statepoint SlotIndex:
  the current base value of %vreg must be present in FI(root_id);
  the stack map reports FI(root_id);
  the runtime recovery block reloads root_id from FI(root_id);
  the spiller decides whether a store is needed and where to place it.
```

It does **not** mean:

```text
emit a store immediately before the statepoint unconditionally
```

The spiller should be able to choose:

```text
FI already contains current vreg:
  emit no store

vreg is in a register and FI is stale:
  store register -> FI at a profitable point

vreg is spilled elsewhere:
  reuse that spill slot as FI if legal, or copy/spill into FI

vreg is rematerializable:
  rematerialize into FI if profitable
```

This is the design that can produce the native-like loop shape:

```asm
; preheader
str xF, [FI_f]

loop:
    ldr xArgF, [FI_f]      ; only if needed as a call argument
    bl  call               ; stack map reports FI_f
                            ; GC may update FI_f
    b loop
```

instead of:

```asm
loop:
    str xF, [FI_f]
    bl  call
    ...
    str xF, [FI_f]
    bl  call
```

The uploaded notes identify this as the purpose of Stage 2: add `gc-stack-live %vreg, root_id` and teach the spiller that it is a required stack use at that SlotIndex. 

---

## 16. Stack slot coloring rules

EH root homes are observable by the runtime and recovery blocks. They may be colorable, but only under stronger interference rules than ordinary spills.

Safe coloring requires:

```text
1. The two root ids are never simultaneously reported in one frame table.

2. No runtime recovery block can need both root ids at the same time.

3. Reusing the FI cannot change what a shared handler reloads.

4. The collector cannot observe stale data as a live root at any safepoint.
```

Unsafe:

```text
invoke S1 unwinds to H:
  root_id f -> FI#10

invoke S2 unwinds to H:
  root_id f -> FI#18

H:
  eh.recover(root_id f)
```

unless the runtime passes callsite identity or performs edge copies. That is not this design.

---

## 17. Interaction with register roots

This design assumes that moving-GC EH-live roots must have stack homes at protected statepoints.

A future ABI could allow updateable register roots, but that requires the runtime to find and update the exact location that post-safepoint code will use. Merely recording that a value was in a register at the callsite is not enough if the callee/runtime saves it elsewhere or if the handler expects a stack reload.

Therefore Stage 1 should conservatively force EH-live roots to stack. Stage 2 may add register-root support only if the runtime ABI has a precise updateable register-save mechanism.

---

## 18. Expected outcomes

### Stage 1 should solve

```text
1. No optimized-mode volatile exnroot allocas.

2. No duplicate logical root for normal + exceptional preservation.

3. Shared handlers recover roots without token-local gc.relocate.

4. Normal continuation and handler reload through the same FI.

5. Frame table reports one base root location per logical root.

6. Straight-line repeated statepoints may reuse known-current FIs.
```

The uploaded notes state the same expected Stage 1 wins: normal relocate and EH reload share the same FI, there is no explicit volatile exnroot alloca, and the duplicate normal-root store plus exnroot store is removed. 

### Stage 1 should not promise

```text
1. Store hoisting out of loops in all cases.

2. Full reuse of ordinary spill slots.

3. Optimal stack slot coloring.

4. Native-quality loop code in all benchmarks.
```

### Stage 2 should solve

```text
1. Store placement by the real spiller.

2. Root-home currentness as allocator dataflow.

3. Reuse or coalescing with ordinary spill slots where legal.

4. Loop-preheader placement for stable loop-carried roots.

5. Native-like preservation of loop-invariant closures such as f.
```

---

## 19. Test plan

### IR tests

Add tests for:

```text
1. Multiple invokes unwind to one runtime recovery block.

2. Handler contains eh.recover, not gc.relocate.

3. Statepoint carries oxcaml-eh-live as a real operand.

4. Volatile exnroot allocas are absent in optimized mode.

5. Same base in gc-live and oxcaml-eh-live becomes one logical root.

6. Derived handler values are rematerialized from recovered bases.
```

### MIR / codegen tests

Check:

```text
1. All invokes sharing one recovery/root id use the same FI.

2. Normal gc.relocate and eh.recover lower through the same FI.

3. Stack map reports FI(root_id).

4. KnownLocation emits no store.

5. PreferredLocation emits store -> FI.

6. Handler reload is a fixed-stack load or target pseudo from FI.

7. Functions with EH roots force stable FP/base-pointer addressing.
```

### Runtime tests

Use a moving-GC stress mode:

```text
1. Force GC during protected calls.

2. Move objects referenced by handler-live roots.

3. Throw after GC.

4. Verify the handler observes the relocated object, not the stale address.
```

Include cases for:

```text
nested traps;
multiple invokes sharing one handler;
multiple handler live-ins;
loop-carried closure f;
derived value rematerialization;
roots changed between invokes;
roots unchanged between invokes;
fallback volatile mode.
```

### Negative verifier tests

Reject:

```text
1. eh.recover with no matching EH-live root on one protected invoke.

2. Two invokes to the same recovery/root id using different FIs.

3. EH root value that is not a base OCaml value.

4. Shared handler gc.relocate tied to one invoke token.

5. Runtime recovery block relying on ordinary vreg live-ins.
```

---

## 20. Open questions

1. **IR spelling:** operand bundle versus explicit statepoint operand side table. Operand bundles are clearer, but integration with `GCStatepointInst` wrappers may require local LLVM changes.

2. **Intrinsic memory effects:** `eh.recover` must be a lowering marker and real memory read. The safest Stage 1 approach is to insert it late and prevent optimization/hoisting rather than relying on subtle IR memory attributes.

3. **GlobalISel:** this design describes SelectionDAG lowering. If OxCaml uses GlobalISel for any target, add equivalent lowering hooks or reject the configuration.

4. **Root id allocation before CFG transforms:** root ids should be assigned after recovery entries are stable, or carried through CFG transforms by a dedicated side table.

5. **Stack slot coloring:** initial Stage 1 should not color EH root homes aggressively. Add coloring only after verifier support proves non-interference.

6. **Register-root ABI:** keep EH roots stack-only until there is a runtime ABI for updateable register roots.

---

## 21. Final recommendation

Implement the design in three layers:

```text
Layer 1: IR / RS4GC
  Identify EH-live base values.
  Assign stable recovery/root ids.
  Do not create optimized-mode exnroot allocas.
  Replace shared-handler gc.relocate with eh.recover.

Layer 2: Statepoint lowering
  Force EH-live roots to stack.
  Use stable EH-root frame indices.
  Distinguish KnownLocation from PreferredLocation.
  Deduplicate normal-live and EH-live.
  Lower normal gc.relocate and handler eh.recover through the same FI.

Layer 3: RA/spiller, long term
  Represent gc-stack-live as a MIR stack-use constraint.
  Let the spiller own currentness, store placement, reload placement,
  rematerialization, and legal slot reuse.
```

The key rule to preserve throughout implementation is:

```text
An EH root home is the canonical stack home for one logical base root.
It is not a second root, and it is not known-current merely because it has
a root id.
```

Stage 1 removes the current duplicate preservation mechanism. Stage 2 makes the root home behave like a real native spill slot across loops and repeated calls.

[1]: https://llvm.org/docs/Statepoints.html?utm_source=chatgpt.com "Garbage Collection Safepoints in LLVM"
[2]: https://llvm.org/docs/StackMaps.html?utm_source=chatgpt.com "Stack maps and patch points in LLVM"
[3]: https://llvm.org/docs/CodeGenerator.html?utm_source=chatgpt.com "The LLVM Target-Independent Code Generator"
