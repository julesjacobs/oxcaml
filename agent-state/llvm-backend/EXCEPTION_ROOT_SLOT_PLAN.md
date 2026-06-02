# Exception recovery root design

## Core problem

OxCaml uses a pushtrap/poptrap exception model. A `pushtrap` installs one
runtime recovery target, and all calls made while that trap is active may raise
to that same recovery target. In LLVM IR, those calls are represented as
`invoke`s with an exceptional edge to the trap recovery block.

That creates a mismatch with ordinary LLVM statepoint lowering.

The ordinary `gc.relocate` model assumes that an exceptional relocation block
can identify the one statepoint token that reached it:

```llvm
invoke statepoint(...) to %normal unwind %landingpad_for_this_invoke

landingpad_for_this_invoke:
  %x.relocated = gc.relocate(token-from-this-invoke, ...)
```

This is only well-defined when the unwind block is specific to one invoke, or
when there is some other unique token at the relocation point.

With pushtrap/poptrap, several invokes can unwind to the same trap recovery
block:

```llvm
invoke statepoint(...) to %normal_a unwind %recover
invoke statepoint(...) to %normal_b unwind %recover

recover:
  landingpad
  call @llvm.aarch64.oxcaml.trap.recover()
  ...
```

A `gc.relocate` in `%recover` would need to know whether the dynamic control
flow came from the first invoke or the second invoke. The shared recovery block
does not have one static statepoint token. Splitting the landingpad per invoke
also does not match the pushtrap runtime model directly, because the installed
trap target is the shared blockaddress.

Therefore the correct transform is not "put `gc.relocate` in the landingpad".
The correct transform is to make the shared recovery block an exception
continuation with explicit root parameters.

## Correct transform

A value used after trap recovery is a recovery parameter. Since the exceptional
edge cannot carry SSA values with a usable statepoint token, each demanded
recovery parameter is represented by a mutable root slot.

For each demanded recovery value:

1. Determine the value that should be seen after each raising invoke.
2. Store that edge value into a root slot immediately before the invoke.
3. Add the root slot to that invoke's `gc-live` bundle.
4. Load the root slot in the shared recovery path.
5. Use the loaded value as the recovered value.

The slot is not an optimization trick. It is the representation of an
exception-continuation parameter.

Example:

```llvm
; Edge A.
store volatile ptr addrspace(1) %x_for_a, ptr %x.exnroot
invoke statepoint(...) to %normal_a unwind %recover
  [ "gc-live"(ptr %x.exnroot) ]

; Edge B.
store volatile ptr addrspace(1) %x_for_b, ptr %x.exnroot
invoke statepoint(...) to %normal_b unwind %recover
  [ "gc-live"(ptr %x.exnroot) ]

recover:
  landingpad
  call @llvm.aarch64.oxcaml.trap.recover()
  %x.recovered = load volatile ptr addrspace(1), ptr %x.exnroot
  br %handler
```

This is equivalent to an edge-sensitive recovery phi:

```llvm
%x.recovered = phi [ %x_for_a, edge-from-a ], [ %x_for_b, edge-from-b ]
```

but it is safe across a collecting invoke because the root slot is part of that
invoke's frame table.

## Base and derived values

The recovery parameter must be a rootable value.

If the demanded recovery value is a normal OCaml value, the slot stores that
value.

If the demanded recovery value is a derived address, the slot stores the base
OCaml value. After recovery, the derived address is rematerialized from the
recovered base.

The transform must not store an arbitrary derived pointer as an independent
self-root. That would let the frame table describe an interior pointer as a
base, which the OxCaml GC cannot handle.

So the recovery transform has two levels:

- recovery root: the base value stored in a root slot and listed in `gc-live`;
- recovered use value: either the recovered root itself or a rematerialized
  derived value computed from the recovered root.

## Demand-driven algorithm

The algorithm should be demand-driven.

Definitions:

- candidate recovery value: a GC value found by scanning a recovery region or a
  recovery boundary;
- demanded recovery value: a candidate with a real post-recovery use that needs
  the value recovered;
- store program: the mapping from raising invoke to the SSA value stored before
  that invoke;
- physical root slot: one mutable stack cell emitted for one compatible store
  program.

The principled algorithm is:

1. Discover the recovery regions and boundary edges created by OxCaml trap
   recovery.
2. Discover candidate recovery values in those regions.
3. Compute demanded recovery values from real post-recovery uses. Uses created
   by the exception-root transform itself do not count as demand.
4. For each demanded recovery value, compute its recovery root. For a derived
   value, this is the base value plus a rematerialization recipe.
5. For each demanded recovery root, compute the store program: for every raising
   invoke that may reach the recovery use, determine the edge value to store.
6. Intern physical root slots by compatible store program, type, and alignment.
7. Emit only the physical root slots needed by demanded recovery values.
8. Emit recovery loads/selectors/rematerializations only where demanded uses
   require them.
9. Add each physical root slot to `gc-live` only for invokes whose store program
   writes that slot.

The important ordering is that the pass plans demands and store programs before
it emits volatile stores or appends anything to `gc-live`.

## Slot compatibility

Two demanded recovery values can share a physical root slot when the slot would
carry the same value on every dynamic path where both values are read.

A conservative compatibility rule is:

- same slot type;
- same slot alignment;
- for every overlapping invoke, the stored SSA value is identical;
- for a non-overlapping invoke, sharing is allowed only if the slot is not read
  for the other demand on paths from that invoke.

In practice, this means the physical slot is keyed by the store program, not by
the syntactic identity of the candidate value.

This distinction matters. `%gcagg104` and `%.1` may be different SSA values or
different logical candidates, but if the only demanded recovery value stores the
same SSA value before the same invoke, the pass should emit one physical root
slot, not two volatile slots.

## Normal continuation coalescing

An explicit exception-root slot can also be the normal-continuation root for
the same value at the same invoke.

If an invoke stores value `V` into an exception-root slot, and `V` is also live
on the normal continuation of that invoke, the pass should avoid representing
`V` twice:

```llvm
store volatile ptr addrspace(1) %v, ptr %v.exnroot
invoke statepoint(...) to %normal unwind %recover
  [ "gc-live"(ptr addrspace(1) %v, ptr %v.exnroot) ]

normal:
  %v.normal = gc.relocate(token, %v, %v)

recover:
  %v.recovered = load volatile ptr addrspace(1), ptr %v.exnroot
```

That shape creates two roots for the same dynamic value: the ordinary live SSA
root `%v`, and the explicit exception-root slot `%v.exnroot`. In final
assembly this can become two stack stores before the same invoke.

The preferred shape is to make the slot the single continuation root for both
successors:

```llvm
store volatile ptr addrspace(1) %v, ptr %v.exnroot
invoke statepoint(...) to %normal unwind %recover
  [ "gc-live"(ptr %v.exnroot) ]

normal:
  %v.normal = load volatile ptr addrspace(1), ptr %v.exnroot

recover:
  %v.recovered = load volatile ptr addrspace(1), ptr %v.exnroot
```

The normal load is required because a collection at the invoke may update the
slot. This is the same root cell serving two continuation parameters: the
normal continuation parameter and the exception continuation parameter.

This coalescing is only valid for a value whose normal continuation semantics
match the slot's store program at that invoke. It must not turn unrelated normal
live values into explicit slots merely because they have the same type. It is a
coalescing rule for values already forced into an explicit root slot by a real
exception recovery demand.

Do not implement this by simply deleting `%v` from the statepoint's `gc-live`
list. Existing `gc.relocate` indices are positional. Removing a `gc-live`
operand without replacing the corresponding `gc.relocate` uses can make the IR
ill-typed or silently relocate the wrong operand. A correct implementation must
either avoid creating the ordinary `gc.relocate` for `V`, or replace its normal
continuation uses with a load from the explicit slot before changing the
statepoint operand list.

## What is wrong with the current shape

The current implementation in `RewriteStatepointsForGC.cpp` is mostly built
around `materializeOxCamlExceptionRootSlots`. It scans for handler-live values,
boundary values, and boundary live-ins, then calls `MaterializeRootSlot`.

`MaterializeRootSlot` has immediate side effects:

- creates an entry-block alloca;
- creates volatile stores before invokes;
- creates a recovery load;
- records the slot in `ExplicitRootSlots`;
- causes the slot to be appended to statepoint `gc-live`.

That means the current code often materializes a root slot for a candidate
before proving that the candidate has a real demanded use.

The current code also treats "need a recovery load at this boundary" as "need a
new backing root slot". Those are different concepts. A boundary edge may need a
load at a different program point, but the load can read the same physical root
slot.

This is why the final IR can contain dead-looking values like
`%gcagg104.exnroot.select`: the selector is a byproduct of a speculative
candidate, while the volatile root slot remains live because it was already
added to `gc-live`.

## Implementation direction

The implementation should replace eager `MaterializeRootSlot` calls with a
planning phase.

Suggested internal objects:

```c++
struct RecoveryDemand {
  Value *OriginalValue;
  Value *RecoveryRoot;
  SmallVector<Instruction *, 4> RematerializationChain;
  SmallVector<Use *, 4> Uses;
};

struct RootStore {
  InvokeInst *Invoke;
  Value *StoredValue;
};

struct RootSlotPlan {
  Type *SlotType;
  Align SlotAlign;
  SmallVector<RootStore, 4> Stores;
  SmallVector<RecoveryDemand *, 4> Demands;
};
```

The exact data structures can differ, but the pass should preserve the
separation:

- demand discovery is side-effect-free;
- store-program construction is side-effect-free;
- slot interning is side-effect-free;
- IR emission happens last.

Boundary selectors should be lazy. Create a boundary selector only when the pass
is about to replace a real use or a real PHI incoming value. Do not create a
selector just because a value is live-in to a boundary block.

Late materialization after the main statepoint rewrite should obey the same
rules. If a late pass creates new demands, it should plan and emit only those
demands. It should not append all late explicit root slots to every statepoint
unless that broad root lifetime is explicitly proven necessary.

## Correctness checks

The transform should assert these properties:

- Every post-recovery use of a pre-recovery GC value is either replaced with a
  recovered value or proven not to need recovery.
- Every demanded derived value has a rootable base and a rematerialization
  recipe.
- No physical root slot stores an unrematerialized derived pointer as a
  self-root.
- Every stored value dominates the invoke where it is stored.
- Shared slots have compatible store programs.
- Every emitted root slot appears in `gc-live` for each invoke that can collect
  after storing that slot.
- When a slot is used as both the normal-continuation root and the
  exception-continuation root for a value, the ordinary SSA root for that same
  value is not also listed in `gc-live` for the same invoke.
- Every removed ordinary `gc-live` root has its normal `gc.relocate` uses
  replaced by a load from the explicit root slot, or no such `gc.relocate` is
  created.
- No root slot is emitted for a candidate with no demanded use.

## Test strategy

The tests should be `.ll` tests for RS4GC, because this transform is about the
IR contract between shared OxCaml recovery blocks and statepoint lowering.

Adversarial cases:

- one shared recovery block reached by two invokes;
- one demanded recovery value with different edge values from different invokes;
- two candidate values where only one has a real post-recovery use;
- two demanded values with identical compatible store programs;
- two demanded values with conflicting store programs;
- a demanded boundary PHI incoming;
- a boundary live-in candidate with no real use;
- a demanded derived pointer that must root the base and rematerialize the
  derived address;
- multiple recovery trampolines feeding the same handler;
- early and late materialization where an early candidate becomes unused after
  relocation;
- a value live on both the normal and exceptional continuations of the same
  invoke, where the explicit exception-root slot should replace the ordinary
  normal `gc.relocate` root.

The expected output should check the structural properties:

- no `gc.relocate` is inserted in a shared recovery block;
- one physical root slot is emitted for each demanded compatible store program;
- unused candidates do not create volatile stores;
- derived values are not stored as independent roots;
- `gc-live` contains the physical root slots needed by the corresponding
  invokes;
- a coalesced normal-plus-exception root has one volatile store before the
  invoke, one root slot in `gc-live`, and no separate `gc.relocate` for the
  original SSA value.

## Incremental notes below

The rest of this file records the previous incremental slot-sharing plan and
its implementation status. It is useful history, but the design above is the
preferred model: explicit exception roots are recovery continuation parameters,
and the implementation should be demand-driven.

## Problem

The remaining closure-call-in-try microbenchmark slowdowns are dominated by
OxCaml explicit exception-root lowering in RS4GC.

In the nested closure benchmark, the IR before RS4GC has one closure pointer
live at the hot `invoke`. After RS4GC, final IR contains eight volatile stores
of that same logical value before the same `invoke`:

```llvm
store volatile ptr addrspace(1) %.1, ptr %gcagg142.exnroot
store volatile ptr addrspace(1) %.1, ptr %gcagg142.exnroot147
store volatile ptr addrspace(1) %.1, ptr %gcagg142.exnroot149
store volatile ptr addrspace(1) %.1, ptr %gcagg142.exnroot151
store volatile ptr addrspace(1) %.1, ptr %.1.exnroot
store volatile ptr addrspace(1) %.1, ptr %.1.exnroot158
store volatile ptr addrspace(1) %.1, ptr %.1.exnroot160
store volatile ptr addrspace(1) %.1, ptr %.1.exnroot162
```

The duplicate stores are introduced by custom OxCaml exception-root handling in
`RewriteStatepointsForGC.cpp`, not by llvmize or CFG.

## Correct model

An exception-root slot is a mutable edge-value cell.

For a PHI-like recovery value, one slot is enough:

```llvm
before invoke1: store %a, %slot
before invoke2: store %b, %slot

handler:
  %x = load %slot
```

That correctly represents:

```llvm
%x = phi [ %a, invoke1 ], [ %b, invoke2 ]
```

Therefore PHIs are not a reason to allocate one slot per incoming edge. The
whole point of the slot is to carry the edge-specific value across the collecting
invoke.

The real sharing rule is:

Two recovery values may use the same slot if their required stores are
compatible.

Compatibility means:

- same slot type and alignment;
- for every `invoke` where both values require a store, the stored value is the
  same;
- if one value requires a store before an `invoke` and the other value is loaded
  after a path from that same `invoke`, then the second value must also be
  correct for that stored value.

The third condition is why the implementation should group by recovery value
semantics, not blindly merge unrelated roots of the same type. A slot can store
different values at different invokes, but it cannot represent two different
logical values after the same invoke.

For the observed duplicate case, all requests store the same SSA value before
the same hot `invoke`, so one slot is enough for all of them.

## Current mechanism

`materializeOxCamlExceptionRootSlots` walks each recovery block beginning with
an OxCaml trap recover intrinsic.

For each recovery block it finds:

- handler-live PHIs;
- handler-live GC values used in the recovery region;
- recovery-only-region boundary PHIs;
- recovery-only-region boundary uses;
- values live into recovery boundary blocks.

For each such value it calls the local `MaterializeRootSlot` lambda.

`MaterializeRootSlot` always creates a fresh entry-block alloca, stores a default
non-moving immediate into it, stores the edge value before each relevant
`invoke`, emits a volatile recovery load, and records the slot in
`ExplicitRootSlots[II]`.

Boundary recovery currently has this load cache:

```c++
DenseMap<Value *,
         DenseMap<BasicBlock *, DenseMap<BasicBlock *, LoadInst *>>>
    BoundaryRootLoads;
```

This caches loads by:

```text
(root value, boundary block, incoming block)
```

Because each cache miss calls `MaterializeRootSlot`, four incoming edges for the
same root value produce four backing slots, even if all four stores write the
same value before the same `invoke`.

There is a second source of duplication. `insertParsePoints` calls
`materializeOxCamlExceptionRootSlots` before the main statepoint rewrite and
again after `relocationViaAlloca`. The late pass is needed because the main
rewrite can create new recovery PHIs and relocated values. But the early pass
can leave slots whose selector/load result no longer has real users. Those
slots are still appended to `gc-live`, so their volatile stores survive.

## Better design

Separate these concepts:

- root slot: the mutable cell that is stored before invokes and loaded on
  recovery paths;
- recovery load: a load from that slot placed where a specific recovery use
  needs a value;
- boundary selector: a PHI at a boundary block that selects among recovery and
  non-recovery incoming values.

The current code conflates "need another recovery load" with "need another root
slot". That is the bug.

The first fix should share root slots while leaving recovery loads and boundary
selectors mostly unchanged.

## Proposed implementation

### 1. Make slot requests explicit

Refactor the local `MaterializeRootSlot` lambda into two steps:

1. Build an `OxCamlExceptionRootSlotRequest`.
2. Materialize or reuse the slot, then emit the requested load.

Suggested request:

```c++
struct OxCamlExceptionRootStore {
  InvokeInst *Invoke;
  Value *StoredValue;
};

struct OxCamlExceptionRootSlotRequest {
  Value *LogicalRoot;
  Type *SlotType;
  Align SlotAlign;
  SmallVector<OxCamlExceptionRootStore, 4> Stores;
};
```

`LogicalRoot` is the root value after `FindExceptionRootValue`, so derived
addrspace(1) values are not stored as independent roots.

Build `Stores` with the same logic as today's `EdgeValueForSite` path:

- collect the relevant recovery store sites;
- resolve PHIs in the unwind destination to the store-site incoming value;
- apply `nonPoisonTrapRecoverySlotValue`;
- verify the stored value dominates the `invoke`;
- canonicalize the `(Invoke, StoredValue)` list in deterministic function order;
- reject contradictory duplicate entries for the same `Invoke`.

The refactor should be behavior-preserving before interning is enabled.

### 2. Add a slot interner with compatible-store merging

Use an interner local to one `materializeOxCamlExceptionRootSlots` invocation.

The interner should not require identical full store programs. That was too
conservative. A PHI slot naturally has different stores before different
invokes, and two requests may still be compatible when their store sets overlap
only partly.

A simple first implementation should still be conservative in one dimension:

- only merge requests with the same `LogicalRoot`, `SlotType`, and `SlotAlign`;
- allow the merged slot to accumulate stores for additional invokes;
- if two requests need stores for the same `Invoke`, require the same
  `StoredValue`.

This is safe because one logical root can legitimately need different incoming
values at different invokes, while two different logical roots might only happen
to share one store in a local shape.

This may not collapse all eight stores in the observed final IR. The two groups
of four slots have different slot-name stems (`%gcagg142.exnroot...` and
`%.1.exnroot...`) even though they store the same SSA value before the hot
invoke. The first implementation should be judged by whether it collapses the
four boundary-edge duplicates inside each group. If it leaves one early slot and
one late/equivalent slot, handle that with the dead-slot cleanup or a separate
equivalence-based merge, not by weakening the first key prematurely.

The interner record can be:

```c++
struct OxCamlExceptionRootSlot {
  Value *LogicalRoot;
  Type *SlotType;
  Align SlotAlign;
  AllocaInst *Slot;
  DenseMap<InvokeInst *, Value *> StoreValues;
};
```

Lookup algorithm:

1. Find candidate slots keyed by `(LogicalRoot, SlotType, SlotAlign)`.
2. For each candidate, check store compatibility:
   - for each `(Invoke, StoredValue)` in the request, either the candidate has
     no store for `Invoke`, or it has the same `StoredValue`;
   - if compatible, add missing stores and reuse the slot.
3. If no candidate is compatible, create a new slot.

When a request reuses a slot:

- create only the recovery load needed by that request;
- insert only missing stores for invokes not already covered by the slot;
- append the slot to `ExplicitRootSlots[II]` only when a store for `II` exists.
- insert the slot into `ExplicitExceptionRootCalls` for every invoke with a
  store, including stores added when a later request extends an existing slot.

`appendExplicitRootSlotsToStatepoints` already deduplicates operands by pointer
identity, so shared slots naturally produce fewer `gc-live` operands.

Do not try to update an existing store's value in place when a later request is
compatible. Existing stores should already have the same value for overlapping
invokes. Updating store operands during interning would make the transformation
harder to reason about and could interact badly with the later
`rewriteOxCamlExplicitRootStoresToBases` step.

### 3. Keep selectors and loads separate

Do not merge `BoundaryRootSelectors` in the first patch. A selector is tied to a
specific boundary block and predecessor list.

Do not merge recovery loads in the first patch unless they are already cached by
the existing `(root value, boundary block, incoming block)` map. Multiple loads
from the same shared slot are cheaper than multiple volatile stores and keep the
control-flow reasoning small.

The important change is:

```text
cache/load miss => maybe new load
cache/load miss != new backing slot
```

### 4. Handle early and late materialization separately

The early and late calls to `materializeOxCamlExceptionRootSlots` currently use
separate local state. A first patch can keep that structure and still remove the
four-per-boundary duplication inside each pass.

After that patch, the nested benchmark may still have one early slot and one
late slot for the same logical runtime value. If so, add a second patch that
prunes slots whose recovery loads became unused after late materialization.

Do not attempt cross-pass slot interning first. The late pass often sees
relocated SSA values, so pointer-identity compatibility between early and late
requests may be misleading. Dead-slot pruning has a clearer correctness story.

After the first patch, inspect the rewritten IR for the nested benchmark:

- if each group of four collapsed to one slot but an early and late slot remain,
  continue with dead-slot cleanup;
- if a single materialization pass still creates multiple slots with identical
  stores because `LogicalRoot` differs, add an explicit equivalence-based merge
  as a separate patch. That merge should key by compatible full store behavior
  and prove that all recovery loads are used in equivalent places, rather than
  simply merging different roots by type.

### 5. Prune trivially unused explicit root slots

Add cleanup after late materialization.

First version:

1. Track which allocas were created as GC exception-root slots.
2. For each candidate alloca, inspect users.
3. Ignore stores to the slot and debug users.
4. If every load from the slot has no non-debug users, remove the slot from all
   `gc-live` bundles that contain that exact slot, erase the loads, erase the
   stores, and erase the alloca.

Do not implement selector-aware liveness in the first cleanup patch. If a load
feeds a selector, keep it. Selector-aware pruning can be a follow-up with its
own test.

If a cleanup removes a slot from `gc-live`, rewrite the statepoint call in the
same style as `appendExplicitRootSlotsToStatepoints`: collect operand bundles,
replace the `gc-live` input list, create a replacement `CallBase`, replace uses,
and erase the old call.

When erasing a dead slot, erase both the invoke stores and the entry-block
initial store. When keeping a slot, keep the initial store volatile for GC roots:
it is the default non-moving value that makes an uninitialized recovery path
safe.

### 6. Leave broad `gc-live` narrowing for later

`insertParsePoints` appends early explicit root slots to every partially
constructed statepoint record, and appends late explicit root slots to every
statepoint in the function.

That broad append is separate from the duplicated hot stores. Narrowing it may
reduce stackmap size, but it needs a separate proof that no recovery-path
statepoint loses a root. Do not mix it into the slot-sharing patch.

## Tests

Add focused LLVM IR tests under:

```text
vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/
```

This directory already has OxCaml RS4GC tests.

Test 1: one slot can represent PHI incoming values.

- Build a small `gc "ocaml"` or `gc "oxcaml"` function with two collecting
  invokes unwinding to one recovery block.
- The recovery value is a PHI with different incoming GC values.
- Check that the rewritten IR uses one exception-root alloca with two stores,
  one before each invoke.
- This guards against reintroducing "one slot per incoming edge" thinking.

Test 2: duplicate boundary-edge requests share one slot.

- Build a recovery-only region with several boundary incoming edges that all
  require the same root across the same hot invoke.
- Red expectation: current code emits multiple volatile stores of the same root
  before the same invoke.
- Green expectation: one volatile store for that root/slot before that invoke.

Test 3, only if needed: trivially unused early slot cleanup.

- Construct or reduce a case where the early pass creates a load that becomes
  unused after late materialization.
- Check that the dead slot is removed from `gc-live` and its volatile stores are
  gone.

Test 4, only if needed: different logical roots with identical store behavior.

- Add this only if the first patch leaves duplicate stores within a single
  materialization pass because the duplicate requests have different
  `LogicalRoot` values.
- The test should make the desired equivalence explicit. Do not use this test to
  justify blind same-type slot merging.

After IR tests, rerun:

- `closure_call_in_try_hit`;
- `closure_call_in_nested_try_hit`;
- `direct_call_in_try_hit` as a regression guard.

Full self-stage is not needed for the first patch unless the focused tests or
microbenchmarks reveal a correctness issue.

## Expected result

The first slot-sharing patch should reduce each repeated boundary-edge group
from four slots to one slot per logical root in that materialization pass.

If early and late materialization both still create a slot for the same runtime
root, the cleanup patch should remove dead early slots where the late pass has
made the early recovery load unused.

The intended final hot shape is one volatile store per logical root per
collecting invoke, not one store per recovery boundary edge.

## Self-review and revised conclusions

Old mistake: treating PHIs as a reason not to share slots. Corrected: PHIs are
exactly what slots are good at. A single slot can carry different edge values
from different invokes.

Old mistake: interning only identical full store programs. Corrected: identical
full store programs are sufficient but not necessary. The better first key is
same logical root/type/alignment plus compatible overlapping stores, allowing
the slot to accumulate stores for additional invokes.

Old mistake: conflating recovery loads with root slots. Corrected: a boundary
edge may still need its own load placement, but that load can read a shared
backing slot.

Old mistake: considering cross-pass interning too early. Corrected: first share
within one materialization pass, then prune dead early slots if the late pass
makes them redundant.

Old mistake: broad `gc-live` append looked like the same issue. Corrected: it is
a separate stackmap-size/root-retention issue and should not be mixed with the
hot-store fix.

Second review issue: same-`LogicalRoot` merging is safe but may not collapse all
observed stores if different logical roots store the same value before the hot
invoke. Revised conclusion: keep same-`LogicalRoot` as the first patch's safety
boundary, measure the remaining IR shape, and only add equivalence-based merging
with a separate test if the data shows it is needed.

Second review issue: extending an existing slot with stores for additional
invokes must also update `ExplicitExceptionRootCalls`. Revised conclusion: make
that an explicit requirement of the interner.

## Open questions

- Is `LogicalRoot` pointer identity stable enough within one materialization
  pass for all duplicate boundary-edge requests in the benchmark? It should be,
  because the observed stores all use the same SSA value.
- If the observed `%gcagg142.exnroot...` and `%.1.exnroot...` groups come from
  different logical roots in the same materialization pass, what is the precise
  equivalence relation that makes them shareable? Do not guess; inspect the IR
  after the same-`LogicalRoot` patch.
- Should exception-root allocas get metadata or be tracked in a side table for
  cleanup? A side table is simpler inside RS4GC; metadata may help future
  debugging.
- Can the first IR test be minimized without relying on AArch64 trap intrinsics,
  or does the OxCaml recovery-region recognizer require them for this shape?

## Implementation status

Implemented first-pass demand/slot improvements in
`materializeOxCamlExceptionRootSlots`.

The interner is intentionally local to one recovery block materialization pass.
It now has two sharing modes:

- same logical recovered value, type, and alignment: reuse the slot when all
  overlapping invoke store sites store the same SSA value, and extend the slot
  with missing stores;
- different logical recovered values with the same type and alignment: reuse
  the slot only when the full store program is identical.

When a reused slot is extended to a new invoke, the invoke is also updated in
`ExplicitRootSlots`/`ExplicitExceptionRootCalls`.

Boundary live-in and boundary-use handling now checks for real dominated
non-PHI uses before constructing a boundary selector, recovery load, or root
slot. Rootability/base checks happen after this demand check, so unused boundary
candidates do not cause speculative root emission or spurious failures.

The adversarial baseline moved only where this design predicts:

- `case08_boundary_switch_two_edges`: 4 volatile stores to 2.
- `case11_boundary_via_trampoline`: 4 volatile stores to 2.
- `case12_boundary_local_loaded_value`: 4 volatile stores to 2.

PHI-heavy cases, independent roots, and cases with genuinely different logical
roots did not collapse. That is the desired first patch behavior.

The cross-logical-root case now has a focused regression:

- `case21_same_store_program_different_logical_roots`: two different logical
  handler values with the same physical store program share one root slot.

Focused validation:

- `llvm-lit -q vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-exception-root-adversarial.ll`
- `llvm-lit -q vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-*.ll`

Both pass with `../llvm-build/bin/llvm-lit` after updating stale CHECK lines for
volatile exception roots and the current C-wrapper argument-root model.
