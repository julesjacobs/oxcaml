# Exception-root slot duplication plan

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
