# Post-recovery exception root plan

## Goal

Fix the remaining unsafe and slow shape for GC values that leave an OxCaml trap
recovery path and rejoin ordinary control flow.

The concrete motivating shape is `closure_call_in_try_hit`:

```llvm
normal:
  %closure.reloc = gc.relocate(...)
  br label %join

caught:
  br label %join

join:
  %closure.next = phi [ %closure.reloc, %normal ], [ %closure, %caught ]
```

The `%closure` incoming value from `caught` is not an exceptional relocate and
is not loaded from an explicit exception root. Current codegen happens to keep
a stackmap root for the statepoint and a separate ordinary spill for the caught
path. That is both slower and not a robust GC semantics.

The intended fixed shape is:

```llvm
entry:
  %closure.exnroot = alloca ptr addrspace(1), align 8

before_statepoint:
  store ptr addrspace(1) %closure, ptr %closure.exnroot
  %tok = invoke token @llvm.experimental.gc.statepoint(...)
           [ "gc-live"(ptr %closure.exnroot, ...) ]
         to label %normal unwind label %recover

normal:
  ; initial conservative form: use the explicit slot as the canonical
  ; post-statepoint value for this handler-live GC pointer.
  %closure.normal = load ptr addrspace(1), ptr %closure.exnroot
  br label %join

caught:
  %closure.caught = load ptr addrspace(1), ptr %closure.exnroot
  br label %join

join:
  %closure.next = phi [ %closure.normal, %normal ],
                      [ %closure.caught, %caught ]
```

This must be created late, after scalar optimization and during/after
RewriteStatepointsForGC. It must not rely on volatile memory or on an alloca root
surviving `-O2`.

## What already exists

`materializeOxCamlExceptionRootSlots` in
`vendor/llvm-project/llvm/lib/Transforms/Scalar/RewriteStatepointsForGC.cpp`
already provides part of the mechanism:

- it identifies OxCaml recovery blocks;
- it creates explicit root slots;
- it stores edge values before invokes;
- it appends the slots to statepoint `"gc-live"`;
- it suppresses exceptional `gc.relocate` for OxCaml trap recovery invokes.

The missing coverage is not the root-slot mechanism itself. The missing coverage
is values used after recovery code branches to a later join.

## Draft plan v0

1. Extend `materializeOxCamlExceptionRootSlots` to find PHI incoming values from
   caught/recovery blocks into ordinary join blocks.
2. For every incoming `addrspace(1)` value from a recovery path, create an
   explicit exception root slot.
3. Store the incoming value before each invoke that can enter that recovery path.
4. Add the slot to the invoke statepoint `"gc-live"` bundle.
5. Replace the PHI incoming value from the recovery path with a load from the
   explicit slot.
6. Keep normal-path values using normal `gc.relocate`.
7. Add tests for the closure shape and moving-GC caught exceptions.

## Review iteration 1

### Problems found

The draft says "caught/recovery blocks" but does not define the region. A naive
reachability walk from the recovery block would run through the join and then
through the rest of the function, which would root too many values and could
rewrite ordinary code.

The draft also assumes every PHI incoming value is the value to store before
every invoke. That is only valid when the value dominates every invoke that can
enter the recovery path. More complex recovery code can have PHIs inside the
recovery-only region, or values computed after `trap.recover`.

The draft keeps normal-path `gc.relocate` for the same value while also rooting
the slot. That is safe but can create duplicate roots and keep the hot-path
spill pressure that caused the slowdown.

### Revision v1

Define a `recovery-only region` for each OxCaml recovery entry:

- seed it with the recovery landingpad block;
- add a successor only when all of its predecessors are already in the region;
- stop at the first block with any predecessor outside the region.

Boundary PHIs are PHIs in successor blocks outside the recovery-only region with
at least one incoming edge from a block inside the recovery-only region.

Collect two classes of `recovery environment values`:

- external GC values used by instructions inside the recovery-only region after
  `trap.recover`;
- GC values used as boundary PHI incoming values on edges from the
  recovery-only region.

For the first implementation, only materialize a recovery environment value when
the value dominates every invoke store site for that recovery entry. If this is
not true, report a verifier-style fatal error in debug/testing builds and leave
the unsupported shape unoptimized until a later patch handles it explicitly.

Use the explicit slot as the canonical post-statepoint value for the
handler-live GC pointer. That avoids duplicate roots for the same object in the
initial safe implementation. A later performance patch can add a hybrid mode
that keeps normal-path `gc.relocate` plus slot synchronization where it proves
profitable.

## Review iteration 2

### Problems found

The `recovery-only region` definition is still too broad for nested traps and
re-raise paths. A block can be reachable only from recovery but install a new
trap or call another may-raise operation. Root slots for the original trap
environment must not be silently extended through arbitrary later protected
regions without proving that they are still the right handler environment.

The plan says "store before each invoke" but this is not enough if there is a
non-raising statepoint in the protected region before a later raising call. A GC
at that non-raising statepoint can move the value, and a later raise needs the
updated value.

The plan says "canonical slot" but does not say how the slot gets synchronized
after a normal continuation. If a protected region has two statepoints and the
value survives both, the slot must contain the relocated value before the second
statepoint.

The plan does not mention derived pointers. Rooting a derived pointer slot is
not generally valid because an alloca in `"gc-live"` has no separate base
pointer.

### Revision v2

Limit the first implementation to a strict recovery-only region:

- include blocks after `trap.recover` until the first boundary join;
- reject or skip regions containing `push.trap`, another invoke statepoint,
  `raise.notrace`, or any operation that starts a new independent exceptional
  control-flow region before the boundary join;
- handle nested traps as separate later work after the single-region invariant
  is tested.

Track root-slot liveness over the whole protected dynamic extent:

- the slot must be in `"gc-live"` for every statepoint where the handler-live
  value can be moved before the handler may later observe it;
- this includes allocation slow paths and polls, not just invoking calls that
  can raise.

Synchronize the slot:

- store the pre-statepoint value before the first relevant statepoint;
- after a normal continuation, load the slot as the canonical relocated value;
- before the next relevant statepoint, no extra store is needed if the canonical
  value is already the slot value and the source value has not changed;
- when the abstract value changes, store the new value before the next relevant
  statepoint.

Derived pointer rule for the first implementation:

- only materialize explicit exception roots for base GC pointers where
  `findBasePointer(V) == V`;
- reject derived handler-live GC values with a clear diagnostic or leave them to
  an existing safe fallback;
- add a later design for base-plus-offset reconstruction if a real source case
  needs it.

## Review iteration 3

### Problems found

The phrase "canonical slot value" can accidentally force every normal-path use
of a handler-live value through memory. That is safe, but it may regress other
benchmarks if applied too broadly.

The plan also needs a post-transform verifier. GC bugs can pass ordinary tests
for a long time, so the implementation should reject bad IR shapes instead of
assuming the transform found everything.

The existing pass currently runs both before statepoint rewriting and again
after `relocationViaAlloca`. The plan must specify where the new logic belongs,
otherwise it may accidentally create alloca roots before scalar optimization or
miss values that only appear after relocation fixup.

### Revision v3

Use two lowering modes:

1. `canonical-slot mode` for the first safe implementation and for values that
   are live on an exceptional recovery edge. The slot is the post-statepoint
   value used by the recovery incoming edge, and normal-path uses may load from
   it when needed to feed a join.
2. `hybrid mode` later, after tests: normal path keeps `gc.relocate`, and the
   explicit root slot is synchronized only when the handler environment changes.

Start with canonical-slot mode only for values that actually flow out of a trap
recovery path. Do not change unrelated normal statepoint roots.

Add a verifier after explicit root materialization and statepoint rewriting:

- for each OxCaml recovery entry using explicit exception roots, collect the
  strict recovery-only region and boundary PHI edges;
- reject any `addrspace(1)` value used in that region, or as an incoming value
  from that region, if it is a protected-path SSA value rather than a load from
  an explicit exception root slot, an existing valid exceptional relocate, or a
  proven nonmoving constant;
- reject explicit root slots missing from a relevant statepoint `"gc-live"`;
- reject derived pointer roots until base-plus-offset support exists.

Place the new transformation inside RewriteStatepointsForGC's late lowering:

- it must run after the main scalar optimizer;
- it may run in the existing pre-statepoint phase inside RS4GC, after `-O2`,
  when it needs to add slots before explicit statepoints are built;
- it must also run or verify after `relocationViaAlloca`, because some boundary
  values are only obvious after normal relocation rewriting;
- no scalar cleanup pass may run after the physical root slots are introduced.

## Review iteration 4

### Problems found

The plan still says "every relevant statepoint" without a mechanical definition.
That is risky in both directions:

- too few statepoints means a moving GC can update a value before a later raise,
  and the handler sees a stale slot;
- too many statepoints means the slot becomes a shadow-stack root for a much
  larger part of the function than necessary.

The phrase "pre-statepoint phase" could also be misread as "before the normal
optimizer", which would contradict the expert advice. The correct meaning is
the phase inside RS4GC after scalar optimization but before explicit
`gc.statepoint` calls are constructed.

The draft says unsupported nested recovery shapes can be "rejected or skipped".
Skipping is not acceptable if a protected-path GC value still escapes through
that path. The implementation may skip an optimization only when the verifier
can still prove the resulting IR is safe.

### Revision v4

Define relevant statepoints for a recovery environment slot as statepoints in
the protected dynamic extent where all of these hold:

- the root value is available on entry to the statepoint, either as the original
  stored value or as the canonical slot value after an earlier statepoint;
- a path exists from that statepoint to the recovery entry without passing
  through the matching `pop.trap` for the active trap;
- a path exists from the recovery entry through the strict recovery-only region
  to a use of the recovery environment value or to a boundary PHI incoming edge;
- the slot has not been overwritten with a different abstract value on all such
  paths.

Implement this conservatively at first:

- begin with the invoking statepoints that have unwind edges to the recovery
  entry;
- include earlier non-raising statepoints in the same active trap region only
  when the value is live from that statepoint to one of those invokes;
- require dominance for the inserted store, otherwise split by edge or reject
  with a diagnostic.

Do not silently skip unsupported GC values. If the verifier sees a
protected-path `addrspace(1)` value escaping through recovery and the transform
did not root it, that is a hard failure for the new tests.

## Stabilized plan

### 1. Add a recovery-region analysis helper

For each OxCaml trap recovery landingpad recognized by
`getOxCamlTrapRecover`, compute:

- the recovery entry block;
- the strict recovery-only region;
- boundary edges from the recovery-only region to ordinary join blocks;
- invoke store sites that can enter the recovery entry;
- the active trap region for those store sites, including the matching
  `push.trap`/`pop.trap` extent.

This helper should be shared by the transform and verifier so the two cannot
silently disagree.

### 2. Collect recovery environment GC values

Collect handled GC pointer values that are:

- used after `trap.recover` inside the strict recovery-only region and defined
  outside that region; or
- used as PHI incoming values on boundary edges from the recovery-only region.

Ignore constants that are proven nonmoving. For the first patch, require
`findBasePointer(V) == V`.

### 3. Materialize explicit exception roots for those values

Reuse the existing `MaterializeRootSlot` machinery where possible.

For each collected value:

- create one entry-block alloca root slot;
- store the value before the first relevant statepoint where the value becomes
  visible to the handler environment;
- keep the slot synchronized after normal continuations by using the slot load
  as the canonical value for this handler environment until the abstract value
  changes;
- add the slot to each relevant statepoint `"gc-live"` bundle, where relevant
  means a statepoint in the active trap extent that can move the value before a
  later transfer to the recovery entry;
- replace recovery-region uses and boundary PHI incoming values with loads from
  the slot.

If a value does not dominate all required store sites, do not guess. Either
split the value by edge using the PHI incoming map already used by
`MaterializeRootSlot`, or reject the shape with a clear diagnostic and a test.

### 4. Keep the first patch conservative about normal-path optimization

For values flowing out of recovery, use canonical-slot mode first:

- the recovery path loads from the slot;
- the normal path may also load from the slot when feeding a join with the
  recovery path;
- unrelated normal-path roots still use normal `gc.relocate`.

This is safe and should remove the duplicate "stackmap root plus ordinary
handler spill" shape seen in `closure_call_in_try_hit`.

After correctness is established, add hybrid mode only if benchmarks show the
canonical slot loads are too expensive.

### 5. Add verifier checks

Add a verifier or fatal validation pass in RS4GC for OxCaml recovery roots:

- no protected-path `addrspace(1)` SSA value may escape from a recovery path
  into ordinary code unless it is loaded from an explicit exception root slot,
  produced by a valid exceptional relocate, or proven nonmoving;
- every explicit exception root slot must be listed in the `"gc-live"` bundle of
  every relevant statepoint in the active trap extent;
- no derived pointer may be rooted directly;
- no unsupported nested recovery shape may be silently accepted when a GC value
  escapes through it.

These checks should run in LLVM tests and should be cheap enough to keep enabled
for assertions builds.

### 6. Tests

Add focused LLVM IR tests:

- one invoke, caught path rejoins normal loop, GC pointer PHI incoming from the
  caught edge;
- two invokes sharing one recovery entry and one post-recovery join;
- scalar non-GC PHI from recovery remains ordinary SSA and is not rooted;
- derived GC pointer on a recovery boundary is rejected or lowered through an
  explicit base root plus offset only if that support is implemented;
- nested trap in recovery is rejected by the first implementation;
- statepoint frametable reports the explicit root slot used by the recovery
  path.

Add OxCaml source tests:

- closure call in `try` where the caught path continues the loop;
- heap-allocated closure captured by the handler with forced compaction;
- allocation/poll before a later raise in the same trap region;
- multiple may-raise calls to one handler;
- nested `try` once supported.

### 7. Benchmark and codegen checks

Before and after the change, check:

- `closure_call_in_try_hit`;
- `direct_call_in_try_hit`;
- `closure_call_in_nested_try_hit`;
- representative compiler benchmarks.

Inspect generated assembly and frametable for the reduced closure case:

- there should not be both a statepoint root spill and a separate caught-path GC
  spill for the same closure;
- the caught path should load the closure from the explicit root slot;
- the frametable statepoint live offset should name that same slot.

### 8. Work order

1. Add LLVM IR tests that reproduce the current bad post-recovery PHI shape.
2. Add the recovery-region helper and verifier checks, initially expecting the
   new tests to fail.
3. Extend explicit root materialization for boundary PHI incoming values and
   recovery-only uses.
4. Re-run LLVM IR tests and inspect generated assembly/frametable.
5. Add OxCaml source tests for moving-GC caught handler values.
6. Run focused exception/GC tests.
7. Run microbenchmarks and compare closure/direct try cases.
8. Only then consider hybrid normal-path relocation optimization.

## Why this is now stable

The final plan avoids the unsafe options found during review:

- it does not delete spills by hand;
- it does not rely on alloca roots surviving scalar optimization;
- it does not root derived pointers directly;
- it does not silently accept nested or non-dominating edge values;
- it adds a verifier for the exact class of GC bug we are worried about.

The implementation is still incremental: it extends the existing explicit
exception-root pass, starts with a conservative root-slot mode, and leaves a
separate hybrid optimization for later if measurements justify it.
