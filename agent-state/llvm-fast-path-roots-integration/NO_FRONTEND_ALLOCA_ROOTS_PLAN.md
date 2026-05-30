# No-Frontend Alloca Roots Plan

## Goal

Remove ordinary frontend alloca roots from the OxCaml LLVM backend without
depending on ordinary LLVM mem2reg to guess the GC model.

The frontend may still initially lower CFG virtual registers through allocas,
but those allocas must remain scalar-promotion artifacts. They must not become
GC root storage.

## Current Problem

`backend/llvm/llvmize.ml` creates one alloca per CFG register. This is
intentional: the backend starts from a mutable register model and expects LLVM
scalar optimization to recover SSA where possible.

The problem is that the same allocas are also used as explicit root slots:

- `live_gc_root_alloca_bundles` passes frontend register allocas in `gc-live`
  operand bundles.
- `call_operand_bundles` still keeps some roots under
  `llvm-unsafe-no-frontend-alloca-roots=1`, notably primitive-call roots and
  known-base fallbacks.
- allocation and poll slow paths copy live values into frontend-created root
  slots, pass those slots in `gc-live`, then reload after `caml_call_gc`.

Once an alloca address appears in a `gc-live` bundle, the address is observable
to statepoint lowering. Normal promotion is no longer legal. LLVM's
`isAllocaPromotable` correctly rejects such allocas.

This is why a value like `%21` in `test_gen_u_array.ll` does not become a plain
SSA value in the optimized unsafe IR. The frontend slot is still entangled with
root materialization.

## Desired Invariant

There are two distinct concepts:

- **Frontend register slot**: temporary mutable storage used to lower CFG
  registers before scalar promotion. Its address must be used only by ordinary
  loads/stores and other promotion-safe intrinsics.
- **GC root slot**: storage whose address is intentionally passed to GC
  lowering and recorded in stack maps.

Required invariant:

1. In no-frontend-root mode, no frontend register slot address may appear in a
   `gc-live` bundle.
2. In no-frontend-root mode, no frontend register slot address may be passed to
   a helper, intrinsic, or call for ordinary root preservation.
3. Any GC heap pointer live across a safepoint must be visible to late lowering
   as a typed `ptr addrspace(1)` SSA value, or through a deliberately
   late-created root slot.
4. A deliberate root slot must not alias a frontend register slot.
5. Any hidden GC pointer shape that late lowering cannot see must fail closed
   under the no-frontend-root path.

## Implementation Plan

### Step 1: Define the Slot Boundary Explicitly

The first implementation step should not change code generation. It should make
the slot distinction observable.

Add enough metadata, naming, or side-table information to distinguish:

- frontend register slots created by `alloca_regs`;
- frontend slow-path root slots created for `caml_call_gc`;
- late or compatibility exception root slots;
- non-GC local stack slots.

The verifier and debug output must use this classification. Inferring the role
of a slot from a printed LLVM name is not robust enough.

If the check runs in `llvmize.ml`, the existing OCaml tables can provide the
classification. If the check runs in LLVM, the role must be represented in the
IR with metadata, an intrinsic, or another stable marker that survives to the
pass. Comments and pretty-printed names are not enough.

### Step 2: Add a Promotion-Blocking Verifier

Add an opt-in verifier in the LLVM path, initially enabled only with the
no-frontend-root diagnostic mode.

For every static alloca whose allocated type is `ptr addrspace(1)`:

- classify it as a frontend register slot, a slow-path root slot, or an
  exception/trap root slot;
- for frontend register slots, reject non-promotion-safe uses;
- in particular, reject any use in a `gc-live` operand bundle.

The diagnostic should print the function, slot name, and the blocking user.

This verifier belongs near the LLVM RS4GC boundary too, because some blocking
uses only become obvious after scalar cleanup and operand-bundle preservation.

### Step 3: Make the Diagnostic Mode Coherent

Change `llvm-unsafe-no-frontend-alloca-roots=1` so it means exactly:

- no ordinary frontend register alloca appears in `gc-live`;
- primitive calls do not keep frontend register roots by default;
- known-base fallback roots do not keep frontend register roots by default;
- slow-path root slots are controlled separately, but enabling them must not
  reintroduce frontend register slots into `gc-live`.

The mode should continue to emit:

- `statepoint-id`;
- deopt metadata;
- call/invoke structure;
- typed `ptr addrspace(1)` values.

This makes the diagnostic mode test the intended model instead of a partial
hybrid.

For reduction work, keep separate narrower compatibility switches:

- keep primitive-call frontend roots;
- keep known-base fallback frontend roots;
- keep frontend slow-path root slots.

Those switches are diagnostic aids only. A passing result with any of them
enabled does not validate the no-frontend-root model.

### Step 4: Rework Slow-Path Root Handling

Allocation and poll slow paths need special care. The current slow-path scheme
stores live values into frontend root slots before `caml_call_gc` and reloads
them after the call. That is explicit root storage, not SSA discovery.

In the no-frontend-root path, choose one of two shapes per slow path:

1. Prefer: represent the slow path as an ordinary statepoint candidate whose
   live `ptr addrspace(1)` SSA values are discovered by RS4GC.
2. Temporary fallback: use separate root slots marked as compatibility root
   storage, never frontend register slots.

The preferred shape is the target. The temporary fallback is allowed only while
reducing failures, and must be easy to disable independently.

Important: disabling frontend slow-path root slots is not the same as proving
slow paths are safe. The proof is that the `caml_call_gc` statepoint sees the
same live GC values as typed SSA values, and that later uses are rewritten to
relocated values.

### Step 5: Keep Explicit Root Slots Separate

Where explicit root slots are still required, keep them separate from frontend
register slots.

Allowed explicit slots:

- late-created exception roots for shared trap handlers;
- any temporary compatibility root slot that is clearly marked as root storage.

Disallowed:

- using the frontend virtual-register alloca as the root slot;
- copying a value through a frontend register slot solely to make its address
  available to `gc-live`;
- relying on SROA to untangle a frontend register slot that has already been
  passed as root storage.

If a temporary compatibility root slot remains in the frontend, annotate or name
it so the verifier can distinguish it from frontend register slots.

### Step 6: Promote Before Statepoint Liveness

Keep `promoteOxCamlGCPointerAllocas` in `RewriteStatepointsForGC`, but make it
diagnostic-friendly:

- count promotable `ptr addrspace(1)` allocas;
- count rejected `ptr addrspace(1)` frontend register allocas;
- when no-frontend-root mode is enabled, fail on rejected frontend register
  allocas unless they are explicitly whitelisted root slots.

This makes the pipeline fail at compile time instead of producing stale
unrooted stack values.

Do not rely on the normal clang `-O3` pipeline alone for this. RS4GC needs a
local last-chance promotion step because OxCaml correctness depends on the
statepoint pass seeing typed SSA values.

### Step 7: Remove Ordinary Frontend Root Bundles by Class

Once the verifier is in place, remove root bundles incrementally:

1. Ordinary OCaml calls with no active trap.
2. Allocation slow paths with no active trap.
3. Poll slow paths with no active trap.
4. Primitive calls that may allocate.
5. Calls with active traps but no handler-live GC values.
6. May-raise invokes whose handler-live values are covered by late exception
   roots.

Each class should be protected by:

- a verifier check that no frontend register slot reaches `gc-live`;
- an LLVM IR test for the statepoint shape;
- a focused source test that forces movement before a later use.

### Step 8: Keep the Addr/Derived-Pointer Work as a Gate

Removing frontend roots only works if late lowering can see every semantic GC
heap pointer.

Do not treat the no-frontend-root path as correct until the following separate
invariants hold:

- heap values are emitted as `ptr addrspace(1)`;
- heap-derived addresses do not cross safepoints as raw `i64` or default
  `ptr`;
- derived `ptr addrspace(1)` values are either rematerialized from relocated
  bases or rejected;
- raw native addresses, such as bigarray data, stay raw and are not roots.

This plan composes with `ADDR_AS_GEP_PLAN.md` and
`DERIVED_POINTER_STATEPOINT_PLAN.md`; it does not replace them.

### Step 9: Validation

LLVM tests:

- frontend register alloca used only by loads/stores is promoted before RS4GC;
- frontend register alloca in `gc-live` is rejected in no-frontend-root mode;
- primitive-call safepoint gets roots from typed SSA values, not frontend
  alloca bundles;
- slow allocation path gets roots from typed SSA values, not frontend slow-path
  slots;
- explicit exception root slots remain allowed and are distinguishable.

Source tests:

- reduced product-array `%21` case from `test_scannable_product_array_4.ml`;
- product iarray/or-null cases from `LATE_ROOT_FAILURE_NOTES.md`;
- allocation-heavy loop with captured module value used after repeated calls;
- primitive C call that may allocate with live OCaml values;
- active-trap case with handler-live value.

Suite validation before making the path default:

- `typing-layouts-arrays`;
- `typing-layouts-iarrays`;
- `typing-layouts-products`;
- `gc-roots`;
- `compaction`;
- `async-exns`;
- `match-exception`;
- `runtime-C-exceptions`;
- self-stage2.

## Acceptance Criteria

The plan is complete when:

1. no-frontend-root mode emits no ordinary frontend register slots in
   `gc-live`;
2. all GC heap pointers live across ordinary safepoints are found by RS4GC as
   typed SSA values or late explicit roots;
3. the verifier rejects any remaining promotion-blocked frontend register slot;
4. focused no-frontend tests pass without the unsafe frontend root fallback;
5. full LLVM-backend validation and self-stage2 pass before making it default.

## First Concrete Patch

The first code patch should be diagnostic-only:

1. record the role of each `ptr addrspace(1)` alloca created by `llvmize.ml`;
2. under `llvm-unsafe-no-frontend-alloca-roots=1`, assert that frontend
   register slots are not emitted into `gc-live`;
3. add a debug/counting path in `promoteOxCamlGCPointerAllocas` that reports
   rejected frontend register slots and their blocking users;
4. add one reduced LLVM IR test where a frontend register slot in `gc-live` is
   rejected;
5. rerun only the focused product-array reproducer to confirm the diagnostic
   identifies the current `%21`/primitive-call/slow-path entanglement before
   changing root emission.

This patch should not try to make the no-frontend-root tests pass. Its purpose
is to make the current mixed model fail in a precise place.

## Pre-Implementation Experiments

Initial experiments are recorded in:

- `no_frontend_alloca_root_experiments/RESULTS.md`

They validate the design direction:

- a plain `ptr addrspace(1)` alloca is promoted by mem2reg;
- the same alloca is not promoted once its address appears in
  `gc-live(ptr %slot)`;
- RS4GC roots and relocates a live typed `ptr addrspace(1)` SSA value without a
  frontend alloca root;
- in the product-array target function, every surviving optimized
  `ptr addrspace(1)` alloca is directly in a `gc-live` bundle;
- the same focused product-array optimized IR has no suspicious derived
  default-pointer hits under the existing audit.

These results support making the first implementation patch diagnostic-only:
detect and report the mixed model before changing root emission.

## Self-Review Log

### Review 1

Issues found:

1. The initial draft jumped straight to verifier logic without first requiring
   a reliable way to classify frontend register slots versus explicit root
   slots. That would make the verifier depend on unstable printed names.
2. The initial draft said slow-path root slots should be disabled under the
   no-frontend-root flag, but did not explain the replacement shape for
   `caml_call_gc`.
3. The initial draft removed primitive-call and known-base fallbacks too
   abruptly. Those fallbacks are useful for reducing failures, but they must not
   be confused with validating the intended model.
4. The initial draft did not state clearly enough that RS4GC needs a
   last-chance promotion step. Ordinary `-O3` scalar optimization is helpful,
   but correctness should not depend on it happening before statepoint
   lowering.

Revisions made:

- Added Step 1 to define slot classification before verification.
- Split slow-path handling into its own step with a preferred SSA-discovery
  shape and a temporary explicit-root fallback.
- Kept primitive-call, known-base, and slow-path compatibility switches as
  reduction aids only.
- Added an explicit requirement that RS4GC retain local last-chance promotion
  before statepoint liveness.

### Review 2

Issues found:

1. The revised plan still did not say how LLVM-side diagnostics would know the
   role of an alloca after leaving `llvmize.ml`.
2. The slow-path section could still be misread as "disable slots and trust it"
   rather than "disable slots only when RS4GC sees equivalent typed SSA live
   values."
3. The plan needed an explicit first patch that improves observability without
   changing generated code.

Revisions made:

- Added a requirement that alloca role information must survive to the checker
  if the checker runs in LLVM.
- Added the proof obligation for slow-path `caml_call_gc` statepoints.
- Added a diagnostic-only first patch with concrete expected behavior.
