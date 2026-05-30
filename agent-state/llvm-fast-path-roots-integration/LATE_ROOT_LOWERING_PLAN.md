# Late Root Lowering Plan

Goal: make the OxCaml LLVM backend stop emitting concrete ordinary GC root
lists early, and instead let the late LLVM statepoint lowering discover and
materialize roots after scalar optimization.

This plan is about ordinary safepoint roots. It does not remove the late
explicit exception-root design for shared trap handlers. That design remains
needed because shared exceptional landingpads cannot use ordinary exceptional
`gc.relocate` reliably.

## Current State

The frontend still emits `gc-live` bundles in `backend/llvm/llvmize.ml`:

- `live_gc_root_alloca_bundles` adds frontend register allocas.
- `live_gc_root_slot_bundles` adds frontend slow-path root slots.
- `call_operand_bundles` combines those root bundles with deopt metadata.
- `call_gc_for_basic_safepoint` stores live roots into slow-path root slots
  for eligible allocation and poll slow paths, calls `caml_call_gc`, then
  reloads them.

The late LLVM pass also computes roots:

- `RewriteStatepointsForGC` rewrites safepoint calls to statepoints.
- It computes normal live GC values with statepoint liveness.
- It materializes explicit exception root slots for shared trap handlers.
- It now canonicalizes raw OxCaml heap addresses so hidden object bases are
  visible to statepoint liveness.

So the system is currently mixed: some ordinary roots are still supplied by the
frontend, while some roots are computed or materialized late.

## Desired Model

The frontend should emit abstract safepoint facts, not concrete ordinary root
storage.

The frontend should keep emitting:

- `statepoint-id` attributes on safepoint candidates;
- deopt metadata needed by the late pass and stack maps;
- precise CFG edges, including `invoke` unwind edges for may-raise operations;
- trap publish/recover intrinsics;
- typed `addrspace(1)` GC values wherever a value semantically is an OCaml heap
  value.

The frontend should stop emitting for ordinary roots:

- frontend `gc-live` bundles for register allocas;
- frontend slow-path root slots;
- stores into those slow-path root slots before `caml_call_gc`;
- reloads from those slow-path root slots after `caml_call_gc`.

The late pass should be responsible for:

- discovering normal GC values live across safepoints;
- inserting `gc.statepoint`;
- inserting normal-continuation `gc.relocate`;
- materializing explicit exception root slots for handler-live GC values;
- adding explicit exception root slots to every relevant statepoint;
- rejecting or diagnosing any hidden GC pointer shape it cannot make safe.

## Invariants

1. Every OCaml heap value that is live across a safepoint must be visible to
   late lowering as either:
   - a typed `addrspace(1)` SSA value; or
   - a late-created explicit root slot with the right GC-live entry.

2. No semantic OCaml heap value may cross a safepoint only as:
   - `i64`;
   - raw `ptr`;
   - an untracked alloca payload;
   - an aggregate component that RS4GC cannot inspect;
   - a derived pointer without a recoverable base.

3. After a statepoint, ordinary uses of a moved GC value must use either:
   - the normal `gc.relocate` result; or
   - a load from a late explicit root slot that is present in the statepoint
     live set.

4. Shared trap handlers must not require exceptional `gc.relocate` from a
   multi-predecessor landingpad. Handler-live values use explicit exception root
   slots.

5. Frontend allocas are allowed as an optimization artifact before scalar
   optimization, but they must not be pinned by early `gc-live` bundles unless
   a specific late-lowering escape hatch says that is intentional.

## Implementation Plan

### Phase 1: Add Diagnostics Without Changing Semantics

Add late diagnostics in the LLVM pass that can run before and after statepoint
rewriting:

- report raw heap memory operands that are derived from `addrspace(1)` but were
  not canonicalized;
- report GC pointer uses after statepoints that are not relocate/reload uses;
- report GC pointer PHIs or uses escaping a shared recovery region without an
  explicit exception root slot;
- report statepoint candidates whose live GC values are only present in
  frontend `gc-live` bundles and not visible through typed IR.

These diagnostics should initially be debug-only or opt-in so the branch can
measure them on generated IR before enforcing them.

### Phase 2: Build LLVM IR Prototype Tests

Before changing `llvmize.ml`, add small LLVM IR tests for the exact shapes the
frontend should eventually emit without ordinary `gc-live` bundles:

- simple OCaml call with live typed `addrspace(1)` value;
- allocation slow path call with live typed value and no frontend root slot;
- poll slow path with live typed value and no frontend root slot;
- loop-carried live value across a poll/allocation;
- raw heap address canonicalization before and inside loops;
- invoke with shared trap handler and handler-live value;
- value live on both normal and exceptional paths;
- C call that may allocate;
- derived pointer with base plus offset;
- aggregate value where only a component is a GC pointer.

Expected output should check that:

- normal values get `gc.relocate` on normal continuations;
- shared trap handler values use explicit exception root slots;
- no exceptional `gc.relocate` is emitted for shared trap handlers;
- no frontend root slot is required for the ordinary case.

### Phase 3: Add a Frontend Flag to Suppress Ordinary `gc-live`

Add a temporary internal switch, for example an LLVM backend option, that
suppresses ordinary frontend root bundles and slow-path root slots but leaves
all other safepoint facts intact.

Under this switch:

- `call_operand_bundles` emits deopt metadata only for ordinary safepoints;
- `call_gc_for_basic_safepoint` does not store/reload slow-path root slots;
- trap-handler-specific frontend constructs remain unchanged;
- calls still carry `statepoint-id`;
- invokes still carry unwind edges.

This must be an opt-in path at first. The default path remains the current
validated behavior.

### Phase 4: Run Focused Source Tests Under the New Flag

Start with tests that are likely to expose missing roots:

- allocation slow paths with forced minor collections;
- polls in loops;
- ordinary OCaml calls that allocate;
- C calls that allocate;
- raises into shared trap handlers;
- handler-live captured values;
- raw heap field loads after calls;
- iarray and unboxed-layout tests that previously exposed stale roots.

For any failure:

1. reduce to a short source test;
2. inspect emitted LLVM IR before and after optimization;
3. decide whether the problem is:
   - hidden GC value in frontend IR;
   - missing late liveness support;
   - missing exception-root materialization;
   - incorrect statepoint candidate marking;
   - runtime/stackmap mismatch.

Do not globally enable the flag until this phase is clean.

### Phase 5: Replace Frontend Ordinary Roots Incrementally

Once focused tests pass, enable the new path by class:

1. Ordinary calls with no active trap.
2. Allocation slow paths with no active trap.
3. Poll slow paths with no active trap.
4. Ordinary calls with active trap but no handler-live values.
5. May-raise invokes whose handler-live values are handled by late exception
   roots.
6. C calls that may allocate.

Each step gets:

- LLVM IR tests;
- focused source tests;
- representative microbenchmarks;
- full LLVM-backend suite if the step changes generated code broadly.

### Phase 6: Delete Dead Frontend Root Machinery

Only after the new path is default and validated:

- remove `live_gc_root_alloca_bundles` for ordinary roots;
- remove frontend slow-path root-slot allocation;
- remove slow-path root store/reload code;
- remove obsolete comments that describe frontend root slots as the primary
  safepoint model.

Keep any frontend data that is still needed to describe abstract safepoint
identity, deopt payloads, trap edges, or stack-adjustment metadata.

## Validation Plan

For each implementation stage:

- LLVM `opt`/`FileCheck` tests for IR shape.
- Focused source tests with moving GC pressure:
  - tiny minor heap;
  - forced compaction where applicable;
  - repeated allocation before a later use.
- `llvm-codegen` expected-output tests where IR shape intentionally changes.
- `typing-layouts-iarrays`, `compaction`, `gc-roots`, `async-exns`,
  `match-exception`, `runtime-C-exceptions`, and `raise-counts`.
- Full LLVM-backend suite before making the new path default.
- Self-stage2 validation before treating it as complete.
- Benchmarks after validation:
  - representative microbenchmarks;
  - compiler-binary benchmark;
  - report summed compile-time ratio as the headline for compiler benchmarks.

## Draft Self-Review 1: Correctness

Concern: removing frontend `gc-live` can expose any hidden GC pointer as a real
GC bug. Tests might miss rare hidden forms.

Revision:

- The plan must put diagnostics before behavior changes.
- The temporary switch must be opt-in.
- The implementation must fail closed for hidden pointers when the switch is
  enabled. A compile-time fatal error is better than silently compiling unsafe
  code.
- The raw heap canonicalizer is necessary but not sufficient; diagnostics must
  detect unsupported hidden shapes rather than assuming canonicalization found
  them all.

## Draft Self-Review 2: Performance

Concern: late materialization could become conservative and introduce more
root slots than the frontend path.

Revision:

- Normal-path values should stay in SSA and use ordinary `gc.relocate`.
- Explicit slots should be reserved for exceptional handler-live values and
  any proven unavoidable physical root case.
- Each phase needs assembly inspection for representative hot paths, not just
  correctness tests.
- The compiler-binary benchmark headline should use summed compile time, not
  harmonic/geometric mean.

## Draft Self-Review 3: Scope

Concern: trying to remove all frontend root machinery in one change is too
large and will make failures hard to attribute.

Revision:

- Use the temporary flag to compare current and new lowering on the same source
  tests.
- Enable by safepoint class, not globally.
- Keep the current validated path available until the new path passes full
  suite and self-stage2.
- Deletion of old code is a final cleanup phase, not part of the first
  correctness patch.

## Stabilized Plan

The first implementation attempt tested the original step 2 directly and
showed that it is not a safe first patch. The right first implementation patch
is now diagnostics/proof work, not a user-facing lowering mode.

1. Add diagnostics and LLVM IR tests that define the no-ordinary-frontend-root
   contract.
2. Add late support for every non-SSA root class that survives scalar
   optimization.
3. Only after the diagnostics prove coverage, add an opt-in frontend lowering
   mode that suppresses ordinary frontend `gc-live` bundles and slow-path root
   slots.
4. Use that mode on narrow IR/source tests and fix every hidden-GC-value issue
   in the late pass or frontend representation.
5. Expand by safepoint class.
6. Only then make the late-root path default and remove dead frontend root
   machinery.

The main correctness condition is precise and testable: with ordinary frontend
roots disabled, every value that can be moved by GC and used after a safepoint
must be visible to the late pass as a typed `addrspace(1)` value or as a
late-created explicit root slot.

## Implementation Attempt Result

The attempted opt-in frontend mode was removed after focused testing found two
real unsoundness classes:

- Ordinary call roots: a handler or continuation can use an OCaml heap value
  that survives scalar optimization only through a frontend-preserved stack
  slot. RS4GC does not currently recover that as a live typed SSA value.
  `typing-layouts-iarrays/test_float32_u_iarray.ml` exposed this as a missing
  frame-table root across `_caml_apply2`, followed by a segfault after moving
  GC.
- Allocation/poll slow-path roots: the frontend-preserved register alloca is
  not guaranteed to hold the current value at an inserted `caml_call_gc` slow
  path. The existing slow-path root slots store current values immediately
  before the GC and reload immediately after. Removing those slots exposed a
  moving-GC failure in `test_scannable_product_iarray_4.ml`.

So the correct next step is a late stack-slot/root recovery design, plus
diagnostics that fail closed when a GC value is live across a safepoint only
through memory that the late pass cannot prove and root.
