# Addr as addrspace(1) GEP Plan

## Goal

Model OCaml heap-derived `Addr` values in the LLVM backend as
`ptr addrspace(1)` GEPs instead of lowering them through `i64` and default
address-space pointers.

The goal is to preserve the base/derived relationship so `RewriteStatepointsForGC`
can relocate the base object and rematerialize the derived address after a
safepoint.

## Non-goals

- Do not make raw/native pointers into managed OCaml pointers.
- Do not scan interior heap addresses as GC roots.
- Do not rely on frontend `gc-live` bundles for ordinary values.
- Do not rely on LLVM scalar optimization preserving physical root slots.

## Terms

- **OCaml value pointer**: a real OCaml heap value, represented as
  `ptr addrspace(1)`. This may be a GC root.
- **Heap-derived address**: an interior address computed from an OCaml value
  pointer plus a byte offset. This should be represented as `ptr addrspace(1)`,
  but must not be scanned as an independent GC root.
- **Raw address**: a native address that is not an OCaml heap value, such as
  bigarray data. This must stay `i64` or default `ptr`.

## Required Invariants

1. `Cmm.Addr` means heap-derived address in the LLVM path.
2. A raw address must not be typed as `Cmm.Addr` before LLVM lowering.
3. A heap-derived address may be used as a memory operand.
4. A heap-derived address must not be passed to a call.
5. A heap-derived address must not remain live across a safepoint unless it is
   rematerialized from a relocated OCaml value pointer plus non-GC offset.
6. At a statepoint, the frame table must expose only OCaml value pointers as GC
   roots, not heap-derived addresses.

## Current Problems

### Current LLVM Type Mapping

`backend/llvm/llvm_ir.ml` currently maps:

```ocaml
| Addr -> i64
```

This hides heap-derived addresses from LLVM's GC pointer analysis.

### Current Address Helper Shape

`backend/llvm/llvmize.ml` has helpers that cast address bases to default
`ptr`:

```ocaml
let cast_to_ptr t arg =
  if T.is_ptr (V.get_type arg) then arg else cast t arg T.ptr

let load_address_from_reg t addr_mode reg =
  let base = load_reg_to_temp t reg |> cast_to_ptr t in
  load_address t addr_mode base T.ptr
```

This erases addrspace(1) even when the base is an OCaml value pointer.

### Raw Address Sites

`bigarray_load` and `bigarray_store` currently compute `ba_data_p` as
`Word_int`, then use `array_indexing ~typ:Addr`.

That is incompatible with the stronger invariant. Bigarray data is a raw
native address, not a managed OCaml heap pointer.

## Plan

### Step 1: Make Raw Address Sites Explicit

Change bigarray data indexing to use raw integer address arithmetic:

- In `bigarray_load`, use `array_indexing ~typ:Int`.
- In `bigarray_store`, use `array_indexing ~typ:Int`.
- For complex bigarray second-half addressing, use `Caddi`, not `Cadda`.

This preserves the current raw pointer behavior and avoids turning bigarray data
into `ptr addrspace(1)`.

Add a focused test or IR check that bigarray data addresses do not appear as
`ptr addrspace(1)` and do not appear in `gc-live`.

### Step 2: Change LLVM Type Mapping for `Addr`

Change `backend/llvm/llvm_ir.ml`:

```ocaml
| Addr -> val_ptr
```

This makes LLVM register slots and SSA temporaries for heap-derived addresses
pointer-typed in addrspace(1).

### Step 3: Split Address Conversion Helpers

Replace generic `cast_to_ptr` use in memory-address lowering with explicit
helpers:

- `heap_address_from_reg`: accepts `Val` or `Addr`, returns `ptr addrspace(1)`.
- `raw_address_from_reg`: accepts `Int`, returns default `ptr`.
- `address_from_reg`: dispatches based on the register type.

Rules:

- `Val` / `Addr`: GEP in addrspace(1).
- `Int`: convert to default `ptr`, then GEP in address-space 0.
- Any other type: fail.

Keep generic `cast_to_ptr` only for non-heap runtime pointers where default
address-space behavior is intentional.

### Step 4: Lower `Cadda` as addrspace(1) GEP

Update the `Iadd` / `Isub` path for `Addr` results:

- Require first operand type `Val` or `Addr`.
- Load the first operand as `ptr addrspace(1)`.
- Load the offset as `i64`.
- Emit `getelementptr i8, ptr addrspace(1) base, i64 offset`.
- Store the result as `Addr`, now `ptr addrspace(1)`.

If an `Addr` result is requested from an `Int` base, fail. That indicates a raw
address site escaped Step 1.

### Step 5: Audit Direct Heap Helper Casts

Find helper code that casts OCaml values to default `ptr` for heap memory
accesses, for example string length / compare helpers.

For heap accesses:

- keep the base as `ptr addrspace(1)`;
- use addrspace(1) GEPs;
- load from `ptr addrspace(1)`.

For runtime structures, stack slots, trap frames, domain state fields, and C
pointers:

- keep default `ptr` or `i64`.

### Step 6: Strengthen Frontend Invariant Checks

Keep the existing `reject_addr_regs` checks for calls.

Add a liveness check in the LLVM path:

- before emitting a call, invoke, allocation slow path, or poll safepoint,
  reject any `Addr` register live across the safepoint;
- allow `Addr` only when it is used locally to form a memory operand before the
  safepoint.

This encodes the Cmm invariant before LLVM has to recover from bad IR.

### Step 7: Strengthen RS4GC Boundary Checks

Add or refine an RS4GC diagnostic:

- If a derived addrspace(1) value is live across a statepoint and RS4GC can
  rematerialize it from a relocated base plus ordinary offset, accept it.
- If a derived addrspace(1) value remains as an independent relocated value,
  reject it for OxCaml unless a correct base-plus-offset repair has been
  implemented.

This catches loop-carried or otherwise obscured heap-derived addresses.

### Step 8: Tests

LLVM IR tests:

- Simple heap-derived GEP across a statepoint rematerializes after base
  relocation.
- Dynamic offset heap-derived GEP rematerializes after base relocation.
- Loop-carried derived pointer is rejected or repaired.
- Raw bigarray-like address stays default pointer / integer and is not in
  `gc-live`.

OxCaml tests:

- Array/string/field access across allocation and poll still passes.
- Bigarray load/store still passes.
- A reduced case that previously produced raw default-ptr heap addressing now
  produces addrspace(1) GEPs and correct RS4GC output.

Native tests for the shared Cmm cleanup:

- `testsuite/tests/lib-bigarray`
- `testsuite/tests/prim-bigstring`
- `testsuite/tests/asmcomp/evaluation_order.ml`
- `testsuite/tests/asmcomp/evaluation_order_broken.ml`

Native assembly spot check:

- Compile a small bigarray get/set example before and after Step 1.
- Confirm native still uses the expected raw address load/store shape.
- Confirm changing bigarray data addressing from `Addr` to `Int` does not
  pessimize or alter the intended native addressing mode.

LLVM bigarray spot check:

- Confirm bigarray data pointers remain `i64` or default address-space
  pointers.
- Confirm bigarray data pointers do not become `ptr addrspace(1)`.
- Confirm bigarray data pointers do not appear in `gc-live`.

Validation:

- Focused LLVM `opt` tests.
- Focused OxCaml tests for arrays, strings, bigarrays, allocation, polls.
- Focused native tests for bigarray/bigstring/evaluation-order coverage after
  the shared Cmm cleanup.
- Full testsuite.
- Self-stage2.
- Microbenchmarks and compiler benchmark comparison.

## Self-review Round 1

### Concern: The Plan Assumes `Addr` Always Means Heap-derived Address

This is currently false. Bigarray data is the known counterexample.

### Revision

Step 1 is mandatory, not optional. Do not change `Addr -> val_ptr` until raw
address sites are fixed or made to fail immediately.

Add a temporary assertion after changing `Addr` type:

- if `Cadda`/`Addr` lowering sees an `Int` base, fail with a message naming raw
  address misuse.

## Self-review Round 2

### Concern: `Val` and `Addr` Both Become `ptr addrspace(1)`

LLVM type alone cannot distinguish an OCaml value pointer from a heap-derived
address. RS4GC must distinguish base and derived pointers.

### Revision

The frontend invariant check remains necessary even if the IR type is correct.

The RS4GC check must inspect base/derived pairs, not just addrspace. A
`ptr addrspace(1)` value is not automatically a valid root.

## Self-review Round 3

### Concern: Rejecting All `Addr` Live Across Safepoints May Be Too Strict

Some valid optimized IR might have a derived GEP live across a statepoint but be
cheaply rematerializable. Rejecting all such cases too early could block good
code.

### Revision

Use two levels of checks:

1. Frontend/Cfg check rejects `Addr` registers live across safepoints. This
   catches violations of the source invariant before LLVM optimization.
2. RS4GC check accepts derived addrspace(1) values only if it can rematerialize
   from relocated base plus non-GC offset. This handles optimizer-created or
   IR-local derived values.

This avoids relying only on the frontend while still allowing simple derived
GEPs in optimized IR.

## Self-review Round 4

### Concern: Bigarray Liveness Is a Separate Issue

Changing bigarray addresses to `Int` keeps raw data pointers out of GC roots,
but it does not prove the bigarray object remains live whenever the data pointer
is used.

### Revision

Do not introduce new bigarray liveness machinery in this plan. The current
lowering already uses raw data pointers; the plan should preserve that behavior.

Add only a regression check that the type cleanup did not make raw data pointers
managed. If a separate bigarray lifetime bug exists, handle it separately.

## Self-review Round 5

### Concern: Step 1 Touches Shared Native Code

The bigarray cleanup is in shared Cmm helper code. Even if the change is
conceptually correct, it may affect native selection, combine, or addressing
mode formation.

### Revision

Treat Step 1 as a shared-backend change:

- run focused native bigarray/bigstring tests before moving on to the LLVM type
  change;
- inspect native assembly for a small bigarray get/set before and after the
  cleanup;
- only proceed to `Addr -> ptr addrspace(1)` after native behavior is confirmed
  unchanged or intentionally improved.

## Stabilized Plan

1. Fix raw/native address producers, starting with bigarray indexing, so they do
   not produce `Addr`.
2. Change LLVM `Addr` type to `ptr addrspace(1)`.
3. Replace memory-address lowering with address-space-aware helpers.
4. Lower `Cadda` to addrspace(1) GEP and fail if the base is raw `Int`.
5. Audit direct heap helper casts and keep heap accesses in addrspace(1).
6. Add frontend liveness assertions for `Addr` across safepoints/calls.
7. Add RS4GC checks for unrematerialized derived addrspace(1) values.
8. Add LLVM IR and OxCaml tests before broad validation.
9. Include focused native tests and native bigarray assembly spot checks for the
   shared Cmm cleanup.

This plan is viable if Step 1 is done first and the RS4GC check fails closed.

## 2026-05-30 Checkpoint

The AFL instrumentation path was a real raw-address misclassification:
`shared_mem[cur_location ^ prev_location]` is a C shared-memory buffer, not an
OCaml heap-derived address. That code now uses `Caddi` for both the byte load
address and byte store address.

The broad temporary `raw_int_addr_base_regs` fallback was removed again. It was
too permissive because it could treat arbitrary integer address arithmetic as a
managed base candidate. After removing it:

- `make install` completed.
- `flambda` passes with
  `llvm-unsafe-no-frontend-alloca-roots=1`, confirming the AFL raw-address case
  is fixed without the broad fallback.
- `typing-layouts-arrays` still fails in the same no-frontend-root mode. That
  means the remaining product-array issue is separate from the AFL/Cadda raw
  address bug and needs its own causality trace.

This supports the stricter rule: fix raw address producers at the source
(`Caddi`/raw arithmetic), and do not paper over raw integer arithmetic in the
LLVM backend by reclassifying it as heap-derived `Addr`.
