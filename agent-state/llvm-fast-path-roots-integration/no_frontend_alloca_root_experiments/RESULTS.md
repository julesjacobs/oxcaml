# No-Frontend Alloca Root Experiment Results

## Summary

These experiments validate the main design point in
`NO_FRONTEND_ALLOCA_ROOTS_PLAN.md`: frontend register allocas cannot be treated
as both scalar-promotion artifacts and GC root slots.

## Experiment 1: `gc-live(ptr %slot)` Blocks Mem2Reg

Input:

- `mem2reg_promotion_comparison.ll`

Command:

```sh
/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/opt \
  -S -passes=mem2reg \
  agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/mem2reg_promotion_comparison.ll \
  -o agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/mem2reg_promotion_comparison.after.ll
```

Result:

- `plain_alloca_promotes` loses its alloca and returns `%obj` directly.
- `gc_live_alloca_does_not_promote` keeps `%slot = alloca ptr addrspace(1)`
  because the slot address appears in `"gc-live"(ptr %slot)`.

Conclusion:

`gc-live(ptr %slot)` is enough to make a frontend register alloca
non-promotable. This is normal LLVM behavior, not an optimizer miss.

## Experiment 2: RS4GC Finds Typed SSA Values Without Frontend Slots

Input:

- `ssa_statepoint_without_frontend_slot.ll`

Command:

```sh
/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/opt \
  -S -passes=rewrite-statepoints-for-gc,verify \
  agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/ssa_statepoint_without_frontend_slot.ll \
  -o agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/ssa_statepoint_without_frontend_slot.rs4gc.ll
```

Result:

RS4GC rewrites the call to:

```llvm
%statepoint_token = call ... @llvm.experimental.gc.statepoint.p0(...)
  [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
%obj.relocated = call coldcc ptr addrspace(1)
  @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
ret ptr addrspace(1) %obj.relocated
```

Conclusion:

For a visible typed `ptr addrspace(1)` SSA value, RS4GC can provide the desired
root and relocate behavior without a frontend alloca root.

## Experiment 3: Product-Array Function Inventory

Script:

- `inventory_alloca_roots.py`

Target function:

- `_camlTest_gen_u_array__Test_128_361_code`

Raw generated IR:

```text
ptr addrspace(1) allocas: 1688
gc-live bundle lines: 540
ptr addrspace(1) allocas directly in gc-live: 139
%21 alloca line: 13330
gc-live bundles mentioning %21: 3
```

The three raw `%21` root bundles are primitive C calls:

- `caml_array_make`
- `caml_equal`
- `caml_array_make`

Optimized unsafe IR:

```text
ptr addrspace(1) allocas: 139
gc-live bundle lines: 568
ptr addrspace(1) allocas directly in gc-live: 139
%21 alloca line: 5778
gc-live bundles mentioning %21: 1
```

Conclusion:

The target optimized function has exactly the bad mixed shape: every surviving
`ptr addrspace(1)` alloca is directly in a `gc-live` bundle. `%21` is pinned by
that mechanism, so relying on mem2reg to remove it is not a valid design.

## Experiment 4: Derived Default Pointer Gate

Command:

```sh
python3 agent-state/llvm-fast-path-roots-integration/audit_derived_default_ptrs.py \
  agent-state/llvm-fast-path-roots-integration/product_array4_after_addr_extcall_patch/test_gen_u_array.opt.unsafeno.ll
```

Result:

```text
summary: 0 suspicious derived default ptrs in 0 functions
```

Conclusion:

For this focused product-array reproducer, the remaining no-frontend-root
failure is not explained by hidden default-address-space heap-derived pointers.
That makes it a good reproducer for the frontend-register-slot/root-slot
boundary problem.

## Design Impact

The experiments support the first implementation patch proposed in
`NO_FRONTEND_ALLOCA_ROOTS_PLAN.md`:

1. classify frontend register slots separately from root slots;
2. reject frontend register slots in `gc-live` under no-frontend-root mode;
3. keep RS4GC last-chance promotion for visible typed GC pointer allocas;
4. remove primitive-call and slow-path frontend roots only after verifier
   coverage is in place.
