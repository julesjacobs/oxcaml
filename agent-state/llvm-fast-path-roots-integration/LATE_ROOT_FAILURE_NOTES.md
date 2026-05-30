# Late Root Failure Notes

Goal: collect concrete failures from removing frontend ordinary root emission,
then turn them into precise requirements for a correct late-root
implementation.

The diagnostic switches used here are deliberately unsafe:

- `llvm-unsafe-no-frontend-alloca-roots=1` removes frontend-preserved alloca
  roots from `gc-live` bundles.
- `llvm-unsafe-no-slow-path-root-slots=1` removes the allocation/poll
  slow-path store/root/reload slots.

These switches are for reducing failures only. They are not intended as a
shipping mode.

## Reproduction Summary

Built diagnostic switches:

```sh
make install
```

with logs in:

- `build_install_late_root_diagnostic_switches_20260529_170328.log`
- `split_late_root_repros_20260529_170646.log`
- `no_frontend_alloca_roots_iarray_dir_20260529_170839.log`

RS4GC boundary reproducer:

```sh
agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/reproduce_rs4gc_boundary.sh
```

This writes:

- `late_root_ir_dumps/scannable4_no_slow_slots/test_gen_u_iarray.rs4gc-before.mir`
- `late_root_ir_dumps/scannable4_no_slow_slots/test_gen_u_iarray.rs4gc-after.mir`
- `late_root_ir_dumps/scannable4_default/test_gen_u_iarray.rs4gc-before.mir`
- `late_root_ir_dumps/scannable4_default/test_gen_u_iarray.rs4gc-after.mir`

The script uses `llc -stop-before=rewrite-statepoints-for-gc` and
`llc -stop-after=rewrite-statepoints-for-gc` on the frontend `.ll`. This is
useful for inspecting RS4GC's behavior on the input root bundles. It is not the
full clang `-O3` boundary; `clang -S -emit-llvm` in this setup has already run
RS4GC.

Split repros:

- `llvm-unsafe-no-frontend-alloca-roots=1`:
  `typing-layouts-iarrays/test_float32_u_iarray.ml` passed by itself, but the
  full `typing-layouts-iarrays` directory still had product-iarray failures.
- `llvm-unsafe-no-slow-path-root-slots=1`:
  `typing-layouts-iarrays/test_scannable_product_iarray_4.ml` fails in the O3
  native case with `Fatal error: allocation failure during minor GC`.
- Both unsafe switches:
  `typing-layouts-iarrays` had 12 failures.

The failures are useful because they split two different root concepts:

- frontend-preserved stack-slot roots,
- current-value slow-path roots for inserted `caml_call_gc` calls.

## Failure Classes

### A. Frontend-Preserved Stack Slot Root Omitted

Status: reproduced with `llvm-unsafe-no-frontend-alloca-roots=1`.

Reproducer:

- Directory run:
  `typing-layouts-iarrays` with
  `TEST_RUN_OCAMLPARAM=_,llvm-backend=1,llvm-path=$LLVM_PATH,llvm-unsafe-no-frontend-alloca-roots=1`.
- Manual reduced failing executable:
  `/tmp/late_roots_no_alloca_product_or_null/test_ignoreable_or_null_product_array.opt`.

Source construct:

- Product iarray tests instantiate `Test_gen_u_iarray.Make_boxed`.
- The relevant wrapper in `test_gen_u_iarray.ml` includes:
  `let init l f = init l (fun i -> of_boxed (f i))`.
- The generated code creates and calls closures that keep functor/module
  environment values live across many safepoints.

Observed failure:

- The executable bus-errors.
- LLDB stops at static data `camlTest_ignoreable_or_null_product_array__s17`
  with `udf #0x5af8`.
- The return address is
  `camlTest_gen_u_iarray__Test_84_84 + 2400`.
- The instruction before the return address is an indirect call:

```asm
ldr x0, [sp, #0x28]
ldr x8, [sp, #0x18]
ldr x1, [x8]
ldr x8, [x1]
blr x8
```

Assembly evidence:

```asm
Ltmp129:
  ldr x0, [sp, #40]
  ldr x8, [sp, #24]     ; stale/unrooted value
  ldr x1, [x8]
  ldr x8, [x1]
  blr x8
Ltmp130:
  cmp x0, #399
```

The value in `[sp,#24]` was created much earlier:

```asm
Ltmp112:
  ldr x8, [sp, #56]
  ldr x0, [sp, #40]
  str x8, [sp, #24]
  ldr x1, [x8]
  ldr x8, [x1]
  blr x8
```

Frame-table evidence:

- Unsafe frame entries around this region include stack roots like
  `40, 48, 80, 72, 64, 56`, but omit offset `24`.
- Default frame entries for the corresponding region include the frontend
  root slots created from the `gc-live` bundle, e.g. offsets like
  `632, 624, 616, 608`, and the temporary current root around the call.

IR evidence:

- In the full optimized unsafe IR, ordinary OCaml heap values in this region
  are direct `ptr addrspace(1)` SSA values. RS4GC sees them and adds them to
  `gc-live`; this part of the SSA shape is correct.
- The bad value is not an unpromoted frontend virtual-register alloca. The bad
  value is a derived object address that has been converted out of managed
  pointer form and then reused after later statepoints.
- The relevant optimized/RS4GC IR shape is:

```llvm
%367 = getelementptr i8, ptr addrspace(1) %365, i64 16
%368 = ptrtoint ptr addrspace(1) %367 to i64
%369 = inttoptr i64 %368 to ptr

%statepoint_token616 = call ... [ "gc-live"(..., ptr addrspace(1) %365, ...) ]
%377 = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %statepoint_token616, ...)

%statepoint_token619 = call ... [ "gc-live"(..., ptr addrspace(1) %377, ...) ]
%388 = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %statepoint_token619, ...)

%statepoint_token622 = call ... [ "gc-live"(..., ptr addrspace(1) %388, ...) ]
%395 = call ptr addrspace(1) @llvm.experimental.gc.relocate(token %statepoint_token622, ...)

%397 = load ptr addrspace(1), ptr %369, align 8
```

  `%365` is correctly relocated through `%377`, `%388`, and `%395`, but `%369`
  is a default-address-space pointer derived from the old `%365`. RS4GC cannot
  rewrite `%369` because it no longer has managed pointer type. The later load
  from `%369` is therefore a stale derived-address use if any intervening
  statepoint moves `%365`.
- Default calls include frontend root allocas too:

```llvm
"gc-live"(ptr %11, ptr %16, ptr %22, ptr %33)
```

and at the indirect call:

```llvm
"gc-live"(ptr %11, ptr %16, ptr %22, ptr %33, ptr %802)
```

Diagnosis:

- This failure is not primarily because ordinary values failed to become
  `ptr addrspace(1)` SSA values. They did become managed SSA values, and RS4GC
  did root and relocate them.
- The actual invariant violation is that a derived address of a managed object
  escaped into `i64`/default `ptr` form and remained live across statepoints.
- The frontend source is the current handling of `Addr` and address arithmetic:
  `backend/llvm/llvm_ir.ml` maps `Cmm.Addr` to `i64`, and
  `backend/llvm/llvmize.ml`'s `do_offset` computes offsets by casting pointer
  inputs to `i64`, doing integer arithmetic, and casting back to the requested
  pointer type. Loads and stores then often request `T.ptr`, so the derived
  address becomes a default-address-space pointer.
- The final `[sp,#24]` spill is the machine manifestation of that IR shape:
  register allocation preserves the old derived-address/base value for the
  later default-pointer use, but the frame table records only the managed roots
  requested by the statepoints.
- This does not look like an LLVM bug. LLVM is not expected to infer that a
  default-address-space pointer produced by `ptrtoint`/`inttoptr` from a managed
  pointer must be relocated. The backend must avoid this IR shape, or must
  explicitly model/recompute derived addresses from relocated bases.

Audit:

- Added diagnostic script:
  `agent-state/llvm-fast-path-roots-integration/audit_derived_default_ptrs.py`.
- Running it on `/tmp/product_or_null_no_alloca.o3.no-rs4gc.ll` found:
  `110 suspicious derived default ptrs in 37 functions`.
- This is intentionally conservative and regex-based, but the hits include the
  concrete crashing sequence above. This pattern is widespread enough that it
  should become an invariant check or compiler-side verifier before late roots
  can be trusted.

### B. Allocation/Poll Slow-Path Current Value Not Rooted

Status: reproduced with `llvm-unsafe-no-slow-path-root-slots=1`.

Reproducer:

- `typing-layouts-iarrays/test_scannable_product_iarray_4.ml`, O3 native case.
- Manual failing executable:
  `/tmp/late_roots_no_slow_slots_scannable4/test_scannable_product_iarray_4.opt`.
- Default comparison:
  `/tmp/late_roots_default_scannable4/test_scannable_product_iarray_4.opt`.

Source construct:

- The test constructs a seven-word product iarray element:

```ocaml
type boxed_t = float * (float * float) * (float * (float * float * float))
type unboxed_t =
  #(float * #(float * float) * #(float * #(float * float * float)))
```

- It instantiates `Test_gen_u_iarray.Make_boxed` and allocates a large module
  value containing many closure fields.

Observed failure:

- Unsafe executable exits with:
  `Fatal error: allocation failure during minor GC`.
- Default executable passes.

Important false lead:

- The first small allocation slow path in `init_48_189` has no explicit
  slow-path root slots in unsafe IR, but LLVM happens to spill the two live
  heap values before the call and the frame table records offsets `8` and `0`.
- That site is not enough to explain the failure.

Concrete bad site:

- Function:
  `camlTest_gen_u_iarray__Make_boxed_44_185_code`.
- Unsafe IR call:
  `/tmp/late_roots_no_slow_slots_scannable4/test_gen_u_iarray.ll:1539`.
- Default IR call:
  `/tmp/late_roots_default_scannable4/test_gen_u_iarray.ll:1609`.

Raw-IR RS4GC boundary:

- Unsafe before RS4GC:
  `late_root_ir_dumps/scannable4_no_slow_slots/test_gen_u_iarray.rs4gc-before.mir:1817`.
- Unsafe after RS4GC:
  `late_root_ir_dumps/scannable4_no_slow_slots/test_gen_u_iarray.rs4gc-after.mir:1937`.
- Default before RS4GC:
  `late_root_ir_dumps/scannable4_default/test_gen_u_iarray.rs4gc-before.mir:1887`.
- Default after RS4GC:
  `late_root_ir_dumps/scannable4_default/test_gen_u_iarray.rs4gc-after.mir:2007`.

The raw-IR boundary result is:

```text
scannable4_no_slow_slots before: roots=24
scannable4_no_slow_slots after:  roots=24
scannable4_default before:       roots=25
scannable4_default after:        roots=25
```

So RS4GC does not invent a slow-path root slot. It preserves the root set it
was given while rewriting the call to `llvm.experimental.gc.statepoint`.

Unsafe IR before the second large allocation slow path:

```llvm
%1199 = extractvalue ... %1196, 1, 0
store ptr addrspace(1) %1199, ptr %4
...
%1200 = load ptr addrspace(1), ptr %4
store ptr addrspace(1) %1200, ptr %204
%1201 = load ptr addrspace(1), ptr %204
store ptr addrspace(1) %1201, ptr %205
...
%1211 = call ... @caml_call_gc(...) [
  "deopt"(...),
  "gc-live"(ptr %8, ptr %22, ptr %30, ..., ptr %202)
]
```

The current value in `%205` is not in `gc-live`.

In the raw-IR RS4GC boundary, the same role is visible as `%204`: the previous
call result is stored there before the allocation slow-path branch, and it is
used after the slow path. The unsafe RS4GC input roots are:

```llvm
"gc-live"(ptr %7, ptr %21, ptr %29, ..., ptr %193, ptr %201)
```

No root corresponding to the current value slot `%204` appears.

Default IR materializes one slow-path root slot per current live value:

```llvm
%1243 = load ptr addrspace(1), ptr %205
store ptr addrspace(1) %1243, ptr %243
%1246 = call ... @caml_call_gc(...) [
  "deopt"(...),
  "gc-live"(ptr %219, ptr %220, ..., ptr %243)
]
...
%1273 = load ptr addrspace(1), ptr %243
store ptr addrspace(1) %1273, ptr %205
```

At the RS4GC boundary, the default build roots the corresponding current value
through `%242`:

```llvm
%1242 = load ptr addrspace(1), ptr %204
store ptr addrspace(1) %1242, ptr %242
%1245 = call ... @caml_call_gc(...) [
  "gc-live"(ptr %218, ptr %219, ..., ptr %242)
]
...
%1272 = load ptr addrspace(1), ptr %242
store ptr addrspace(1) %1272, ptr %204
```

Assembly evidence:

- Unsafe:

```asm
blr x8
Ltmp21:
  sub x27, x27, #248
  ldr x8, [x28]
  cmp x8, x27
  b.hi LBB0_6
LBB0_3:
  ...
  stp x9, x0, [x27, #216]   ; x0 is used after the slow path
...
LBB0_6:
  bl _caml_call_gc
Ltmp23:
  b LBB0_3
```

- Default:

```asm
LBB0_6:
  ...
  ldr x8, [sp, #216]
  str x8, [sp, #24]
  ldr x8, [sp, #208]
  str x8, [sp, #16]
  str x0, [sp, #8]          ; extra current root
  bl _caml_call_gc
Ltmp23:
  ...
  ldr x0, [sp, #8]
  b LBB0_3
```

Frame-table evidence:

- Unsafe `Ltmp23` frame:

```asm
.short 243
.short 24
.short 216
.short 208
...
.short 32
```

- Default `Ltmp23` frame:

```asm
.short 419
.short 25
.short 200
.short 192
...
.short 16
.short 8
```

Diagnosis:

- The omitted value is not merely an old preserved register alloca. It is a
  current value produced by the previous call and still live after the inserted
  allocation slow path.
- In machine code that value is in `x0` at the slow-path branch.
- `caml_call_gc` may move the object. Without a root slot, the GC cannot update
  the value.
- The default frontend slow-path root slot is semantically doing a
  store-root-call-reload around this inserted safepoint.
- A correct late-root implementation must recreate this operation late, at the
  actual inserted safepoint, not rely only on old frontend alloca roots.
- RS4GC is not a pass that invents roots for heap values hidden in memory or
  aggregates. It rewrites the direct pointer root set it is given and the
  direct pointer values its liveness analysis can see.

Optimized-IR detail:

After `clang -O3 -S -emit-llvm`, the module is already statepointed in this
setup. Inspecting
`late_root_ir_dumps/scannable4_no_slow_slots/test_gen_u_iarray.o3.ll` shows a
more precise reason the current value is not inferred:

```llvm
%statepoint_token418 = call ... @llvm.experimental.gc.statepoint.p0(...)
%477 = call { { i64, i64 }, { ptr addrspace(1) } }
    @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token418)
...
%statepoint_token419 = call ... @llvm.experimental.gc.statepoint.p0(
    i64 2031617, ..., @"\01_caml_call_gc", ...)
...
%486 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %477, 1, 0
```

The pointer result from the previous call is hidden inside aggregate `%477`
across the second statepoint. The direct `ptr addrspace(1)` value `%486` is not
created until after the `caml_call_gc` statepoint. RS4GC's liveness computation
is direct SSA-value liveness: it adds operands whose type is a handled GC
pointer type, and it explicitly does not implement first-class aggregate
tracking. An aggregate that contains a GC pointer is therefore not the same
thing as a live direct GC pointer root.

In the default build, the slow-path root slot forces the pointer field out of
the aggregate before the second statepoint:

```llvm
store ptr addrspace(1) %507, ptr %51
%statepoint_token427 = call ... @llvm.experimental.gc.statepoint.p0(
    i64 2031617, ..., @"\01_caml_call_gc", ...)
    [ "gc-live"(..., ptr %51) ]
...
%562 = load ptr addrspace(1), ptr %51
```

That explicit slot is why the frame table contains the current value in the
default build.

Existing LLVM aggregate-splitting mechanisms checked:

- `scalarizer` does not help. It is a vector scalarization pass, not a general
  first-class struct/aggregate return splitter. Running
  `opt -passes=scalarizer` on the failing optimized IR leaves the
  `gc.result` aggregate and the post-`caml_call_gc` `extractvalue` in the same
  shape.
- `sroa` does not help. It scalarizes analyzable `alloca` memory objects; the
  bad value is not an alloca aggregate but a first-class aggregate SSA result
  from `llvm.experimental.gc.result`.
- `instcombine` is not a solution. Its aggregate transforms simplify
  `extractvalue`/`insertvalue` patterns and may move projections toward uses;
  it does not enforce early projection before statepoints.
- CodeGen has generic aggregate ABI lowering/flattening, but that runs after
  RS4GC. It is too late for GC root discovery.

Conclusion: current LLVM does not appear to provide an existing pass that means
"split first-class aggregate SSA values containing GC pointers before each
statepoint so RS4GC can see the pointer fields." This needs either an
OxCaml-specific pre-RS4GC pass, or an extension to RS4GC's liveness/root
discovery to understand first-class aggregate values containing GC pointers.

Safety tripwire:

- Restored recursive struct detection in RS4GC's `containsGCPtrType`.
- Added hidden diagnostic option
  `-mllvm -rs4gc-fail-on-unhandled-gc-aggregate`.
- With the option enabled, RS4GC fails as soon as liveness sees a first-class
  aggregate containing a GC pointer. This turns the silent bad-code class into
  an immediate backend error for experiments.
- The option cannot be enabled globally yet. It also trips in the default rooted
  build on existing aggregate-return values such as:

```llvm
%411 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) }
    @"\01_caml_fresh_oo_id"(...)
```

This shows that aggregate GC values are not rare enough to ban outright in the
current pipeline. The tripwire is useful for diagnosis, but the real fix still
needs to either expose those aggregate pointer fields before RS4GC or teach
RS4GC how to represent them.

## D. Aggregate Projection at Safepoints Experiment

File:

- `late_root_ir_dumps/aggregate_cross_safepoint_experiment.ll`

Command:

```sh
/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/opt \
  -S -passes=rewrite-statepoints-for-gc \
  agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/aggregate_cross_safepoint_experiment.ll \
  -o agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/aggregate_cross_safepoint_experiment.rs4gc.ll
```

Results:

- `baseline_hidden`: aggregate result crosses `@foo`, pointer field is
  extracted only after `@foo`. RS4GC inserts zero relocates for the field.
- `exposed_scalar`: pointer field is extracted before `@foo` and used after.
  RS4GC inserts one `gc.relocate` and rewrites the return to use the relocated
  value.
- `rebuild_after_safepoint`: pointer field is extracted before `@foo`, then
  the aggregate is rebuilt after `@foo` from non-GC fields plus the relocated
  pointer. RS4GC relocates correctly.
- `rebuild_from_original`: rebuilding by inserting the relocated pointer back
  into the original aggregate also works in the one-pointer-field example, but
  this is not a safe general rule unless every GC pointer field in the original
  aggregate is overwritten with its relocated value.
- `rebuild_then_second_safepoint`: after rebuilding, a second `@foo` is placed
  before the next extract. RS4GC does not relocate the rebuilt aggregate's
  hidden pointer at the second statepoint. This recreates the original bug.
- `aggregate_consumer_after_safepoint`: rebuilding for an immediate aggregate
  consumer after the first safepoint codegens cleanly, but if that consumer is
  itself a safepoint-like call, aggregate arguments containing GC pointers are
  another representation that needs explicit handling.

Assembly result:

- `baseline_hidden` keeps the hidden pointer in `x19` across `@foo` with no GC
  root.
- `exposed_scalar`, `rebuild_after_safepoint`, and `rebuild_from_original` all
  spill the pointer to `[sp,#8]` across `@foo`, and the frame record contains
  the root. The rebuild itself disappears for return-only cases.
- `rebuild_then_second_safepoint` reloads the pointer after the first `@foo`
  and keeps it in `x19` across the second `@foo`, showing that rebuilt
  aggregates must not be allowed to cross later safepoints without being
  projected again.

Conclusion:

The viable targeted transform is not "explode every aggregate globally." It is
"for each safepoint, project any GC pointer fields of aggregate values live
across that safepoint into direct SSA values before the safepoint, make those
direct SSA values live across the safepoint, and rewrite post-safepoint uses to
the relocated scalar values." Rebuilding is acceptable only after the safepoint
and only if the rebuilt aggregate does not cross another safepoint without being
handled again.

## E. Split/Rebuild at Every Safepoint Shapes

File:

- `late_root_ir_dumps/aggregate_split_rebuild_shapes.ll`

Commands:

```sh
/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/opt \
  -S -passes=rewrite-statepoints-for-gc \
  agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/aggregate_split_rebuild_shapes.ll \
  -o agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/aggregate_split_rebuild_shapes.rs4gc.ll

/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/llc \
  -O3 \
  agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/aggregate_split_rebuild_shapes.rs4gc.ll \
  -o agent-state/llvm-fast-path-roots-integration/late_root_ir_dumps/aggregate_split_rebuild_shapes.s
```

Shapes tested:

- repeated split/rebuild across two safepoints,
- aggregate with two GC pointer fields,
- nested aggregate with an inner GC pointer field,
- PHI of rebuilt aggregate values,
- immediate aggregate consumer after split/rebuild,
- invoke normal-result aggregate split in the normal destination.

RS4GC results:

- `two_safepoints_rebuild_each_time`: 2 relocates, one for each `@foo`.
- `two_gc_fields`: 3 relocates. Two roots are live across the first `@foo`;
  only the returned field is live across the later `consume_two_scalars`
  statepoint.
- `nested_gc_field`: 1 relocate for the nested pointer field.
- `phi_of_rebuilt_aggregates`: 5 relocates. The synthetic branch shape also
  causes the live input arguments to be relocated across the `choose`
  statepoint.
- `aggregate_consumer_after_split`: 1 relocate across the explicit `@foo`.
- `invoke_normal_result_split`: normal path relocates correctly after the
  invoke result is extracted in the normal destination.

Assembly observations:

- Rebuilds disappear in simple return cases. The generated code keeps one stack
  slot for the GC pointer across one or more safepoints:

```asm
bl  _make_pair
str x1, [sp, #8]
bl  _foo
bl  _foo
ldr x0, [sp, #8]
```

- Two GC fields use two adjacent stack slots across the first safepoint:

```asm
bl  _make_two
stp x2, x0, [sp]
bl  _foo
ldp x1, x0, [sp]
```

- The PHI case did not create obviously duplicated root slots in this synthetic
  example. LLVM simplified the symmetric branch shape to choose the input
  pointer before the aggregate-producing call. There is one redundant
  `ldr`/`str` to the same slot before the second `@foo`, but no stack-slot
  explosion.
- The invoke-normal result shape codegens like the call result shape: extract
  after the invoke's normal edge, spill one root slot across `@foo`, reload.

Open concerns:

- This is not yet a proof for arbitrary aggregate PHI cycles. It shows the
  local split/rebuild rule is accepted by RS4GC and does not obviously confuse
  codegen on representative straight-line, multi-field, nested, branch, and
  invoke-normal shapes.
- Rebuilt aggregate values used as aggregate arguments to a later statepoint
  call are still aggregate GC values at the call boundary. Whether that is
  acceptable depends on the call's GC/root convention and should be handled
  explicitly in the real pass design.

## F. Retest After Aggregate Explosion

Status: `llvm-unsafe-no-slow-path-root-slots=1` now looks viable for focused
broad coverage; `llvm-unsafe-no-frontend-alloca-roots=1` is still unsafe.

The aggregate explosion pass in RS4GC exposes first-class aggregate fields that
contain GC pointers before RS4GC liveness runs. This addresses the previous
`Make_boxed_44_185` no-slow-root-slot failure, where the current value was
hidden inside a `gc.result` aggregate across a later `caml_call_gc`.

Focused coverage with only slow-path root slots omitted:

- `typing-layouts-iarrays`: 81 passed.
- `typing-layouts-arrays`: 134 passed.
- `typing-zero-alloc`: 5 passed.
- `compaction`: 8 passed, 3 skipped.
- `gc-roots`: 10 passed.
- `weak-ephe-final`: 23 passed, 5 skipped.
- `async-exns`: 5 passed.
- `match-exception`: 16 passed.
- `runtime-C-exceptions`: 2 passed.
- `exception-extra-args`: 6 passed.

That does not prove the final implementation, but it is good evidence that the
next incremental step can be: remove or disable allocation/poll slow-path root
slots after aggregate GC fields are exposed before RS4GC.

The remaining failure is different. With
`llvm-unsafe-no-frontend-alloca-roots=1`,
`typing-layouts-iarrays/test_ignoreable_or_null_product_array.ml` still
bus-errors in the `-Oclassic` native run.

Source construct:

```ocaml
let a = A.init 1000 I.of_int in
check_i a;
let a = A.init 1001 I.of_int in
check_i a;
```

The second `a` is produced by `A.init`, then used by `check_i`. The generated
IR stores that heap value in a frontend-preserved alloca and reloads it after
calls that may safepoint.

Unsafe frontend IR around the corresponding region has safepoint calls but no
frontend alloca roots:

```llvm
%3021 = call ... @caml_apply2(...) "statepoint-id"="0"
  [ "deopt"(...) ]
...
store ptr addrspace(1) %3026, ptr %299
...
%3041 = call ... @caml_apply3(...) "statepoint-id"="0"
  [ "deopt"(...) ]
...
store ptr addrspace(1) %3046, ptr %305
...
%3067 = call ... %3066(...) "statepoint-id"="0"
  [ "deopt"(...) ]
...
%3089 = call ... @camlTest_gen_u_iarray__check_i_upto_86_86(...)
  "statepoint-id"="0" [ "deopt"(...) ]
```

The default frontend IR has the same source shape, but roots the live
frontend-preserved slots:

```llvm
%3021 = call ... @caml_apply2(...)
  [ "deopt"(...), "gc-live"(ptr %11, ptr %16, ptr %22, ptr %33) ]
...
%3041 = call ... @caml_apply3(...)
  [ "deopt"(...), "gc-live"(ptr %11, ptr %16, ptr %22, ptr %33, ptr %299) ]
...
%3067 = call ... %3066(...)
  [ "deopt"(...),
    "gc-live"(ptr %11, ptr %16, ptr %22, ptr %33, ptr %299, ptr %305) ]
...
%3089 = call ... @camlTest_gen_u_iarray__check_i_upto_86_86(...)
  [ "deopt"(...),
    "gc-live"(ptr %11, ptr %16, ptr %22, ptr %33, ptr %299, ptr %305) ]
```

Unsafe assembly around the same region:

```asm
bl  _caml_apply2
ldr x8, [sp, #64]
str x8, [sp, #24]
...
str x0, [sp, #48]
blr x8
...
ldr x8, [sp, #48]
ldr x9, [sp, #16]
...
bl  _camlTest_gen_u_iarray__check_i_upto_86_86
...
ldr x8, [sp, #24]
ldr x8, [x8, #16]
ldr x1, [x8]
ldr x8, [x1]
blr x8
```

The frame-table entries around these call sites include offsets such as
`72`, `64`, `56`, and sometimes `48`, but not the small spill slots
`24` or `16`. The default build has frontend alloca root offsets such as
`632`, `624`, `616`, and `608`, plus the temporary result slots. Those are the
semantic roots that keep the same values updated by moving GC.

Exact causality path:

1. The source functor code keeps module/closure environment values and the
   produced iarray value live across calls that may safepoint. The small source
   shape is `let a = A.init ... in check_i a`.
2. The LLVM frontend represents several of those long-lived heap values with
   frontend-preserved `alloca ptr addrspace(1)` slots: persistent environment
   slots such as `%11`, `%16`, `%22`, `%33`, and local slots such as `%299` and
   `%305`.
3. In the default build, each relevant safepoint call carries those slots in a
   `gc-live` bundle. RS4GC and the frame-table emitter therefore know that those
   stack locations are GC roots.
4. In `llvm-unsafe-no-frontend-alloca-roots=1`, those `gc-live` bundle entries
   are removed. The IR still contains ordinary loads from the alloca slots and
   ordinary later uses of the loaded heap pointers.
5. Since the loaded heap pointers are now ordinary values, LLVM codegen is free
   to copy and spill them like normal values. In the unsafe assembly it creates
   folded spill copies at `[sp,#24]` and `[sp,#16]`.
6. The frame table for the unsafe call sites lists only the known roots:
   `80`, `72`, `64`, `56`, and sometimes `48`. It does not list `24` or `16`.
7. If a moving GC runs during one of the intervening calls, it updates only the
   frame-table roots. It cannot update the unlisted folded spill copies at
   `[sp,#24]` and `[sp,#16]`.
8. Later code reloads one of those unlisted copies, for example
   `ldr x8, [sp,#24]`, and uses it as a closure/module receiver to load an
   indirect-call target.
9. In the crash run, that stale receiver leads to a target in static data
   (`camlTest_ignoreable_or_null_product_array__s17`) instead of executable
   code, so `blr x8` jumps into data and hits the bus error.

The important detail is that this is not just "the array `a` was not rooted".
There can be a rooted copy elsewhere, such as `[sp,#64]` in the unsafe assembly.
The bug is that the generated code later uses an unrooted duplicate. A moving GC
updates the rooted copy but not the duplicate, so duplicate untracked heap
pointers across safepoints are unsound.

Where the missing root contract is introduced:

- `backend/llvm/llvmize.ml:4404` computes `preserved_reg_slots` from Cfg
  liveness. These are the Cfg regs that the frontend knows may contain OCaml
  heap values across safepoints or trap edges.
- `backend/llvm/llvmize.ml:4614` emits allocas for Cfg regs because the input
  Cfg is not always SSA. These allocas are ordinary LLVM memory unless later
  marked as GC roots.
- `backend/llvm/llvmize.ml:801` turns live preserved Cfg regs into
  `"gc-live"` bundle operands by passing the corresponding allocas to LLVM.
- `backend/llvm/llvmize.ml:907` combines the call's `deopt` bundle with the
  frontend alloca-root bundle. The unsafe flag
  `llvm-unsafe-no-frontend-alloca-roots=1` removes exactly this link.

Final IR shape before RS4GC:

- The raw `llc -stop-before=rewrite-statepoints-for-gc` boundary still has the
  ordinary alloca/load/store graph and the calls still have statepoint/debug
  metadata, but the ordinary calls have only `deopt` bundles:

```llvm
%3020 = call ... @"\01_caml_apply2"(...)
  [ "deopt"(...) ]
...
%3040 = call ... @"\01_caml_apply3"(...)
  [ "deopt"(...) ]
...
%3066 = call ... %3065(...)
  [ "deopt"(...) ]
...
%3088 = call ... @"\01_camlTest_gen_u_iarray__check_i_upto_86_86"(...)
  [ "deopt"(...) ]
```

- The default pre-RS4GC IR at the same call sites has the missing semantic root
  contract:

```llvm
%3020 = call ... @"\01_caml_apply2"(...)
  [ "deopt"(...), "gc-live"(ptr %10, ptr %15, ptr %21, ptr %32) ]
...
%3040 = call ... @"\01_caml_apply3"(...)
  [ "deopt"(...), "gc-live"(ptr %10, ptr %15, ptr %21, ptr %32, ptr %298) ]
...
%3066 = call ... %3065(...)
  [ "deopt"(...),
    "gc-live"(ptr %10, ptr %15, ptr %21, ptr %32, ptr %298, ptr %304) ]
...
%3088 = call ... @"\01_camlTest_gen_u_iarray__check_i_upto_86_86"(...)
  [ "deopt"(...),
    "gc-live"(ptr %10, ptr %15, ptr %21, ptr %32, ptr %298, ptr %304) ]
```

The stale `[sp,#24]` and `[sp,#16]` stack slots are introduced later by machine
codegen as ordinary spills/copies. They are not explicit semantic root slots in
the raw pre-RS4GC IR.

Important correction after checking the optimized boundary:

- The raw `llc` boundary is not the same as the actual optimized clang path.
  It skips the normal `-O3` scalar pipeline.
- Running `opt -O3` on the unsafe IR for
  `_camlTest_gen_u_iarray__Test_84_84` shows that the ordinary long-lived
  OCaml values in the hot region do become direct `ptr addrspace(1)` SSA
  values before RS4GC.
- The remaining allocas in that optimized IR are the explicit slow-path root
  slots around `caml_call_gc`, which are intended physical roots.
- Running RS4GC on that optimized IR does add direct SSA roots and relocates
  for the ordinary calls, for example the `caml_apply2`, indirect call, and
  `check_i_upto` calls in the line-160/line-163 region.

So this specific repro should not be described as "ordinary values failed to
become SSA before RS4GC". At the optimized IR boundary they did become SSA.
The remaining bug is later in the lowering/codegen path: the final assembly
contains duplicate machine spill slots for statepoint-live roots, but the frame
table does not list those duplicate slots.

Example final assembly from the unsafe build:

```asm
Ltmp81:
  ldr x8, [sp, #64]
  str x8, [sp, #24]   ; duplicate spill, not listed in the frame table
  ...
  blr x8
Ltmp82:
  ldr x9, [sp, #64]
  str x9, [sp, #16]   ; duplicate spill, not listed in the frame table
  ...
  bl _camlTest_gen_u_iarray__check_i_upto_86_86
...
Ltmp84:
  ldr x8, [sp, #24]   ; reloads the unlisted duplicate
  ...
  blr x8
Ltmp85:
  ldr x9, [sp, #16]   ; reloads the unlisted duplicate
  ...
  bl _camlTest_gen_u_iarray__check_i_upto_86_86
```

The frame-table entries for these call sites list offsets such as `80`, `72`,
`64`, `56`, and sometimes `48`, but not `24` or `16`. If a moving GC runs at
one of the intervening statepoints, it can update the recorded copy at `64`
while the duplicate at `24` or `16` remains stale.

Can we fix it back up at the RS4GC boundary?

- For this repro, the optimized IR boundary already has the normal values in
  the representation we want: direct `ptr addrspace(1)` SSA.
- That means a fail-closed verifier before RS4GC is still useful, but it would
  not catch this final assembly bug.
- The next check needs to be after RS4GC and during machine lowering: every
  post-RS4GC machine spill/reload of a statepoint-live GC value must either be
  the statepoint-owned/root-recorded location, or must not be live across a
  later statepoint.
- If machine codegen can introduce an unrecorded duplicate and then use it
  after an intervening statepoint, then the unsafe late-root path is not sound
  even when the optimized IR is SSA-first.

Aggregate explosion does not help here. The missing value is not a first-class
aggregate field anymore; it is an ordinary GC pointer hidden in
frontend-preserved memory, then copied into normal machine spills. RS4GC can
discover direct typed SSA `ptr addrspace(1)` values, but it does not infer that
an arbitrary alloca slot containing a GC pointer is a semantic root unless that
slot is passed in a root bundle or otherwise marked.

Conclusion: removing all frontend alloca root bundles is still a correctness
bug. A proper late-root implementation must either preserve enough abstract
root information through optimization or materialize the semantic root slots
late before statepoint/frame-table lowering. It must also ensure later uses are
rewritten to the relocated/rooted value, not to stale duplicate copies.

Viable paths from here:

- Incremental safe path: keep frontend alloca roots, remove slow-path root
  slots once aggregate explosion is enabled and tested more broadly.
- Proper late-root path: make ordinary frontend-preserved roots visible late
  before RS4GC. That likely means either eliminating those allocas into typed
  SSA before statepoint rewriting, or adding an explicit abstract root marker
  that survives optimization and is lowered late to root slots/root operands.
- Safety path: if the aggressive no-frontend-root mode remains as a diagnostic,
  add a fail-closed check for `ptr addrspace(1)` allocas or frontend root slots
  live across safepoints without a corresponding root representation.

### C. Early Physical Root Slots Are Not Optimizer-Stable

Status: previously observed in LLVM IR prototypes and confirmed by the LLVM
expert advice in `ADVICE.md`.

Shape:

```llvm
store ptr addrspace(1) %obj, ptr %exnroot
invoke statepoint(...) [ "gc-live"(ptr %exnroot) ] unwind label %recover
recover:
  %for_handler = load ptr addrspace(1), ptr %exnroot
```

Problem:

- This is intended to mean that GC may update `*%exnroot` during the
  statepoint.
- If this is fed through ordinary scalar optimization, LLVM can reason about
  the alloca/store/load as normal memory and turn the handler value back into
  SSA/PHI-like values.
- That recreates the exact thing we are trying to avoid on exceptional edges:
  handler-live GC values that are not represented by a stable root slot.

Diagnosis:

- Physical root slots should be a late codegen representation.
- They should be materialized after the scalar optimizer has already run.
- After materialization, do not run SROA/mem2reg/GVN/DSE/InstCombine-like
  scalar cleanup over the physical root-slot form.

## Requirements Emerging So Far

1. Late roots must account for GC values live only through memory, not just
   typed SSA values.
2. Late roots must distinguish a preserved register alloca from a current-value
   slow-path root slot. They are not semantically interchangeable.
3. Late roots must also distinguish normal-path roots from exceptional-handler
   roots. Shared trap handlers cannot use exceptional `gc.relocate` from a
   multi-predecessor landingpad.
4. Removing frontend roots is safe only after a pass can either:
   - prove that every live value is visible as a typed SSA value at every
     relevant safepoint, or
   - materialize and list an explicit root slot at that safepoint.
5. Slow-path `caml_call_gc` lowering needs late current-value root slots around
   the inserted call. These slots must cover values that are live after the
   slow path, including values currently sitting only in registers.
6. Frontend-preserved stack-slot roots need either:
   - replacement by optimizer-visible SSA values before RS4GC, or
   - a late pass that can prove the slot contains a GC value and list it in the
     safepoint root set.
7. Physical root slots should be introduced late. They should not be exposed to
   ordinary scalar optimization as if they were normal allocas.
8. Any unsupported hidden-root shape must fail closed.

## Addr as addrspace(1) GEP experiment

Experiment file:

- `agent-state/llvm-fast-path-roots-integration/addr_gep_statepoint_experiment.ll`

Question:

- Can `Cmm.Addr` be represented as a `ptr addrspace(1)` GEP instead of
  lowering through `ptrtoint` / integer arithmetic / default-address-space
  `inttoptr`?

Result:

- For simple field addresses, yes. RS4GC sees the derived address, keeps the
  base object live, relocates the base, and rematerializes the GEP after the
  statepoint:

```llvm
%field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 24
call @may_gc() "statepoint-id"="0"
%field = load ptr addrspace(1), ptr addrspace(1) %field.addr
```

becomes:

```llvm
%statepoint_token = call ... [ "gc-live"(ptr addrspace(1) %obj) ]
%obj.relocated = call ptr addrspace(1) @llvm.experimental.gc.relocate(...)
%field.addr.remat = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 24
%field = load ptr addrspace(1), ptr addrspace(1) %field.addr.remat
```

- This also works with dynamic offsets and with the optimized form of a simple
  loop where the address is always recomputable from the current base.
- The current vendored RS4GC already has a recovery pass,
  `canonicalizeOxCamlRawHeapMemoryAddresses`, that recognizes some existing
  raw default-address-space heap memory operands and rewrites them back into
  addrspace(1) GEPs before liveness. The test
  `vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-raw-heap-addresses.ll`
  covers this.

Critical caveat:

- A derived address that is itself loop-carried and incremented across a
  safepoint is still unsafe in the current representation:

```llvm
%addr = phi ptr addrspace(1) [ %begin, %entry ], [ %addr.next, %loop ]
%field = load ptr addrspace(1), ptr addrspace(1) %addr
call @may_gc() "statepoint-id"="0"
%addr.next = getelementptr i8, ptr addrspace(1) %addr, i64 8
```

RS4GC currently emits:

```llvm
"gc-live"(ptr addrspace(1) %.0, ptr addrspace(1) %addr, ptr addrspace(1) %field)
%obj.relocated = gc.relocate(... %.0 ...)
%addr.relocated = gc.relocate(... %.0, %addr ...)
%addr.next = getelementptr i8, ptr addrspace(1) %addr.relocated, i64 8
```

Codegen then spills and reloads `%addr` across the statepoint. OxCaml frame
table emission records the base object as the GC root for base/derived pairs,
not the interior address. That means the reloaded derived address can be stale
after a moving GC.

Conclusion:

- Modeling `Addr` as addrspace(1) GEPs is directionally right because it
  preserves the base/derived relation for LLVM.
- It is not sufficient by itself. We also need an invariant or pass that
  prevents live derived `Addr` values from being relocated as independent
  post-statepoint values.
- Good generated IR should either:
  - keep only the base object plus a non-GC offset live across the safepoint,
    then rebuild the GEP after relocating the base, or
  - fail closed when a derived addrspace(1) value is live across a safepoint
    and cannot be rematerialized from a relocated base plus ordinary offset.

Follow-up test: does `-O2` introduce pointer IVs from base-plus-index?

- Experiment file:
  `agent-state/llvm-fast-path-roots-integration/addrspace1_o2_pointer_iv_experiment.ll`
- Tested shapes:
  - byte offset loop: `gep i8, ptr addrspace(1) %base, (%i << 3)`
  - same with a statepoint candidate in the loop body
  - typed GEP index loop
  - sum loops without opaque calls
  - address-space 0 comparison loops
- Commands:
  - `opt -S -O2`
  - `opt -S -passes='default<O3>'`
  - `opt -S -passes='loop-reduce'`
  - `opt -S -passes='default<O2>,loop-reduce'`

Result:

- In these tests, LLVM did not introduce a loop-carried `phi ptr
  addrspace(1)`.
- It also did not introduce a loop-carried `phi ptr` for the analogous
  address-space 0 tests.
- The optimized IR kept the integer induction variable and recomputed the GEP
  from `%base` and `%i` in the loop.

Interpretation:

- I do not currently have evidence that the standard `-O2` IR optimizer creates
  the bad pointer-IV shape from clean base-plus-offset addrspace(1) input.
- The bad shape is still a real input shape RS4GC may see if the frontend emits
  or preserves a loop-carried `Addr`, or if a later/custom pass creates one.
- Therefore the frontend/lowering should still prefer base-plus-offset form,
  and RS4GC should still fail closed or repair if a derived addrspace(1) value
  is live across a safepoint.

Required changes for an `Addr` as addrspace(1) GEP prototype:

1. Clean up `Addr` meaning before changing LLVM types.
   - `Cmm.Addr` is documented as a derived pointer into the OCaml heap.
   - Some current Cmm helper code uses `Addr` for raw/native addresses. In
     particular, `bigarray_load` and `bigarray_store` compute addresses from
     `ba_data_p`, which is loaded as `Word_int`; those should not become
     managed addrspace(1) values.
   - The bigarray sites should use `array_indexing ~typ:Int` and plain integer
     address arithmetic for the complex second half offset.

2. Change the LLVM type mapping.
   - In `backend/llvm/llvm_ir.ml`, map `Addr` to `ptr addrspace(1)` instead of
     `i64`.
   - This will make register allocas and SSA temporaries for heap-derived
     addresses pointer-typed.

3. Stop using generic default-pointer casts for heap memory addresses.
   - `cast_to_ptr` currently converts integers to default `ptr`.
   - `load_address_from_reg` currently casts every address base to default
     `ptr` and returns default `ptr`.
   - Replace this with address-space-aware helpers:
     - `Val` / `Addr` base: load as `ptr addrspace(1)` and GEP in addrspace(1).
     - `Int` base: load as `i64`, convert to default `ptr`, and GEP in
       address-space 0.

4. Make `Cadda` / selected `Iadd` with `Addr` result preserve addrspace(1).
   - The current `do_gep` is close, but it starts with `cast_to_ptr`, which
     means default `ptr`.
   - It should require a `Val` or `Addr` base and emit a GEP whose result is
     `ptr addrspace(1)`.
   - If the base is `Int`, that should fail for `Addr` result after raw-address
     call sites have been fixed.

5. Audit helper code that casts OCaml values to default `ptr`.
   - Examples: string length/compare helper code currently does `cast t s T.ptr`
     before heap loads.
   - Those loads should use addrspace(1) heap addresses where possible, so the
     base/derived relation remains visible to RS4GC.
   - Runtime stack/domain/trap pointers should stay default `ptr` or integer.

6. Keep existing `Addr` rejection at calls and strengthen live-across checks.
   - Calls already reject `Addr` arguments.
   - Add a liveness-based assertion for any `Addr` register live across a
     safepoint/call boundary in the LLVM path. This encodes the Cmm invariant
     directly before RS4GC has to recover from bad IR.

7. Add RS4GC fail-closed diagnostics.
   - Accept simple derived addrspace(1) values that RS4GC rematerializes from a
     relocated base.
   - Reject any derived addrspace(1) value that remains in the explicit
     statepoint live set as an independent value unless we have implemented a
     base-plus-offset repair.
   - This should catch loop-carried or otherwise obscured `Addr` values.

8. Tests:
   - LLVM IR: simple addrspace(1) GEP across a safepoint rematerializes from
     relocated base.
   - LLVM IR: dynamic offset rematerializes from relocated base.
   - LLVM IR: loop-carried derived pointer is rejected or repaired.
   - OxCaml source: heap field/string/array access across allocation/poll.
   - OxCaml source: bigarray load/store still lowers through raw/default
     address arithmetic and does not become a GC root.

## Current Working Model

The intended end state is still late roots, but not the naive version.

Naive version that failed:

- remove frontend root bundles,
- trust RS4GC to infer everything from typed SSA.

Why it failed:

- some real OCaml roots are hidden in memory slots,
- allocation slow paths need current-value root slots at the inserted
  `caml_call_gc`,
- early physical root slots can be optimized back into unsafe SSA shapes.

More plausible late-root design:

1. Keep the optimized abstract representation as long as possible.
2. Make hidden GC values explicit before or during late statepoint/root
   materialization.
3. Materialize slow-path current-value root slots late at each inserted
   `caml_call_gc`.
4. Materialize exceptional-handler root environments late for values live into
   shared trap handlers.
5. Feed physical root slots directly into statepoint/codegen, with only
   root-aware cleanup afterward.
6. Add verifier-style checks that reject any addrspace(1) value live across a
   safepoint that is neither relocated nor represented by an explicit root.
