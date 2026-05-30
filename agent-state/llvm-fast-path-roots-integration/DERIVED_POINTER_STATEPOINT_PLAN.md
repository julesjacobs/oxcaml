# Derived Pointer Statepoint Plan

## Goal

Make the OxCaml LLVM backend safe and efficient when an OCaml heap-derived
address is live near a safepoint.

The desired rule is:

- GC roots are OCaml value pointers.
- Heap-derived addresses may be used for memory operations.
- Heap-derived addresses must not be treated as independent GC roots.
- If a heap-derived address is needed after a safepoint, reconstruct it from
  the relocated OCaml value pointer and ordinary non-GC offset information.

This plan is for the remaining cases after `Addr` is represented as
`ptr addrspace(1)` GEPs.

## Terms

- **OCaml value pointer**: a `ptr addrspace(1)` that points to the start of an
  OCaml heap value. This is a valid GC root.
- **Heap-derived address**: a `ptr addrspace(1)` computed from an OCaml value
  pointer plus an offset. This is not a valid independent GC root.
- **Base pointer**: the OCaml value pointer associated with a heap-derived
  address.
- **Normal continuation**: the non-exceptional successor of a statepoint call
  or invoke.
- **Handler continuation**: the exceptional successor of an invoke.

Important distinction: `ptr addrspace(1)` is the managed pointer type, but type
alone does not prove a value is an OCaml value pointer. A `ptr addrspace(1)` can
also be a heap-derived address. Root legality depends on the base-pointer proof,
not only on the LLVM type.

## Current Findings

RS4GC already computes and materializes base pointers. Its `findBasePointers`
logic can insert PHIs/selects so a base pointer is available at a statepoint.

That is necessary but not sufficient for OxCaml. Stock statepoint lowering can
still create a relocation pair for the derived pointer itself. That means the
derived pointer becomes an independent relocated value, which is not an OCaml
GC-safe representation.

The current OxCaml experiment
`-rs4gc-remat-addrspace1-derived-from-base-at-alloc` handles only simple
normal-call derived chains. It currently does not handle:

- PHI/select values whose incoming values are rematerializable derived values.
- Invoke statepoints.
- Handler-live derived addresses.

## Actual IR Inventory

This inventory was collected from generated `.ll` files by running clang with:

```text
-mllvm -rs4gc-remat-addrspace1-derived-from-base-at-alloc
-mllvm -rs4gc-debug-oxcaml-derived-remat
-mllvm -rs4gc-fail-on-oxcaml-derived-relocates=false
```

### Current Strongest-Mode Failing Test

The focused test is
`typing-layouts-iarrays/test_ignoreable_or_null_product_array.ml` with:

```text
llvm-unsafe-no-frontend-alloca-roots=1
llvm-unsafe-no-slow-path-root-slots=1
```

The latest generated failing files show one dominant shape:

```llvm
%derived = getelementptr i8, ptr addrspace(1) %base, i64 C
%base    = inttoptr i64 %base_int to ptr addrspace(1)
%sp      = invoke ... to label %normal unwind label %handler
```

Observed examples:

- `gen_u_iarray.ll`: `%100 = getelementptr i8, ptr addrspace(1) %95, i64 32`
  across an invoke of `camlGen_u_iarray__trickledown_39_39`.
- `test_ignoreable_or_null_product_array.ll`: `%169 = getelementptr i8, ptr
  addrspace(1) %168, i64 -4` across an invoke of `caml_apply2`.

In the same files, the same `getelementptr/inttoptr` kind is accepted across
normal calls:

- `gen_u_iarray.ll`: 24 accepted normal-call rematerialization candidates, 20
  rejected invoke candidates.
- `test_ignoreable_or_null_product_array.ll`: 4 accepted normal-call
  rematerialization candidates, 2 rejected invoke candidates.

This says the immediate current blocker is not general GEP rematerialization.
It is invoke support for simple constant-offset GEPs from bases that RS4GC
already knows.

### Older Saved Late-Root Dump

The older dump
`late_root_ir_dumps/product_or_null_no_alloca/test_gen_u_iarray.ll` shows a
different shape:

```llvm
%derived = phi ptr addrspace(1) [ %a, %pred1 ], [ %b, %pred2 ]
%base    = phi ptr addrspace(1) [ %a_base, %pred1 ], [ %b_base, %pred2 ]
```

Observed example:

```llvm
%.1.lcssa = phi ptr addrspace(1) [ %.0144, %L3932.preheader ],
                                  [ %.2, %L3934.loopexit ]
%.1.lcssa.base = phi ptr addrspace(1) [ %.0144, %L3932.preheader ],
                                       [ %.2.base, %L3934.loopexit ],
                                       !is_base_value
```

This shape is rejected as `not-chain` because it is not a single instruction
chain from derived to base. It is not the current first blocker after the
Addr-as-GEP work, but it remains a plausible later blocker for the stronger
late-root path.

### Clean Files in the Same Inventory

The following already-generated files did not report unhandled derived
relocates under the same clang check:

- `typing-layouts-iarrays/gen_product_iarray_helpers.ll`
- `typing-layouts-iarrays/gen_u_iarray.ll`
- `typing-layouts-iarrays/test_gen_u_iarray.ll`
- `typing-layouts-iarrays/test_scannable_product_iarray_3.ll`
- `typing-layouts-iarrays/test_scannable_product_iarray_4.ll`
- `compaction/test_compact.ll`
- `weak-ephe-final/weaktest.ll`

This narrows the immediate work: handle simple invoke GEPs first, then retest
to see which shape appears next.

## Information-Gathering Checklist

The current inventory identified the failing value shape, but did not by
itself prove that the first implementation step was safe for the actual
generated IR. Before changing RS4GC invoke rematerialization, these were the
facts to collect from the two current failures:

1. **Use topology for the derived value**: for `%100` in `gen_u_iarray.ll` and
   `%169` in `test_ignoreable_or_null_product_array.ll`, list every use after
   the invoke and classify it as normal-continuation use, handler-continuation
   use, or pre-statepoint use. The first implementation may only accept the
   case if all post-statepoint uses are normal-continuation uses.
2. **Normal destination shape**: inspect whether the invoke normal destination
   contains PHIs, whether RS4GC splits the normal edge, and where normal
   `gc.relocate` values are inserted. The rebuild must be inserted at a point
   that actually executes on the normal edge and after the relocated base is
   available.
3. **Base provenance**: confirm that `%95` and `%168` are OCaml value pointers,
   not heap-derived addresses that merely have `ptr addrspace(1)` type. Current
   examples use `inttoptr` from allocation-pointer arithmetic, which is expected
   for newly allocated OCaml values, but the pass must not bless arbitrary
   `inttoptr` values as bases.
4. **Stackmap effect**: for an accepted reduced invoke-GEP case, check the
   generated stackmap-facing output. The derived pointer must not be recorded
   as an independent root; only the base root should be relocated, and the
   derived address should be rebuilt from the relocated base.
5. **Negative handler case**: add a reduced IR test where the same simple GEP is
   live into the handler. It should still fail closed until handler
   base-plus-offset environment lowering exists.

Status after the follow-up investigation: items 1, 2, and the IR-level part of
3 are answered for the inspected generated IR. Item 4 remains an implementation
validation requirement because the ad hoc `opt -> llc` path hit an OxCaml GC
printer assertion. Item 5 remains a required LLVM IR regression test.

Useful ways to gather any additional cases:

- dump optimized IR immediately before RS4GC for the focused failing functions;
- add temporary RS4GC debug printing for the candidate derived value, its base,
  its users, and the selected rebuild insertion point;
- reduce `%100` and `%169` to standalone LLVM IR tests before broadening the
  pass.

If a future derived GEP is handler-live, Milestone 1 must not be expanded to
accept it. Such a case should move to either avoiding handler-live derived
values or implementing handler base-plus-offset environment state.

## Gathered Implementation Facts

The requested information was gathered in:

```text
agent-state/llvm-fast-path-roots-integration/derived_invoke_info/
```

The main optimized-before-RS4GC files are:

- `gen_u_iarray.optO3.ll`
- `test_ignoreable.optO3.ll`

The main explicit-statepoint outputs are:

- `gen_u_iarray.optO3.rs4gc.ll`
- `test_ignoreable.optO3.rs4gc.ll`

### Use Topology

In `test_ignoreable.optO3.ll`, the inspected failing `caml_apply2` invoke has
a shared handler block and normal-continuation GEP uses:

```llvm
%143 = getelementptr i8, ptr addrspace(1) %142, i64 -4
%148 = getelementptr i8, ptr addrspace(1) %147, i64 -4
%151 = invoke ... @"\01_caml_apply2"(...)
        to label %L12158 unwind label %L12153
```

The normal continuation later loads through these GEPs. The handler recomputes
its own addresses from recovered handler state. This means the inspected
failing GEPs are normal-continuation values, not handler-live values.

In `gen_u_iarray.optO3.ll`, the relevant first-order shape is the same: field
addresses are constant-offset GEPs from an allocation-derived OCaml value
pointer, then normal continuations use them. The handler uses explicit
exception-root state for the OCaml value pointer, not the field address itself.

### Normal Destination Shape

The inspected normal destinations do not have PHIs before the RS4GC insertion
point. The existing statepoint lowering already inserts normal `gc.relocate`
values at the beginning of the normal destination. That is also where a rebuilt
GEP can be inserted once its relocated base exists.

This is important because the runtime exception path skips ordinary edge code.
For the inspected cases, rematerialization only needs to execute on the normal
continuation.

### Existing LLVM Hook

RS4GC already has generic invoke rematerialization support in
`rematerializeLiveValues`. For an `InvokeInst` with
`Info.UsesExplicitExceptionRoots`, it rematerializes in the normal destination
and does not rematerialize in the unwind destination.

The current OxCaml-specific derived-pointer experiment rejects every invoke
before it can use that model. The immediate implementation should therefore
reuse the existing explicit-exception-root invoke rematerialization path rather
than inventing a new control-flow mechanism.

### Bad Output Shape Today

With the fail-closed diagnostic disabled, `test_ignoreable.optO3.rs4gc.ll`
shows the bug directly:

```llvm
%statepoint_token20 = invoke ... [
  "gc-live"(ptr addrspace(1) %163, ptr addrspace(1) %158,
            ..., ptr addrspace(1) %162, ptr addrspace(1) %157, ...)
]

%167 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(
  token %statepoint_token20, i32 5, i32 0) ; (%162, %163)
%168 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(
  token %statepoint_token20, i32 6, i32 1) ; (%157, %158)
```

Here `%163` and `%158` are derived GEPs, while `%162` and `%157` are their
bases. The derived GEPs are still in `gc-live` and get independent relocates.
That is exactly what the OxCaml diagnostic must keep rejecting.

`gen_u_iarray.optO3.rs4gc.ll` also shows rematerialized GEPs named
`%.rematNN` later becoming live across another invoke and then being relocated
independently. Therefore the implementation must be stable across repeated
statepoints: rebuilding a GEP after one statepoint must not make it an
independent root at the next statepoint.

Disabling the separate use-rematerialization option with
`-rs4gc-remat-derived-at-uses=false` did not reduce the invoke rejection counts
in either file. The current problem is not caused only by that option.

### Base Provenance

The inspected `gen_u_iarray` base is from allocation-pointer arithmetic and is
the newly allocated OCaml block value pointer. Rebuilding field GEPs from the
relocated base is the intended representation.

The inspected `test_ignoreable` bases are more subtle. They come through
integer arithmetic and `inttoptr` into `ptr addrspace(1)` before the GEP. The
chosen base is still what RS4GC's base analysis would relocate, but the pass
must not globally treat arbitrary `inttoptr` values as legal OCaml value
pointers. The accepted pattern should be restricted to cases where RS4GC has
already selected a managed base and the derived expression is a simple
constant-offset chain from that base.

### Stackmap Check

Standalone `llc` on the manually rewritten RS4GC output hit an OxCaml GC
printer assertion:

```text
LLVM ERROR: [OxCamlGCPrinter] frame size has bottom bits set: 282
```

This prevented a clean manual stackmap check through the ad hoc
`opt -> llc` path. The IR-level failure is still conclusive: derived GEPs are
being emitted in `gc-live` and independently relocated when the diagnostic is
disabled. Stackmap-facing validation should be done through focused LLVM tests
and the normal compiler/codegen path after the implementation is changed.

### Conclusion From Gathering

The first implementation step remains viable:

- accept only constant-offset GEP chains from the RS4GC-selected base;
- accept invoke cases only when explicit exception roots are active;
- rematerialize only on the normal continuation;
- keep handler-live derived addresses failing closed;
- add a regression that proves no derived GEP appears in `gc-live` or as an
  independent `gc.relocate`;
- include repeated-statepoint coverage, because `gen_u_iarray.optO3.rs4gc.ll`
  shows a GEP rebuilt after one statepoint can become live across a later
  invoke.

## Required Invariants

1. At a statepoint, every listed GC root must be an OCaml value pointer or an
   explicit root slot that contains an OCaml value pointer.
2. A heap-derived address must not appear as an independent `gc.relocate`
   derived pointer in OxCaml output.
3. A heap-derived address used after a normal continuation must be rebuilt from
   the relocated base pointer.
4. A heap-derived address used by an exception handler must be represented as
   handler environment state: relocated base pointer plus non-GC offset.
5. If the pass cannot prove one of those representations, it must fail closed.
6. Any offset used to rebuild a heap-derived address must be ordinary non-GC
   data. It must not be a hidden absolute heap address or an integerized GC
   pointer.
7. After a statepoint, no use may continue to read the pre-statepoint
   heap-derived address if that address depends on a potentially moved base.
8. If RS4GC assumes a call or invoke result is a base pointer, the frontend must
   guarantee that result is not a heap-derived `Addr`. If that guarantee cannot
   be made from type alone, encode it explicitly or reject the IR.

## Plan

### Step 1: Keep the Fail-Closed Diagnostic

Keep `-rs4gc-fail-on-oxcaml-derived-relocates` enabled by default.

This diagnostic is the guardrail: it prevents the compiler from silently
emitting a derived pointer as an independent relocated GC value.

Do not weaken this diagnostic to get tests passing.

### Step 2: Build Small LLVM IR Tests for Actual Shapes First

Add or extend RS4GC tests for these cases:

- `gep(base, constant)` live across an invoke and used only on the normal
  continuation. This is the current first real blocker.
- a GEP rebuilt after one statepoint and then live across a later invoke. This
  is the repeated-statepoint shape observed in `gen_u_iarray.optO3.rs4gc.ll`.
- `gep(base, constant)` live across an invoke and used by the handler. This
  should keep failing until handler environment lowering exists.
- mixed normal-plus-handler use of the same GEP across an invoke. This should
  also keep failing until handler environment lowering exists.
- `gep(base, constant)` live across multiple invokes with a shared handler.
- `gep(base, constant)` live across a normal call. This already appears to work
  in generated IR, but needs a regression test.
- `gep(base, dynamic_offset)` live across a normal call.
- reject `inttoptr(ptrtoint(base) + dynamic_offset)` unless the pass can prove
  the integer is only an offset from the relocated base, not an absolute heap
  address.
- `freeze(gep_or_base)` live across a normal call.
- PHI of equivalent rematerializable derived values live across a normal call.
- select of equivalent rematerializable derived values live across a normal
  call.
- call return values and invoke return values whose result type is
  `ptr addrspace(1)`, to confirm the return value is a base only when the
  source-level type is an OCaml value pointer and not `Addr`.
- normal destination with PHI nodes, or a test proving RS4GC splits invoke
  normal destinations before rematerialization so remat code is not hidden on
  an unexecuted edge.

Each test should check either:

- the derived value is omitted from `gc-live` and rebuilt from the relocated
  base; or
- the pass fails with the OxCaml diagnostic.

For accepted cases, add a stackmap-facing check once the reduced test lowers
cleanly. The final safety property is about what the stack map asks the GC to
scan and update. The generated large-module `opt -> llc` path currently hits an
OxCaml GC printer assertion, so do not block the first IR test on that ad hoc
path; validate stackmap-facing behavior through reduced LLVM tests or the
normal compiler/codegen path after the implementation is changed.

The tests should be written before broadening the implementation.

### Step 3: Support the Current Invoke GEP Shape

First extend rematerialization for the current real blocker:

```llvm
%derived = getelementptr i8, ptr addrspace(1) %base, i64 C
%sp      = invoke ... to label %normal unwind label %handler
```

Requirements:

- The derived value is used only on the normal continuation.
- The base is an RS4GC-selected managed base that is live and relocated on the
  normal continuation. Do not infer this only from `ptr addrspace(1)` or from
  an arbitrary `inttoptr`.
- The derived value is removed from `gc-live`.
- The derived value is rebuilt in the normal destination from the relocated
  base and constant offset.
- All normal-continuation uses after the invoke use the rebuilt value.
- Handler-live uses still fail closed.
- A GEP rebuilt after one statepoint is treated the same way if it becomes live
  across a later invoke. The pass must not fix only original frontend GEPs.

This is the smallest change that targets the current failing IR and the
repeated-statepoint shape seen in `gen_u_iarray`.

Implementation shape:

- Extend the current OxCaml derived-pointer filter so it no longer rejects every
  invoke.
- For an invoke, require `Info.UsesExplicitExceptionRoots`.
- Reuse the existing generic RS4GC invoke rematerialization path that inserts
  rematerialized values in the normal destination for
  `Info.UsesExplicitExceptionRoots`.
- Record the rebuilt GEP as the post-statepoint replacement for the original
  derived value, using the same replacement discipline as normal
  `gc.relocate`.
- After rewriting, assert or FileCheck that no accepted derived value remains
  in the statepoint `gc-live` bundle and no accepted derived value has an
  independent `gc.relocate`.

### Step 4: Generalize Normal-Continuation Rematerialization

After Step 3 works, extend the rematerialization analysis from "single
instruction chain" to a small expression graph that can describe:

- GEP from base plus constant offset.
- GEP from base plus non-GC SSA offset.
- freeze of a rematerializable pointer.
- bitcast/no-op pointer cast of a rematerializable pointer.
- PHI/select of rematerializable expressions when all alternatives have the
  required relocated bases and non-GC operands available at the rebuild point.

The output of this analysis should be a rematerialization recipe:

- relocated base value;
- ordinary offset/value operands needed to rebuild the address;
- rebuild insertion point.

The analysis must reject a dynamic offset if the offset is actually an absolute
address disguised as an integer. The intended accepted form is an ordinary
integer offset used by a GEP. The intended rejected form is pointer arithmetic
where the value to preserve across the statepoint is really an interior pointer
encoded as `i64`.

Offset admissibility rules:

- constants are admissible;
- integer arithmetic from non-GC integers is admissible;
- values derived from `ptrtoint` of a managed pointer are not admissible unless
  the expression is proven to be a difference from the same relocated base;
- until that proof exists, reject such pointer-integer offsets.

For normal calls, insert rebuilds immediately after the base `gc.relocate`.

For invoke normal continuations, insert rebuilds in the normal destination
after the normal-continuation `gc.relocate` values.

Do not allow this path for handler uses yet.

After inserting a rebuild, all post-statepoint normal-continuation uses of the
pre-statepoint derived address must use the rebuilt value. It is not enough to
remove the derived value from `gc-live`.

Implementation hook: integrate the rebuilt values with the same replacement
machinery that redirects uses to `gc.relocate` results. A rematerialized value
must be treated as the post-statepoint replacement for the original derived
value at that parse point.

### Step 5: Split Invoke Cases by Use

Before deciding whether an invoke-derived pointer can be rematerialized, classify
uses of the derived value after the invoke:

- **normal-only**: all post-statepoint uses are dominated by the invoke normal
  destination;
- **handler-live**: at least one use is reachable from the unwind destination or
  from a shared handler body;
- **mixed**: both normal and handler continuations need the value.

Normal-only derived values can use the normal-continuation rematerialization
path.

Handler-live or mixed derived values need handler environment lowering.

This classification is a proof obligation, not a heuristic. For Milestone 1,
the implementation may use a narrower rule: accept only when every post-invoke
use that must be replaced is dominated by the invoke normal destination, and no
use is in or reachable only through the unwind destination/shared handler. If
that proof is not local and obvious, reject the value and keep the diagnostic.

### Step 6: Handler Environment Lowering for Derived Values

Before implementing handler environment lowering, try to eliminate handler-live
derived addresses at the source/IR level:

- sink short-lived derived-address construction to the memory operation that
  uses it;
- keep the base pointer and integer offset live instead of the derived pointer;
- rebuild the derived address inside the handler when the handler truly needs
  it.

If this avoids the real failing shapes, prefer it. It is simpler than adding
new handler environment state.

For a handler-live derived address, do not try to use exceptional
`gc.relocate` from the shared landingpad.

Instead, materialize explicit handler environment state late:

- one explicit exception root slot for the OCaml value base;
- one ordinary non-GC slot or SSA value for the offset;
- a handler-side rebuild from loaded relocated base plus offset.

The root slot must contain the base pointer, not the derived pointer.

Every statepoint that can raise to that handler must list the base root slot in
`gc-live`. The handler reloads the relocated base from the slot and rebuilds
the derived address locally.

If a non-raising safepoint can move the base before a later raising invoke to
the same handler, the normal continuation must synchronize the handler base
slot with the relocated base before the next may-raise operation. Otherwise the
handler slot can contain a stale base even though the later invoke lists the
slot as a root.

For dynamic offsets, the offset state must also be current for each invoke edge
that can enter the handler. The offset slot is ordinary memory, not a GC root,
and it must be stored before the may-raise operation whose handler observes it.

This is the derived-pointer version of the existing explicit exception-root
model.

This step should be implemented only for shapes that cannot be eliminated by
sinking/rebuilding around the actual use.

### Step 7: Avoid Treating Arbitrary PHIs as Bases

Do not make `-rs4gc-addrspace1-phi-select-base` a default solution.

It may be useful as an experiment, but it is too broad: a PHI of
`ptr addrspace(1)` values can be a PHI of interior pointers. Treating it as an
OCaml value pointer would hide the exact problem we are trying to prevent.

The proper fix is to classify the PHI expression:

- If it is a PHI of bases, it may be a base.
- If it is a PHI of derived expressions with reconstructible base+offset, it
  should be rematerialized.
- If it is a PHI of unknown or incompatible derived values, reject it.

For a PHI/select of derived expressions, do not require all alternatives to have
the same base value. The required property is that each alternative has a
rematerialization recipe and all recipe inputs can be made available at the
rebuild point. If that requires inserting a corresponding PHI/select of
relocated bases or offsets, test that explicitly before relying on it.

### Step 8: Frontend and Cmm Restrictions

Keep frontend checks that reject `Addr` registers live across calls/safepoints
until the late pass handles the equivalent LLVM shapes.

Once the late pass can handle a class of shapes, relax the frontend check only
for that class.

This keeps the default compiler conservative while allowing opt-in experiments
with:

- `llvm-unsafe-no-frontend-alloca-roots=1`
- `llvm-unsafe-no-slow-path-root-slots=1`

Add an audit for `Addr` values crossing function boundaries:

- `Addr` should not be returned from an OCaml call as if it were a normal
  managed value pointer.
- `Addr` should not be passed to a call unless the callee ABI explicitly expects
  a raw/interior address and the call is known not to be a safepoint.
- If such cases exist, lower them as base-plus-offset or reject them before
  RS4GC.

### Step 9: Validation Path

Use this order:

1. LLVM IR tests for each rematerialization and rejection shape.
2. Focused real test:
   `typing-layouts-iarrays/test_ignoreable_or_null_product_array.ml` with both
   efficiency flags enabled.
3. Focused directories likely to exercise derived heap addresses:
   `typing-layouts-iarrays`, `llvm-codegen`, `lib-bigarray`,
   `prim-bigstring`, `compaction`, and GC-root tests.
4. Full testsuite with the default mode.
5. Full testsuite with the strongest efficient mode once focused tests pass.
6. Self-stage2 and stage2 testsuite.
7. Microbenchmarks and compiler benchmark totals.

Do not run broad validation with the strongest efficient mode until the focused
IR tests distinguish all of these outcomes:

- accepted normal rematerialization;
- accepted normal-only invoke rematerialization;
- accepted handler base-plus-offset environment lowering;
- rejected ambiguous derived pointer.

## Milestones

### Milestone 1: Current Invoke GEP Shape

Support constant-offset `getelementptr` rematerialization on invoke normal
continuations when the derived value is normal-only.

This is the immediate real blocker from the latest generated IR.

### Milestone 2: Normal Calls and Expression Cleanup

Support simple and PHI/select rematerialization for non-invoke statepoints.

This should make ordinary allocation and poll points better without changing
exception semantics.

### Milestone 3: Broader Normal-Only Invokes

Support invoke statepoints for more derived expression recipes when the derived
address is used on the normal continuation and not by the handler.

This should be proven by IR tests before touching real source tests.

### Milestone 4: Avoid Handler-Live Derived Addresses

Try source/IR shaping that keeps base-plus-offset live instead of the derived
address across may-raise operations.

This milestone asks whether handler environment lowering is actually needed for
the real compiler output.

### Milestone 5: Handler-Live Base-Plus-Offset Environment

Only if Milestone 3 is insufficient, add explicit handler environment support
for derived addresses.

## Open Design Questions

### Can Normal-Only Invoke Rematerialization Be Implemented Locally?

Likely yes. The normal continuation already gets normal `gc.relocate` values.
If the derived pointer is not handler-live, rebuilding it in the normal
destination should be equivalent to normal-call rematerialization.

This needs an LLVM IR test where the derived value is live across an invoke but
only used on the normal path.

### How Should Handler Offsets Be Represented?

Use ordinary non-GC data. For constant offsets, no slot is needed. For dynamic
offsets, the offset must be available in the handler. If it is not naturally
available, materialize an ordinary explicit handler environment slot for it.

The offset slot is not a GC root.

### What About Multiple Invokes Sharing One Handler?

The handler environment must contain the value that is semantically live for the
currently active trap. This means every invoke edge that can reach the handler
must ensure the base root slot and offset state are current before the invoke.

This mirrors the explicit exception-root design for handler-live OCaml value
pointers.

### What If the Derived Expression Is Too Complex?

Reject it first.

Only add support after a reduced LLVM IR test demonstrates the exact shape and a
clear rematerialization recipe.

## Success Criteria

- No OxCaml statepoint relocates a heap-derived address independently.
- Simple and PHI/select normal-continuation derived addresses are
  rematerialized from relocated bases.
- Invoke normal-only derived addresses are rematerialized on the normal
  continuation.
- Handler-live derived addresses use base-plus-offset handler environment
  state.
- The strongest efficient mode passes focused source tests before any broader
  enablement.

## Self-Review Loop

### Review 1: Offset and PHI Handling Was Too Vague

Problem:

- The first plan accepted "dynamic offsets" without defining what makes an
  offset safe.
- It described PHI/select support as if all alternatives needed the same base,
  which is unnecessarily restrictive and also not the real safety condition.
- It did not explicitly say that post-statepoint uses must be rewritten to the
  rebuilt derived value.

Revision:

- Added offset admissibility rules.
- Replaced "same base" with "all recipe inputs are available at the rebuild
  point".
- Added the requirement that rematerialized values participate in the same
  post-statepoint replacement machinery as `gc.relocate`.

### Review 2: Handler Support Was Too Eager

Problem:

- The first plan went directly to handler environment lowering for
  handler-live derived addresses.
- That may be more machinery than needed if the compiler can avoid making
  derived addresses handler-live in the first place.

Revision:

- Added a milestone to first try avoiding handler-live derived addresses by
  sinking/rebuilding derived-address construction near the actual use.
- Made explicit handler base-plus-offset environment lowering a later milestone
  only for shapes that cannot be eliminated.

### Review 3: Base-Pointer Facts Need a Source

Problem:

- LLVM's existing base inference assumes call and invoke results are bases.
- That is only sound for OxCaml if a `ptr addrspace(1)` call result is really
  an OCaml value pointer, not a heap-derived `Addr`.

Revision:

- Added an invariant and audit for `Addr` values crossing function boundaries.
- Added tests for call/invoke return shapes.
- Added the requirement to encode or reject any case where type alone cannot
  distinguish value pointers from heap-derived addresses.

### Review 4: Inventory Narrows the Shape But Not the Semantics

Problem:

- The inventory says the current failure is a constant-offset GEP across an
  invoke.
- It does not yet say whether the derived value is normal-only or handler-live.
- It does not yet prove where a rebuilt value should be inserted in RS4GC's
  invoke lowering.
- It does not yet prove that the apparent base is always a real OCaml value
  pointer.

Revision:

- Added a required information-gathering section before implementation.
- Gated Milestone 1 on proving the current failures are normal-only.
- Added stackmap-facing validation for accepted invoke rematerialization.
- Added a negative handler-live IR test so the implementation cannot silently
  accept a case whose exceptional semantics are not represented.

### Review 5: New Facts Make The Pending-Data Gate Stale

Problem:

- The plan still described use topology and insertion-point facts as missing,
  even after the generated IR showed normal-continuation use for the inspected
  failures.
- It also treated stackmap validation as if the ad hoc large-module
  `opt -> llc` path had to work before the first implementation. That path hit
  an OxCaml GC printer assertion, so it is not the right first gate.

Revision:

- Converted the pending data section into an information-gathering checklist
  with status.
- Kept stackmap-facing validation as a required implementation test, but moved
  it to reduced LLVM tests or the normal compiler/codegen path.
- Updated the first implementation scope to the facts now gathered:
  normal-only constant-offset GEPs across invokes with explicit exception
  roots.

### Review 6: Repeated Statepoints Are Part Of Milestone 1

Problem:

- The gathered `gen_u_iarray` output shows `%.rematNN` GEPs rebuilt after one
  statepoint and then live across a later invoke.
- A fix that only handles original frontend GEPs would pass the first small
  test but fail the real repeated-statepoint shape.

Revision:

- Added repeated-statepoint LLVM test coverage to Step 2.
- Added an explicit Step 3 requirement that rebuilt GEPs must be handled the
  same way when they become live across later invokes.
- Required integration with RS4GC's post-statepoint replacement machinery, not
  a one-off textual replacement.

### Review 7: Base And Handler Proofs Need To Stay Fail-Closed

Problem:

- The `test_ignoreable` bases go through integer arithmetic and `inttoptr`, so
  the implementation must not accidentally bless arbitrary `inttoptr` values as
  OCaml value pointers.
- "Normal-only" must be a proof, not an assumption from the source test.

Revision:

- Step 3 now requires an RS4GC-selected managed base that is relocated on the
  normal continuation. Type alone is not enough.
- Step 5 now says the Milestone 1 proof may be deliberately narrow: every
  replaced post-invoke use must be dominated by the invoke normal destination,
  and no use may be in or reachable only through the unwind
  destination/shared handler. Otherwise reject the value.

### Stable Plan After Review

After the actual IR inventory, the plan is viable if implemented in this order:

1. Keep the fail-closed diagnostic.
2. Add focused IR tests for:
   - normal-only constant-offset GEP across invoke;
   - the same shape repeated across two statepoints;
   - handler-live and mixed-use negative cases.
3. Implement normal-only invoke rematerialization for constant-offset GEPs when:
   - explicit exception roots are active;
   - the base is the RS4GC-selected managed base and is relocated on the normal
     continuation;
   - all replaced post-invoke uses are dominated by the normal destination.
4. Reuse RS4GC's existing invoke rematerialization/replacement machinery so the
   rebuilt value is the post-statepoint value, and assert that accepted derived
   values no longer appear in `gc-live` or independent `gc.relocate`s.
5. Retest the real iarray case and inventory the next failure shape.
6. Implement broader normal-call and PHI/select expression recipes only when
   actual IR requires them.
7. Try to avoid handler-live derived addresses by source/IR shaping.
8. Add handler base-plus-offset environment lowering only if real reduced cases
   require it.

The remaining risks are explicit:

- dynamic offsets need conservative admissibility;
- PHI/select support must rebuild from relocated recipe inputs, not bless PHIs
  as bases;
- invoke support must not rely on edge code that the runtime exception path
  skips;
- `Addr` must not cross call boundaries as if it were a normal OCaml value
  pointer.

The plan should pause for more data if any of the current failing derived GEPs
are handler-live, if RS4GC has no local insertion point that dominates all
normal uses, or if the apparent base comes from an unproven `inttoptr` path.
