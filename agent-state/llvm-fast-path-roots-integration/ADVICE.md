Yes: for this use case, treat gc-live(%alloca) as a late codegen representation, not as an optimizer-stable source-level root abstraction.

There is not currently a general LLVM IR marker meaning “this alloca/load/store sequence is a semantic moving-GC root slot; scalar optimization must preserve the memory object and must treat the statepoint as possibly updating the slot.” The alloca-in-gc-live path exists and works in lowering, but it is not the representation you should feed through the normal -O2 scalar pipeline.

LLVM’s own statepoint docs point in that direction: for relocating collectors, they recommend optimizing in the abstract machine model and lowering to explicit statepoints later; the explicit form exists, but frontend emission of it “may inhibit optimization,” and RewriteStatepointsForGC is recommended late in the pipeline, after most optimization. The same docs say alloca entries in gc-live can describe stack regions/spill slots, but also warn that this alternate form is “not well exercised” and that RewriteStatepointsForGC does not handle allocas.

What went wrong in your examples

Your examples are exactly the failure mode of using a physical root slot too early.

You intend this memory object to have extra semantics:

store %obj, %exnroot

invoke statepoint(...) [ "gc-live"(ptr %exnroot) ]
  unwind %recover

recover:
  %for_handler = load %exnroot

Meaning:

The runtime/GC may mutate *%exnroot while the statepoint is active.
The later load must observe that possibly updated value.

But ordinary scalar optimization mostly sees:

alloca + store + later load

and it is very good at converting that to SSA. Once it has done that, the handler value becomes a PHI of %obj, %obj1, %other, or %derived, which is precisely the wrong representation for a moving collector after a safepoint.

LLVM LangRef does say a gc.statepoint is assumed to read and write all memory, and that legal IR may not use a GC pointer argument after a statepoint without using the relocated value. Operand bundles also are not mere metadata; dropping them changes semantics. So a minimal reproducer where opt -O2 drops a still-required gc-live alloca or rewrites the program into stale post-statepoint GC uses is worth reducing and filing. But practically, this is still an unsupported corner of the alloca-in-gc-live form, not a representation to rely on for mid-level optimization. The docs explicitly warn that this path is not well exercised.

Recommended pipeline

Use two representations.

Before optimization, use an abstract representation:

; no physical exception root slots yet
; ordinary GC pointers in addrspace(1)
; ordinary invoke edges for may-raise/may-allocate operations
; handler may have ordinary SSA live-ins/PHIs temporarily

Run the normal optimizer here.

Then, after scalar optimization, run a late GC/EH lowering pipeline roughly like this:

1. Identify each trap handler / landingpad.
2. Compute GC values live into the handler from exceptional edges.
3. Materialize exception-root allocas for only those values.
4. Replace handler GC live-ins with loads from those slots.
5. Lower may-allocate/may-raise calls/invokes to statepoints.
6. Add the root slots to the gc-live bundle of every relevant statepoint.
7. On normal continuations, use ordinary gc.relocate for hot normal-path values.
8. Store relocated values back to the exception-root slots before the next statepoint
   whose exceptional edge can reach the handler.
9. Do not run SROA, mem2reg, GVN, DSE, InstCombine-style cleanup over this form.
10. Go to llc/codegen.

In other words, your exception-root materialization pass should be a late lowering pass, not part of the input IR that opt -O2 sees.

The statepoint docs say the abstract model is used for most optimization, then lowering switches to the explicit representation before codegen; they also say RewriteStatepointsForGC should be run much later, after most optimization, for code quality.

Concrete lowering shape

After optimization, you want to produce something like this and then go essentially straight to codegen:

entry:
  %exnroot = alloca ptr addrspace(1), align 8

  ; value currently visible to the handler if the first invoke raises
  store ptr addrspace(1) %obj, ptr %exnroot, align 8

try:
  %tok1 = invoke token @llvm.experimental.gc.statepoint(...)
            [ "gc-live"(ptr %exnroot, ptr addrspace(1) %obj) ]
            to label %normal1 unwind label %recover

normal1:
  %obj1 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok1, i32 ..., i32 ...)

  ; keep the exception slot synchronized with the relocated value
  store ptr addrspace(1) %obj1, ptr %exnroot, align 8

  %tok2 = invoke token @llvm.experimental.gc.statepoint(...)
            [ "gc-live"(ptr %exnroot, ptr addrspace(1) %obj1) ]
            to label %normal2 unwind label %recover

normal2:
  ...

recover:
  %lp = landingpad token cleanup

  ; no exceptional gc.relocate here
  %for_handler =
      load ptr addrspace(1), ptr %exnroot, align 8

  ; handler code uses %for_handler
  ...

For multiple raising sites with different abstract handler values, the store immediately before each statepoint must reflect the value the handler should see if that statepoint raises:

left_path:
  store ptr addrspace(1) %left_value, ptr %exnroot
  %tok.left = invoke token @llvm.experimental.gc.statepoint(...)
                 [ "gc-live"(ptr %exnroot, ptr addrspace(1) %left_value) ]
                 to label %left_cont unwind label %recover

right_path:
  store ptr addrspace(1) %right_value, ptr %exnroot
  %tok.right = invoke token @llvm.experimental.gc.statepoint(...)
                  [ "gc-live"(ptr %exnroot, ptr addrspace(1) %right_value) ]
                  to label %right_cont unwind label %recover

recover:
  %lp = landingpad token cleanup
  %v = load ptr addrspace(1), ptr %exnroot

This preserves the OCaml trap invariant: one installed runtime trap address, many LLVM invoke exceptional edges, one shared handler block, and no exceptional gc.relocate from a multi-predecessor landingpad.

Placement relative to RewriteStatepointsForGC

There are two viable designs.

The cleaner design is to write your own combined late pass that lowers OCaml may-raise/may-allocate operations to statepoint invokes and materializes the exception roots at the same time. That gives you full control and avoids asking upstream RewriteStatepointsForGC to reason about your OCaml trap model.

The other design is a split lowering:

After O2, before RS4GC:
  materialize exception-root slots enough to remove GC SSA values from the shared landingpad’s exceptional live-ins.

Run RS4GC or your statepoint rewrite:
  produce normal-path gc.relocate values.

After RS4GC:
  add/synchronize exception-root slots in gc-live bundles;
  store relocated normal-continuation values back into the slots;
  remove any accidental exceptional relocates if your pipeline produced them.

The key is that by the time the statepoint explicit form exists, the shared landingpad must not require gc.relocate(token %landingpad, ...).

Derived pointers

For derived pointers, do not root the derived pointer slot unless your collector can recover the base from that exact pointer. The statepoint docs call out that alloca stack-region entries have no separate base-pointer indication.

Use the base-plus-offset form late:

%baseroot  = alloca ptr addrspace(1)
%offslot   = alloca i64

store ptr addrspace(1) %base, ptr %baseroot
store i64 %offset, ptr %offslot

%tok = invoke token @llvm.experimental.gc.statepoint(...)
          [ "gc-live"(ptr %baseroot, ptr addrspace(1) %base) ]
          to label %normal unwind label %recover

normal:
  %base.reloc = call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok, ...)
  store ptr addrspace(1) %base.reloc, ptr %baseroot

recover:
  %lp = landingpad token cleanup
  %base2 = load ptr addrspace(1), ptr %baseroot
  %off2  = load i64, ptr %offslot
  %derived2 = getelementptr i8, ptr addrspace(1) %base2, i64 %off2

Again: create this after scalar optimization. Do not give SROA a chance to turn it back into %derived = gep %base, %offset.

Is there an existing marker?

There are three things people often consider, but none is the right “statepoint alloca root slot, please preserve through -O2” marker.

llvm.gcroot does declare a stack object containing a GC root to the code generator, but it belongs to the older accurate-GC intrinsic family; the LangRef explicitly describes statepoints as an alternative to llvm.gcroot. It is not a magic annotation that makes gc-live(%alloca) alloca slots optimizer-stable under the statepoint model.

volatile loads/stores can stop the most obvious promotion and forwarding, but they are a blunt optimizer barrier, not a precise GC-root semantic. They also do not by themselves make all relevant passes understand “the GC may update this slot at this statepoint and the handler must reload the updated value.”

Opaque calls or inline asm can force the alloca address to escape and can keep the slot materialized, but that is a workaround, not the intended statepoint representation. It tends to pessimize more than necessary and still leaves you responsible for stackmap/root synchronization.

A custom intrinsic can be reasonable if you need an early abstract marker, for example:

call void @ocaml.exnroot.set(token %trap, i32 %slot, ptr addrspace(1) %value)
%v = call ptr addrspace(1) @ocaml.exnroot.get(token %trap, i32 %slot)

but that intrinsic should be an abstract GC/EH construct that your late pass lowers into allocas, stores, loads, and gc-live operands. Do not lower it to ordinary alloca memory before -O2.

Bottom line

Use this rule:

Before O2:
  GC values are abstract SSA values.
  Trap handlers may be represented abstractly.
  No physical exception-root allocas that must survive scalar optimization.

After O2:
  Lower to statepoints.
  Materialize exception-root stack slots.
  Add those slots to gc-live.
  Replace handler GC live-ins with loads from those slots.
  Use normal gc.relocate only on normal continuations.
  Do not run scalar promotion/simplification after this point.

That gives you the performance property you want: the hot normal path can still use ordinary SSA and gc.relocate, while only the values genuinely live into OCaml trap handlers are forced into explicit stack roots.

Should we insert gc.relocate also only after o2

Yes—the clean pipeline is abstract GC SSA through O2, then late insertion of statepoints, normal-path gc.relocate, and explicit handler root slots near codegen.

Thought for 3m 26s

Yes. In the normal LLVM statepoint model, insert gc.relocate late, after the main -O2 scalar pipeline, usually in the same late lowering stage that turns abstract may-allocate/may-raise operations into gc.statepoint calls/invokes.

The rule is:

Before O2:
  ordinary / abstract GC-pointer SSA
  ordinary calls/invokes or frontend-specific abstract safepoints
  no physical exnroot allocas
  no gc.relocate

After O2:
  lower calls/invokes to gc.statepoint
  compute live GC values
  insert gc-live bundles
  insert normal-path gc.relocate
  materialize exceptional root slots for shared trap handlers
  do not run the generic scalar O2 pipeline again

LLVM’s own statepoint documentation says RewriteStatepointsForGC should run “much later” in the pipeline, after most optimization, and that this improves code quality. It also says PlaceSafepoints runs before RewriteStatepointsForGC and does not produce full relocation sequences.

The reason is the same as with your exception-root allocas: explicit statepoint IR is not a good high-level optimization representation. Once a statepoint exists, LLVM IR has a special rule: a GC pointer argument of the statepoint cannot be used afterward in reachable code; the explicitly relocated value from gc.relocate must be used instead. And gc.relocate itself is the operation that returns the potentially relocated pointer value at the safepoint.

So the intended shape is not:

frontend emits statepoint/gc-live/gc.relocate
run opt -O2
hope generic optimization preserves the exact GC protocol

It is:

frontend emits optimizable abstract IR
run opt -O2
late GC lowering inserts statepoints and relocates
llc/codegen lowers the explicit statepoint form

For your OCaml trap case, the late result should look like this on the normal path:

%tok1 = invoke token @llvm.experimental.gc.statepoint(...)
          [ "gc-live"(ptr addrspace(1) %obj, ptr %exnroot) ]
          to label %normal1 unwind label %recover

normal1:
  %obj1 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok1, i32 %baseidx, i32 %objidx)

  ; If the shared handler may later need this value, synchronize the
  ; explicit exceptional root slot with the relocated value.
  store ptr addrspace(1) %obj1, ptr %exnroot

  %tok2 = invoke token @llvm.experimental.gc.statepoint(...)
            [ "gc-live"(ptr addrspace(1) %obj1, ptr %exnroot) ]
            to label %normal2 unwind label %recover

And the exceptional shared handler should still avoid exceptional gc.relocate:

recover:
  %lp = landingpad token cleanup
  %for_handler = load ptr addrspace(1), ptr %exnroot
  ...

A compact pass ordering would be:

1. Frontend / early LLVM IR:
     - abstract GC pointers
     - abstract trap regions
     - invokes for may-raise operations if useful for CFG
     - no explicit root-slot allocas that must survive optimization
     - no gc.relocate

2. Run normal optimization:
     - O2, inlining, SROA, mem2reg, GVN, LICM, etc.

3. Late GC/EH lowering:
     - choose exact statepoint sites
     - compute live GC values at each statepoint
     - compute values live into each trap handler
     - materialize exnroot/base-root slots
     - rewrite may-allocate/may-raise calls to statepoint calls/invokes
     - insert gc-live bundles
     - insert gc.relocate on normal continuations
     - store relocated values back to exnroot slots where needed
     - rewrite shared handlers to reload from exnroot slots

4. Only run late passes that are known safe for explicit statepoint IR,
   then codegen.

One caveat: once you have emitted a gc.statepoint, you cannot postpone gc.relocate while still leaving ordinary post-statepoint uses of the old pointer in the IR. That would be illegal statepoint IR. So “insert gc.relocate after O2” really means do not lower to explicit statepoints until after O2. If explicit statepoints are already present, their required relocates must also be present for all reachable post-statepoint GC uses.

For your backend, I would make the late pass that handles OCaml traps either wrap or replace RewriteStatepointsForGC. The key invariant after that pass is:

normal continuation:
  use gc.relocate results

shared exceptional continuation:
  do not use exceptional gc.relocate from the multi-pred landingpad;
  reload handler-live GC values from explicit GC root slots

That gives you the best performance story: O2 sees clean SSA before lowering, and codegen sees explicit, precise, moving-GC-safe statepoint IR after lowering.

How would we do the transformation that inserts gc.relocate and stack/root slots?

I’ll frame this as a late LLVM lowering pass: its input IR, liveness analyses, rewrites for statepoints and shared handlers, and final invariants.

A key choice is to own normal relocation and exceptional-root materialization in one coordinated late rewrite, preserving the handler environment abstractly until real stack slots are created.

Thought for 11m 21s

Yes. I would implement this as a late trap-aware statepoint lowering pass, preferably by forking/extending RewriteStatepointsForGC, not as a separate peephole after stock RS4GC.

LLVM’s stock RewriteStatepointsForGC already does most of the hard work: it rewrites calls/invokes to gc.statepoint, computes live GC values, computes base pointers, and inserts the required normal gc.relocate sequence. The LLVM docs describe it exactly as the pass that lowers the abstract model to explicit statepoints and “associated full relocation sequence.” The part you need to change is the invoke exceptional-path handling: current RS4GC explicitly attaches exceptional relocates to the unwind landingpad and asserts that the unwind block has a unique predecessor.

The clean transformation is:

Before late pass:
  optimized abstract IR
  ordinary invoke unwind edges to the shared OCaml handler
  handler may have SSA PHIs/live-ins
  no explicit root allocas
  no gc.relocate

Late pass:
  1. materialize handler live-ins into explicit root slots
  2. rewrite handler to load from those slots
  3. rewrite may-GC calls/invokes to gc.statepoint
  4. add root slots to gc-live
  5. insert gc.relocate only on normal continuations
  6. rewrite normal-path uses to relocated SSA values
  7. emit no exceptional gc.relocate in the shared landingpad

LLVM’s docs explicitly recommend targeting the abstract machine model for relocating collectors and lowering late, because directly generating the explicit form can inhibit optimization. The same docs say allocas in gc-live can describe explicit spill slots, and that the generator is responsible for spilling/filling around the safepoint.

1. Start from optimized abstract IR

After -O2, allow the IR to look like normal SSA:

left:
  invoke void @may_raise_1()
          to label %left.cont unwind label %recover

right:
  invoke void @may_raise_2()
          to label %right.cont unwind label %recover

recover:
  %v = phi ptr addrspace(1) [ %obj, %left ],
                            [ %other, %right ]
  %lp = landingpad token cleanup
  ret ptr addrspace(1) %v

This is fine as an abstract representation. It is not yet moving-GC-safe explicit statepoint IR.

2. Materialize the exceptional handler environment

For every OCaml trap handler landingpad %recover, compute the GC values that enter the handler from outside the handler region.

The common cases are:

recover:
  %v = phi ptr addrspace(1) [ %obj, %left ], [ %other, %right ]
  %lp = landingpad token cleanup
  ...

and direct external uses:

recover:
  %lp = landingpad token cleanup
  call void @use(ptr addrspace(1) %obj)

For each such handler live-in, create an entry-block alloca:

entry:
  %v.exnroot = alloca ptr addrspace(1), align 8

Then insert the exceptional-edge copy explicitly as a store before each invoke that may unwind to that handler:

left:
  store ptr addrspace(1) %obj, ptr %v.exnroot, align 8
  invoke void @may_raise_1()
          to label %left.cont unwind label %recover

right:
  store ptr addrspace(1) %other, ptr %v.exnroot, align 8
  invoke void @may_raise_2()
          to label %right.cont unwind label %recover

Then replace the handler PHI/live-in with a load after the landingpad:

recover:
  %lp = landingpad token cleanup
  %v.for_handler = load ptr addrspace(1), ptr %v.exnroot, align 8
  ret ptr addrspace(1) %v.for_handler

At this point, the shared handler no longer needs an SSA GC pointer from the exceptional edge. It needs only the stack/root slot.

This is the key trick: the store before the invoke acts like the exceptional-edge copy, but it is a real memory store that the GC can update during the statepoint.

3. Rewrite the invoke to an invoke statepoint

Now rewrite the call/invoke to llvm.experimental.gc.statepoint.

Schematic result:

left:
  store ptr addrspace(1) %obj, ptr %v.exnroot, align 8

  %tok.left =
    invoke token @llvm.experimental.gc.statepoint(...)
      [ "gc-live"(ptr %v.exnroot,
                  ptr addrspace(1) %obj /* if normally live */) ]
      to label %left.sp.cont unwind label %recover

left.sp.cont:
  %obj.left.reloc =
    call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok.left,
                                     i32 %obj.base.idx,
                                     i32 %obj.ptr.idx)
  br label %left.cont

The important split is:

gc-live contains:
  - explicit root slot allocas needed by the exceptional handler
  - ordinary SSA GC values needed by the normal continuation

gc.relocate is emitted only for:
  - ordinary SSA GC values used after the statepoint on normal paths

gc.relocate is not emitted for:
  - the explicit root slot alloca
  - the shared exceptional landingpad

The gc-live operand bundle must contain every GC pointer that may need updating by the collector. A gc.relocate returns the potentially relocated value of a pointer at the safepoint, and its indices refer into the corresponding statepoint’s gc-live bundle.

For example, if the bundle is:

[ "gc-live"(ptr %v.exnroot,
            ptr addrspace(1) %obj) ]

then %obj is at gc-live index 1, so the normal relocation is conceptually:

%obj.reloc =
  call ptr addrspace(1)
    @llvm.experimental.gc.relocate(token %tok, i32 1, i32 1)

You do not call gc.relocate on %v.exnroot. %v.exnroot is a stack location reported to the stack map. The GC updates the slot. The handler reloads from the slot.

4. Split the normal continuation, not the exceptional handler

For invokes, create a unique normal continuation block where you can place gc.result and normal gc.relocate calls:

before:
  %r = invoke ptr addrspace(1) @foo()
          to label %normal unwind label %recover

after:
  %tok = invoke token @llvm.experimental.gc.statepoint(...)
           [ "gc-live"(...) ]
           to label %foo.sp.cont unwind label %recover

foo.sp.cont:
  %r = call ptr addrspace(1)
        @llvm.experimental.gc.result(token %tok)

  %x.reloc = call ptr addrspace(1)
        @llvm.experimental.gc.relocate(token %tok, i32 ..., i32 ...)

  br label %normal

Update PHIs in %normal to use %foo.sp.cont as the predecessor.

Do not split the unwind edge into one landingpad per statepoint if you want to preserve the OCaml trap invariant. The statepoint invoke should still unwind to the single installed handler:

unwind label %recover

The runtime trap frame should contain the machine address corresponding to %recover.

5. Patch/fork RewriteStatepointsForGC

Stock RS4GC is close, but not sufficient for this model. The current implementation path for invokes creates exceptional relocates in the unwind block and asserts that the unwind block has a unique predecessor. You want a trap-aware mode that does this instead:

// Pseudocode.
if (auto *II = dyn_cast<InvokeInst>(CB)) {
  BasicBlock *Unwind = II->getUnwindDest();

  if (isOcamlTrapLandingPad(Unwind)) {
    // Do not insert gc.relocate(token %landingpad, ...).
    // Do not require Unwind->getUniquePredecessor().
    //
    // Correctness is provided by explicit exnroot slots listed in gc-live.
    Result.UnwindToken = nullptr;
  } else {
    // Existing RS4GC behavior for ordinary unique-predecessor EH pads.
    insertExceptionalRelocatesInLandingPad(...);
  }

  // Still insert normal relocates in the unique normal statepoint continuation.
  insertNormalRelocates(...);
}

Internally, keep two separate sets per statepoint:

RelocLive[SP]:
  ordinary GC SSA values needing normal gc.relocate

RootLive[SP]:
  explicit stack/root slots to report in gc-live

Then build the final bundle from both:

gc-live(SP) =
  RootLive[SP]
  ∪ base/pointer operands required for RelocLive[SP]

But call CreateGCRelocates only for RelocLive[SP], not for the root-slot allocas.

6. Let normal use rewriting fix stores after earlier statepoints

A nice property of doing the exceptional-store insertion before the statepoint rewrite is that the store operand is just an ordinary use.

Suppose the optimized abstract IR is:

call void @may_gc_but_return()
invoke void @may_raise()
        to label %normal unwind label %recover

and the handler needs %obj.

Your pre-statepoint root materialization inserts:

call void @may_gc_but_return()

store ptr addrspace(1) %obj, ptr %obj.exnroot
invoke void @may_raise()
        to label %normal unwind label %recover

Then the statepoint rewrite for the earlier @may_gc_but_return will rewrite the later store operand to the relocated value:

%tok0 = call token @llvm.experimental.gc.statepoint(...)
          [ "gc-live"(ptr addrspace(1) %obj) ]

%obj0 = call ptr addrspace(1)
          @llvm.experimental.gc.relocate(token %tok0, i32 ..., i32 ...)

store ptr addrspace(1) %obj0, ptr %obj.exnroot

%tok1 = invoke token @llvm.experimental.gc.statepoint(...)
          [ "gc-live"(ptr %obj.exnroot, ...) ]
          to label %normal unwind label %recover

So you do not have to precompute “current relocated value” yourself. Insert the stores in abstract IR, then let the normal statepoint use-rewriter update them.

This is why it is much easier to integrate this with RS4GC than to run a post-pass over already explicit statepoints.

7. Handler rewrite example

Input after -O2:

define ptr addrspace(1) @f(
    ptr addrspace(1) %obj,
    ptr addrspace(1) %other,
    i1 %cond) gc "ocaml" personality ptr @ocaml_personality {
entry:
  br i1 %cond, label %left, label %right

left:
  invoke void @may_raise_1()
          to label %left.cont unwind label %recover

left.cont:
  ret ptr addrspace(1) %obj

right:
  invoke void @may_raise_2()
          to label %right.cont unwind label %recover

right.cont:
  ret ptr addrspace(1) %other

recover:
  %v = phi ptr addrspace(1) [ %obj, %left ],
                            [ %other, %right ]
  %lp = landingpad token cleanup
  ret ptr addrspace(1) %v
}

After trap-aware statepoint lowering:

define ptr addrspace(1) @f(
    ptr addrspace(1) %obj,
    ptr addrspace(1) %other,
    i1 %cond) gc "ocaml" personality ptr @ocaml_personality {
entry:
  %v.exnroot = alloca ptr addrspace(1), align 8
  br i1 %cond, label %left, label %right

left:
  store ptr addrspace(1) %obj, ptr %v.exnroot, align 8

  %tok.left =
    invoke token @llvm.experimental.gc.statepoint(...)
      [ "gc-live"(ptr %v.exnroot,
                  ptr addrspace(1) %obj) ]
      to label %left.sp.cont unwind label %recover

left.sp.cont:
  %obj.left =
    call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok.left, i32 1, i32 1)
  ret ptr addrspace(1) %obj.left

right:
  store ptr addrspace(1) %other, ptr %v.exnroot, align 8

  %tok.right =
    invoke token @llvm.experimental.gc.statepoint(...)
      [ "gc-live"(ptr %v.exnroot,
                  ptr addrspace(1) %other) ]
      to label %right.sp.cont unwind label %recover

right.sp.cont:
  %other.right =
    call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok.right, i32 1, i32 1)
  ret ptr addrspace(1) %other.right

recover:
  %lp = landingpad token cleanup

  ; No exceptional gc.relocate here.
  %v.for_handler =
    load ptr addrspace(1), ptr %v.exnroot, align 8

  ret ptr addrspace(1) %v.for_handler
}

If may_raise_1 allocates, triggers a moving GC, and then raises, the stack map for %tok.left reports %v.exnroot. The collector updates that slot. The runtime restores the trap frame and jumps to %recover. The handler load sees the updated pointer.

LLVM models a statepoint as reading and writing all memory, and legal explicit-statepoint IR must use relocated values rather than stale GC pointer arguments after a statepoint. That memory effect is what makes the store-before-statepoint and load-after-statepoint representation valid once inserted late.

8. Derived pointers: root the base, not the derived pointer

For a handler-live derived pointer:

%derived = getelementptr i8, ptr addrspace(1) %base, i64 %off

invoke void @may_raise()
        to label %normal unwind label %recover

recover:
  ret ptr addrspace(1) %derived

do not put %derived directly in an explicit alloca root unless your collector can recover the object base from that exact interior pointer. LLVM’s docs warn that alloca stack-region entries have no corresponding base-pointer indication.

Instead materialize:

entry:
  %base.exnroot = alloca ptr addrspace(1), align 8
  %off.exnslot  = alloca i64, align 8

before.invoke:
  store ptr addrspace(1) %base, ptr %base.exnroot, align 8
  store i64 %off, ptr %off.exnslot, align 8

  %tok =
    invoke token @llvm.experimental.gc.statepoint(...)
      [ "gc-live"(ptr %base.exnroot,
                  ptr addrspace(1) %base,
                  ptr addrspace(1) %derived) ]
      to label %normal unwind label %recover

normal:
  %base.r =
    call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok, i32 %base.idx, i32 %base.idx)

  %derived.r =
    call ptr addrspace(1)
      @llvm.experimental.gc.relocate(token %tok, i32 %base.idx, i32 %derived.idx)

  ...

recover:
  %lp = landingpad token cleanup
  %base.h = load ptr addrspace(1), ptr %base.exnroot, align 8
  %off.h  = load i64, ptr %off.exnslot, align 8
  %derived.h = getelementptr i8, ptr addrspace(1) %base.h, i64 %off.h
  ret ptr addrspace(1) %derived.h

For the normal path, use ordinary gc.relocate for %derived if %derived is normally live. For the exceptional shared handler, reconstruct from the relocated base loaded out of the explicit root slot.

Before RS4GC, LLVM provides gc.get.pointer.base and gc.get.pointer.offset intrinsics for finding base/offset information; they are intended for use before lowering to explicit statepoints. In a production OCaml/OxCaml pipeline, it may be simpler to preserve base-plus-offset information directly in your frontend or earlier lowering.

9. Store-placement choices

There are two reasonable policies.

The simplest policy is:

For every invoke statepoint that can unwind to handler H:
  store each handler-live value for H into its exnroot immediately before the invoke
  list that exnroot in that invoke’s gc-live bundle

This is very robust. It may insert more stores, but only for values live into handlers and only before may-raise statepoints.

A more optimized policy is:

Keep the exnroot slot valid for a longer region.
Store only when the logical handler environment changes.
List the slot in gc-live for every statepoint while it contains a live GC value.
Rely on the GC to update the slot at each statepoint.

That can remove repeated stores, but it adds root slots to more stack maps and requires a dataflow analysis tracking what each slot currently contains. I would implement the simple policy first, then add slot coloring and store coalescing after the correctness tests are solid.

One nuance: after a statepoint, if the slot was listed in gc-live, the runtime may already have updated the slot. Therefore this mirror store is often redundant:

%obj.r = gc.relocate(token %tok, ...)
store ptr addrspace(1) %obj.r, ptr %exnroot

It is still a valid conservative representation, and it can make the slot-content invariant easier to reason about. For performance, omit it when the slot was already live in the just-executed statepoint and still represents the same logical handler value.

10. Invariants to check after the pass

Add a custom verifier for your lowering. Check at least these:

1. No gc.relocate uses a landingpad token in an OCaml shared trap handler.

2. Every GC SSA value used after a statepoint on a normal path is the
   corresponding gc.relocate result, gc.result result, or a value derived from
   those.

3. Every GC value loaded by a shared handler comes from an explicit exnroot slot.

4. For every invoke statepoint that can unwind to a handler using exnroot slot S,
   S appears in that statepoint’s gc-live bundle.

5. The store to S dominates the invoke statepoint on that dynamic path.

6. Derived handler values are represented as relocated base + non-GC offset,
   unless the collector explicitly supports rooting arbitrary interior pointers.

7. No scalar optimizer such as SROA, mem2reg, GVN, or DSE runs after this lowering.

LLVM’s own verifier checks many local intrinsic restrictions, but the statepoint docs note that the central relocation invariant is a dynamic/static correctness property and is not fully checked by the generic verifier. So you should have your own verifier for the OCaml trap-specific invariants.

The production plan

I would implement it like this:

Pass: OcamlStatepointLowering, late IR pass

Input:
  optimized abstract IR with invoke unwind edges to OCaml trap handlers

Phase A:
  discover OCaml trap landingpads
  compute handler live-ins
  create exnroot/base-root/offset slots
  replace GC handler PHIs/live-ins with loads after landingpad
  insert stores before invokes that unwind to those handlers

Phase B:
  run RS4GC-style statepoint construction
  compute normal live GC values
  compute base pointers
  build gc-live bundles from:
    - ordinary normal-live GC SSA values
    - explicit exnroot allocas for the invoke’s handler
  create gc.statepoint calls/invokes

Phase C:
  create gc.result where needed
  create gc.relocate only in normal continuations
  rewrite normal-path uses through relocated SSA values
  skip exceptional relocates for OCaml shared trap landingpads

Phase D:
  run custom verifier
  go to codegen

The key design decision is that the exceptional root store/load transformation happens before or inside the statepoint rewrite, so normal use rewriting can still update the store operands to the correct relocated SSA values from earlier safepoints. Trying to bolt the root slots on after stock RS4GC is much harder, and stock RS4GC’s invoke path is exactly where the unique-landingpad assumption lives.