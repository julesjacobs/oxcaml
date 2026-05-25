# LLVM backend plan: keep allocation/poll fast paths in registers

## Goal

Make `-llvm-backend` stop forcing values live across heap allocation checks and
poll checks into permanent stack slots.

The important target is the fast path:

- heap allocation fast path: bump/check/continue
- poll fast path: compare/continue

Values should be allowed to stay in registers across those fast paths. The slow
path may spill, reload, and call `caml_call_gc`. Correctness must not depend on
callee-saved registers surviving the slow path.

## Current problem

The LLVM backend currently conflates two different requirements:

1. A value is live across a GC safepoint, so the GC needs to find it if the
   slow path is taken.
2. A pseudo-register's LLVM alloca must remain addressable for hidden control
   flow, especially trap/exception paths.

The conflation is in `backend/llvm/llvmize.ml`:

- `preserved_reg_slots` includes every `Val` register live across `Alloc Heap`
  and `Poll`.
- `store_into_reg` uses `store volatile` for preserved slots.
- `live_gc_root_alloca_bundles` reports only preserved allocas in the
  `"gc-live"` bundle.

That makes the logical register slot unpromotable by LLVM. A loop with a poll
or allocation keeps live OCaml values in stack slots even when the fast path
does not call the runtime.

The fast/slow CFG shape is already mostly right:

- `heap_alloc` emits the fast allocation check and branches to a cold
  `caml_call_gc` block only on failure.
- `poll` emits the fast poll check and branches to a cold `caml_call_gc` block
  only when the poll is taken.

The bad part is that root representation forces stack traffic before the branch.

## Native backend model to match

Native code records a frame descriptor at the slow call site. It records roots
from the actual locations of live values at that call site:

- register roots are encoded as tagged register offsets;
- stack roots are encoded as stack offsets.

The runtime `caml_call_gc` stubs save machine registers into `gc_regs`, and the
runtime stack scanner can update roots through either stack slots or `gc_regs`.

LLVM's OxCaml frame printer already understands both kinds of root locations:
`OxCamlGCPrinter.cpp` emits register roots with the same odd tagged encoding and
stack roots as frame offsets. LLVM `StackMaps.cpp` also separates statepoint GC
pointer locations from explicit alloca roots.

That means the long-term model can support register roots. The first step does
not need to rely on that.

## Chosen first step

First move allocation/poll roots to explicit slow-path root slots.

Do not start by passing statepoint GC pointer values directly and enabling
`-max-registers-for-gc-values`. That path is real, but it adds more moving
parts:

- `StatepointLowering.cpp` currently defaults `max-registers-for-gc-values` to
  `0`.
- `FixupStatepointCallerSaved.cpp` rewrites live-through statepoint operands
  after register allocation unless `DeoptLiveIn` is used.
- direct value roots require precise `gc.relocate`/refresh handling.
- invoke/landing-pad statepoints have extra restrictions.

The safer first step is to keep the existing stack-root runtime contract, but
make those stack roots separate from the pseudo-register allocas that `mem2reg`
should promote.

This first step mainly helps functions, loops, and values whose only
slot-preserving safepoints are eligible heap allocation or poll checks. If a
value is also live across a normal call, raising terminator, or trap-sensitive
path that still requires a preserved pseudo-register slot, it may remain
stack-resident because `preserved_reg_slots` is currently function-wide.

## First implementation slice

Implement this only for basic-instruction heap allocation and poll slow paths.
Leave normal calls, external calls, raises, traps, landing pads, and tail calls
using the existing conservative behavior.

### 1. Split the concepts in `llvmize.ml`

Rename or restructure the current policy so the distinction is explicit:

- `live_gc_root_regs_across`: values that must be visible to the GC if this
  statepoint is reached.
- `preserved_reg_slots`: pseudo-register slots that must stay addressable
  because hidden control flow can read them.
- `eligible_basic_safepoint_for_slow_root_slots`: a single predicate deciding
  whether an `Alloc Heap` or `Poll` instruction may use the new slow-path root
  slot strategy.

Then stop adding ordinary roots live across `Alloc Heap` and `Poll` to
`preserved_reg_slots` only when the instruction satisfies
`eligible_basic_safepoint_for_slow_root_slots`.

Keep these conservative for the first slice:

- terminator safepoints;
- trap/exception-preserved values;
- active-trap cases where the current alloca-slot behavior is needed;
- `invoke`/unwind cases where root refresh on the exceptional edge is not yet
  implemented;
- any unsupported root type.

Use the same eligibility decision in both places:

- the analysis that builds `preserved_reg_slots`;
- the emission path in `heap_alloc` and `poll`.

Compute eligibility from liveness plus active-trap/unwind facts before both
preservation analysis and emission use it. If that requires changing the order
of `prepare_fun_info`, compute active traps before `preserved_reg_slots`, or
pass equivalent trap/unwind information into the preservation analysis. Do not
let analysis and emission independently guess whether a safepoint is eligible.

### 2. Add dedicated slow-path root slots

Add entry-block allocas used only as GC root spill slots for allocation/poll
slow paths.

Use static entry-block allocas, not dynamic allocas in the slow block. Frame
table offsets must be stable.

The simplest safe design is a per-function pool sized to the maximum number of
`Val` roots live across any eligible `Alloc Heap` or `Poll` statepoint. At each
eligible slow path:

1. Compute the ordered live-root list for that instruction.
2. In the cold `call_gc` block, materialize each root through the normal
   logical-value loading path, such as `load_reg_to_temp`, not by raw-loading
   the pseudo-register alloca.
3. Store those loaded values into the first `N` root slots.
4. Pass those root slot allocas in the `"gc-live"` bundle.
5. After `caml_call_gc` returns normally, load the updated values from the root
   slots.
6. Store the updated values back into the corresponding pseudo-register allocas
   with normal non-volatile stores.
7. Branch to the existing join block.

The pseudo-register allocas are still normal allocas. Because their addresses
are not passed to the statepoint bundle for these basic safepoints, LLVM can
promote them. The slow-path stores and reloads should turn into cold-block
traffic and join-block phi values, not hot-path stack traffic.

The dedicated root-slot memory operations need an explicit optimizer contract.
The runtime may update these slots through stack scanning during `caml_call_gc`,
outside normal LLVM IR dataflow. The old preserved-slot path avoided this by
using volatile loads/stores for preserved pseudo-register slots. The new root
slots must either use volatile stores/reloads for the root-slot memory itself,
or use another explicitly justified mechanism that prevents LLVM from deleting
the reload or forwarding the pre-call value across the statepoint. Normal
non-volatile stores back into the pseudo-register allocas after the reload are
still desired so `mem2reg` can form the join value.

The constant-aware materialization is required. Today `Const_int` results are
stored to their pseudo-register alloca only when `preserve_reg_slot` is true;
otherwise the value may live only in `const_ints`. Once allocation/poll roots
stop being preserved, raw-loading the alloca can read an uninitialized slot.

Known exact immediate values need separate handling. If `get_const_int_for_reg`
proves a live `Val` is an immediate, it does not need a moving-GC root slot and
must not be refreshed with `store_into_reg` on only the slow path, because that
would clear `const_ints` and can make the fast path later read an uninitialized
pseudo-register alloca. Prefer excluding exact immediates from the slow-path
root-slot list. If an implementation chooses to materialize them anyway, it must
also preserve a valid value for both fast and slow predecessors after the join.

### 3. Keep root bundle construction explicit and local

The current root representation is:

```ocaml
(Reg.t * LL.Value.t) list
```

where the value is currently `poison`.

For the first slice, avoid a broad root-representation rewrite. Add the smallest
local representation needed for allocation/poll slow paths, for example:

```ocaml
type slow_path_root_slot = { reg : Reg.t; root_slot : LL.Value.t }
```

Existing call paths can keep using the old alloca-root behavior until they are
migrated.

The important abstraction boundary is not the type name; it is that bundle
construction for eligible allocation/poll statepoints receives explicit root
slot operands and refresh receives the matching `reg -> slot` mapping. Do not
reuse `live_gc_root_alloca_bundles` in a way that silently falls back to the
pseudo-register alloca or ignores the explicit slot. Do not overload the old
`(Reg.t * LL.Value.t)` pair with a new meaning, because existing code currently
ignores the second component for bundle construction. Do not rewrite ordinary
call root handling as part of the first slice unless a minimal adapter is
unavoidable.

### 4. Keep unwind cases conservative at first

The first slice should only use slow-path root slots where the normal return
edge is enough to refresh roots before the join.

If an allocation or poll statepoint is emitted as an `invoke` with an unwind
edge and the refresh semantics are not obvious, keep that instruction on the
old preserved-slot path. That preserves correctness while the common fast path
improves.

The first-slice eligibility predicate should require the emitted slow call to
be a normal `call`, not an `invoke`. In implementation terms, do not use the
new strategy when `unwind_label` is present.

Later, extend the same root-slot materialization to invoke statepoints by
making the exceptional path's required roots explicit and verifying landing-pad
behavior.

## Required invariants

- Every GC statepoint reports every live OCaml heap value that may be used after
  the statepoint.
- Root locations describe the slow call PC, not the fast-path check PC.
- After a taken slow path returns, later code observes relocated root values.
- Fast allocation/poll paths execute no root spill stores and no root reloads.
- Correctness does not assume `caml_call_gc` preserves caller registers.
- Root-slot stores/reloads around the slow call are volatile, or have an
  explicitly justified equivalent optimizer barrier, so GC updates performed
  through stack scanning are observed after `caml_call_gc`.
- The pseudo-register alloca for a normal live value is not passed as a
  `"gc-live"` alloca root for eligible allocation/poll statepoints.
- Stack root slots passed in `"gc-live"` are static frame objects.
- `Addr` derived pointers are not scanned independently; only valid base `Val`
  roots are reported.
- `Valx2` is not silently mishandled. Current LLVM root discovery uses
  `Cmm.is_val`, which excludes `Valx2`, so the old path is not a conservative
  `Valx2` root-reporting path. For the first slice, do not add substantial
  `Valx2` machinery; add a proof, comment, or assertion that this path only
  handles `Val` roots, and leave any separate `Valx2` correctness work out of
  scope.
- Terminator calls, raises, active-trap behavior, and tail-call lowering are
  unchanged by the first slice.
- Allocation statepoint IDs and poll statepoint IDs keep their existing
  frame-table meaning.

## Verification for the first slice

Add focused tests before broad validation.

### IR checks

Create or extend `testsuite/tests/llvm-codegen` cases for:

- heap allocation with two heap values live across the allocation;
- `%poll` with at least one heap value live across the poll.

Keep these test functions small and avoid other safepoints that keep the same
values in `preserved_reg_slots`; otherwise the function-wide preservation set
can hide whether allocation/poll changed.

Check the default generated IR:

- live-root pseudo-register stores across the fast check are absent for the
  purpose of root preservation, not merely changed from volatile to
  non-volatile;
- the `"gc-live"` bundle for the slow `caml_call_gc` names dedicated root-slot
  allocas, not the promoted pseudo-register allocas;
- slow-path root stores occur in the cold `call_gc` block before the call with
  the chosen volatile-or-equivalent memory semantics;
- slow-path root reloads occur after the call before the join and are not
  optimized into pre-call values;
- a live `Val` immediate across allocation/poll is not reported as a moving
  root, or is handled in a way that preserves a valid value on both fast and
  slow predecessors after the join;
- cases rejected by `eligible_basic_safepoint_for_slow_root_slots` still use the
  conservative alloca-root path.

Prefer small script/FileCheck-style structural checks over giant promoted expect
snapshots. The checks should assert the root-slot structure, absence of
root-preservation volatile traffic for eligible values, slow-block stores and
reloads, and the local fast-path shape without locking in incidental temporary
numbering.

### Assembly checks

For a tight allocation loop and a tight poll loop, inspect assembly and check:

- the fast path has no stores/reloads whose only purpose is preserving live
  roots for the slow path;
- root spill/reload traffic is in the slow block or cold call path;
- the slow call still branches back to the existing continuation.

Do not overfit to exact register names or ban all stack traffic. Existing debug
checks, prologue code, normal register pressure, and unrelated calls may still
produce stack traffic. Check the local fast-path shape and root-preservation
traffic specifically.

### Runtime correctness checks

Add small executable tests that force the slow path:

- minor allocation pressure with heap values live across allocation and used
  afterward;
- a poll-triggering loop with a heap value live across the poll and used
  afterward.

The tests should fail if relocated values are not restored from root slots.
The existing poll statepoint fixture that keeps only an integer live across
`%poll` is not enough for this change; add a heap-value poll case.

For poll, make the slow path deterministic enough to prove relocation: the test
should request or cause a minor collection while a young heap object is live
across `%poll`, then use that object afterward. A poll test that only reaches
`caml_call_gc` without a moving collection does not prove root refresh.

Also validate that root-slot reloads after `caml_call_gc` survive optimization.
The runtime updates roots through stack scanning, not normal IR dataflow, so the
PR must prove optimized IR or final assembly does not forward the pre-call value
or delete the slow-path root-slot reloads.

### Existing validation after focused tests pass

Run the focused LLVM codegen tests first. Then run the full LLVM backend test
suite. Then run self-stage2 validation.

## Later steps

### Direct statepoint value roots

Once slow-path root slots are correct and tested, experiment with passing GC
pointer values directly as statepoint GC roots for allocation/poll. That is the
route to true register roots at the slow call site.

This requires:

- enabling a bounded OxCaml-specific value for `max-registers-for-gc-values`;
- verifying `FixupStatepointCallerSaved` behavior for `Oxcaml_alloccc`;
- implementing root refresh from relocated values, not from stack root slots;
- testing register-root frame table output;
- handling invoke/landing-pad statepoints deliberately.

### Broader safepoints

After allocation/poll are fixed, revisit:

- ordinary OCaml calls;
- allocating external calls;
- raises;
- trap-heavy code;
- local allocation slow paths.

Those are not the first step because the call is on the normal path, not a cold
allocation/poll slow path.

## Suggested first agent goal

Implement the first slice for simple heap allocation and poll statepoints.

Acceptance criteria:

- For eligible `Alloc Heap` and `Poll` instructions without unsupported unwind
  requirements, ordinary live `Val` roots across the instruction no longer
  force their pseudo-register allocas into `preserved_reg_slots`.
- The slow `caml_call_gc` path materializes exact live `Val` roots into
  dedicated static root slots using constant-aware logical loads, passes those
  slots in `"gc-live"`, reloads them after the call, and stores relocated
  values back into the pseudo-register slots before the join.
- The implementation uses an explicit small representation/helper for
  allocation/poll slow-path root slots, instead of overloading the old live-root
  pair or accidentally reusing `live_gc_root_alloca_bundles`.
- Root-slot stores/reloads use volatile or another justified optimizer contract,
  and optimized IR or final assembly proves the post-call reload is preserved.
- Known exact immediate `Const_int` values are not naively refreshed on only the
  slow path; they are either excluded from moving-root handling or proven valid
  on both join predecessors.
- The same named eligibility predicate controls both removal from
  `preserved_reg_slots` and emission of slow-path root slots.
- `invoke`/unwind cases, active-trap cases, and unsupported root types remain on
  the conservative path unless the PR implements and tests their refresh
  semantics.
- Fast allocation/poll assembly for the added tests has no root-preservation
  stack traffic.
- Forced-GC runtime tests prove live heap values used after allocation/poll are
  still correct.
- Existing allocation/poll statepoint metadata remains correct.
- Full LLVM backend tests pass, then self-stage2 passes.

Non-goals for the first agent:

- broad benchmarking;
- changing ordinary call safepoints;
- enabling callee-saved register roots;
- changing runtime frame descriptor format;
- removing conservative handling for traps and invoke/landing-pad cases.
