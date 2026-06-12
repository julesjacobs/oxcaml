# RA-derived GC roots: findings and implementation plan

Written 2026-06-11, after landing commits `abef125858` (statepoint-crossing
spill-slot roots) and `5f300b9b38` (entry-cost fixes). This documents the
investigation of moving GC root tracking from ISel/RS4GC machinery into the
register allocator, with in-place root updates. Scope agreed with Jules:
keep exnroots in allocas for now; handle gc.relocate + statepoint lowering
first.

## Background: how we got here

- The latent parser.pp.ml miscompile (FIXED, `abef125858`, see FINDINGS.md)
  was caused by RA spill-slot copies of GC values crossing statepoints
  unlisted. The fix, `llvm/lib/CodeGen/OxCamlStatepointSpillRoots.cpp`, runs
  between RA and VirtRegRewriter (VirtRegMap + LiveStacks alive), collects
  GC slot families globally (folded gc operands of any statepoint + the VRM
  stack slot of `VRM.getOriginal(V)` for every gc reg operand V anywhere),
  and appends each family slot that is live both INTO (`getRegSlot(true)`)
  and ACROSS (`getRegSlot()`) a statepoint as a folded
  `(IndirectMemRefOp, 8, FI, 0)` gc operand + `(i,i)` gc-map pair +
  FixedStack `(load store)` MMO, bumping both count immediates. The
  entering-liveness check excludes folded relocate-def spills (garbage on
  entry; machine verifier catches as "loads from dead spill slot").
  Operand surgery: pop the tail (allocas + gc-map + regmask + implicits),
  append, re-push (this LLVM has no MachineInstr::insert).
  KEY INSIGHT: this pass already implements in-place slot updates + family
  tracking; the GC runtime already has in-place semantics (slots and
  registers rewritten in place). The RA-derived-roots design promotes this
  pass from "patch the holes" to "the authoritative mechanism".

- The entry-cost fixes (`5f300b9b38`): CombineSPBump for oxcaml without
  prologue stack check; llvmize `read_stack_pointer` via read_register +
  `AArch64MIPeepholeOpt::visitCmpWithSPCopy` (COPY $sp + SUBS -> cmp sp,xN);
  dead C-call args no longer caller-rooted
  (`-rs4gc-oxcaml-root-dead-c-call-args` restores; `HasUseAfterCall`
  refined: a value defined in the call's own block before the call is dead
  along the back edge). Benches: micro geomean 0.662->0.652, minibench
  0.906->0.886, compiler bench 0.9732->0.9715.

## Confirmed current-state facts (file/line pointers)

1. "GC roots in vregs" exists ONLY at alloc statepoints.
   `StatepointLowering.cpp` ~line 580: `MaxVRegPtrs` =
   `numeric_limits<unsigned>::max()` iff `SI.CLI.CallConv ==
   CallingConv::OxCaml_Alloc`, else **0** (cl-opt
   `-max-registers-for-gc-values` overrides for debugging). Ordinary OCaml
   call statepoints lower every gc value via `RecordType::Spill` = ISel
   statepoint spill slots. The register allocator NEVER sees gc values
   across ordinary calls; ISel does its own (bad) allocation.

2. The relocate-as-new-def mechanism: `InstrEmitter.cpp` ~line 1170
   manually ties one STATEPOINT def per vreg-relocated value to its
   gc-operand use (`MI->tieOperands(Def++, Use)`); `visitGCRelocate`
   (`SelectionDAGBuilder`) emits CopyFromReg of the def vreg
   (`StatepointLowering.cpp` ~1313, RecordType::VReg). Consequences:
   a. Tied register operands FORCE every live gc value into a register at
      every alloc statepoint (cont's blocks reloading ~20 values from slots
      right before caml_call_gc are this constraint, not pressure).
   b. Tied operands are unfoldable (`TargetInstrInfo.cpp` foldPatchpoint:
      "Cannot fold tied operands") so RA cannot leave a value in its spill
      slot across an alloc statepoint.
   c. Each statepoint fragments the live range (new vreg + COPY inserted by
      TwoAddress; RegisterCoalescer merges most — we observed
      `%4081 = STATEPOINT ... %4081(tied-def 0)` — but split/evict
      heuristics still see the seams).

3. ISel spill machinery for ordinary calls (all in StatepointLowering.cpp):
   `spillIncomingStatepointValue` (~405) emits store unless location
   already set; `reservePreviousStackSlotForValue` (~299) +
   `findPreviousSpillSlot` (~183) reuse slots and skip stores ONLY when the
   incoming value chases to a relocate with a known Spill record or to
   `FuncInfo.StatepointStableRootHomes`. The PHI case gives up unless ALL
   inputs agree on one slot — upstream TODO comment in the file documents
   this. OxCaml addition `StatepointStableRootHomes`
   (`SelectionDAGBuilder.cpp` ~11020-11180, `FunctionLoweringInfo.h:154`):
   assigns a stable FI to `phi(seed, relocate-continuation-of-self)` only;
   `maybeStoreStableStatepointRootSeed` stores the seed on the entry edge
   (called from HandlePHINodesInSuccessorBlocks). Verified working on
   minimal loops (one store in entry, zero in loop, incl. two statepoints);
   verified NOT firing in real loops (boyer tautologyp, kb rporec) because
   joins merge values whose previous slots differ -> store per iteration +
   slot shuffling at joins. /tmp/stable-home{,2}.ll were the reproducers
   (recreate easily: loop phi over seed + relocate, oxcaml_nofpcc callee,
   "statepoint-id"="0", oxcaml_module flag).

4. Frametable emission already supports everything needed: register roots
   (odd encoding, x0-x15 -> 0-15, x19-x25 -> 16-22) and stack roots
   (Indirect locations; identity convention emitted == true SP offset
   verified by synth harness `llstash/synth-statepoint-offsets*.ll`;
   `statepoint-id` encodes bit0=alloc, ((id>>1)&7)*16 = trap bytes added by
   OxCamlGCPrinter to every stack root, (id>>16)=alloc words).
   `parseStatepointOpers` (StackMaps.cpp ~446) emits locations from gc-map
   base/derived pairs (base must == derived for oxcaml, else fatal) and
   from the gc-alloca section.

5. `FixupStatepointCallerSaved.cpp`: post-RA, spills caller-saved regs
   across statepoints, rewrites reg operands to FI tuples in place (record
   counts unchanged); AArch64 `shouldSpillStatepointGCPtr` forces only
   x16-x18/x26-x28 for oxcaml. Becomes redundant for oxcaml under the new
   design.

## The in-place design (scoped)

Lower oxcaml call statepoints with in-place root-update semantics:

- `gc.relocate` lowers to IDENTITY: `setValue(&Relocate,
  getValue(DerivedPtr))`. No statepoint defs, no tied operands, no
  CopyFromReg chains. Cross-block relocates need nothing (the underlying
  value's vreg export covers them). New RecordType (e.g. `InPlace`).

- PRESERVING sites (OxCaml_Alloc: call_gc/realloc/local_realloc): gc values
  stay plain USE operands (untied). RA assigns freely: register ->
  register root (GC updates the register; later uses of the same vreg are
  correct BECAUSE the GC rewrote it); spilled -> operand folds to slot
  (now legal since untied) -> slot root; sibling slots via
  OxCamlStatepointSpillRoots as today.

- CLOBBER-ALL sites (ordinary OCaml calls): list NO gc operands at all.
  The regmask forces RA to spill anything live-across into family slots;
  OxCamlStatepointSpillRoots lists crossing slots from LiveStacks globally
  (its family collection does not need operands at that site — it scans all
  statepoints in the function; with no operands anywhere on clobber-all
  sites, families come from preserving-site operands and folded slots, plus
  (TODO check) values that only ever cross clobber-all sites would have NO
  operand anywhere -> family invisible! FIX: keep gc operands on
  clobber-all statepoints too (they fold to slots since no registers
  survive the regmask — wait, use-operands are reads BEFORE the clobber so
  RA may put them in caller-saved regs as inputs; that location is invalid
  metadata for during-the-call. Two options: (a) keep operands and teach
  the frametable step to ignore register locations at clobber-all sites,
  relying on the crossing-slot listing for the real roots; (b) no operands
  + extend family identification to "any vreg in any gc-live bundle at ISel
  time" recorded into a side table (the gc bit, below) so families never
  depend on operands. (b) is cleaner; the gc bit is needed anyway.)
  Values dead after the call need nothing (uniform with the dead-C-call-arg
  change).

- Exnroots/invokes: UNCHANGED for now (alloca-based slots, current RS4GC
  machinery). Only CallInst statepoints switch to in-place initially.

- Runtime: zero changes. In-place updates are already its semantics.

Deletes (eventually): ISel statepoint spill slots + findPreviousSpillSlot +
StatepointStableRootHomes, tied-def emission + relocate copies,
FixupStatepointCallerSaved's oxcaml role, RS4GC call-homes
(`-rs4gc-oxcaml-value-slot-homes` call bit). Both halves of the redundant
spill-store problem (loop re-spills, join slot-shuffling) disappear
structurally.

## The new invariant and its hazards (the real work)

In-place means A VREG'S VALUE CHANGES AT STATEPOINTS WITHOUT A DEF. MIR
passes assume a vreg's value is constant over its live range. Two hazard
classes:

1. Code motion: MachineLICM / MachineCSE / MachineCopyPropagation /
   MachineSink can hoist, merge, or forward a gc-derived computation
   (e.g. `ADDXri %gcreg, 8`) or a copy of a gc vreg across a statepoint.
   With in-place semantics the same vreg is loop-invariant-looking, so
   LICM WILL want to hoist derived address computations out of call-bearing
   loops -> stale interior pointer after GC. (AArch64 addressing folding
   keeps most adds inside ldr/str, which is safe — the load executes
   atomically with its address compute — but standalone hoisted adds are a
   real hazard.) NOTE: register roots at alloc sites ALREADY rely on a
   weaker version of this invariant today (an RA copy of a gc value in a
   second preserved register crossing call_gc unlisted = the same bug class
   we fixed for slots); there has never been a verifier for it; young-flip
   gates pass empirically.

2. Rematerialization: InlineSpiller remat of a gc-pointer-producing
   instruction across a statepoint reproduces the PRE-GC value. In practice
   remat candidates are almost all statics (ADRP+ADD of symbols — exempt,
   GC does not move statics); loads are not remat'd. Needs a gate anyway.

Both need the same primitive: a GC BIT ON VREGS — set at ISel from gc-live
bundles (+ relocate identity values), OR'd by RegisterCoalescer on merge,
inherited by LiveRangeEdit::createFrom for split/spill siblings (VRM
getOriginal family is the existing approximation used by
OxCamlStatepointSpillRoots). Derived-ness can be a second bit or
conservatively the same bit (any vreg computed FROM a gc vreg by a non-load
instruction is "derived"; derived must not cross statepoints at all —
RS4GC's remat-before-statepoint maintains this at IR level; the verifier
checks it at MIR level).

## Verifier (step 0, the keystone)

A machine function pass (flag-gated, run after RA in checking builds):
for every statepoint in an oxcaml function,
- every gc-family location live across (registers via LiveIntervals + VRM
  assignment or post-rewrite scan; slots via LiveStacks) is listed in the
  statepoint's operands/descriptor;
- no gc-DERIVED vreg's live range crosses any statepoint;
- at clobber-all sites no gc family is register-resident across (regmask
  should guarantee; verify anyway).
Run it on the CURRENT pipeline first: it measures how often today's code
already violates the weak invariant (preserved-reg copies across call_gc).
Plus the runtime gate that catches everything empirically:
OXCAML_YOUNG_FLIP=1 OCAMLRUNPARAM=s=4k (minor heap flips between two
spaces, retired space PROT_NONE'd, stale young dereference faults at the
guilty instruction; committed, env-gated; 16K pages need s>=4k).

## Step 0 status: BUILT (2026-06-11), first corpus results

Implemented (LLVM only, no codegen change; lit gate = exactly the known 20):
- gc bit on vregs: `MachineRegisterInfo::{set,is}OxCamlGCPtr` (DenseSet).
  Seeded in `InstrEmitter.cpp` after STATEPOINT emission (gc-section vreg
  operands + relocate def vregs; gated on gc oxcaml/ocaml). Inherited in
  `MRI::cloneVirtualRegister` (covers LiveRangeEdit split/spill siblings).
  OR'd in `RegisterCoalescer::joinCopy` after updateRegDefsUses. Cleared in
  clearVirtRegs. Not serialized to MIR.
- `lib/CodeGen/OxCamlGCRootVerifier.cpp`: report-only pass right after
  OxCamlStatepointSpillRoots (LiveIntervals/LiveStacks/VRM/SlotIndexes).
  Flags `-oxcaml-gc-root-verifier` (default off) and `-...-fatal`.
  GC set = MRI bits + statepoint gc operands + closure over COPYs (operands
  missing the bit are reported as "bit-propagation gap"). Derived set =
  defs of non-copy/non-load/non-statepoint instrs with gc/derived uses.
  Checks per statepoint (live across = liveAt(getRegSlot(true)) &&
  liveAt(getRegSlot())): gc vreg in unlisted register; gc vreg in
  regmask-clobbered register; gc family slot (listed slots anywhere +
  VRM original slots of all gc vregs) unlisted; derived vreg crossing.
  Smoke lit test: test/CodeGen/AArch64/oxcaml-gc-root-verifier.ll.
  Detector sanity: with `-oxcaml-statepoint-spill-roots=0` it finds 41
  slot violations in closure_conversion (the parser bug class).

Corpus run (841 llstash modules, DEFAULT pipeline, i.e. with the landed
spill-roots fix active): 198 unlisted gc family slots in 22 modules;
94 unlisted registers in 52 modules; 58 derived; 313 bit gaps; 0 clobbered.

- FINDING A (real, MIR): residual parser-bug class. RA re-spills an
  ISel-slot-resident gc value into a SECOND slot that crosses statepoints
  unlisted. closure_conversion close_c_call0: %2134 = LDRXui %stack.0
  (listed ISel slot, GC updates it) -> STRXui %stack.27 -> reloads after
  statepoints; %stack.27 stale. Operand-derived family collection in
  OxCamlStatepointSpillRoots cannot see it (family never a statepoint reg
  operand). FIX: extend collectGCSlots to bit-based families (VRM original
  slot of every gc-bit vreg). Caveat before listing from bits: bit taint
  via coalescing means a listed slot might hold a non-value range; phi/copy
  merging preserves "is an OCaml value", so believed safe — re-validate
  with young-flip + stage gates.
- FINDING B (real, IR — NOT fixable in MIR): llvmize emits mutable-field
  writes as `store volatile i64` with `ptrtoint` of the value; LLVM merges
  paths into i64 phis; the i64 gc pointer crosses caml_call_gc invisible
  to RS4GC and is stored into the heap stale.
  final_module_block_representation (closure_conversion, line 3934):
  `%52 = ptrtoint %.0207`, phi with tagged immediates, crosses
  statepoint_token202 (caml_call_gc — GC ALWAYS runs there), then
  `store volatile i64 %.2148`. %.0207 is a closure env value (also stored
  as ptr addrspace(1) elsewhere). FIX direction: llvmize should emit
  volatile value stores typed `ptr addrspace(1)` (cast at the store, not
  before), so RS4GC sees liveness. Probably explains most of the 94
  register + many derived reports.
- FINDING C (verifier imprecision): bits are per-VREG; after phi-elim/
  coalescing one vreg can hold unrelated ranges (asmlink %268: MOVi32imm 1
  range crosses, STRXpre writeback def elsewhere tainted the vreg) ->
  derived category is noisy. Per-VNInfo (per live-range def) tracking is
  the refinement if the verifier should become fatal-by-default.
- Bit gaps (313): vregs created by TwoAddress/PHIElimination via
  createVirtualRegister (no inheritance) that did not coalesce back into a
  marked vreg. Harmless for the verifier (closure covers), but must be
  fixed (hook those passes or refresh pass) before step 2 code-motion
  gates rely on the bit alone.

Operational notes: the 8th lit failure `statepoint-call-lowering.ll` is
pre-existing and outside the gate filter (verified identical codegen
before/after via saved before-5 clang). llstash/test-cc.sh is now stale
(stash symbol suffixes vs rebuilt stage tree -> link error); superseded by
stage2 + ocamltest + young-flip gates.

## Step 0 follow-up fixes (2026-06-11, same session)

1. FINDING A FIXED: `collectGCSlots` now derives families from the gc bit
   (all marked vregs + statepoint operands + COPY closure -> VRM original
   slots), not just statepoint reg operands. Corpus slot violations
   198 -> 0.
2. FINDING B RETRACTED as a bug: the examined value (`field_count` in
   final_module_block_representation) is statically a TAGGED IMMEDIATE
   (flambda2 kinds it; the other phi arm is `or ...,1`). llvmize's i64
   typing is justified; crossing as i64 is safe. The general lesson: a
   gc-marked vreg crossing unlisted is only a bug if its VALUE is listed
   at that statepoint (RS4GC says maybe-pointer) — then the unlisted copy
   diverges after relocation. Verifier refined with value connectivity
   (EquivalenceClasses over COPY edges, spill load/store FI edges, VRM
   original/slot edges) to split "second location of a listed value"
   (real) from "family not listed at all" (immediate by IR typing; info
   only, not fatal).
3. REGISTER SECOND-LOCATION CLASS FIXED the same way as slots:
   OxCamlStatepointSpillRoots (flag `-oxcaml-statepoint-register-roots`,
   default on) now appends every gc vreg that is live across a statepoint
   in a regmask-PRESERVED physical register and not already listed, as a
   plain register use gc operand -> frametable register root; the GC
   updates the register in place (this pilots the in-place semantics of
   step 1 early). Guards: dual liveAt check, regmask clobber check
   (excludes all clobber-all sites incl. raise), IMPLICIT_DEF reaching
   defs skipped, unscannable regs (x16-x18/x26-x28) spilled later by
   FixupStatepointCallerSaved as for any listed operand. Confirmed real
   instance it fixes: Misc.loop_77 `%20 = COPY %173`, %20 tied-def listed
   at call_gc, %173 lives across in $x13 used after (frametable now lists
   root 27 = x13). Misc case value may be an int index (Val-kinded
   conservatively); listing is safe either way (GC tag-checks).
   Corpus register violations 94+10 -> 0.

Post-fix corpus state (841 modules): 0 slot, 0 register, 0 clobbered;
58 derived (per-vreg taint imprecision, needs per-VNInfo to be
actionable); 313 bit-propagation gaps (TwoAddress/PHIElimination create
unmarked vregs that don't always coalesce back — fix before step 2 gating
relies on the bit alone). Lit gate: exactly the known 20 throughout.

## Per-VNI value analysis added to the listing pass (2026-06-11, later)

Listing locations by per-register gc taint is UNSOUND: after coalescing
and phi elimination one register holds different VALUES in different live
ranges, so a tainted family's location can contain a raw integer at the
statepoint; listing it hands the GC a garbage root, and bits that alias a
young address make the GC forward garbage and corrupt the heap (silent at
GC time, victim faults later, far from the culprit). `GCValueness` in
OxCamlStatepointSpillRoots.cpp now decides per live-range value:
- value seeds: statepoint tied defs; listed statepoint reg-operand VNIs
  at their statepoint; loads from ISel statepoint pool slots (listed FIs
  with no LiveStacks interval — those only ever hold spilled gc values);
  entry-block ABI copies of addrspace(1) formal arguments (new ISel-side
  marking: MRI::setOxCamlGCArg from LowerArguments; inherited through
  clone/coalesce);
- inherited per-VNI through COPYs and reloads; phi-defined VNIs require
  all predecessors to be values (optimistic recursion, sound because
  every live cycle is entered through a dominating def);
- spill slots are summarized per-FI (LiveStacks VNIs carry no def
  location): value-holding iff every isStoreToStackSlot store to the slot
  stores a value (slots are per-original, so this loses little);
- everything else (arithmetic, immediates, ordinary loads) is non-value
  and never listed. NOTE: tagged-immediate values that travel as i64
  (flambda2 kind says immediate, e.g. Misc.loop_77's index arg and
  field_count) are correctly classified non-value and skipped — the OLD
  unconditional listings of those were harmless luck, and my earlier
  classification of Misc.loop_77 as a "real bug fixed" was wrong in the
  same way as the field_count retraction.
Env-gated debug hooks (temporary, uncommitted): OXSR_DEBUG_ARGS (arg
seeds at ISel), OXSR_DEBUG_VAL (seed/pool summary per function).
The VERIFIER does not yet know the value filter and re-reports the
deliberately skipped non-values (corpus: 340 slot / 21+17 register
reports of this kind) — port GCValueness to the verifier or expect that
noise.

## LATENT PRE-EXISTING MISCOMPILE FOUND (2026-06-11) — gate blocker

While validating the fixes, stage builds SEGV'd nondeterministically.
Deterministic repro built: a BOOT-PROFILE compiler binary compiling
typing/typecore.ml under OXCAML_YOUNG_FLIP=1 OCAMLRUNPARAM=s=4k dies
SIGBUS ~100% (stale young pointer read in Ctype.lower_contravariant,
`ldr x1,[x10]`, x10 loaded from a slot that IS listed at every
intervening statepoint — and the C-call/call_gc descriptors all list the
right slots, suggesting a frame-walk desync or heap corruption upstream).
Bisection matrix: boot compilers built with TODAY's clang, yesterday's
fix-A-only clang, BEFORE-5 clang, and CLEAN-HEAD BASELINE clang all crash
8/8; the native-backend compiler (current sources) and the release-profile
_install (yesterday's clang) pass. So the bug is PRE-EXISTING (at least
as old as the baseline), LLVM-backend-specific, code-shape sensitive
(boot dune profile triggers it), and NOT caused by any of this week's
work — every previous green gate shipped it. Repro harness:
  cd <boot tree>/default && OCAMLLIB=.../_install/lib/ocaml \
  OXCAML_YOUNG_FLIP=1 OCAMLRUNPARAM=s=4k ./boot_ocamlopt.exe \
  -args0 /tmp/repro.args0   # dune-verbose-captured typecore command
Status: ROOT-CAUSED AND FIXED (2026-06-11, two distinct compiler bugs).

Forensic method that cracked it (reusable): break at minor GC #N-1
(count first), memory-scan the whole process for the stale value V0
pre/post `caml_empty_minor_heap_promote` (lldb python), classify the
missed holders by region (current stack / cached stacks / heap), then
SIMULATE the runtime's stack walk (First_frame=sp+16, ret=*(sp-8),
descr=caml_find_frame_descr via expression eval, sp+=frame_size) and
check each missed holder's frame offset against its descriptor's
live_ofs. Result: live current-stack slots at sp+0x30/sp+0x20 of
expand_abbrev_gen / lower_contravariant frames were NOT LISTED.

BUG 1 (ancient, the typecore crash): values whose ONLY statepoint
presence is an EXNROOT ALLOCA (try-wrapped calls list roots via the
gc-alloca section; numGCPtrs=0, no register operands anywhere) get
reloaded from the exnroot and RE-SPILLED by the allocator into ordinary
spill slots crossing the statepoint (`STRXui %580, %stack.15` right
before OXCAML_PUSH_TRAP + invoke caml_apply2, ctype.ml:1198) — and
NOTHING listed those slots: RS4GC tracks the value only through the
alloca; ISel never sees it; the operand-derived family collection of the
original spill-roots pass cannot reach it. Present since (at least) the
clean-head baseline. FIX: value-home FIs — gc-alloca-section slots
(exnroots) and ISel pool slots are runtime-maintained value homes; loads
from them seed both family membership (computeGCRegs) and value-ness
(GCValueness), in the pass AND the verifier.

BUG 2 (introduced by the wider listing, caught by young-flip turning
SIGBUS into a GC-side SIGSEGV on value -144 = a leftover local_sp):
LiveStacks liveness is PATH-INSENSITIVE. A loop-carried slot whose store
sits later in the loop body is "live across" a statepoint at the loop
head, but holds UNINITIALIZED garbage on the first iteration
(Typecore.apply_contains_r: %stack.2 stored at 1144B, listed at the
528B call_gc; pre-store content was C-stack debris -144; the GC darkened
it as a "block" and crashed). Listing without init is unsound; NOT
listing re-creates the iter>=2 staleness. FIX: when a listed slot has no
store dominating the listing statepoint, the pass emits an entry-block
initialization storing a known-valid value (the ABI register of an
addrspace(1) formal argument, via storeRegToStackSlot) and extends the
slot's LiveStacks interval so stack slot coloring cannot pack another
slot into the now-meaningful gap. The pass is therefore no longer
metadata-only (one init store per affected slot, entry block only). If
no gc argument exists, such slots are skipped (verbose-reported).
NOTE: the ORIGINAL landed pass had the same uninitialized-listing hazard
latently (narrower families); the dominance+init guard now applies to
all listed slots uniformly.

Validation: typecore+flip repro 15/15 green on the fixed boot compiler
(was 100% SIGBUS); -verify-machineinstrs clean; lit at the known 20.
The typecore+flip repro MUST join the validation gate alongside
parser.pp.ml. Verifier/pass disagreement remains (the verifier lacks the
GCValueness filter and over-reports ~800 slot candidates the pass
correctly skips as non-values) — port GCValueness into the verifier or
treat slot reports as candidates only.

BUG 3 (the release-profile stage1 tail — why vreg lowering was disabled):
the cont pattern. At ALLOC statepoints the tied-def vreg lowering forces
~18 live values into registers (reloaded from their ISel pool slots right
before `sub x27`), lists them as register roots, and after the GC jumps
to the shared retry continuation WITHOUT store-backs — the pool-slot
copies stay live (reloaded later) and unlisted. The pass cannot fix this
post hoc: pool slots have no LiveStacks intervals, and the dedicated
ValueHomeLiveness dataflow added for them (loads/stores/listings + exact
all-blocks liveness and defined-ness; over- AND under-listing are both
unsafe for these multiplexed slots) correctly handles the boot-profile
shapes but the multi-location surface kept producing new release-profile
instances (forensics: cont_96_350 frame, descriptor 18 regs + 2 slots,
live holders at sp+168/240 unlisted). DECISION: MaxVRegPtrs defaults to
0 for oxcaml too (StatepointLowering.cpp; -max-registers-for-gc-values
overrides; oxcaml-alloc-statepoint-register-roots.ll updated to pass the
flag) — every gc value keeps a single canonical ISel slot location at
every statepoint until step 1-2 lands in-place semantics. The perf cost
is the price of correctness for now; in-place recovers it properly.

BUG 4 (deterministic, exposed by slot-only mode; ancient in any code
shape that hits it): **StatepointStableRootHomes seed stores on critical
edges**. maybeStoreStableStatepointRootSeed emits the seed store at the
END of the incoming block (phi-copy placement). When that block has
another successor (critical edge), the store also executes on the other
path, clobbering the home while OLDER ssa values of the phi still read
it. Found as a deterministic miscompile of Parmatch.pressure_variants
(bb.14 stored the loop phi's next seed and 3% of the time exited to a
path that reloaded the previous value -> close_variant never called ->
closed polymorphic-variant matches reported non-exhaustive with
`AnyOtherTag; every boot/stage build died on -warn-error). FIX:
shouldUseStableStatepointRootHome refuses the home unless every seed
edge is the incoming block's sole outgoing edge.
Hunt method (reusable): 10-line poly-variant repro file; module-level
bisect via a wrapper that switches -max-registers-for-gc-values per
module (parmatch.ml); function-level bisect via the OXSR_VREG_FUNCS env
hook in StatepointLowering (pressure_variants_196_549, ~9 rounds);
lldb call-trace diff (close_variant 1 vs 0 hits) -> slot event analysis
-> bb.14's storeless seed write on the critical edge.

BUG 5 (defensive, found en route): stable-home FIs were registered in
FuncInfo.StatepointStackSlots, making them allocatable as ordinary pool
slots by allocateStackSlot at statepoints that don't reference the home.
Fixed: homes are no longer pooled; reservePreviousStackSlotForValue
handles non-pool (home) indices without per-statepoint reservation.

Debug hooks added (env-gated, harmless): OXSR_VREG_FUNCS (per-function
vreg lowering for bisection), OXSR_DEBUG_VHL/OXSR_DEBUG_ARGS/
OXSR_DEBUG_VAL traces.

BUG 6 (2026-06-11/12, the remaining stage1 flip-stress class): the
GCValueness PER-FI slot summary ("value iff EVERY store to the FI
stores a value") is wrong after StackSlotColoring, which runs BEFORE
the pass and merges disjoint live ranges of DIFFERENT vregs into one
frame index (and collapses LiveStacks VNIs to one, so per-VNI precision
is unrecoverable). A slot reused for a gc value on one range and raw
data on another was never listed -> live young pointer skipped at an
alloc statepoint -> stale after GC. Forensic confirmation (typecore
flip repro, GC #109381): V0 lived in cont_96_350(Closure_conversion)'s
frame at sp+0x20/sp+0x70 (%stack.32 family; pass verbose said "skip
non-value %stack.32"), in listed sp+0x80 (updated fine), and in the
gc_regs bucket slot 1 = x1 (dead-across, refreshed from sp+0x80 after;
the bucket was the mysterious "heap holder" of the earlier scan — see
gcl/gcwatch commands in /tmp/gcscan2.py).
FIX (OxCamlStatepointSpillRoots.cpp, GCValueness): replaced the per-FI
summary with PER-QUERY REACHING-STORE analysis (lastStoreBefore in
block, else AND over per-pred block-exit value-ness, memoized,
optimistic on cycles and on store-free paths from entry — those are
guarded by the dominance check + entry-init). Value-ness of a store
source needed five new evidence rules:
 1. pool/exnroot store seed: a vreg stored into a ValueHomeFI is a gc
    value (ISel only spills gc-section values there; llvmize deopt args
    are constants so the pool never holds raw data). Covers values
    whose def is structurally opaque, e.g. the alloc fast path's
    STRXpre write-back that forms the block pointer (cont_96's case).
 2. MOOxCamlGCValue MMO flag (NEW, MachineMemOperand.h MOTargetFlag3,
    set in TargetLoweringBase::get{Load,Store}MemOperandFlags when the
    IR moves a ptr addrspace(1), gc oxcaml/ocaml only): p1-typed heap
    loads/stores are recognizable post-RA. regValue trusts flagged
    single-def loads; the constructor seeds flagged stores' sources.
 3. entry physreg arg spill: STRXui $x7, %stack.N at entry with no
    prior def of x7 — value iff MRI livein vreg isOxCamlGCArg (the
    spiller folds the arg-copy def into a direct physreg store).
 4. statepoint-result physreg spill: STRXui $x0, %stack.N right after
    a STATEPOINT that implicit-defs x0 — value iff the DIRECT callee's
    llvmize return type {{i64,i64},{results...}} has p1 at the element
    riding that x-register (float elements use d-regs and are skipped
    in the mapping). Plus consumer evidence (a reload of the same
    content whose VNI is seeded) as a fallback.
 5. odd move-immediates are tagged OCaml scalars (values the GC
    ignores); rejecting them poisoned loop phis seeded with 1 whose
    other arms carry real pointers (Parmatch.combine %332 register
    case).
Corpus after fixes (ccmain2/ltf/parmatch/typecore IR, exact build
flags -O3 -ffixed-x15 -ffixed-x26 ...): every remaining "skip
non-value" verified benign by hand — wrap_over_application %stack.22/23
hold Int_ids.add results typed {{i64,i64},{i64}} (genuine immediates),
number_18 %stack.9 holds a LOADgot static pointer (immobile). NOTE:
the earlier llc-without-build-flags corpus runs were misleading
(register pressure differs without -ffixed-x15/x26); always replicate
the clang-wrapper.log flags.
Verifier over-reporting note: the entry-init LiveStacks extension makes
slots look live from function entry, so the verifier now flags
pre-first-store statepoints (content = the init value, never read by
the program) — verifier FPs, would be silenced by porting GCValueness
into it (parked).

BUG 7 (2026-06-12, found because BUG 6's fix moved the crash INTO the
GC): frame-descriptor REGISTER roots (odd live_ofs entries) are
resolved by scan_stack_frames against `regs = gc_regs` — the
SAVE_ALL_REGS bucket that ONLY the alloc-family runtime entries
(caml_call_gc / caml_call_local_realloc / realloc_stack, the
OxCaml_Alloc CC) populate. RESTORE_ALL_REGS frees the bucket WITHOUT
clearing Caml_state->gc_regs, so at a GC triggered inside a C CALL
(CSR_AArch64_OxCaml_C_Call preserves x19-x26, so values CAN sit there)
a register entry reads the last FREED bucket = garbage. My register
appending (the earlier 94->0 fix) had been adding such entries at
C-call statepoints; crash: Regalloc_irc.run's descriptor at
`bl caml_c_call` (ret 0x10041afbc) listed reg16=x19; a C-primitive
allocation GC'd, the walk oldified gc_regs[16] = stale residue ->
SIGBUS in get_header_val. FIX: register roots are appended ONLY at
alloc-family statepoints, identified by their regmask preserving X0
(no other OxCaml callee CC does); the verifier's "second unlisted
register location" finding is gated the same way and downgraded to
info elsewhere. Soundness note: genuinely-AS1 values never cross
non-alloc statepoints in registers in slot-only mode (RS4GC spills,
uses-after are relocate reloads); what crosses C calls in CSRs is
machtype-Int data (immediates), which needs no listing. A future
verifier check could assert the former invariant.

## Staging plan (each step full-gated)

Flag: `-oxcaml-statepoint-inplace` (ISel-level), default off until proven.

- Step 0: gc/derived bits on vregs + verifier. No behavior change. ~2-3d.
- Step 1: in-place lowering for ALLOC statepoints only (preserving CC =
  smallest semantic delta; relocates identity; tied defs gone; operands
  foldable). Expected win: cont-class forced-reload blocks disappear.
  ~3-5d + validation.
- Step 2: ordinary calls (drop ISel spilling + stable-home machinery from
  the path; slots via OxCamlStatepointSpillRoots; LICM/CSE/copy-prop/remat
  gates active for gc vregs). Expected win: loop re-spills + join
  shuffling gone (boyer tautologyp: 3 stores/iteration; kb rporec/unify).
  Riskiest step. ~1-2w incl. soak.
- Step 3: delete dead machinery; THEN revisit exnroots/homes on the
  simplified base (gc-ness + RA could subsume exnroot slots later by
  modeling raise edges as clobber-all, forcing handler-live values into
  listed family slots — out of scope for now).

## Validation gate (the full checklist used for the two landed commits)

1. lit: `llvm-build/bin/llvm-lit -j8 test/CodeGen/AArch64/oxcaml*
   test/Transforms/RewriteStatepointsForGC` — expect exactly the 20 known
   pre-existing failures (list in oxcaml-llvm-workspace-map memory; the two
   arg-root tests are dual-mode now).
2. Single-module repro: `bash ../llstash/test-cc.sh ""` -> REPRO-EXIT 0.
3. Young-flip: stage compiler on parser.pp.ml, OXCAML_YOUNG_FLIP=1 s=4k/1k
   (cwd `_llvm_self_stage2_boot_build/default`).
4. Stage builds: `STAGE0_INSTALL=$PWD/_install LLVM_WRAPPER=$PWD/../clang-wrapper
   tools/build-llvm-self-stage-install.sh`; stage2 = same with
   STAGE0_INSTALL=_llvm_self_stage_install and all dirs ->
   `_llvm_self_stage2_*`.
5. `SELF_STAGE=2 LLVM_WRAPPER=$PWD/../clang-wrapper
   tools/run-llvm-stage5-ocamltest.sh` — bar: 6756 passed / 0 failed.
   (If the fake root's runtime is stale vs runtime/ changes:
   `make -s runtime-stdlib` refreshes `_build`.)
6. Benches (OCAMLOPT=_install/bin/ocamlopt.opt, OCAMLLIB=_install/lib/ocaml,
   LLVM_PATH=../clang-wrapper):
   micro `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run.py`
   (44 cases; baseline geomean 0.652, total 0.703);
   minibench `agent-state/test-suite-29e4cd/minibench_suite/run.py`
   (baseline geomean 0.886, total 0.868; CASES=a,b env selects);
   compiler bench `agent-state/test-suite-29e4cd/run_compiler_bench.py`
   (baseline module-median ratio 0.9715, round-total 0.9699, vs
   `_native_install` — do not rebuild the native side).
   Tiny cases swing +/-15% from layout luck; rerun CASES=... 4-5x before
   believing any single-case delta; layout randomization via ld64
   `-order_file` with shuffled symbol lists is the unbiased option (not
   built yet).

## Known leftovers / parked items

- Redundant spill stores: fixed structurally by step 2; do NOT build the
  ISel PHI-home generalization if going the RA route.
- Exception-path residuals (try/raise probes 1.05-1.09, layered ~1.07,
  boyer_like_failed_unify ~1.24 layout-stable): trap push is 5 instrs vs
  native's 3 (stp form measured WORSE June 8 — but that experiment was
  layout-noisy; retest under randomization); raise materializes exn into
  x8 then movs to x0 (regalloc hint); raise path is branch-taken (unwind
  coldness heuristic).
- `make -s fmt` reformats ~15 unrelated files (formatter drift) — revert
  unrelated churn before committing, keep only intended diffs.
- bench harnesses dump asm + post-RS4GC IR to
  `minibench_suite/inspect/*.{s,ll}` and micro `.build/inspect/` on every
  run — use these instead of recompiling by hand. Frametable decoder:
  /tmp/ftparse.py pattern (parse `.long LtmpN-...` + `.short` framesize/
  numlive/roots; odd root = register, even = sp offset).
