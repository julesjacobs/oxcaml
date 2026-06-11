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
