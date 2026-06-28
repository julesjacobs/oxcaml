# Latent parser.pp.ml GC miscompile — findings

## Symptom
Any LLVM-built stage1/stage2 `ocamlopt` compiling `parser.pp.ml` with
`OCAMLRUNPARAM=s=1k..64k` segfaults (flaky at default params — blocked stage2
twice). Pre-existing at baseline; independent of all recent RS4GC work.
Surface crash: `Ident.compare` called with junk args (0, 1) far downstream.

## Root cause (PROVEN)
**LLVM's register allocator keeps GC values live across statepoints in
locations the frametable descriptor does not list, so the GC relocates some
copies of a pointer and not others.**

Concrete instance (current clang, `Closure_conversion.cont_96_350`, alloc
statepoint `Ltmp951`, MIR block `bb.31.L10146`, statepoint-id 393217):
- The statepoint lists 18 register gc operands (tied-defs, relocated by the
  GC via register roots) plus two operands InlineSpiller correctly folded to
  spill slots (`%stack.31/32` → sp+192/200 after coloring) — all correct.
- But other spill slots (e.g. greedy `%stack.34` → sp+184) also hold GC
  values whose `LiveStacks` intervals cross this statepoint. They belong to
  *different split families*: values that are statepoint operands elsewhere
  in the function, spilled on one path and reloaded on a path that passes
  through this statepoint (the merged home store-back block `L10167`
  reloads them and writes them into the RS4GC home slots).
- A minor GC at Ltmp951 rewrites every listed location; the unlisted
  crossing slots keep pre-GC young addresses → a stale `simple` is written
  into a home slot → flows into `Env.add_simple_to_substitute`
  (closure_conversion.ml:1467) → substitute map → `Lambda_to_flambda.cps`
  Lvar lookup dereferences it (SIGBUS at cps+396 under the young-flip
  instrument).

Why stock LLVM never hits this: caller-saved registers die at calls, GC
pointers in callee-saved registers are force-spilled into *listed* slots
(`FixupStatepointCallerSaved`, AllowGCPtrInCSR=false), and relocate
tied-defs end the input interval at the statepoint. OxCaml's
register-preserving alloc calling convention re-opened the hole: values stay
in registers across allocation points, so their spill-slot copies cross
statepoints that never reference them as operands.

Dead ends ruled out along the way: the "+24 descriptor arithmetic" theory
(192/200 ARE correct folded listings — verified by tracking slot ids across
StackSlotColoring: statepoint operands and reload instructions are remapped
consistently); offset conventions verified exact with the synthetic harness
(`llstash/synth-statepoint-offsets*.ll`): emitted stack root == true
SP-offset + 16·(trap depth encoded in statepoint-id bits 1–3).

## Detection instrument (committed, env-gated, zero default cost)
`OXCAML_YOUNG_FLIP=1` + `OCAMLRUNPARAM=s=4k`: minor heap alternates between
two spaces each GC; retired space is PROT_NONE'd → any stale young-pointer
dereference faults at the guilty instruction. Deterministic under lldb.
This (not frametable-guided scanners, which only check listed locations)
is the verification gate for the fix.

## Fix (implemented)
Metadata-only — zero mutator instructions, registers still preserved across
allocations. New machine pass `OxCamlStatepointSpillRoots`
(`llvm/lib/CodeGen/OxCamlStatepointSpillRoots.cpp`), inserted in
`TargetPassConfig::addRegAssignAndRewriteOptimized` between the register
allocator and `VirtRegRewriter`, while `VirtRegMap` + `LiveStacks` are alive.
Gated on `gc "oxcaml"`/`"ocaml"` functions; flag
`-oxcaml-statepoint-spill-roots[-verbose]`.

1. Identify GC slot families globally: every slot referenced as a folded gc
   operand at any statepoint, plus `VRM.getStackSlot(VRM.getOriginal(V))`
   for every gc register operand `V` of any statepoint. (Spill slots are
   per-original until StackSlotColoring — which runs later and remaps all
   references consistently — so these are exactly the slots that can carry
   GC values across statepoints.)
2. At each statepoint, append every GC slot whose `LiveStacks` interval is
   live both entering (early-clobber slot index) and during (register slot
   index) the statepoint and that isn't already listed: an
   `(IndirectMemRefOp, 8, FI, 0)` gc operand + `(i,i)` gc-map pair, counts
   bumped, with a FixedStack `(load store)` memoperand.
   The entering-liveness requirement excludes slots whose live range starts
   AT the statepoint (folded relocate-def spills) — they contain garbage on
   entry; the machine verifier caught this in the first iteration.

StackMaps then emits each appended slot as an ordinary Indirect location →
frametable stack root → the GC updates the slot copy too.

Soundness: spill stores happen at defs, which dominate reloads, so a slot
live-into a statepoint holds a valid copy of its family's value; it gets
updated at every statepoint it crosses, staying in sync with the register
copy.

## Verification status
- [x] machine verifier clean on closure_conversion (`-verify-machineinstrs`)
- [x] `llstash/test-cc.sh ""` (single-module swap repro): exit 0 (was SIGSEGV)
- [x] OXCAML_YOUNG_FLIP=1 s=4k/1k gate: fault at cps+396 gone, cmx produced
- [x] Ltmp951 descriptor now lists the crossing slot (sp+184) in addition
      to 192/200; 102 slots appended module-wide in closure_conversion
- [x] lit: failures are exactly the 20 known pre-existing ones (no new)
- [x] full self-stage1 AND self-stage2 rebuilds pass (stage2 had failed
      twice in a row on this bug before the fix)
- [x] parser.pp.ml s=1k/4k/16k/64k clean on BOTH fresh stage compilers;
      young-flip s=4k clean on the stage2 compiler
- [x] SELF_STAGE=2 ocamltest: **6756 passed / 284 skipped / 0 failed**
      (`agent-state/.../ocamltest_stage2_spillroots_clean_20260610.log`) —
      the stage2 bar, previously unreachable. Two infra issues fixed on the
      way (both fallout of the earlier committed prologue-entry rename to
      `caml_llvm_call_realloc_stack_stkarg`, not of this fix): the
      ocamltest fake root's runtime from `_build` predated the symbol
      (`make -s runtime-stdlib`), and
      `stack_check_size_contract.sh`/`challenges.sh` grepped for the old
      prologue slow-path symbol (updated, keeping "ordinary check" greps
      from matching the new one).
- [x] minibench unchanged: total LLVM/native ratio 0.875 (v5 baseline
      0.885); boyer 0.99, all cases ≤ 1.0
      (`agent-state/.../minibench_spillroots_20260610.log`)
