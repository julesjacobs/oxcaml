# Progress

2026-06-29 scope correction: do not count generic optimization-level changes
such as `-O4` toward the compiler-performance goal. Those would apply equally
to a native-built compiler, so the remaining work must improve the LLVM-built
path specifically: backend code generation, LLVM/BOLT handling, LLVM-only
profile/layout work, or another change that is not available to the native
build under the same benchmark setup.

2026-06-29 unroll follow-up: reran the no-loop-unrolling idea through the real
LLVM backend wrapper rather than relying on the earlier invalid benchmark.
Global `--disable-loop-unrolling` for the main compiler rebuild is rejected:
the wrapper logs show 271 optimizer invocations / 271 fresh IR compilations,
then the build failed while compiling `otherlibs/dynlink` and `.ocamlcommon`
with `allocation failure during minor GC` / `SEGV`; no
`_llvm_nounroll_clean_install/bin/ocamlopt.opt` was produced. Whole-runtime
stdlib no-loop is also rejected: the runtime stdlib build itself completed
with 74 fresh IR compilations, but a normal main rebuild against it failed
with the same minor-GC/SEGV pattern and produced no install. A narrower
`stdlib__Hashtbl.ll`-only no-loop wrapper matched exactly that stdlib IR file
once, but a normal main rebuild against the resulting stdlib still failed with
minor-GC allocation failures in `otherlibs/dynlink` and `.ocamlcommon`, again
with no installed compiler. Conclusion: the Hashtbl full-unroll code-size
pathology is real, but disabling loop unrolling, even only for Hashtbl in the
stdlib, is not a safe compiler-throughput improvement in this pipeline.
Artifacts are under
`agent-state/test-suite-29e4cd/unroll_investigation_20260629/`, especially
`build_nounroll_clean_main.log`, `build_nounroll_stdlib_main_normal.log`,
`build_hashtbl_nounroll_main_normal.log`, and the corresponding
`llvm-wrapper-*` / `opt-*` logs.

2026-06-29 BOLT ICP frametable investigation update: the current
LLVM-built-compiler-only BOLT path still does not meet the required `+6%`
target over the native-built compiler. Full BOLT indirect-call promotion can
create new promoted direct-call return PCs for OCaml-managed calls. Existing
descriptor retaddr patching is insufficient because those new return PCs did
not exist in the input frametables. Example failure:
`camlStdlib__List__concat_map_64_160_code` gained a promoted direct call whose
return PC `0x3c5a81b` had no descriptor, while the original fallback indirect
call return PC had one. Prototype ELF-level synthetic frametable growth can
make startup pass by redirecting one zero-count frametable pointer to appended
descriptors, but two tested descriptor-selection policies are not safe enough:
a broad callsite-matching synthesizer produced 12,228 new descriptors and then
failed compiling `backend/llvm/llvmize.ml` with `allocation failure during
minor GC`; a conservative direct-call/fallback-jump pattern produced 195 new
descriptors and still corrupted GC once the demand-driven set included the
descriptor for return PC `0x3b9993a`. This indicates that safe ICP support
needs a principled OCaml/BOLT frametable model for BOLT-created callsites, not
blind descriptor cloning.

Also tested a restricted `-indirect-call-promotion=calls
-icp-top-callsites=10` build. BOLT reported zero optimized callsites, so it
needed no synthetic frametable entries and passed both `-version` and a direct
`backend/llvm/llvmize.ml` compile. Benchmarking against the native-built
compiler on the five-module compiler-throughput workload gave only `+3.40%`
with `samples=7, inner_repetitions=3`
(`native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-icp-top10-inner3.json`),
worse than the existing best `cache-hfsort-peep-rodata` result (`+3.77%`).
Conclusion: ICP remains potentially LLVM-path-specific, but the safe top10
variant is effectively a no-op, and the real ICP variants are blocked on
correct frametable synthesis.

2026-06-29 rejected LLVM-path experiment: a targeted X86 hidden-flag prototype
that disabled folded stack-slot reloads after OxCaml statepoints fixed the
focused `loop_invariant_gc_across_call_dynamic_reps` shape locally (normal real
wrapper full-size run about `1.51x` LLVM/native; prototype about `1.02x` to
`1.09x`, depending on sample count). This confirmed that the microbenchmark
slowdown is post-statepoint X86 spill folding, not a frontend-root or generic
optimization-level issue. However, the prototype is not viable as implemented:
exception one-sample screening was mixed, benchmarksgame was mostly neutral,
and a real guarded self-stage boot build reproducibly crashed `llc` in greedy
register allocation/InlineSpiller while compiling `tools/simdgen/simdgen.ml`
and `external/owee/owee_elf_notes.ml` (`MachineOperand::getReg()` assertion)
even after excluding statepoint/patchpoint/stackmap pseudos. The experimental
source change and temporary wrappers were removed; normal `make -C
_build/llvm-tools -j2 llc` passes again. Any future fix must be more precise
than blocking broad post-statepoint folding, and must pass a real LLVM build
before benchmarking.

2026-06-29 BOLT follow-up: tested additional LLVM-built-compiler-only BOLT
variants; none reaches the `+6%` target, and none beats the existing best
`cache-hfsort-peep-rodata` result (`+3.77%`) on comparable inner-repetition
runs. Indirect-call promotion is not safe with the current OCaml frametable
patcher: both
`-indirect-call-promotion=calls` and the same with `-icp-old-code-sequence`
patched all existing descriptors but failed immediately at startup with
`caml_scan_stack: missing frame descriptor`. The missing PCs are newly-created
promoted-call return addresses, e.g. in
`camlStdlib__List__concat_map_64_160_code`; the patcher rewrites old
descriptors but cannot synthesize extra descriptors for BOLT-added callsites.
So ICP requires real frame-table growth/synthesis before it can be benchmarked.
Safe layout-only variants were neutral or worse than best:
`cache-hfsortplus-peep-rodata` starts and measured `+3.82%` on a short screen
but only `+3.35%` on `samples=5, inner=3`; `blocknormal-hfsort-peep-rodata`
starts but screened at `+3.21%`. Artifacts live under
`agent-state/test-suite-29e4cd/bolt_compiler_20260629/`, including
`native-current-vs-llvm-constfilter-cache-hfsortplus-peep-rodata-inner3.json`,
`native-current-vs-llvm-constfilter-blocknormal-hfsort-peep-rodata-screen.json`,
and the corresponding `constfilter-...icp...` logs.

2026-06-29 later BOLT follow-up: tested post-layout profile refinement and
register reassignment; neither is a keeper. Profiling the current best BOLTed
compiler worked. Artifacts live under
`agent-state/test-suite-29e4cd/bolt_compiler_20260629/`: `perf2bolt`
recognized a `boltedcollection` and emitted
`ocamlopt.constfilter.cache-hfsort-peep-rodata.profiled.lbr.fdata`. Reapplying
the best safe BOLT recipe to the original relocatable compiler with that
translated profile produced a runnable binary with complete frametable patching
(`patched 224543`, `unresolved 0`), but it screened at only `+3.70%`:
`native-current-vs-llvm-constfilter-refined-cache-hfsort-peep-rodata-screen.json`.
Merging the original and translated post-BOLT profiles is not valid as-is:
`merge-fdata` rejects mixed normal/`boltedcollection` inputs, and stripping the
marker makes BOLT report `98.0%` of samples in stale functions; that binary
starts but screens at `-0.49%` in
`native-current-vs-llvm-constfilter-original-refined-noheader-cache-hfsort-peep-rodata-screen.json`.
BOLT `-reg-reassign` is unsafe for this compiler binary: both the existing
`cache-hfsort-peep-rodata-frameopt-regreassign` artifact and a new isolated
`cache-hfsort-peep-rodata-regreassign` build patch all existing frame
descriptors but abort on `-version` with `allocation failure during minor GC`.
Current best stronger `inner_repetitions=3` result remains
`cache-hfsort-peep-rodata` at `+3.77%`; the likely next LLVM-only work is either
real frametable synthesis for BOLT-created callsites so ICP can be measured
safely, or a precise LLVM codegen improvement rather than more generic BOLT
layout variants.

2026-06-29 compiler-performance investigation update: the large
`Hashtbl.create`/capacity-growth code-size pathology is real, but the tested
global full-unroll cap is not a useful compiler-throughput lever. Local,
currently untracked workspace artifacts include captured `stdlib/hashtbl.ml`
LLVM IR in
`agent-state/test-suite-29e4cd/unroll_investigation_20260629/hashtbl_compile/`;
rerun the investigation rather than relying on those paths after a fresh clone.
`LoopFullUnrollPass` fully unrolls the bounded doubling loops (`L125` in
`camlHashtbl__create_inner_7_118_code`, `L4212` in
`camlHashtbl__create_83_232_code`), creating the long compare ladder. Local
ablations fix the assembly shape: `--disable-loop-unrolling` shrinks
`create_83` from 208 assembly lines to 58, and
`--unroll-full-max-count=41` does the same while cap 48 allows the ladder.
However, the whole-compiler build evidence only rejects the narrower full-unroll
cap 41, not no-loop-unroll generally: the direct no-loop-unroll build failed
with a minor-GC allocation failure, and the later install that measured `-6.45%`
logged zero wrapper invocations, so do not use that benchmark as evidence about
LLVM no-loop-unroll throughput. The full-unroll cap 41 build completed with 148
runtime and 2228 main wrapper invocations, smoked, and using the real binary
still measured `-5.21%` vs native in the local workspace artifact
(`native-current-vs-llvm-fullcap41-real-screen.json`). The same screen with
the current LLVM compiler is roughly parity with native (`+0.15%`,
`native-current-vs-llvm-constfilter-current-screen-rerun.json`). Do not pursue
a global full-unroll cap; no-loop-unroll would need a clean LLVM-path build
before it can be judged. Any future fix for this code-size issue must be more
surgical and rebenchmarked against the compiler workload before keeping.

Last updated: 2026-06-29. Important correction: the broad benchmark run in
`spill_fusing_effect/` used the temporary `llc-wrapper.sh`, which fed raw
pre-optimization LLVM IR directly to `llc`. That is not the project pipeline:
`tools/llvm-rs4gc-llc-wrapper.sh` runs
`default<O3>,rewrite-statepoints-for-gc,verify` before `llc`. The raw-wrapper
run made `matmul` look catastrophically slow because 211 frontend-style
`alloca`s survived to codegen. With the real wrapper, fresh `SAMPLES=7`
minibench results are `matmul` native `0.1158s`, LLVM `0.1062s`, ratio
`0.9171`, and `matmul_transposed` native `0.0920s`, LLVM `0.0659s`, ratio
`0.7162`. Treat `matmul` as roughly parity given native-sample variance; treat
the broad `spill_fusing_effect/` tables as invalid for
LLVM-vs-native performance and rerun any global spill-fusing ablation through
the real wrapper.

Full corrected rerun with the real wrapper (`SAMPLES=7,WARMUPS=2`) is saved in
`agent-state/test-suite-29e4cd/real_wrapper_full_bench_20260629/`. Across the
84 broad benchmark cases (minibench, benchmarksgame, exception microprobe),
total LLVM/native runtime ratio is `0.8822`, geomean `0.8840`, with 21 cases
slower than native, 10 above 1.05x, and 7 above 1.10x. Including the four
focused loop-invariant probes gives 88 cases total, total ratio `0.8858`,
geomean `0.8972`, 23 slower, 12 above 1.05x, and 9 above 1.10x. Largest broad
slowdowns are `exception/closure_call_many_handler_live_roots_raise` `1.1912x`,
`minibench/hash_batch_murmur_mix` `1.1807x`,
`exception/raise_caught_cross_function` `1.1734x`, `benchmarksgame/nbody_1`
`1.1401x`, and `exception/raise_payload_caught_cross_function` `1.1256x`.
The focused GC loop-invariant probes remain the largest overall slowdowns:
dynamic reps `1.5823x`, fixed reps `1.5735x`. Summary:
`agent-state/test-suite-29e4cd/real_wrapper_full_bench_20260629/summary.md`.

Slowdown deep dive completed for the top corrected cases. Artifacts and report:
`agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/report.md`.
Main classifications: loop-invariant GC probe is post-RA X86 spill-fusing of
scalar loop state around statepoints; exception slowdowns are
invoke/landingpad/trap-loop state placement rather than a pure GC root bug;
`nbody_1` is largely a float `sqrt` lowering gap (`sqrt@PLT` vs native
`vsqrtsd`, patched assembly closes about half the gap); `finance_greeks_pnl` is
a smaller call/statepoint-adjacent spill scheduling issue; `matmul` is healthy
under the corrected pipeline.

Current AMD64 investigation: the
`loop_invariant_gc_across_call_dynamic_reps` slowdown is caused by X86
spill-fusing of scalar reloads after an OxCaml statepoint/call, not by the GC
root stack-slot mechanism and not primarily by the chosen physical register
names. Pre-regalloc MIR is reasonable; greedy/inline-spiller split the scalar
live-through-call values, create ordinary scalar spill slots, and X86
`foldMemoryOperandImpl` folds the post-call accumulator reload into memory
arithmetic. `-disable-spill-fusing` fixes the focused microbenchmark
(`~0.410s -> ~0.237s` at `reps=20`; both no-hoist/no-fusing `~0.230s`), while
`-disable-spill-hoist` alone does not (`~0.408s`). Next step: rerun the
global spill-fusing ablation through `tools/llvm-rs4gc-llc-wrapper.sh`, then
prototype a narrow X86/OxCaml predicate that suppresses only harmful
statepoint-adjacent scalar spill folding, and add a focused MIR test that
still verifies GC roots remain stack roots. Detailed plan:
`amd64-arm-parity-plan.md`; raw/summary benchmark artifacts:
`agent-state/test-suite-29e4cd/spill_fusing_effect/summary.md` and
`summary.json` are raw-`llc` artifacts, not valid project-pipeline performance
results.

Previous status (2026-06-12): THE LATENT FLIP MISCOMPILE IS FIXED (7 bugs
total) AND THE FULL GATE IS GREEN: stage1+stage2 builds, boot-flip 3/3,
stage1-binary flip-stress 16/16 module runs (typecore ctype typedecl
env typeclass parmatch matching translcore btype subst printtyp),
SELF_STAGE=2 ocamltest 6756/0 (allocation.ml expect promoted for
slot-only spill codegen), lit = known pre-existing failures only
(statepoint-call-lowering.ll verified failing at baseline 57e9764b3c
too), minibench geomean 0.8870 vs native (no regression vs 0.9064
prior slot-only / 0.886 vreg-era). All changes uncommitted.

## BUG 6: mixed spill slots after StackSlotColoring (2026-06-12)

The stage1-binary flip-stress failures (typecore/ctype/typedecl/env)
survived the five fixes below. Forensics (watchpoint-grade, see
/tmp/gcscan2.py `gcl`/`gcwatch`) pinned the stale root to
Closure_conversion.cont_96_350: the same young value V0 sat in listed
sp+0x80 (GC updated it), in UNLISTED RA family slots sp+0x20/sp+0x70
(%stack.32 — read later, the crash chain), and in the gc_regs bucket
slot of dead x1 (the earlier "heap holder" red herring — buckets are
malloc'd near the domain). The pass skipped %stack.32 as "non-value"
because ONE of the slot's stores was raw: StackSlotColoring runs before
the pass and merges different vregs' disjoint ranges into one FI (and
collapses LiveStacks VNIs), so the per-FI "all stores are values"
summary is structurally wrong. Fixed in OxCamlStatepointSpillRoots:
per-query REACHING-STORE value-ness (block-local last store, else AND
over pred-exit states, memoized/optimistic on cycles), with new value
evidence: ValueHome-store seeding (covers STRXpre alloc write-backs),
a new MOOxCamlGCValue MMO flag set by TargetLoweringBase for IR
ptr-addrspace(1) loads/stores, entry physreg arg spills via MRI
liveins, direct-callee statepoint-result return-type lookup, consumer
evidence via seeded reloads, and odd immediates as tagged scalars.
Corpus (ccmain2/ltf/parmatch/typecore IR with EXACT build flags from
clang-wrapper.log — llc without -ffixed-x15/x26 hides everything):
remaining skips all verified benign (i64-typed immediates, LOADgot
statics). Parked: port GCValueness into the verifier (it now FPs on
entry-init-extended live ranges); statepoint-call-lowering.ll lit
failure needs a pre/post check.

BUG 7 (same day): the BUG 6 fix moved the crash INTO the GC and
exposed that register roots had been appended at C-CALL statepoints,
where they are unresolvable: only the alloc-family runtime entries run
SAVE_ALL_REGS, and RESTORE frees the bucket without clearing
Caml_state->gc_regs, so the walk reads a FREED bucket (crash:
Regalloc_irc.run's `bl caml_c_call` descriptor listed reg16=x19; an
allocating C primitive GC'd; oldify(stale residue) SIGBUS'd in
get_header_val). Fixed: register appends and the verifier's real
register findings are gated on alloc-family statepoints (regmask
preserves X0). Validation cascade running.

## RA-derived roots, step 0 + residual root fixes (2026-06-11, uncommitted)

Built the gc bit on virtual registers (MachineRegisterInfo, seeded at
statepoint emission, inherited through splits, OR'd by the coalescer) and
the flag-gated post-RA `OxCamlGCRootVerifier` pass. First corpus run (841
stashed modules) found residual root-listing bugs; both classes fixed in
OxCamlStatepointSpillRoots the same metadata-only way:
- gc-bit-based slot families (RA re-spills of ISel-slot-resident values
  created unlisted second slots): 198 violations -> 0.
- register second locations (an RA/pre-RA copy of a value that IS listed
  at the statepoint survives in a preserved register unlisted, e.g.
  Misc.loop_77 %173/$x13): now appended as in-place register roots
  (`-oxcaml-statepoint-register-roots`): 94 violations -> 0.
The earlier "llvmize ptrtoint store" suspicion was RETRACTED: that value
is statically a tagged immediate; the verifier now separates that shape
(value-connectivity union-find) and reports it as info only.
Corpus after fixes: 0 slot / 0 register / 0 clobbered violations.
LATER THE SAME DAY: per-register taint proved unsound for LISTING (raw
ranges of coalesced vregs would be handed to the GC as roots), so the
pass gained a flow-sensitive per-VNI value analysis (GCValueness) — only
provably-value contents are listed. AND: stage builds exposed a
DETERMINISTIC PRE-EXISTING latent miscompile (typecore + young-flip
SIGBUS, crashes with baseline/before-5/current clang alike, native
passes — NOT caused by this week's work; every earlier green gate
shipped it). Root cause open; this now gates landing. Details:
RA_DERIVED_ROOTS_PLAN.md (step 0 sections + "LATENT PRE-EXISTING
MISCOMPILE FOUND").
CONTINUED (same day, root-cause marathon): the latent crash decomposed
into FIVE compiler bugs, all root-caused: (1) exnroot-homed values
RA-re-spilled into unlisted slots [fixed: value-home FIs seed family +
value-ness]; (2) loop-carried listed slots uninitialized on iteration 1
[fixed: dominance check + entry init]; (3) the cont pattern — tied-def
vreg lowering at alloc statepoints creates unlisted live pool-slot
copies, unfixable post hoc [decision: MaxVRegPtrs=0 default for oxcaml
until in-place statepoints (plan step 1-2); register lowering stays
available via -max-registers-for-gc-values]; (4) DETERMINISTIC:
StatepointStableRootHomes seed stores on critical edges clobber the
home (Parmatch.pressure_variants -> bogus non-exhaustiveness errors;
broke every -warn-error build under slot-only mode) [fixed: homes
require non-critical seed edges]; (5) home FIs were in the statepoint
slot pool and could be handed to unrelated values [fixed: not pooled].
Full forensics, repro recipes and hunt methods in
RA_DERIVED_ROOTS_PLAN.md.

## Entry-cost codegen fixes (2026-06-11)

Three systematic per-function/per-callsite costs found by reading hot asm vs
native (boyer truep, kb match_rec, micro try/raise probes), all fixed:

1. Split prologue (`sub sp,#16; str x30; sub sp,#N` -> one
   `sub sp,#16+N; str x30,[sp,#N+8]`): `CombineSPBump` is now allowed for
   OxCaml functions that need no PROLOGUE stack check (the check must see SP
   before any allocation). LR stays at the frame top via the existing CSR
   offset fixup; the epilogue already combined.
   (AArch64FrameLowering.cpp)
2. Ordinary stack checks read SP via `llvm.read_register` instead of inline
   asm (llvmize.ml, AArch64 now matches x86), and a new
   `AArch64MIPeepholeOpt::visitCmpWithSPCopy` folds `COPY $sp` + `SUBS` into
   `cmp sp, xN` (guarded: same block, no SP writes/calls between). Saves an
   instruction plus an inline-asm scheduling barrier at ~every function
   entry (372/725 functions in the minibench suite).
3. C-wrapper call arguments with no use after the call are no longer
   caller-rooted (`-rs4gc-oxcaml-root-dead-c-call-args` restores the old
   behaviour). Callees root their own parameters per the OCaml FFI contract
   (CAMLparam), exactly as with the native compiler; caller-rooting forced a
   dead stack store per C call site (visible in every compare/hash loop).
   `HasUseAfterCall` also refined: a value defined in the call's own block
   before the call is dead along the back edge (single-entry blocks),
   removing the loop-recurrence false positive. Two RS4GC lit tests updated
   to check both modes.

Results: micro 44-case geomean 0.662 -> 0.652, minibench geomean
0.906 -> 0.886 (no case above bdd's 1.001), compiler bench module-median
ratio 0.9732 -> 0.9715 (round-total 0.9740 -> 0.9699) against the identical
native baseline binary. Validation: fresh stage1+stage2 builds, SELF_STAGE=2
ocamltest `6756 passed / 0 failed`, lit at the known pre-existing set,
parser.pp.ml repro + OXCAML_YOUNG_FLIP gates clean.

Full investigation + staged plan for RA-derived GC roots (in-place
statepoint lowering, gc bit on vregs, verifier): see
`agent-state/test-suite-29e4cd/RA_DERIVED_ROOTS_PLAN.md`.

Identified but NOT yet implemented (next): redundant statepoint re-spills of
loop-carried GC values - ISel's `findPreviousSpillSlot` gives up on PHIs
whose inputs map to different slots (upstream TODO in StatepointLowering);
the OxCaml `StatepointStableRootHomes` mechanism handles only
`phi(seed, relocate-of-self)`. Generalizing it to arbitrary GC phis with
per-edge seed stores would remove per-iteration stores + join-block slot
shuffling (boyer tautologyp, kb rporec). Longer-term direction discussed:
derive frametables entirely from RA state (gc-ness bit on vregs,
LiveStacks/VRM as source of truth, in-place root updates) - yesterday's
OxCamlStatepointSpillRoots pass is the seed of that design.

## Latent parser.pp.ml GC miscompile: FIXED (2026-06-10)

## Current Goal

Keep the LLVM backend self-stage2-clean, then improve runtime performance until
the LLVM-built compiler beats both native and the older LLVM baseline on total
microbench, minibench, and compiler benchmark time.

## Latent parser.pp.ml GC miscompile: FIXED (2026-06-10)

See FINDINGS.md (repo root) for the compact full story. Summary:

- Root cause: with the register-preserving alloc calling conventions, greedy
  RA live-range splitting leaves spill-slot copies of GC values whose
  LiveStacks intervals cross statepoints where that value family is not an
  operand. The GC relocates all listed roots; the unlisted crossing slots go
  stale and the merged home store-back block reloads them into RS4GC home
  slots. InlineSpiller statepoint-operand folding only covers slots that are
  the value's active location AT the statepoint. (The "+24 descriptor
  arithmetic" theory was disproven: statepoint FI operands and spill
  instructions are remapped consistently by StackSlotColoring.)
- Fix (metadata-only, zero mutator instructions): new machine pass
  `OxCamlStatepointSpillRoots`
  (vendor/llvm-project/llvm/lib/CodeGen/OxCamlStatepointSpillRoots.cpp),
  between RA and VirtRegRewriter. Collects GC slot families globally
  (folded gc operands + VRM original slots of gc reg operands), appends each
  slot live into-and-across a statepoint as a folded gc operand + gc-map
  pair + FixedStack MMO. Gated on gc "oxcaml"/"ocaml";
  `-oxcaml-statepoint-spill-roots[-verbose]`.
- Validation: -verify-machineinstrs clean; single-module swap repro exit 0
  (was deterministic SIGSEGV); OXCAML_YOUNG_FLIP s=1k/4k clean; fresh
  self-stage1 AND self-stage2 builds pass (stage2 had failed twice on this
  bug); parser.pp.ml s=1k..64k clean on both fresh stage compilers + flip
  on stage2; lit = the 20 known pre-existing failures only;
  SELF_STAGE=2 ocamltest `6756 passed / 284 skipped / 0 failed`
  (`ocamltest_stage2_spillroots_clean_20260610.log`) — the stage2 bar.
- Testsuite fallout fixed along the way (from the earlier committed
  prologue-entry rename to `caml_llvm_call_realloc_stack_stkarg`, not from
  this fix): `_build` runtime needed `make -s runtime-stdlib` for the
  ocamltest fake root, and `stack_check_size_contract.sh`/`challenges.sh`
  now accept the new prologue slow-path symbol.

## Current Change (v3: full call homes, non-volatile slots, init-store pruning)

- Root-caused and fixed the call-statepoint homes miscompile: init stores were
  classified by BLOCK (any entry-block store was treated as initialization),
  so a consumer-NCD defining store that legitimately sits in the entry block
  below interior call statepoints (call statepoints do not split blocks) was
  skipped by `SlotDefStoreDominates`/`oxcamlRootSlotHasSingleDefiningStore`/
  `CanAliasToSlot`.  A channel in `Cmi_format.read_cmi_lazy` was homed at a
  statepoint above its defining store, the reload read the init immediate `1`,
  and SSA repair rewired the defining store to that reload.  Init stores are
  now classified by VALUE: constant operand and entry block
  (`isOxCamlRootSlotInitStore`).  Verified on the isolated cmi_format repro
  (exit 139 -> 0), the exact culprit ordinal (budget=67 skip=66), and an
  829-module object swap of the whole boot compiler.
- `-rs4gc-oxcaml-value-slot-homes` default is now `3` (invokes AND calls).
- Exception-root slot accesses are now NON-volatile by default
  (`-rs4gc-oxcaml-volatile-exnroot-slots`, default false).  Slots escape into
  statepoint gc-live bundles, so alias analysis already forbids forwarding
  across registered statepoints; RS4GC runs in `addIRPasses` before
  CodeGenPrepare/ISel, and non-volatility lets ISel drop dead reloads and
  forward between statepoints.
- Entry-block init stores are pruned when the slot's single defining store
  dominates every load and every statepoint listing the slot
  (`-rs4gc-oxcaml-prune-root-slot-init-stores`, default true).  This removes
  3 dead stores + an immediate materialization from `unify1`-shaped hot
  paths (probes had been paying them on every call).
- Static verifier added: `-rs4gc-oxcaml-verify-root-slots` checks every slot
  reload is dominated by the slot's defining store; clean across all 839
  stashed modules with the final configuration.
- Probes (medians, ratio vs native; layout-luck caveat below): boyer 0.99x
  (was 1.038x committed, 1.052x at head), `catch_failure_then_unify`
  1.256x -> ~1.08-1.11x, `nested_failed_unify` ~1.0-1.2x depending on code
  layout.  NOTE: these tiny probes swing +/-15% from function-alignment
  layout luck alone (verified: same binary +64B alignment moved
  nested_failed_unify 1.398x -> 1.025x); `-align-all-functions` was tested
  and rejected (helps one probe, hurts another).  Only
  `boyer_like_failed_unify` keeps a layout-stable residual (~1.24x).
- Validation of the final configuration: lit at the known pre-existing
  failure sets; cmi_format + 829-module swap repros clean; three fresh boots
  (homes=3, +non-volatile, +pruning) each with the stdlib.pp.ml GC-stress
  sweep s=1k..256k clean, with an always-on young-root checker runtime
  installed during the sweeps; self-stage1 clean
  (`self_stage_v5.log`).
- Stage2 is currently BLOCKED by the pre-existing bug below: the v5 stage1
  compiler hit the latent parser.pp.ml SEGV during the stage2 boot phase in
  two consecutive builds (`self_stage2_v5.log`, `self_stage2_v5_retry.log`),
  while the committed-state stage2-v4 run had passed by luck (its compiler
  crashes on the same repro at s=1k).  Re-running the exact failing command
  standalone passes at default heap params - the in-build trigger is
  layout/timing sensitive.  The full ocamltest suite was therefore run
  against the v5 STAGE1 install instead (`SELF_STAGE=1`):
  `6756 passed`, `284 skipped`, `0 failed`
  (`ocamltest_stage1_v5.log`).
- Final benchmark totals for the v5 configuration (2026-06-10, this machine):
  - Compiler module medians: LLVM/native `0.9732`, `2.75%` speedup
    (`compiler_bench_current_vs_native_20260610_002708.json`).
  - Micro (44 cases): total-time ratio `0.707`, geomean `0.658`, worst case
    `1.13` (`micro_v5_20260610.log`).
  - Minibench: total-time ratio `0.885` (`11.5%` speedup), boyer `1.012x`
    (was ~1.13x), worst case binary_trees `1.117x`
    (`minibench_v5_20260610.log`).

## Known Pre-existing Bug (discovered 2026-06-10, NOT caused by v3)

The stage2-v5 build initially failed: the stage1 compiler segfaulted
compiling `parser.pp.ml`.  Investigation showed a LATENT GC-stress
miscompile that PRE-EXISTS this work entirely:

- Deterministic repro: any LLVM-built stage1/stage2 `ocamlopt` compiling
  `parser.pp.ml` (cwd `_llvm_self_stage2_boot_build/default`) with
  `OCAMLRUNPARAM=s=1k..64k` segfaults 100%; at default heap params it is
  flaky (this is why stage2 builds usually pass and occasionally die).
- Crash: `Ident.compare` receives corrupt arguments (`0` and `1`) via
  `find_value_approximation`'s map lookup; fp-chain:
  `Closure_conversion.cont_96_350+5052` -> `classify_fields_of_block+152`
  -> `List.map` -> `compare`.
- Pre-existence proven: the committed-state stage2-v4 compiler crashes
  identically; so do stage1 builds with EVERY feature combination including
  homes=0/volatile/no-pruning; so does the BASELINE-era clang
  (`llvm-build-clean-head`, 57e9764b3c) and the pre-redesign clang
  (`llvm-build-old-rs4gc`).  The native compiler is clean at s=1k.
  This is very likely the same deep bug previously blamed on call homes via
  the `Lambda_to_flambda.cps` ordinal-304 repro.
- Module isolated: with ONLY `flambda2_from_lambda__Closure_conversion.o`
  LLVM-built (every other object native), the crash reproduces; all-native
  is clean.  Repro loop ~90s: `llstash/test-cc.sh "<extra clang flags>"`
  (stashed IR `/tmp/ccstash/flambda2_from_lambda__Closure_conversion.*.ll`,
  object swap + relink via `llstash/driver5.py <module-list-file>`, good
  tree `_native_build/main`, bad tree `_llvm_self_stage_main_build/main`).
- Ruled out: value-slot homes (crashes at homes=0), slot aliasing, lazy
  boundary loads, volatile vs non-volatile slots, init-store pruning,
  register roots at non-alloc callsites (only at realloc_stack /
  local_realloc / caml_call_gc sites, which save all regs), trap-byte
  offset mismatches in `find_value_approximation` (descriptors audited
  consistent: framesize 64 / offset 40 under active trap vs 48/24 outside),
  missed-young-root scanning (always-on checker runtime silent - though it
  shares the frametable, so it is blind to frametable holes).
- The DEBUG runtime (`-runtime-variant d`) does NOT reproduce (compile
  succeeds at s=1k) - the bug needs the stock runtime's exact allocation
  pattern.
- Remaining suspects: statepoint live-set hole or stack-slot sharing in one
  of `Closure_conversion`'s large functions, or a runtime/frametable scan
  disagreement only visible under precise minor-GC timing.
- The `parser.pp.ml` s=1k repro should be added to the standard validation
  gate once fixed; the stdlib.pp.ml sweep alone does NOT catch it.

### Round 5 (2026-06-10): frametable offset semantics, synth harness

The fix candidates from round 4 were tested and REFUTED for this crash:
SP-relative resolution for statepoint stackmap operands (PreferFP=false -
kept as hardening with a fatal on non-SP resolution, but descriptors were
already SP-resolved), registering all exnroot slots on all statepoints
(cont has NO exnroot slots - no trap regions), and register-coalescer
involvement (-join-liveintervals=0 still crashes).  The corrupting GC
suspends cont at an ALLOC statepoint whose IR carries proper relocates;
the stale value (`simple`) is read after the GC from a frame slot the
descriptor does not cover, flows through add_simple_to_substitute
(closure_conversion.ml:1467) into the substitute map, and cps faults on
it (young-flip, deterministic, cps+396).

NEW INSTRUMENT: llstash/synth-statepoint-offsets*.ll - tiny synthetic
oxcaml modules whose emitted frametables can be diffed against the
actual frame layout by eye, no GC runs needed.  Findings so far:
- The "statepoint-id" attribute ENCODES trap depth: bit0 = alloc,
  ((id>>1)&7)*16 = ActiveTrapBytes which OxCamlGCPrinter ADDS to every
  stack root and to nothing else; a synthetic id of 18/19 silently
  shifts roots by +16.
- AArch64RegisterInfo::eliminateFrameIndex (statepoint case) ALSO adds
  AFI->getOxCamlActiveTrapBytes(MI) (computed from real MIR trap pushes)
  into the operand offset.  RS4GC-converted statepoints (default IDs, no
  trap bits) therefore get the +16-per-trap exactly once (verified
  consistent in find_value_approximation); LLVMIZE-EMITTED statepoints
  with explicit IDs inside trap regions are suspected of DOUBLE-counting
  (ID bits + AFI bytes) - shifting every stack root by +16 per depth.
- The guilty cont statepoint has ID 196609 (alloc, ID-depth 0); its
  emitted stack roots are 192/200 while the MIR-listed slots sit at
  +24 less - the remaining unexplained delta.  Next: replicate cont's
  exact shape in the synth harness (alloc id, several relocated values,
  448-byte frame, split prologue) and diff descriptor vs actual slots;
  each iteration is a pure compile, minutes.

### ROOT CAUSE FOUND (2026-06-10, round 4)

The mechanism is proven with a new debug instrument, OXCAML_YOUNG_FLIP
(runtime/minor_gc.c + domain.c, env-gated): the minor heap alternates
between two locations each collection and the retired space is
PROT_NONE'd, so any dereference of a stale young pointer faults AT THE
GUILTY INSTRUCTION.  Requires s>=4k (16K pages).  With it:

- The crash chain is: an LLVM-compiled statepoint in
  `Closure_conversion.cont` leaves a STALE young pointer in a frame slot;
  cont passes the stale `simple` to `Env.add_simple_to_substitute`
  (closure_conversion.ml:1467); the pair is stored in the substitute map;
  `Lambda_to_flambda.cps` (Lvar case) immediately looks it up and
  dereferences -> fault at cps+396 (or, unprotected, reads recycled
  memory -> the Ident.compare(0,1) crash).
- Guilty statepoint #1 (default config): the alloc statepoint at
  cont+2240 (Ltmp951).  Its descriptor lists the values as REGISTER
  roots (gc_regs) plus slots 192/200 - but the code after it reloads the
  same values from RA SPILL SLOTS 168/176, which are NOT in the
  descriptor.  The GC updates the registers; the spill slots keep the
  pre-GC pointers; the post-GC reloads resurrect them.
- With -max-registers-for-gc-values=0 (zero register roots), the SAME
  fault occurs via a different cont alloc statepoint (cont+1620,
  Ltmp1031): an RA spill slot (~offset 32-48) carrying the value across
  the statepoint is again absent from the descriptor's root list.

UNIFIED ROOT CAUSE: LLVM's register allocator (greedy/InlineSpiller/
split machinery) carries GC values across STATEPOINTs in locations the
statepoint operand list does not reference - sibling registers kept live
because the oxcaml alloc calling convention PRESERVES registers, and RA
spill slots whose live range crosses the statepoint.  The frametable
only describes the operands' locations at the statepoint, so the GC
updates those and every other copy goes stale.  Stock LLVM avoids this
because (a) caller-saved registers die at calls, (b) gc pointers in
callee-saved registers are force-spilled (AllowGCPtrInCSR=false), and
(c) relocate defs end the input interval at the statepoint.  The oxcaml
register-preserving alloc CC re-opens the hole.  Relevant code:
FixupStatepointCallerSaved.cpp + AArch64RegisterInfo::
shouldSpillStatepointGCPtr (only forces x16-x18/x26-x28 today).

FIX DIRECTION (next session): make statepoint-crossing GC live ranges
single-location.  Candidates: (1) make RA statepoint-aware so a GC vreg
live across a statepoint is forced fully into its statepoint-listed
location (fold + no sibling copies); (2) have the statepoint CLOBBER
GC-holding registers from RA's perspective (kill dual residency, accept
spills); (3) at MIR fixup time, enumerate LiveStacks intervals crossing
each statepoint whose original vreg was a GC pointer and append their
slots to the stackmap.  (3) is likely the least invasive and matches the
frametable design.  Verify any fix with OXCAML_YOUNG_FLIP=1 s=4k on the
parser repro, then the stdlib sweep and full stages.

### Second investigation round (2026-06-10, bug still open)

Two REAL adjacent holes were found and fixed (neither is the parser crash):

- RS4GC rematerialization walked GEP chains THROUGH `!is_base_value` GEPs
  (comballoc secondary object starts), so a derived pointer could be
  rebuilt after a statepoint from a DIFFERENT object's relocation.  Both
  chain walkers now stop at marked GEPs (they are the chain root); the
  comballoc-object-starts test now rematerializes fields from their own
  object's relocation.  This path is real but unexercised in
  closure_conversion (objects byte-identical before/after the fix).
- The inline-asm prologue stack checks emitted by `emitOxCamlStackCheck`
  (AArch64FrameLowering.cpp) called the NATIVE `caml_call_realloc_stack`,
  which saves the live x29; stack growth then walks the frame-pointer
  chain from it (fiber.c WITH_FRAME_POINTERS rewrite) into LLVM frames,
  rewriting any spilled word that looks like an old-stack address.  Added
  `caml_llvm_call_realloc_stack_stkarg` (runtime/arm64.S): same stack-arg
  protocol but stores xzr to terminate the chain, exactly like
  `caml_llvm_call_realloc_stack`.  NOTE: runtimes must be rebuilt from the
  new arm64.S before linking code from the new clang (stage0
  `_install/lib/ocaml/libasmrun.a` was patched in place with the new
  arm64.o).
- Also added: `-rs4gc-oxcaml-verify-object-starts` (reports base-pointer
  chains crossing object-start GEPs) and an `OXCAML_LLVM_NO_COMBALLOC`
  env-var gate in asmgen.ml for bisection.

Additional EXCLUSIONS for the parser crash (each tested directly):
comballoc entirely off (module IR regenerated without it - still crashes),
`-O1` (still crashes; not an -O3-only pass), GC values forced to spill
slots (`-max-registers-for-gc-values=0`), the realloc fp-chain rewrite
(disabled in fiber.c - still crashes), ALL stack reallocation (64M-word
initial main stack - still crashes), i64-laundered GC pointers (the
`-rs4gc-heuristic-report-oxcaml-statepoint-crossing-inttoptr` hits all
triage to immediates: or-tag patterns, local_sp arena offsets, Int_ids
table ids).

Repro status: FULLY DETERMINISTIC under lldb (no ASLR): identical crash
pc (`Ident.compare+48`) and registers (x0=0, x1=1) every run, with the
fp-chain compare <- List.map+172 <- classify_fields_of_block+152 <-
curry2 <- cont_96_350+5052.  (Watchpoints perturb signal timing and
change pool-reuse history; use content/condition triggers, not hit
indexes.)

Round-3 narrowing (same day): the crash is `CCenv.find_var` called from
`Lambda_to_flambda.cps` on `Lvar` (lambda_to_flambda.ml:480) walking the
closure_conversion_aux VARIABLES map into a node pointer that lands in
RECLAIMED major-pool memory (recycled small values such as 1, 3, 0x1500).
Refuted by direct experiment: init-placeholder leaks (new debug flag
`-rs4gc-oxcaml-tag-init-placeholders` gives every slot placeholder a
unique odd immediate; the crash value stays plain 1 = Val_long 0),
immediate idents inserted through any aux Env add path (conditional
breakpoints never fire), missing write barriers (caml_modify relocation
count differences are sound MIR tail-merging; all calls survive post-opt
IR), and major-GC starvation (o=10000 still crashes).  Remaining
mechanisms: a frametable liveness hole at a closure_conversion statepoint
(live env/map collected) or relocation/SSA wiring handing code a stale or
wrong pointer.  Next experiment documented in the memory note: walk the
map from its root at the deterministic crash and classify the dead node's
parent (header color/generation, sibling fields) to separate
collected-while-reachable from wrong-value-at-construction.

Watchpoint findings: the corrupt cells' last writers are the GC itself
copying the young original verbatim during promotion
(oldify_one <- caml_scan_stack, then oldify_mopup) - so the corruption
predates promotion and the heap/frametable/GC are clean as writers.
Crash-state decode: List.map was invoked by LLVM-compiled
classify_fields_of_block with `f` = a TAG-0 ordinary block whose field0
happens to be Ident.compare's code pointer - NOT a closure (tag 247);
map blindly entered compare with junk args.  A well-formed-but-WRONG
value therefore flows into the f/list arguments inside cont/classify on
a GC-timing-dependent path: prime suspect is SSA-repair /
exnroot-reload / relocation WIRING selecting the wrong value on a rare
statepoint path.  Next: breakpoint at classify's map callsite on the
crash iteration (no watchpoints) and trace f/list register provenance
back through cont's statepoint reloads against the stashed RS4GC IR.

## Previous Change (v2: consumer-NCD store placement)

- Refined the uniform root design: the slot's single defining store now sits
  at the latest point that dominates every protected invoke of the regions
  sharing the slot (nearest common dominator of the consumers), hoisted out
  of cycles while the value stays available, falling back to the definition
  when the value does not dominate that point.  This fixes a 35% regression
  on `nested_failed_unify`: store-at-def had hoisted argument slot stores to
  the function entry, taxing every call of functions like `unify1` whose
  protected region sits on a conditional path.  Shared slots migrate their
  store upward as later regions join (`EnsureDefStoreCovers`), and
  value-slot homes only apply at statepoints dominated by the store.
- Probes after the change (medians, same machine/run, ratio vs native):
  `nested_failed_unify` 1.252x (head 1.250x), `boyer_like_failed_unify`
  1.251x (head 1.228x), `catch_failure_then_unify` 1.224x (head 1.245x),
  `closure_env_in_try_hit` 1.088x (head 1.089x),
  `many_handler_live_roots_raise` 0.666x (head 0.670x), boyer 1.038x (head
  1.052x).  No bad regressions remain; boyer's win is retained.
- Debug tooling added behind flags: `-rs4gc-oxcaml-call-home-budget/-skip/
  -dump` to bisect call-statepoint homing by ordinal.  Using them, the
  call-homes failure was narrowed to a SINGLE homed call statepoint in
  `Lambda_to_flambda.cps` (skip=304 budget=305 with the llstash harness
  reproduces in ~10s); its IR, frametable record, regalloc slot sharing and
  backedge wiring all audit as consistent, so the residual bug is deeper in
  call-statepoint lowering.  Homes stay invoke-only by default.
- Validation: full RS4GC + AArch64 oxcaml lit suites at the pre-existing
  known-failure sets; fresh boot passes the stdlib.pp.ml GC-stress repro at
  s=1k/4k/64k; self-stage log
  `agent-state/test-suite-29e4cd/self_stage_uniform_roots_v4.log`.

## Previous Change

- RS4GC now classifies each pre-RS4GC addrspace(1) value live over a
  safepoint into exactly two categories: handler-live values get one volatile
  root slot stored once at the value's definition (per-invoke stores remain
  only for PHIs defined on unwind edges), reloaded after each safepoint and at
  recovery entries, with the slot registered on the statepoints; everything
  else uses standard gc.relocate.  This replaces the per-role slot
  materialization that gave boyer's `rewrite_with_lemmas` three duplicate term
  slots re-stored before every protected invoke (now: 2 slots, 1 store each
  per definition, none per invoke).
- Boundary rejoins use lazy per-edge reloads of the single slot; selectors and
  recovery loads alias the underlying slot (single-defining-store slots only),
  so SSA repair does not resurrect relocates or per-invoke stores.
- Fixed a latent null `Info.StatepointToken` read in the old
  `canonicalizeExplicitRootHomesAndFilterLiveSets`; homes are now assigned per
  record from the function-wide value→slot map against the original call.
- Benchmarks (this machine, medians): boyer native `0.0854s`, old-HEAD LLVM
  `0.0933s` (1.093x), new LLVM `0.0869s` (1.018x).  `many_handler_live_roots_raise`
  0.66x vs native.  `boyer_like_failed_unify`/`catch_failure_then_unify`
  (synthetic always-raise loops) are ~4-7% slower than old HEAD because the
  old per-invoke stores fed store-to-load forwarding into the handler reload;
  accepted trade for the normal-path win.
- Lit: all oxcaml RS4GC tests pass incl. new
  `oxcaml-exception-root-single-slot-per-value.ll`; the 13 generic RS4GC and
  7 AArch64 oxcaml failures are pre-existing (identical sets at 57e9764b3c).
- The first self-stage run hit two real bugs, found with a deterministic
  reproducer (`_llvm_boot_*` boot compiler compiling `stdlib.pp.ml` under
  `OCAMLRUNPARAM=s=4k`) and an object-level bisect harness (`../llstash`,
  swaps per-module `.o` between head-clang and new-clang dune trees, re-ars
  archives, relinks, reruns the repro):
  - `Info.ExplicitRootHomes` held raw `Value*`; when a homed value was the
    result of an earlier statepoint-rewritten call, the deferred RAUW left
    the home dangling and the replacement gc.result was never SSA-repaired.
    Fixed with `WeakTrackingVH` handles.
  - Slot aliasing (selector/reload sharing a root slot) is unsound when the
    slot's defining store can re-execute during the alias's lifetime; guarded
    by `CanAliasToSlot` (defining store must dominate the alias and not be
    reachable from it).
  - After those fixes, homing live slot values at CALL statepoints (removing
    them from gc-live in favor of the slot) still corrupts the heap under GC
    pressure even though IR-level SSA repair is provably complete; the
    miscompiled module was `lambda_to_flambda` (`cps` function) and the
    failure is below the IR (call-statepoint lowering of slot-homed frames is
    suspected, possibly the CSR root map).  Homes default to INVOKE
    statepoints only (`-rs4gc-oxcaml-value-slot-homes`, 0=off 1=invokes
    2=calls 3=all); calls keep full relocation.  Investigating `=2` with the
    bisect harness is the open follow-up.
- With invokes-only homes: boyer `1.020x` vs HEAD `1.048x` on the same
  machine/run; the boot compiler passes the `s=4k` GC-stress repro at 4k/16k/
  256k minor heaps (head-clang boots pass it too; the fully-homed build did
  not).
- Self-stage validation of the invokes-only configuration: in progress.

## Previous Change

- Current branch HEAD includes `b6c22b9142` (`Enable comballoc for LLVM
  backend`).
- Default mode is back to normal RS4GC/gc.relocate lowering; the global
  all-volatile-root-slot experiment is not the active path.
- Pending validation fixes:
  - LLVM-backend statmemprof native variants for
    `discard_in_callback.ml` and `stop_start_in_callback.ml` now expect the
    `combined-f33` profile shape, matching LLVM comballoc.
  - `tools/setup-llvm-stage4-ocamltest.sh` now builds a real fake-root
    `otherlibs/systhreads` directory and links generated `threads.h` and
    `st_pthreads.h` into it when present. This fixes self-stage2 ocamltest
    compilation of `tests/lib-systhreads/swapgil.ml`.
- `685d252ac0` was unsafe: the exception-root merge/filtering change let the
  LLVM self-stage compiler segfault while compiling `stdlib/bytes.ml`.
- `9f38c181d9` reverts the unsafe LLVM source/test changes from `685d252ac0`,
  while keeping the useful self-stage script fixes.
- The self-stage scripts now allow explicit `LLVM_EXTRA_FLAGS` when needed and
  preserve clean native/LLVM separation.
- Boyer remains a useful slowdown case. A fresh run showed native median
  `0.08898s`, LLVM median `0.09533s`, ratio `1.0714x`.
- In `rewrite_with_lemmas`, pre-RS4GC has one source value `%2` (`term`) live
  through several handler roles: returned when lemmas are exhausted and reused
  when caught `Unify` retries the loop. RS4GC materializes those roles as
  separate exception-root slots (`%exnroot`, `%exnroot124`, and related PHI
  roots), so the first protected call stores the same `%2`-derived value into
  multiple volatile exnroots. This is a conservative artifact of the current
  handler-boundary materialization, not a distinct source value.

## Evidence

- Full installed-compiler LLVM-backend tests passed after the comballoc test
  fixes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_current_install_llvm_backend_comballoc_fixed_20260608_153338.log`.
- Self-stage build using `_install` as stage 0 passed:
  log `agent-state/test-suite-29e4cd/self_stage2_comballoc_fixed_20260608_153957.log`.
- Second self-stage build using `_llvm_self_stage_install` as stage 0 passed
  and produced `_llvm_self_stage2_install`:
  log `agent-state/test-suite-29e4cd/self_stage2_second_comballoc_fixed_20260608_154430.log`.
- Full self-stage2 ocamltest rerun passed:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage2_comballoc_fixed_rerun_20260608_155852.log`.
  The previous full run hit a one-off `tests/lib-threads/signal.ml` native
  output miss; focused `tests/lib-threads` rerun and the full rerun both passed
  that test.
- Rebuilt custom LLVM `opt` and `clang` in `../llvm-build`.
- Focused LLVM tests pass:
  `oxcaml-volatile-root-allocas.ll`,
  `oxcaml-self-base-phi-exception-root.ll`, and
  `oxcaml-statepoint-stable-phi-root-home.ll`.
- Full self-stage2 now passes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage_after_rollback_rerun_20260607_105819.log`.
- Current vs native totals after the rollback:
  - Micro: LLVM/native `0.971072`, `2.98%` speedup,
    log `agent-state/test-suite-29e4cd/micro_after_rollback_20260607_110522.log`.
  - Minibench: LLVM/native `0.927656`, `7.80%` speedup,
    results `agent-state/test-suite-29e4cd/minibench_after_rollback_results.json`.
  - Compiler module medians: LLVM/native `0.965285`, `3.60%` speedup,
    results `agent-state/test-suite-29e4cd/compiler_bench_current_vs_native_20260607_110910.json`.
- This beats the saved old LLVM baseline at `57e9764b3c` on all three totals:
  - Old micro ratio was `0.982986` (`1.73%` speedup).
  - Old minibench ratio was `0.932000` (`7.30%` speedup).
  - Old compiler module-median ratio was `0.981823` (`1.85%` speedup).
- Code-review-revise loop on the net improvement stack:
  - `git diff --check 57e9764b3c..HEAD` passed.
  - Focused LLVM tests above passed again.
  - Reviewed stable root home lowering, statepoint slot allocation, regmask
    call-splitting default, and the inactive volatile-root mode. No source fix
    was needed after the rollback.

## Follow-up Opportunities

- 2026-06-29 sqrt lowering fix:
  - AMD64 LLVM mode now selects non-builtin `sqrt`/`sqrtf` through the existing
    AMD64 SIMD selector, reusing the same native-parity path that lowers to
    `llvm.sqrt.*` in Llvmize.
  - Fresh custom LLVM boot build passed using
    `_llvm_sqrt_boot_context_build/default/boot_ocamlopt.exe` and the project
    RS4GC wrapper: `boot fresh ir: 828`.
  - Focused regression script passed:
    `testsuite/tests/llvm-codegen/amd64_sqrt_intrinsic.sh`.
  - Direct probe showed `llvm.sqrt.f64`/`llvm.sqrt.f32` in IR and
    `vsqrtsd`/`vsqrtss` in assembly, with no sqrt libcall in code.
  - Focused `benchmarksgame_ocaml` `nbody_1` rerun with the fresh compiler:
    native median `0.8371s`, LLVM median `0.8357s`, LLVM/native `0.9983`.
    Captured LLVM nbody IR has two `llvm.sqrt.f64` calls and captured assembly
    emits `vsqrtsd` instead of `sqrt@PLT`.
  - Normal `make -s boot-compiler` remains blocked in this checkout by a
    pre-existing boot-workspace dune sandbox/source-copy failure: rules for
    files such as `parsing/lexer.mll`, `parsing/parser.mly`,
    `middle_end/flambda2/parser/flambda_parser.mly`, and `tools/make_opcodes.mll`
    report missing deps under `_build/default` even though the source files are
    present and tracked. The custom LLVM boot script avoids that path.
- Individual slowdowns are still worth investigating:
  `closure_env_in_try_hit` about `1.29x`,
  `closure_env_in_try_no_raise` about `1.25x`,
  `catch_failure_then_unify` about `1.24x`,
  `boyer_like_failed_unify` about `1.23x`, and minibench `boyer` about `1.13x`.
- Do not resurrect the global all-volatile-root-slot mode as the default without
  fresh evidence; it regressed total micro time in the latest run.
- 2026-06-29 exception slowdown follow-up:
  - `raise_caught_cross_function` is now traced to LLVM `loop-reduce`
    introducing a second loop-carried tagged call-argument IV for the
    invoke-statepoint. AMD64 runtime-entry clobbers then force that IV through
    an extra spill/reload slot around each protected call.
  - Native AMD64, like arm64, marks all normal registers destroyed at raise; it
    does not rely on registers being live into handlers. The better native shape
    recomputes the tagged call argument at the call.
  - The existing OxCaml LSR guards miss exception statepoints because they look
    for call-form `IntrinsicInst`; protected calls are invoke-form
    `@llvm.experimental.gc.statepoint`.
  - Relinked ablation medians for `raise_caught_cross_function`: native
    `0.0740s`, LLVM `0.0849s` (`1.1470x`), recompute-tag `0.0770s`
    (`1.0406x`), global `--disable-lsr` `0.0798s` (`1.0780x`).
  - Detailed notes and artifacts are in
    `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/report.md` and
    `.../exception/raise_caught_cross_function/`.
- 2026-06-29 LSR invoke-statepoint fix:
  - Updated LLVM LoopStrengthReduce so OxCaml statepoint detection uses
    `CallBase`, covering both call-form and invoke-form statepoints.
  - Made statepoint call-argument LSR skipping the default, turned the broad
    whole-statepoint-loop LSR disable back into an opt-in escape hatch, and
    kept pre-inc loop-exit IVs for OxCaml loops containing statepoints. This
    preserves the original single IV for protected call args instead of
    creating a second loop-carried tagged IV.
  - Added `CodeGen/X86/oxcaml-lsr-statepoint-invoke.ll`.
  - Validation: rebuilt `_build/llvm-tools/bin/llc`; direct FileCheck pipeline
    for the new test passed; saved `raise_caught_cross_function` post-RS4GC IR
    now emits `leaq 1(%rax,%rax), %rax` at the call and no `addq $2`.
  - Quick benchmark reruns with the real wrapper and rebuilt tools:
    `raise_caught_cross_function` `1.0729x` LLVM/native (`SAMPLES=3`) vs the
    earlier corrected broad baseline around `1.1734x`; loop-invariant GC
    dynamic probe `1.4490x` vs earlier `1.5823x`. Full-suite numbers still need
    a longer rerun.
- 2026-06-29 exception slowdown class-2 follow-up after the LSR fix:
  - Fresh focused exception microprobe run with rebuilt local `llc`
    (`SAMPLES=5,WARMUPS=1`, `_install/bin/ocamlopt.opt`, local RS4GC wrapper):
    `raise_caught_cross_function` `1.0884x`, payload cross-function `0.9398x`,
    `closure_call_many_handler_live_roots_raise` `1.1912x`,
    `boyer_like_failed_unify` `1.1314x`, `catch_failure_then_unify` `1.1020x`,
    `nested_failed_unify` `1.0144x`.
  - Native and LLVM both use trap-stack recovery and both clobber ordinary
    registers at raise. Remaining slowdowns are state placement around
    protected calls and, in Boyer/catch, real exnroot/relocate pressure.
  - `closure_call_many_handler_live_roots_raise` is the clean live-root case:
    2 post-RS4GC exnroot allocas, no `gc.relocate`s, hot invoke carries
    `gc-live(ptr %...exnroot)`, and final asm still spills scalar loop state
    through frame slots around the invoke.
  - `catch_failure_then_unify`/Boyer are mixed cases: 8 post-RS4GC allocas,
    14 statepoints, 11 relocates, three exnroot homes in `unify1`, plus folded
    reloads around protected calls.
  - `-rs4gc-oxcaml-exn-ssa-roots` had no effect on the representative AMD64
    artifacts. Forcing `-rs4gc-oxcaml-exn-ssa-all` reduced catch's root slots
    (`alloca` 8 -> 2, exnroot refs 51 -> 10) but made the asm shape worse
    (larger frame and folded reloads 5 -> 7), so it is not a default fix.
  - Current no-spill-fusing ablation is mixed: helps
    `raise_caught_cross_function` (`1.0884x -> 1.0356x`) and Boyer slightly
    (`1.1314x -> 1.1127x`), barely changes
    `closure_call_many_handler_live_roots_raise` (`1.1912x -> 1.1844x`),
    worsens catch (`1.1020x -> 1.1152x`), and badly worsens nested
    (`1.0144x -> 1.2023x`). Global spill-fusing suppression is not the
    exception fix.
  - Updated `amd64-arm-parity-plan.md` with the class-2 findings and next plan:
    keep the existing exnroot/value-home root mechanism, inspect MIR state
    placement around invoke-statepoints, and compare against arm64 artifacts
    before proposing a narrow AMD64 placement/root-materialization change.
  - Followed up with MIR cuts at `greedy`, `oxcaml-statepoint-spill-roots`,
    `virtregrewriter`, and `prologepilog` for
    `closure_call_many_handler_live_roots_raise` and
    `catch_failure_then_unify`; see
    `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/exception/class2-mir-findings.md`.
    Result: closure's hot invoke lists only the exnroot home and has ordinary
    scalar/domain-state frame homes; catch's `run_4_9_code` correctly gains
    one backend-listed GC spill root (`%stack.4`) while exception temporaries
    are rejected. The next implementation target is compact native-like
    placement/reload around invoke-statepoints, not frontend roots, global
    no-spill-fusing, or forced SSA exn roots.
- 2026-06-29 compiler-binary benchmark rerun:
  - Compared `_native_install/bin/ocamlopt.opt` against
    `_llvm_current_stage2_install/bin/ocamlopt.opt`; both compilers compiled
    representative compiler modules in normal native mode with no
    `-llvm-backend`.
  - Used matching build trees/CMIs: native side `_native_build/main`, LLVM side
    `_llvm_current_stage2_main_build/main`. Verified `_native_build/log` does
    not contain `llvm-backend=1`.
  - Modules: `cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`,
    `typecore`, and `typemod`; 1 warmup, 7 measured repetitions.
  - Result artifact:
    `agent-state/test-suite-29e4cd/compiler_bench_native_vs_llvmbuilt_native_mode_20260629_023513.json`.
    Sum of module medians: native `16.0902s`, LLVM-built `16.1625s`, ratio
    `1.0045x`. Round-total median: native `16.0943s`, LLVM-built `16.1595s`,
    ratio `1.0041x`. Module-ratio geomean `1.0076x`, median `1.0125x`.
  - Largest slow module ratios: `cfg_selectgen` `1.0293x`, `typecore`
    `1.0141x`, `translcore` `1.0127x`, `typemod` `1.0125x`. `llvmize`
    (`0.9912x`) and `env` (`0.9937x`) were slightly faster with the
    LLVM-built compiler.
- 2026-06-29 BOLT compiler-binary experiment:
  - Built BOLT tools in a separate CMake tree:
    `_build/llvm-bolt/bin/llvm-bolt`, `perf2bolt`, and `merge-fdata`.
  - Relinked the LLVM-built compiler from the existing stage2 build products
    with `-ccopt -Wl,--emit-relocs`, producing
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/ocamlopt.reloc`.
    The relinked binary has `.rela.text`, `.rela.rodata`, `.rela.eh_frame`,
    `.rela.data*`, and `.symtab`.
  - Fixed a local BOLT runtime build issue in
    `vendor/llvm-project/bolt/runtime/instr.cpp`: GCC emitted a mangled
    anonymous-namespace reference to `__bolt_instr_conservative`, while BOLT
    emits the runtime data symbol unmangled. Forcing the asm symbol name let
    `llvm-bolt -instrument` emit an instrumented binary.
  - Full profile-guided BOLT is still not measurable in this environment.
    `perf record` is blocked by `kernel.perf_event_paranoid=4` for hardware and
    software events, direct `sysctl` is denied, and passwordless `sudo` is not
    available. BOLT instrumentation of OCaml-managed code segfaults immediately
    in `caml_garbage_collection`/`caml_do_call_gc`, consistent with inserted
    instrumentation calls/return addresses violating OCaml frame-table/GC stack
    assumptions.
  - A no-profile BOLT rewrite of all code also segfaults on `-version`. A
    minimal rewrite with `-skip-funcs='.*'` runs, confirming the toolchain and
    relocation-enabled binary are basically usable. A conservative runnable
    subset that skips all `caml*` functions also runs:
    `ocamlopt.nocaml.bolt`.
  - Benchmarked the runnable subset against the relocation-enabled base, both
    compiling the same compiler modules in native mode with the same
    `_llvm_current_stage2_main_build/main` CMIs and
    `_llvm_current_stage2_install/lib/ocaml`; result artifact:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/bolt_skip_caml_vs_reloc_bench_20260629_025323.json`.
    Sum of module medians: base `16.1975s`, BOLT-skip-caml `16.3050s`, ratio
    `1.0066x`. Round-total median: base `16.2409s`, BOLT-skip-caml
    `16.2972s`, ratio `1.0035x`. Geomean module ratio `1.0023x`.
  - Conclusion: the current runnable BOLT subset has no useful win. The
    potentially interesting BOLT measurement requires either enabling perf
    sampling for the uninstrumented compiler or teaching BOLT/OxCaml how to
    rewrite OCaml-managed functions while updating/preserving the runtime frame
    metadata needed by GC and exceptions.
- 2026-06-29 BOLT follow-up with perf enabled:
  - Enabled perf sampling externally and collected LBR data for a corrected
    relocation-enabled compiler. Built/reused BOLT tools:
    `_build/llvm-bolt/bin/llvm-bolt`, `perf2bolt`, and `llvm-bat-dump`.
  - Fixed AMD64 native frametable emission to live in `.data` like the arm
    backend shape: `backend/amd64/emit.ml` now emits the frametable as a data
    object symbol, uses zero fill, and uses data-section label metadata for
    frame-table-relative entries. A first attempt with text-section label
    metadata broke native emission; matching arm's data-section metadata fixed
    that.
  - Rebuilt LLVM self-stage through stage2. Stage2 passed only after reducing
    dune parallelism with `DUNE_BUILD_FLAGS=-j2`; the earlier high-parallelism
    stage2 boot build crashed during large Flambda2 module compilation. The
    resulting `_llvm_boltfix2_stage2_install/bin/ocamlopt.opt` passes a native
    allocation/exception smoke test.
  - Relinked the stage2 compiler with `-ccopt -Wl,--emit-relocs`:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/ocamlopt.boltfix2.reloc`.
    The startup frametable is in `.data` and has relocations.
  - Added
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/patch_ocaml_frametables.py`.
    BAT alone is too imprecise for OCaml frame descriptors because BOLT shortens
    instructions inside basic blocks. The patcher now disassembles old/new
    binaries and maps old frame descriptor PCs to new call return PCs by call
    target and BAT-estimated original call instruction address. Matching on the
    call instruction, rather than the return PC, is required for reordered
    blocks because return PCs can sit at BAT block boundaries. For split
    functions, the patcher parses BOLT cold-to-hot BAT entries and matches hot
    plus cold fragments together against the parent input function.
  - No-profile BOLT now works for OCaml-managed code:
    `ocamlopt.boltfix2.noprofile.bat.callpatched` starts and compiles/runs the
    native smoke test. The patcher rewrote all 224,538 descriptors by call-site
    mapping with zero unresolved descriptors.
  - Collected a profile from the reduced standalone compiler-module workload
    (`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`) and converted it
    with `perf2bolt`:
    `ocamlopt.boltfix2.subset.lbr.perf.data` and
    `ocamlopt.boltfix2.subset.lbr.fdata`. `perf2bolt` read 187,793 samples and
    2,999,194 LBR entries.
  - Profile-guided BOLT with function reordering only works:
    `ocamlopt.boltfix2.profile-funconly.bat.callpatched` starts and passes the
    reduced compiler-module workload. Command used `-data=...subset.lbr.fdata`,
    `--enable-bat`, `-reorder-functions=hfsort`, `-reorder-blocks=none`,
    `-align-macro-fusion=none`, and `-peepholes=none`.
  - Full profile-guided BOLT now works on the reduced compiler-module workload:
    `ocamlopt.boltfix2.fullbolt-nosplit.bat.calladdrpatched` passes with
    `-reorder-blocks=ext-tsp` and `-reorder-functions=hfsort`, and
    `ocamlopt.boltfix2.fullbolt.bat.coldpatched` passes with
    `-split-functions -split-strategy=profile2` as well. The successful split
    patch rewrote all 224,538 frame descriptors by call-site mapping with zero
    BAT fallback mappings.
  - Quick timing on the reduced workload, three samples:
    stage2 real median `9.585s`; relocation-enabled median `9.634s`;
    no-profile BOLT median `9.514s` (`0.988x` vs reloc); profile-loaded
    no-reorder median `9.576s` (`0.994x`); profile function-only median
    `9.600s` (`0.996x`). Artifact:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/boltfix2-stable-benchmark.json`.
  - Full BOLT timing on the same five-module workload with three compile
    repetitions per timing sample: relocation-enabled median `28.849s`;
    no-profile BOLT median `28.344s` (`0.983x`); profile function-only median
    `28.729s` (`0.996x`); full BOLT no-split median `27.807s` (`0.964x`);
    full BOLT split median `28.230s` (`0.979x`). Artifact:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/boltfix2-fullbolt-benchmark.json`.
  - Next BOLT tasks: promote the patcher into maintained tooling, validate the
    BOLTed compiler on a larger native build/test slice, and then compare the
    BOLTed LLVM-built compiler against the native-built compiler.
