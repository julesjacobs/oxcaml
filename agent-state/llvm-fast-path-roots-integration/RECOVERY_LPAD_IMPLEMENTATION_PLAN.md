# Recovery landingpad implementation plan

## Goal

Implement native-like OxCaml AArch64 trap recovery in the LLVM backend using
direct recovery landingpads, explicit exceptional edges, and narrow handler-live
value handling.

This file is the implementation plan. The prerequisite experiment plan is
`STATEPOINT_RECOVERY_EXPERIMENT_PLAN.md`; run those experiments first enough to
settle the valid GC pointer representation.

## Target model

For each active trap region:

- `trap.publish` stores the address of the real recovery landingpad;
- every may-raise operation in the region has an explicit unwind edge to that
  recovery landingpad;
- the recovery landingpad is marked `runtime-entered`;
- `trap.recover` snapshots runtime ABI state from `x0`, `x26`, `x27`, and
  `x28`;
- non-GC handler-live values stay as ordinary SSA/PHI values;
- GC handler-live values use exceptional `gc.relocate` when valid, otherwise
  narrow explicit root slots;
- no broad volatile trap-live slots are used as the default representation.

## Implementation stages

### Stage 0: lock down experiment result

Before code changes, decide from the IR experiments:

- whether unique invoke statepoints can use exceptional `gc.relocate`;
- whether shared landingpads require split recovery entries;
- when explicit root slots are required;
- whether `gc.relocate` must appear before `trap.recover`, and how that affects
  the recovery-block invariant.

Update this file if the experiments change those conclusions.

### Stage 1: LLVM recovery block shape

Change the OxCaml trap recovery recognizer to accept direct recovery landingpads
with PHIs before the landingpad:

```llvm
recover:
  %ordinary = phi ...
  %lp = landingpad token cleanup
  ...
  %rec = call @llvm.aarch64.oxcaml.trap.recover()
```

Rules:

- PHIs are allowed before the landingpad.
- `landingpad token cleanup` remains required.
- No ordinary handler code may execute before the recovery ABI snapshot.
- If the statepoint experiment requires exceptional `gc.relocate` before
  `trap.recover`, allow only those relocates before `trap.recover`.
- Protected-path SSA values may be used after the recovery snapshot, or after
  allowed exceptional relocates.

Tests:

- positive direct landingpad with scalar PHI;
- negative protected-path use before the allowed recovery prefix;
- negative malformed landingpad;
- negative recovery block not named by a dominating `trap.publish`.

### Stage 2: machine verifier and PHI boundary

Relax the runtime-entered machine verifier rule only while the machine function
is still in SSA.

Rules:

- before `NoPHIs`, runtime-entered blocks may contain machine PHIs;
- after `NoPHIs`, runtime-entered blocks must not contain machine PHIs;
- required AArch64 OxCaml live-ins remain exactly `x0`, `x26`, `x27`, `x28`.

Tests:

- MIR/llc pipeline test that verifies after ISel with runtime-entered PHIs;
- MIR/llc pipeline test that verifies after PHI elimination has no PHIs;
- negative MIR test with a runtime-entered PHI after `NoPHIs`;
- negative MIR test with an extra live-in such as `x1`.

### Stage 3: runtime-entry clobber pass order

Move AArch64 OxCaml runtime-entry clobber insertion after machine PHI
elimination and before register allocation.

Do this with a small pass-pipeline hook or target hook. Do not copy the whole
target-independent optimized register allocation pipeline.

Required pass order:

1. instruction selection;
2. machine SSA cleanup;
3. live variable setup;
4. PHI elimination;
5. AArch64 OxCaml runtime-entry clobber insertion;
6. register allocation.

Tests:

- `-debug-pass=Structure` or equivalent checked pipeline evidence;
- scalar PHI value copied before the raising call;
- regalloc spills/reloads the scalar value across the call;
- final assembly does not use caller-saved garbage for handler-live values.

### Stage 4: LLVM statepoint integration

Implement the GC pointer rule chosen by Stage 0.

Preferred shape when valid:

- keep one recovery entry per statepoint edge that needs exceptional
  `gc.relocate`;
- perform edge-local `gc.relocate`;
- then branch to shared handler code with ordinary PHIs.

Fallback shape when required:

- allocate a narrow root slot for each handler-live GC value that cannot be
  represented by exceptional `gc.relocate`;
- store into that slot before each may-raise operation;
- make the slot visible to OxCaml frame/root metadata in a way that survives
  optimization;
- load the slot in recovery after the allowed recovery prefix.

The experiments showed that a plain alloca in `"gc-live"` before `opt -O2` is
not enough. `opt -O2` can turn the safepoint into `"gc-live"()`. Escaping the
slot or making the load/store volatile keeps the memory operations but still
does not preserve a root location in the stackmap. Therefore this fallback must
use an OxCaml-specific precise root representation, a late-lowering root slot,
or another LLVM construct that the optimizer cannot silently erase.

Do not make all trap-live values volatile.

Tests:

- unique invoke statepoint with exceptional relocate;
- two recovery entries relocating separately then merging;
- shared handler through explicit root slot;
- forced GC before raise proving handler-live GC pointer is updated;
- frame table/root-map check for the explicit slot case.

### Stage 5: OCaml LLVM emitter changes

Update `backend/llvm/llvmize.ml` AArch64 trap lowering.

Emitter responsibilities:

- generate `trap.publish` with the direct recovery landingpad blockaddress;
- emit may-raise calls in active trap regions as `invoke` to the active
  recovery landingpad;
- emit direct `raise_notrace` as an AArch64 pseudo/call-like terminator with
  the same modeled recovery edge;
- keep static trap regions distinct even if they branch to a shared OCaml
  handler;
- route non-GC handler-live values through normal SSA/PHI;
- route GC handler-live values through the Stage 4 mechanism;
- remove broad volatile trap-live slots once focused tests cover the narrower
  representation.

Tests:

- source-level direct call in try, no raise;
- source-level direct call in try, caught raise;
- closure call in try;
- nested try;
- multiple static traps sharing one handler;
- handler uses integer local from before the raising call;
- handler uses GC pointer from before the raising call;
- direct `raise_notrace` caught by current handler.

### Stage 6: remove non-native recovery artifacts

Audit and remove recovery-side code that is no longer part of the model.

Targets:

- remove or narrow `write_trap_pointer_register`;
- ensure `trap.recover` models previous trap pointer from `x26`;
- avoid recovery-side inline asm register rewrites when regalloc can carry the
  value;
- keep stack repair/x29 repair only if the current trap frame layout still
  requires it.

Tests:

- assembly check: no recovery-side `mov x26, ...` inline asm for the intended
  native-shaped path;
- nested handler restore of x26;
- frame-pointer enabled recovery;
- Linux and Darwin triples.

### Stage 7: validation and benchmarks

Run validation in increasing scope:

1. LLVM lit tests for modified CodeGen files.
2. Focused `testsuite/tests/llvm-codegen`.
3. Full LLVM-backend testsuite.
4. Self-stage2 testsuite.
5. Representative microbenchmarks.
6. Native-built compiler vs LLVM-built compiler benchmark.

Benchmark focus:

- `direct_call_in_try_hit`;
- `closure_call_in_try_hit`;
- `closure_call_in_nested_try_hit`;
- `layered_try_raise_hit_only`;
- `env_find_same_layered_hit`;
- current representative microbenchmark set;
- `typecore.ml`, `cfg_to_linear.ml`, and other compiler-binary files.

## Review checkpoints

Stop and revise if any of these happen:

- stock LLVM rejects the statepoint shape we planned to depend on;
- optimizer merges recovery blocks in a way that loses statepoint identity;
- runtime-entered PHIs cannot be made verifier-clean before and after PHI
  elimination;
- root slots are needed so broadly that the design recreates the current
  volatile-slot cost;
- stack maps or frame tables cannot describe the explicit root slot accurately;
- final assembly still contains skipped trampoline work;
- source tests require hidden control-flow edges LLVM cannot see.

## Expected outcome

If the plan works, exception-specific overhead should move closer to native:

- hot non-raising `try` no longer pays broad volatile slot traffic;
- expected-control-flow raise paths use native-shaped direct raise/recovery;
- ordinary handler-live values are handled by PHI elimination and regalloc;
- GC handler-live values are handled precisely enough for the runtime GC;
- remaining slowdowns should be attributable to separate issues such as string
  compare lowering or unrelated compiler-binary effects.
