# AMD64 vs ARM LLVM backend parity plan

## Goal

Bring the AMD64 LLVM backend back onto the same mechanism as the ARM LLVM
backend, with differences explained by AMD64 ABI/runtime requirements rather
than inherited old x86 backend behavior.

The GC mechanism is the current statepoint/root model:

- no frontend root allocas in the normal path;
- no interior pointers in stackmaps;
- derived pointers are rematerialized from relocated base objects;
- live roots are discovered and verified late, after register allocation;
- frametable entries describe the actual machine frame.

When ARM and AMD64 differ, use the native AMD64 backend as the ABI reference
for calling conventions, trap frames, stack growth, C stack switching, frame
pointer behavior, and runtime helper contracts.

## Current AMD64 weak spots

### 1. Exception and trap handling

ARM uses runtime-entered trap recovery with target trap push/recover intrinsics
and a runtime-entry machine pass. AMD64 now appears to use the same high-level
mechanism for normal traps, not the old `wrap_try` path.

Investigation findings:

- Current AMD64 `Pushtrap` lowering dispatches to `emit_x86_pushtrap`.
- A focused `try`/`raise_notrace` compile emitted
  `llvm.x86.oxcaml.push.trap.with.domain`,
  `llvm.x86.oxcaml.trap.recover`, an `invoke ... unwind label`, and
  `llvm.x86.oxcaml.pop.trap.with.domain`.
- The generated assembly used native-style trap frames: push recovery target,
  push previous `Caml_state->exn_handler`, publish `%rsp`, and recover through
  a runtime-entered block.
- No call to `wrap_try` was emitted in the focused trap case, although a
  private `wrap_try` definition is still emitted into LLVM IR.
- Focused checks passed with the local LLVM wrapper:
  `testsuite/tests/llvm-codegen/trap_recovery_runtime.sh` and
  `testsuite/tests/llvm-codegen/amd64_exception_backtrace.sh`.

Remaining concerns:

- `backend/llvm/llvmize.ml` still contains the old `wrap_try` /
  returns-twice implementation and still defines `wrap_try` on AMD64 even when
  it is unused.
- Some comments/tests are stale, for example AMD64 trap-root coverage still
  says the x86 exception edge uses old `wrap_try`.
- X86 still carries both explicit-domain trap intrinsics and legacy hidden-`r14`
  trap intrinsics for bootstrapping existing installed compilers. This may be
  acceptable temporarily, but it is still two target interfaces.
- AMD64 trap recovery is more fragile than ARM because it must reconstruct
  `%rbp`, track active native trap-frame bytes, and emit correct CFI around
  push/pop trap frames and runtime-entered recovery blocks.
- X86 needs an extra late `MoveRecoverOnly` runtime-entry pass before pseudo
  expansion. That may be justified, but it is another AMD64-specific moving
  part that needs direct tests.

Plan:

- Prove `wrap_try` is dead for supported AMD64 LLVM-backend configurations:
  compile representative ordinary traps, `raise_notrace`, reraise, nested
  handlers, async exception paths, and stack-overflow paths; check for zero
  `wrap_try` call sites, not merely for a definition.
- Stop emitting the unused private `wrap_try` definition on AMD64 once the
  no-call evidence is covered by tests.
- Delete the unreachable old `Pushtrap` branch and recover-RBP-era comments if
  no supported target still uses it. If some target still needs it, split it
  into an explicitly unsupported/legacy path so AMD64 cannot accidentally
  regress to it.
- Update stale tests/comments that claim AMD64 still uses the old `wrap_try`
  exception edge.
- Decide whether the legacy hidden-`r14` X86 trap intrinsics can be removed now
  or must remain for bootstrapping. If they must remain, document the
  transition rule and add checks that freshly generated AMD64 IR uses only the
  explicit-domain intrinsics.
- Add or strengthen focused LLVM/MIR tests for:
  - `OXCAML_TRAP_RECOVER` placement after scheduling/pseudo movement;
  - runtime-entered live-ins `RAX`, `R14`, and `R15`;
  - active trap byte accounting with nested traps;
  - `%rbp` reconstruction in recovery blocks;
  - CFI around trap push/pop and runtime-entered handlers;
  - raise-notrace edge lowering to recovery blocks.
- Keep the existing runtime checks in the gate:
  `trap_recovery_runtime.sh`, `amd64_exception_backtrace.sh`, async exception
  tests, stack-overflow tests, and mixed-blocks/GC stress where trap handlers
  and frametables interact.

### 2. Statepoint and GC root policy

AMD64 uses the right high-level GC mechanism, but some AMD64 rules still live
as x86-only branches in shared statepoint fixup code. In particular, ordinary
`OxCaml_WithFP` calls force register operands to stack because `%rbp` is
preserved but not a scannable OCaml root register.

Calling-convention comparison:

- AArch64 and AMD64 both define the same six OxCaml LLVM calling-convention
  IDs: `OxCaml_WithFP`, `OxCaml_WithoutFP`, `OxCaml_C_Call`,
  `OxCaml_C_Call_StackArgs`, `OxCaml_Alloc`, and `OxCaml_C_Direct_Call`.
- Both targets thread two runtime registers through ordinary calls and returns
  (`x28`/`x27` on AArch64, `r14`/`r15` on AMD64), and both have separate
  C-call, stack-args C-call, direct-C-call, and allocation preserved masks.
- The important ARM advantage is not a missing AMD64 calling-convention ID; it
  is that ARM expresses more of the rootability policy in target register
  hooks and target masks, including the in-place C-call preserved mask and
  target-forced spills for registers without ordinary `gc_regs` slots.
- AMD64 should keep following that shape: target-specific rootability and ABI
  facts belong in X86 register/calling-convention/frame hooks, while the shared
  statepoint fixup pass should ask target questions instead of branching on
  x86_64 behavior.

Status:

- The ordinary-call `%rbp` rule now belongs to `X86RegisterInfo` through a
  statepoint-calling-convention-aware `shouldSpillStatepointGCPtr` target hook.
  This follows the ARM design: the shared fixup pass asks the target whether a
  physical register can be a root location for this statepoint kind.
- `FixupStatepointCallerSaved.cpp` no longer has an x86_64 branch for ordinary
  `OxCaml_WithFP` calls. The shared pass still has shared OxCaml C-call logic
  because C calls must spill all register root operands on both targets.
- Target-forced spills of preserved registers use the same read-write stack
  root semantics as the existing spill-all path: if the collector may relocate
  the slot, post-statepoint users reload from the slot rather than trusting the
  preserved physical register.

Plan:

- Move AMD64 rootability decisions into target ABI/register hooks, similar to
  ARM `shouldSpillStatepointGCPtr`. Initial cleanup done for ordinary
  `OxCaml_WithFP` `%rbp`; continue auditing other AMD64-specific statepoint
  branches before broadening the abstraction.
- Extend the shared OxCaml ABI abstraction with explicit questions such as:
  which registers may appear as frametable register roots, which must be
  spilled, which registers are runtime-only, and which statepoint kinds may use
  register roots.
- Remove or justify x86-only branches in `FixupStatepointCallerSaved.cpp`.
- Keep the invariant that OxCaml stackmaps contain only real object roots:
  every base/derived pair must be identical by stackmap emission, and derived
  values must be rematerialized after the safepoint.
- Run root verifier tests before every wider validation run.

### 3. Stack checks and stack growth

ARM's stack growth path is simpler. AMD64 has real extra obligations:
`%rbp` rewriting, native trap-frame depth, C stack switching, and SIMD-preserving
helper variants.

Plan:

- Treat AMD64 stack growth as one ABI contract instead of scattered helper-name
  choices.
- Centralize helper selection for normal stack checks, prologue stack checks,
  allocation slow paths, polling, and stack-growth calls.
- Verify frame-pointer behavior against native AMD64; do not copy ARM's frame
  chain behavior literally where native AMD64 rewrites `%rbp`.
- Test stack overflow timing, prologue checks, CFG checks, stack growth under
  active traps, and stack growth with live roots and live SIMD values.

### 4. Frame descriptors and active trap offsets

AMD64 frametable correctness is more delicate because native trap frames affect
SP-relative stackmap offsets. ARM has a cleaner offset model.

Plan:

- Audit every place AMD64 active trap bytes are computed, encoded into
  statepoint IDs, and added to frame-index offsets.
- Keep statepoint stack locations SP-relative unless there is a proven printer
  contract for some other base register.
- Add MIR/LLVM tests for stackmap offsets with zero, one, and nested active
  traps.
- Keep mixed-blocks and GC stress tests in the focused validation set because
  previous failures included missing frame descriptors.

### 5. C calls and C stack switching

AMD64 has direct C-call stack switching and multiple C-call helper conventions.
ARM does not need the same amount of target-specific machinery.

Plan:

- Compare `OxCaml_C_Call`, `OxCaml_C_Call_StackArgs`, and
  `OxCaml_C_Direct_Call` against the native AMD64 backend.
- Make the calling-convention tables, call lowering, stack switching, preserved
  masks, runtime-register returns, and stackmaps agree.
- Test noalloc direct C calls, allocating C calls, C calls with stack
  arguments, callbacks, exceptions across C boundaries where applicable, and
  C calls with live OCaml roots.

### 6. SIMD preservation

AMD64 must preserve XMM/YMM/ZMM state across GC, polling, stack growth, and
runtime helper calls. ARM's floating/vector preservation story is simpler.

Plan:

- Define one AMD64 SIMD save-class policy: none/SSE/AVX/AVX512.
- Use that policy consistently for allocation slow paths, polling, stack
  growth, and C-call helper paths.
- Add tests where SIMD values and vector roots are live across each safepoint
  kind.
- Verify helper suffix selection against the subtarget and against the live
  values at the call.

### 7. Performance around safepoints and calls

The main AMD64 performance risk is extra stack traffic around statepoints,
calls, exception roots, and preserved registers. Local post-RA peepholes are the
wrong default fix.

Plan:

- For each large slowdown, compare AMD64 LLVM against ARM LLVM mechanism and
  native AMD64 code shape.
- Classify the cause as ABI-required, regalloc policy, statepoint lowering, or
  legacy x86 mechanism.
- Prefer regalloc/calling-convention/root-policy fixes over local instruction
  forwarding or reload peepholes.
- Keep code-review on every performance commit and require focused tests plus
  at least a representative benchmark rerun.

### 8. Legacy frontend-ish AMD64 paths

AMD64-specific control-flow, trap, and runtime-register tricks in
`backend/llvm/llvmize.ml` should be suspicious unless they are clearly the
shared ARM mechanism parameterized by AMD64 ABI facts.

Plan:

- Audit all `Target_system.X86_64` branches in `llvmize.ml`.
- Classify each branch as:
  - real AMD64 ABI difference;
  - target lowering that should move into vendored LLVM;
  - obsolete old x86 backend mechanism;
  - shared logic that should be expressed without an architecture branch.
- Remove obsolete branches and move target behavior into X86 lowering,
  register info, frame lowering, calling convention tables, or shared OxCaml
  ABI helpers.

## Validation gates

Use this order after each real change:

1. Focused lit/MIR tests for the touched target mechanism.
2. Focused OxCaml tests for `llvm-codegen`, `llvm-gc-roots`, and/or
   `llvm-stack-checks` as appropriate.
3. Root verifier / machine verifier enabled where possible.
4. Standard installed compiler with `-llvm-backend`.
5. Full LLVM-backend test suite.
6. LLVM self-stage2 build.
7. Self-stage2 test suite.
8. Performance benchmark against the native AMD64 backend.

Commit real progress regularly. Each commit should have focused validation and
code review before building more changes on top of it.
