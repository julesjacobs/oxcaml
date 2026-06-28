# Plan to restore x86-64 support for the LLVM backend

## Scope

Target Linux/AMD64 first. Treat 32-bit x86 as out of scope unless a later goal
explicitly asks for it.

Use the current `jujacobs/llvm-backend` work as the base. The old
`jujacobs/llvm-amd64-support` branch is useful as a checklist and as source
material for focused patches, but it predates the current root tracking,
statepoint, frametable, and AArch64 stack/trap work. Do not copy it wholesale,
and do not keep old x86 mechanisms where they differ from the current ARM64
LLVM backend design.

When the right AMD64 behavior is unclear, match the existing native AMD64
backend first, especially for calling conventions, register roles, stack layout,
runtime entry/exit protocols, and assembly helper behavior. The intended shape
is the current ARM64 LLVM backend generalized to AMD64 target details, not two
independent mechanisms and not compatibility with the old low-quality x86 LLVM
backend.

## Quality bar

The goal is not just to make AMD64 compile through the LLVM backend. AMD64 must
reach the same quality bar as the current ARM64 backend:

- GC roots are discovered after register allocation and are correct for every
  statepoint kind.
- Frametables describe the actual machine frame layout.
- Stack checks have the same quality as ARM64: frontend byte contracts,
  LLVM prologue checks, CFG checks, stack-growth calls, and frametable/root
  metadata all agree.
- Stack growth, allocation slow paths, and polling preserve all live OCaml
  values, including float and SIMD values.
- Exception handling, trap recovery, unwinding, CFI, and backtraces are
  production-quality and not based on known-weak old x86 shortcuts.
- C calls, C-stack switching, noalloc direct calls, and stack-argument calls
  have correct stackmaps and root preservation.
- Tests cover AMD64 directly rather than inferring correctness from ARM64.
- The test suite passes under the LLVM backend, then a self-stage2 compiler
  built through the LLVM backend also passes the same test suite.
- After correctness is established, performance is measured against the native
  AMD64 backend before treating the work as ready.

## Current state

The current LLVM backend is effectively ARM64-only in validated use:

- The main `testsuite/tests/llvm-codegen`, `llvm-gc-roots`, and
  `llvm-stack-checks` coverage skews ARM64 and many checks are gated with
  `arch_arm64` or check AArch64-specific assembly/intrinsics.
- `oxcaml/tests/backend/llvmize/dune.inc` already has many AMD64 LLVM-backend
  frontend/IR smoke rules. Reuse those early instead of starting from zero.
- `backend/llvm/llvmize.ml` rejects non-AArch64 targets when stack checks are
  enabled.
- ARM64 has modern backend support for stack checks, native trap handling,
  runtime register handling, and target-specific statepoint/root behavior.
- X86-64 still has some old backend paths in the frontend lowering, especially
  around `wrap_try`/RBP trap recovery, but those are from the earlier x86
  backend and should not be assumed to be production-quality.

Vendored LLVM already contains some x86-64 OxCaml calling-convention skeleton:

- `X86CallingConv.td` defines OxCaml argument/return registers and callee-saved
  register sets.
- `X86RegisterInfo.cpp` has cases for OxCaml callee-saved lists and masks.
- `X86ISelLowering.cpp` has some OxCaml calling-convention awareness.

That is not enough for proper support. The missing work is mainly complete
target runtime invariants, direct C-call support, SIMD/vector call-boundary
support, stack growth, statepoint/root correctness, exception/trap handling,
C-call lowering, and AMD64-specific operations.

## Bring-up plan

### 1. Establish an AMD64 smoke baseline

- Build the current branch on AMD64 with the standard compiler and the vendored
  LLVM used by the backend.
- Compile tiny programs with `-llvm-backend -S -keep-llvmir`, initially with
  stack checks disabled if needed, to expose the first target failures without
  mixing in stack growth.
- Add a small AMD64 LLVM-backend smoke test directory or split existing
  `llvm-codegen` expectations so x86 checks are not hidden behind ARM64-only
  tests.
- Keep this baseline intentionally small: integer calls, allocation-free
  arithmetic, simple allocation, direct call, indirect call, and one tail call.

### 2. Implement and verify x86-64 calling conventions and runtime registers

The existing x86 OxCaml calling-convention definitions are only a starting
point. After rebuilding LLVM, implement or verify:

- OCaml integer arguments/results use the expected AMD64 register order.
- Float and vector values crossing OCaml call boundaries use the correct
  XMM/YMM/ZMM locations. SIMD call-boundary support is not optional if SIMD
  values can be live across OCaml calls or allocation slow paths.
- Runtime registers use the intended physical registers, especially domain
  state and allocation pointer.
- X86 has ARM64-equivalent runtime-live-in handling, including target hooks such
  as `isRuntimeEnteredLiveIn` / `getRuntimeEnteredLiveIns` where needed.
- The register allocator cannot allocate runtime registers such as R14/R15 in
  OxCaml functions except where the convention explicitly allows them.
- Direct, indirect, tail, allocation, C-call, C-stack-call, and direct noalloc
  C-call conventions have the right argument assignments and callee-saved masks.
- `OxCaml_C_Direct_Call` is supported in the x86 calling-convention tables,
  register masks, and lowering paths. Current x86 support appears to cover
  `OxCaml_C_Call`, `OxCaml_C_Call_StackArgs`, and `OxCaml_Alloc` more fully
  than direct noalloc C calls.

Expected LLVM files to inspect and adjust:

- `vendor/llvm-project/llvm/lib/Target/X86/X86CallingConv.td`
- `vendor/llvm-project/llvm/lib/Target/X86/X86RegisterInfo.cpp`
- `vendor/llvm-project/llvm/lib/Target/X86/X86ISelLowering.cpp`
- `vendor/llvm-project/llvm/lib/Target/X86/X86FrameLowering.cpp`

Use the AArch64 OxCaml target code as the model for modern invariants, not the
old x86 branch by default. For ABI details, compare against the existing native
AMD64 backend and make LLVM-generated code interoperate with it.

### 3. Restore minimal AMD64-specific operation lowering

The current `backend/llvm/llvmize.ml` mostly contains ARM64-specific lowering.
An AMD64 configured build will produce AMD64-specific operations from
`backend/amd64`, so the LLVM backend needs target dispatch before many smoke or
self-stage failures can get as far as exceptions or GC.

Recommended shape:

- Reintroduce a small target-specific lowering interface instead of growing one
  giant match in `llvmize.ml`.
- Port only the minimal AMD64 operations needed for smoke/self-stage first.
- Expand SIMD and builtin coverage incrementally with tests.

Initial AMD64 operation coverage:

- Addressing and LEA-like operations.
- Integer loads/stores with AMD64-specific widths/sign-extension.
- Byte swaps and 32-bit extension operations.
- Floating arithmetic with memory operands.
- `rdtsc` / `rdpmc` if required by the configured compiler.
- Pause/fence/prefetch operations needed by runtime or generated code.

Second wave:

- SIMD operations.
- SIMD memory operations.
- Vector constants and casts.
- Packed float operations.
- `cldemote`, prefetch variants, and int128 helpers if tests or self-stage
  require them.

Old material to mine:

- `backend/llvm/llvmize_specific_types.ml`
- old `backend/amd64/llvmize_specific.ml`
- old AMD64-specific match arms in `backend/llvm/llvmize.ml`
- old AMD64 LLVM codegen tests

Do not blindly copy the old SIMD lowering. Port by operation family and add
focused tests as each family becomes necessary.

### 4. Implement ARM64-quality AMD64 stack checks and stack growth

This is the hard blocker before removing the AArch64-only guard in
`backend/llvm/llvmize.ml`, but it is also a correctness feature in its own
right. The AMD64 implementation should satisfy the same stack-check contract as
ARM64, not just emit a slow-path call that happens to work in simple cases.

Work needed:

- Add x86-64 frame-lowering support for the current `oxcaml-stack-check`
  function attributes.
- Preserve the current producer/consumer contract for
  `oxcaml-stack-check-bytes` and `oxcaml-stack-check-before-bytes`.
- Disable the red zone for OxCaml stack-check functions.
- Emit a prologue check that compares the OCaml stack pointer against the stack
  threshold without pushing temporary data on the OCaml stack.
- Add or revive an AMD64 runtime helper equivalent to the old
  `caml_llvm_prologue_realloc_stack`, but align it with the current ARM64 stack
  growth contract.
- Reuse or closely match the existing AMD64 save/restore policy in
  `runtime/amd64.S` for `caml_call_realloc_stack{,_sse,_avx,_avx512}` and
  `caml_call_gc{,_sse,_avx,_avx512}` rather than inventing a separate policy.
- Preserve and restore all OCaml registers that can be live across stack growth.
- Preserve float and SIMD registers conservatively until helper selection is
  proven correct.
- Ensure statepoint IDs and frametable stack offsets remain correct when the
  prologue can call into runtime stack reallocation.
- Ensure ordinary CFG stack checks and LLVM prologue stack checks are both
  represented as statepoints when they can allocate, and that roots live across
  those checks are reported exactly once.

Old material to mine:

- `runtime/amd64.S` from `jujacobs/llvm-amd64-support`, especially
  `caml_llvm_prologue_realloc_stack`.
- Old x86 `X86FrameLowering.cpp` changes that emitted inline stack checks and
  disabled the red zone.

Validation:

- Add AMD64 versions of the LLVM stack-check tests.
- Check both small-frame and large-frame functions.
- Port or extend `stack_check_size_contract` so AMD64 checks the same byte-count
  contract as ARM64.
- Check stack growth success and stack overflow failure paths.
- Test functions with roots live across CFG stack checks, prologue checks, and
  stack-growth calls.
- Run with GC/stack stress where possible.

### 5. Make statepoints, frametables, and roots target-correct on AMD64

The current backend relies on post-register-allocation root discovery and LLVM
statepoints. AMD64 must satisfy the same contract as ARM64.

Work needed:

- Audit and fix the existing `OxCamlGCPrinter` AMD64 support. It already has
  x86 register mapping and non-AArch64 return-address-size handling, so this is
  not a from-scratch printer, but it still must be proven against the current
  backend contracts.
- Audit SP-relative, RBP-relative, base-pointer, stack-realignment, and
  dynamic-stack-adjust cases.
- Decide whether X86 should force `STATEPOINT` frame-index operands to resolve
  SP-relative, as AArch64 does in `eliminateFrameIndex`, or prove the printer
  handles every RSP/RBP/base-pointer form correctly.
- Verify root spill slots after register allocation using
  `OxCamlStatepointSpillRoots` and `OxCamlGCRootVerifier`.
- Confirm the backend emits correct stackmaps for direct OCaml calls, indirect
  OCaml calls, allocation slow paths, C calls, and C-stack calls.
- Decide whether register roots are allowed at each statepoint kind on AMD64;
  if not, force spills consistently.

Validation:

- Enable or duplicate `llvm-gc-roots` tests for AMD64.
- Add focused LLVM/MIR tests for stack root offsets and callee-saved live roots.
- Use young-heap stress and allocation-heavy tests to exercise relocation.

### 6. Rework x86 exception and trap handling to ARM64 quality

The current non-AArch64 path still contains the old `wrap_try` and recover-RBP
approach. Because the previous x86 backend was known to be weak, this path
should not be the final design.

Required direction:

- Implement a modern x86 analogue of the AArch64 native trap machinery, with
  target-aware LLVM intrinsics or pseudos lowered in the x86 backend.
- Track active trap state in frame lowering so frametable/debug records match
  the actual machine frame.
- Ensure CFI and unwinding work through raise, reraise, notrace raise, and
  exception handler entry.
- Decide explicitly whether x86 dynamic trap frames require extending the
  statepoint-ID/printer contract for active trap bytes. Current active-trap-byte
  handling is AArch64-specific; x86 must either avoid needing it or add the same
  quality of encoding and printing support.
- Treat `wrap_try`/recover-RBP only as a diagnostic stepping stone. Final AMD64
  support should not rely on that old path unless it has been redesigned and
  validated to the same standard as the ARM64 trap machinery.

Expected LLVM target files to inspect and adjust:

- `vendor/llvm-project/llvm/lib/Target/X86/X86Instr*.td`
- `vendor/llvm-project/llvm/lib/Target/X86/X86ISelLowering.cpp`
- `vendor/llvm-project/llvm/lib/Target/X86/X86ISelDAGToDAG.cpp`
- `vendor/llvm-project/llvm/lib/Target/X86/X86MachineFunctionInfo.*`
- `vendor/llvm-project/llvm/lib/Target/X86/X86FrameLowering.cpp`
- `vendor/llvm-project/llvm/lib/Target/X86/X86RegisterInfo.cpp`

Validation:

- Simple `try ... with` tests.
- Nested handlers.
- Reraise and `raise_notrace`.
- Backtrace tests.
- Exceptions crossing allocation and C-call sites.

### 7. Validate C calls and stack switching

AMD64 must support all current C-call conventions used by the LLVM backend:

- `oxcaml_ccc`
- `oxcaml_c_stackcc`
- `oxcaml_c_directcc`
- `oxcaml_alloccc`

Work needed:

- Verify stack switching to the C stack on AMD64.
- Confirm `llvm.read_register` / `llvm.write_register` use the correct `rsp`
  behavior in wrappers and direct calls and do not confuse frame lowering, CFA
  tracking, unwinding, or root locations.
- Validate noalloc direct C calls and C calls with stack arguments.
- Check root preservation around blocking and non-blocking C calls.
- Revisit slow-path helper selection when float or SIMD registers are live.

Old material to mine:

- Old branch fixes for explicit stackmaps on C calls.
- Old branch logic for choosing `caml_call_gc_sse`, `caml_call_gc_avx`, or
  `caml_call_gc_avx512` when SIMD state must be preserved.

Validation:

- Add MIR/asm tests proving stack restore and alignment after C-stack switches.
- Add CFI/unwind checks for C-stack-switching functions.
- Add root-location checks across direct noalloc C calls and stack-argument C
  calls.

## Test material to reuse or port

Current tests that should gain AMD64 counterparts or AMD64 expectations:

- `testsuite/tests/llvm-codegen/allocation_frametable.ml`
- `testsuite/tests/llvm-codegen/poll_statepoint.ml`
- `testsuite/tests/llvm-codegen/raw_stack_word.ml`
- `testsuite/tests/llvm-codegen/trap_recovery_runtime.sh`
- `testsuite/tests/llvm-codegen/stack_growth.ml`
- `testsuite/tests/llvm-codegen/stack_check_size_contract.ml`
- `testsuite/tests/llvm-gc-roots`
- `testsuite/tests/llvm-stack-checks`

Old branch tests to mine by name:

- `testsuite/tests/llvm-codegen/amd64_core_ops.ml`
- `testsuite/tests/llvm-codegen/amd64_direct_call_stackmap.ml`
- `testsuite/tests/llvm-codegen/amd64_exceptions.ml`
- `testsuite/tests/llvm-codegen/amd64_raise_notrace_alloc.ml`
- `testsuite/tests/llvm-codegen/amd64_probe_is_enabled.ml`
- `testsuite/tests/llvm-codegen/amd64_int128_ops.ml`
- `testsuite/tests/llvm-codegen/amd64_{avx,bmi,fma,lzcnt,popcnt,prefetch,simd}_*`

## Validation sequence

Use this order so failures stay attributable:

1. Rebuild vendored LLVM after each target-side change.
2. Run focused LLVM lit/MIR tests for x86 OxCaml calling convention, stack
   checks, root spills, and trap lowering.
3. Run the AMD64 rules in `oxcaml/tests/backend/llvmize/dune.inc` as
   frontend/IR smoke coverage.
4. Compile tiny AMD64 programs with the standard compiler and `-llvm-backend`.
5. Run AMD64 `llvm-codegen` smoke tests.
6. Run AMD64 `llvm-stack-checks`.
7. Run AMD64 `llvm-gc-roots`.
8. Run allocation, exception, C-call, and backtrace tests with GC stress.
9. Install a compiler with the LLVM backend enabled and run smoke tests.
10. Run the full relevant test suite under the standard compiler using the
    LLVM backend.
11. Run self-stage2 validation.
12. Run the same relevant test suite under the self-stage2 LLVM-backend
    compiler.
13. Run `make -s fmt`, `make -s boot-compiler`, and `make -s test` before
    calling the work complete.
14. After correctness is green, measure performance against the native AMD64
    backend and record the results before deciding what remains.

During agent work in this workspace, follow `AGENTS.md`:

- Run `eval "$(../../../scripts/agent-tmp-env)"` inside the agent checkout
  before LLVM-backend work.
- Use the standard installed compiler with `-llvm-backend` for focused
  reproducers before escalating to self-stage2-only reproducers.
- Avoid concurrent `make` or `dune` commands in the same checkout.
- Commit real code or test progress regularly so each step has a reviewable
  checkpoint.
- After each progress commit, do a code-review pass on that commit before
  building further on it: look for correctness bugs, missing tests, ABI
  mismatches with the native AMD64 backend, and gaps relative to the ARM64
  quality bar.
- Iterate on review findings immediately, either by amending the current
  checkpoint before push or by adding a follow-up fix commit when the branch is
  already shared.

## Main risks

- AMD64 frame layout differs enough from AArch64 that root offsets can be
  subtly wrong, especially with frame pointers, stack realignment, base
  pointers, and dynamic stack adjustment.
- Stack-check byte contracts can go stale if CFG stack use, LLVM frame size,
  prologue stack use, or stack-growth statepoints disagree.
- The old x86 exception path was not designed around the current backend
  invariants and may hide correctness bugs.
- Stack growth must not clobber live OCaml, float, or SIMD registers.
- C calls can invalidate roots if stackmaps or runtime-register threading are
  incomplete.
- SIMD live-across-allocation support needs conservative preservation until the
  exact helper selection is proven.
- Tests currently skew heavily toward ARM64, so early "green" x86 results may
  mean low coverage rather than correctness.

## First concrete patch stack

1. Add AMD64 smoke tests and a minimal way to compile with stack checks disabled
   for diagnosis.
2. Audit and fix x86 OxCaml calling-convention live-ins, reserved registers,
   callee-saved masks, direct noalloc C-call support, and SIMD call-boundary
   support.
3. Reintroduce target-specific AMD64 operation lowering, starting with the
   minimal scalar set needed to reach meaningful smoke tests.
4. Implement AMD64 stack checks and the runtime stack-growth helper.
5. Remove the AArch64-only stack-check guard only after AMD64 stack checks are
   implemented.
6. Enable AMD64 root/statepoint tests and fix frame/root offset issues.
7. Replace the old x86 trap path with ARM64-quality native trap handling, or
   redesign it until it satisfies the same quality bar.
8. Expand AMD64 operation lowering to SIMD and remaining builtins.
9. Run self-stage2 and then the broader test suite.
