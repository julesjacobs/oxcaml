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

Scope rule: performance wins for the compiler-throughput goal must be
LLVM-path-only. Generic driver optimization levels or build flags such as
`-O4` do not count, because they can be applied equally to the native-built
compiler. Valid candidates are LLVM backend/codegen changes, LLVM-built-only
pipeline decisions, or post-link work that is specific to the LLVM-built binary.

Current focused benchmark finding:

- The largest investigated slowdown is
  `loop_invariant_gc_across_call_dynamic_reps`, a dynamic-repetition variant of
  the loop-invariant GC microbenchmark. Making `reps` dynamic removes the
  five-copy constant-entry cloning from the original benchmark, leaving a single
  nested hot loop in the module entry.
- The dynamic benchmark still shows AMD64 LLVM roughly 1.5x slower than native
  in local runs, so constant outer-loop cloning is not the main cause.
- Assembly ablations on the linked LLVM object show what is not causal:
  replacing the hot-loop `String.length x` with a constant does not materially
  improve runtime; changing only the string byte-load address form toward
  native does not help; rewriting LLVM frame-slot references from `%rbp`
  offsets to equivalent `%rsp` offsets does not help; inlining the tiny `tick`
  body alone does not help.
- A full native-shaped replacement of LLVM's hot module-entry loop closes the
  gap. With `reps=20`, local medians were approximately:
  native original `0.266s`, LLVM control `0.404s`, native-shaped LLVM loop
  `0.265s`.
- A native-control/register-layout variant that kept LLVM's string-length
  address form also closed the gap (`0.266s` at `reps=20`). This rules out the
  string-length addressing sequence as the important difference.
- An unsafe diagnostic variant that keeps loop state in registers across this
  known leaf `tick` call runs faster than native (`0.225s` at `reps=20`), which
  confirms the broad cost is the call/statepoint-adjacent loop-state
  spill/reload shape, not the call instruction itself.
- Therefore the concrete LLVM-vs-native gap for this benchmark is the AMD64
  LLVM loop-state/register-allocation shape around the statepoint call. Native
  arranges the hot loop around `%rbx`/`%rdi` and compact `%rsp` slots:
  root `x` at `(%rsp)`, reps bound at `8(%rsp)`, `i` in `%rdi`/`16(%rsp)`,
  inner accumulator in `%rbx`/`24(%rsp)`, and outer state at `32/40(%rsp)`.
  LLVM's generated shape instead uses a generic `%rax`/`%rcx` loop with
  statepoint-adjacent spills/reloads through separate frame slots.
- MIR pass investigation pins down where this shape appears. In `after-greedy`,
  the hot entry loop still has virtual registers and no frame slots in the loop;
  the statepoint lists the string root as a virtual GC operand. After
  `OxCamlStatepointSpillRoots`, the statepoint's GC operand is correctly a
  stack slot for the string root, while the two integer loop-state slots are
  ordinary regalloc spill slots, not GC roots. After virtual-register rewriting,
  those integer slots become the final `%rax`/`%rcx` spill/reload pattern.
- The AMD64 ABI model explains why the scalar values must be saved somewhere:
  `CSR_64_OxCaml_WithFP` preserves only `%rbp`, so managed calls clobber normal
  integer registers. Native's `%rbx`/`%rdi` loop shape is not relying on those
  registers being preserved; it also saves and reloads them around `tick`.
- A focused ablation that keeps LLVM's frame layout and statepoint root slot,
  but changes only the hot inner loop's scalar plan toward the native
  `%rbx`/`%rdi` shape, improves the same linked LLVM object from a median
  `0.406s` to `0.231s` at `reps=20` in a same-run comparison
  (`native = 0.269s`). This is the most precise evidence so far: the large
  slowdown is caused by the scalar loop-state allocation/schedule chosen around
  the statepoint, not by the string root stack mechanism or frame-pointer
  addressing.
- Deeper MIR/debug investigation identifies the exact decision point. The
  pre-regalloc MIR for the hot loop is still reasonable: `%128` is the loop
  counter, copied to fixed `$rax` only for the `tick` call, and `%127` is the
  loop-carried accumulator. Greedy register allocation then splits the
  live-through call intervals because `csr_64_oxcaml_withfp` preserves only
  `%rbp`. The inline spiller creates stack slots for the scalar split pieces:
  `%148` from original `%128` is assigned `%rcx`, spilled to `%stack.4`, and
  reloaded after the call; `%145` from original `%127` is assigned `%rax`,
  spilled to `%stack.5`, and used after the call. The GC root is separate:
  original `%118` becomes the root stack slot `%stack.2`.
- The harmful part is not the register names. Reserving early caller-saved
  registers for regalloc left the hot loop unchanged. Adding MIR
  `preferred-register` hints for `%127 -> $rbx` and `%128 -> $rdi` was too weak
  to change the final loop. Temporarily changing X86 `GR64` allocation order to
  put `RBX, RDI` first changed the physical registers but kept the bad
  store/reload/folded-memory shape and did not improve runtime
  (`0.414s` at `reps=20`). `basic` regalloc is also slow (`0.412s`) and still
  keeps the loop state in stack slots/folded memory operations.
- The successful ablation is X86 spill-fusing. With normal `llc` options,
  `reps=20` control was `0.410s`. `-disable-spill-hoist` alone was unchanged
  (`0.408s`). `-disable-spill-fusing` improved to `0.237s`, and
  `-disable-spill-hoist -disable-spill-fusing` improved to `0.230s`, matching
  the hand-written scalar-loop ablation. The no-fusing assembly still spills
  before the statepoint call, which is correct for clobbered scalar registers,
  but reloads the scalar values into registers after the call and uses
  register-register arithmetic:
  `movq -48(%rbp), %rcx; movq -40(%rbp), %rdi; addq %rdi, %rax`.
  The slow default instead folds the accumulator reload into
  `addq -40(%rbp), %rax`, producing the high-cost loop shape.
- Implementation hook: generic spilling goes through
  `InlineSpiller::foldMemoryOperand` in
  `vendor/llvm-project/llvm/lib/CodeGen/InlineSpiller.cpp`; X86 implements the
  fold in `X86InstrInfo::foldMemoryOperandImpl`, guarded globally by the
  hidden `disable-spill-fusing` option in
  `vendor/llvm-project/llvm/lib/Target/X86/X86InstrInfo.cpp`. A real fix should
  be targeted, not a global disabling of X86 spill folding.
- Important benchmark correction: the first broad spill-fusing ablation used
  `agent-state/test-suite-29e4cd/llc-wrapper.sh`, a temporary helper that fed
  raw pre-optimization LLVM IR directly to `llc`. That is not the intended
  OxCaml LLVM pipeline. The real wrapper,
  `tools/llvm-rs4gc-llc-wrapper.sh`, runs
  `default<O3>,rewrite-statepoints-for-gc,verify` before `llc`.
  The raw-wrapper run made `matmul` look about 7x slower because 211
  frontend-style `alloca`s survived to machine code. Re-running only
  `matmul,matmul_transposed` with the real wrapper and `SAMPLES=7,WARMUPS=2`
  gives:
  - `matmul`: native `0.1158s`, LLVM `0.1062s`, ratio `0.9171`
    (roughly parity; native samples varied).
  - `matmul_transposed`: native `0.0920s`, LLVM `0.0659s`, ratio `0.7162`.
  The saved LLVM assembly has the expected direct double-load,
  `vmulsd`/`vaddsd`, XMM-accumulator loop shape. Therefore the broad
  `spill_fusing_effect/` tables are invalid as project-pipeline performance
  data and must not drive prioritization. They should be rerun through the real
  wrapper before drawing any global conclusion about spill-fusing flags.
- Full corrected rerun with the real wrapper (`SAMPLES=7,WARMUPS=2`) is saved
  in `agent-state/test-suite-29e4cd/real_wrapper_full_bench_20260629/`. Across
  the 84 broad benchmark cases (minibench, benchmarksgame, exception
  microprobe), total LLVM/native runtime ratio is `0.8822`, geomean `0.8840`,
  with 21 cases slower than native, 10 above 1.05x, and 7 above 1.10x.
  Including the four focused loop-invariant probes gives 88 cases total, total
  ratio `0.8858`, geomean `0.8972`, 23 slower, 12 above 1.05x, and 9 above
  1.10x. Largest broad slowdowns are
  `exception/closure_call_many_handler_live_roots_raise` `1.1912x`,
  `minibench/hash_batch_murmur_mix` `1.1807x`,
  `exception/raise_caught_cross_function` `1.1734x`,
  `benchmarksgame/nbody_1` `1.1401x`, and
  `exception/raise_payload_caught_cross_function` `1.1256x`. The focused GC
  loop-invariant probes remain the largest overall slowdowns: dynamic reps
  `1.5823x`, fixed reps `1.5735x`. Full summary:
  `agent-state/test-suite-29e4cd/real_wrapper_full_bench_20260629/summary.md`.
- Slowdown deep dive artifacts and classification are in
  `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/report.md`.
  The float `sqrt` target has been implemented for AMD64 LLVM by selecting
  non-builtin `sqrt`/`sqrtf` through the existing AMD64 SIMD path. A fresh
  custom LLVM boot compiler emits `llvm.sqrt.f64`/`llvm.sqrt.f32` and final
  `vsqrtsd`/`vsqrtss`; focused `nbody_1` rerun is now LLVM/native `0.9983`.
  The best remaining targets are targeted statepoint-adjacent scalar
  spill-fusing suppression for the loop-invariant GC probe, and then exception
  invoke/landingpad/trap-loop state placement using `raise_caught_cross_function`
  as the pure exception-regalloc case and
  `closure_call_many_handler_live_roots_raise` as the exnroot-pressure case.
- A later `typing/ctype.ml` MIR comparison narrows the root-pressure issue to
  greedy register allocation/spill placement before
  `OxCamlStatepointSpillRoots`. Greedy causes OXSR to append `1629` stack-slot
  roots for `ctype`, while diagnostic `-regalloc=basic` appends only `74`
  but miscompiles boot. In `camlCtype__unify3_569_1424_code`, slots such as
  `%stack.24` are ordinary GC-family spill homes live across many ordinary
  `OxCaml_WithFP` calls; OXSR correctly lists them after the fact. Required
  physical-register fixup roots are a separate correctness mechanism and should
  not be optimized away. The candidate fix should therefore make AMD64 greedy
  avoid or shorten these long-lived GC spill homes before OXSR, not delete
  frametable roots late.

Plan:

- For each large slowdown, compare AMD64 LLVM against ARM LLVM mechanism and
  native AMD64 code shape.
- Classify the cause as ABI-required, regalloc policy, statepoint lowering, or
  legacy x86 mechanism.
- Prefer regalloc/calling-convention/root-policy fixes over local instruction
  forwarding or reload peepholes.
- For the dynamic loop-invariant GC slowdown, investigate why AMD64 LLVM
  register allocation and spill placement choose the inferior `%rax`/`%rcx`
  scalar loop-state schedule instead of native's `%rbx`/`%rdi` schedule. The fix
  direction should be to make AMD64 instruction selection/regalloc see the same
  safe choices that native uses for ordinary managed calls, not to add a
  benchmark-specific post-RA peephole.
- For this specific slowdown, prototype a principled X86/OxCaml policy that
  prevents harmful spill fusing for scalar reloads immediately after OxCaml
  statepoints/calls when the reload feeds loop-carried arithmetic. Start with a
  narrow target-side predicate in X86 spill folding, keyed by OxCaml
  calling-convention/statepoint context and hot-loop reload use, then compare
  against global `-disable-spill-fusing` to ensure the fix captures the win
  without disabling beneficial X86 folding elsewhere.
- Add a focused MIR test that compiles the reduced statepoint loop through
  regalloc and checks that the post-call scalar accumulator is explicitly
  reloaded into a register before arithmetic, while the GC root remains a
  statepoint stack root. This test should fail on the current default and pass
  with the targeted no-harmful-fusing fix.
- Benchmark after each prototype: at minimum rerun
  `loop_invariant_gc_across_call_dynamic_reps` with dynamic reps, plus the
  broader LLVM/native benchmark suite to catch cases where suppressing spill
  folding regresses ordinary x86 code.
- Preserve the safety rule that ordinary managed-call statepoints must expose
  GC roots in scannable locations. Any performance fix must distinguish
  non-GC loop state that can remain in registers from GC roots that need stack
  root semantics, and must respect real call clobbers rather than assuming
  arbitrary direct calls are leaf/noalloc.
- Keep code-review on every performance commit and require focused tests plus
  at least a representative benchmark rerun.

Exception-heavy slowdown update after the invoke-statepoint LSR fix
(`0f316ce2cf`):

- A fresh focused exception microprobe run with rebuilt local `llc`
  (`SAMPLES=5,WARMUPS=1`, `_install/bin/ocamlopt.opt`, project-style
  RS4GC wrapper) shows the original simple LSR case is mostly fixed:
  `raise_caught_cross_function` is now `1.0884x` LLVM/native and
  `raise_payload_caught_cross_function` is `0.9398x`.
- The remaining stable exception slowdowns are now
  `closure_call_many_handler_live_roots_raise` `1.1912x`,
  `boyer_like_failed_unify` `1.1314x`, and
  `catch_failure_then_unify` `1.1020x`; `nested_failed_unify` was near parity
  in this run (`1.0144x`) despite being slower in the earlier broad run.
- Native and LLVM both use the OCaml trap stack and both assume ordinary
  registers are destroyed when control reaches a handler. The remaining
  difference is state placement around protected calls, not a requirement for
  registers to stay live into handlers.
- `closure_call_many_handler_live_roots_raise` is the clean exnroot-pressure
  case: post-RS4GC IR has two exnroot allocas, no `gc.relocate`s, and the hot
  invoke lists the exnroot as `gc-live`. Final LLVM asm still spills scalar
  loop state through `%rbp` frame slots around the invoke; native homes similar
  values in its tighter trap/frame layout and reloads fewer values on the
  normal path.
- `catch_failure_then_unify`/`boyer_like_failed_unify` combine EH overhead with
  true root work: post-RS4GC IR has 8 allocas, 14 statepoints, 11 relocates,
  and three exnroot homes in `unify1`. Their hot loops have folded reloads at
  protected calls plus handler reloads from exnroot homes.
- The existing root mechanism is deliberate and should not be replaced by a
  second frontend-root style path. Exnroot homes are value homes that the GC can
  update in place across exceptional control flow; `OxCamlStatepointSpillRoots`
  and the verifier know about them.
- The experimental `-rs4gc-oxcaml-exn-ssa-roots` option had no effect on the
  representative AMD64 artifacts. Forcing `-rs4gc-oxcaml-exn-ssa-all` reduced
  `catch_failure_then_unify` root slots (`alloca` count 8 -> 2, exnroot refs
  51 -> 10), but grew the hot frame/reload shape and increased folded reloads
  (5 -> 7). This matches the RS4GC source comment warning that SSA-rooting
  handler-only values stretches live ranges and makes regalloc spill them
  again. Do not turn this on globally.
- A current no-spill-fusing ablation is mixed: it helps
  `raise_caught_cross_function` (`1.0884x -> 1.0356x`) and Boyer slightly
  (`1.1314x -> 1.1127x`), barely moves
  `closure_call_many_handler_live_roots_raise` (`1.1912x -> 1.1844x`), worsens
  `catch_failure_then_unify` (`1.1020x -> 1.1152x`), and badly worsens
  `nested_failed_unify` (`1.0144x -> 1.2023x`). Global spill-fusing suppression
  is therefore not the class-2 fix.
- MIR cuts after greedy, after `OxCamlStatepointSpillRoots`, after
  virtregrewrite, and after prolog/epilog are recorded in
  `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/exception/`,
  with the summary in
  `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/exception/class2-mir-findings.md`.
  In the closure live-root case, the hot statepoint lists only the exnroot
  value home; `OxCamlStatepointSpillRoots` rejects the landingpad exception
  temporary as not live across statepoints, and the remaining spill slots are
  ordinary scalar loop/domain state. In `catch_failure_then_unify`, the pass
  correctly appends one real spilled GC value (`%stack.4` in `run_4_9_code`) to
  the protected `unify1` statepoint while rejecting the exception temporary.
  This confirms the normal backend GC mechanism is active; the next target is
  native-like compact placement/reload of ordinary state around
  invoke-statepoints, not a second root mechanism.

Next class-2 plan:

- Keep `closure_call_many_handler_live_roots_raise` as the pure live-root
  testcase and `catch_failure_then_unify`/Boyer as the mixed exnroot+relocate
  testcase.
- Instrument MIR after greedy, `OxCamlStatepointSpillRoots`, and
  virtregrewrite for these cases to separate GC value homes from ordinary
  scalar spill slots. The likely target is statepoint/trap-adjacent placement
  of non-GC loop/list state, not the root-listing mechanism.
- Compare AMD64 against the arm64 LLVM output for the same source once an arm64
  artifact is available. Specifically check whether arm64 avoids the extra
  scalar homes around protected calls or simply schedules them better.
- Prototype only narrow changes: either improve X86 placement for scalar values
  live across invoke-statepoints, or reduce unnecessary exnroot materialization
  when the value already has a normal-path statepoint home. Any prototype must
  keep the current backend-recognized exnroot/value-home model and must be
  validated with self-stage plus exception/GC stress tests.

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
