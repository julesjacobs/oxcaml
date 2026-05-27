# Checklist: LLVM runtime-state complications

Unchecked boxes mean unresolved design/validation work. Each item is grounded in
the current compiler/runtime code so the next experiment can test the actual
constraint rather than an abstract concern.

## Design 1: SSA state, `x27`/`x28` allocatable

- [x] **Will LLVM actually coalesce `ds` and `alloc` into `x28` and `x27`?**

  The current IR already gives LLVM strong hints: `make_arg_types` prepends
  runtime args and `make_ret_type` returns runtime values in
  `backend/llvm/llvmize.ml`, and the AArch64 OxCaml calling convention assigns
  the first two integer args/results to `X28` and `X27` in
  `vendor/llvm-project/llvm/lib/Target/AArch64/AArch64CallingConvention.td`.

  The thing preventing this from working today is not the IR contract. It is
  `getStrictlyReservedRegs` in
  `vendor/llvm-project/llvm/lib/Target/AArch64/AArch64RegisterInfo.cpp`, which
  reserves `W27` and `W28` for OxCaml calling conventions. The experiment is
  direct: stop reserving them and inspect whether the register allocator keeps
  the runtime-state virtual registers there.

  Prototype result: removing the reservation and dropping `-ffixed-x27` and
  `-ffixed-x28` lets allocation-loop code use `x27`/`x28` directly again. The
  current validation is assembly inspection, not a full benchmark verdict yet.

- [ ] **Can unrelated values live in `x27`/`x28` when `ds`/`alloc` are not live?**

  If `x27`/`x28` become allocatable, LLVM can use them for ordinary values when
  the runtime-state values are dead or spilled. That is fine only if the runtime
  observes those registers at defined boundaries, not at arbitrary machine
  instructions.

  Runtime code strongly suggests boundary observation for many cases:
  `runtime/arm64.S` saves `ALLOC_PTR` to `Caml_state(young_ptr)` in
  `SAVE_ALL_REGS`, `caml_call_gc`, and C-call stubs, then reloads it after the
  runtime transition. But this must be audited for every path that can inspect
  `ALLOC_PTR` or `DOMAIN_STATE_PTR`.

- [x] **Existing physical `x28`/`x27` reads in LLVM lowering become suspect.**

  `backend/llvm/llvmize.ml` has explicit physical reads:

  - `read_domainstate_pointer_register` reads `x28`;
  - `read_allocation_pointer_register` reads `x27`;
  - `prepare_call_args ~use_physical_runtime_regs` can read physical `x28/x27`;
  - the AArch64 exception path around `wrap_try` reads physical `x28/x27` and
    stores them back into the logical runtime-state allocas;
  - trap-handler entry restores logical `allocation_ptr` from physical `x27`.

  Under Design 1, those physical reads are valid only immediately after a
  boundary has forced the SSA values into the physical registers. Otherwise they
  must be rewritten to use SSA values or guarded by an explicit sync.

  Prototype result: `read_domainstate_pointer_register` was removed, the
  `caml_send*` physical-register shortcut was disabled, and AArch64 trap-handler
  entry no longer refreshes logical `allocation_ptr` by rereading physical
  `x27`. Exception entries are handled by a single boundary snapshot instead.

- [ ] **Fast allocation updates SSA `alloc`, not necessarily physical `x27`.**

  `heap_alloc` in `backend/llvm/llvmize.ml` loads `allocation_ptr`, subtracts
  the allocation size, and stores the result back to `allocation_ptr`. It does
  not directly write physical `x27`.

  Under Design 1, this is intended: if register allocation keeps `alloc` in
  `x27`, the machine code becomes the native-like physical update. If it does
  not, then physical `x27` may be stale until the next boundary. Therefore every
  path that reads physical `x27` after a fast allocation must either prove that
  `alloc` is currently allocated to `x27` or perform a sync.

- [ ] **Allocation and poll slow paths must force the current SSA `alloc` into the runtime ABI.**

  `heap_alloc` and `poll` both branch to `call_gc_for_basic_safepoint`, which
  calls `caml_call_gc` using `Oxcaml_alloc`. `call_simple` prepares runtime
  args through `prepare_call_args`, so the current logical `allocation_ptr`
  should become the second calling-convention argument, i.e. `x27`.

  This looks aligned with Design 1. The validation point is generated assembly:
  before `_caml_call_gc`, the value that represents current SSA `alloc` must be
  in `x27`, and after return the new `alloc` must be recovered through the call
  result path.

- [ ] **Stack checks have two separate mechanisms, and one currently reads raw `x28`.**

  CFG stack checks in `backend/llvm/llvmize.ml` call
  `caml_llvm_call_realloc_stack` through `Oxcaml_alloc`, so they fit the
  ordinary calling-convention sync model.

  Prologue stack checks are different. `AArch64FrameLowering.cpp` emits raw
  inline asm for `emitOxCamlStackCheck`; that asm loads `[x28, #40]` and calls
  `_caml_call_realloc_stack`. Because `x28` is currently reserved, this raw use
  has been safe enough. If `x28` becomes allocatable, this inline asm must be
  given explicit operands/clobbers or rewritten as real machine instructions so
  LLVM knows it uses `x28` as domain state.

- [x] **Exception paths need a real state-transfer story.**

  Raising through `raise_` calls runtime raise helpers via `call_simple`.
  Separately, AArch64 trap handling in `llvmize.ml` uses `x26` as the trap
  pointer and reads physical `x27` in trap-handler entry to refresh
  `allocation_ptr`.

  The hard case for Design 1 is: callee allocates using SSA `alloc`, then raises
  before a normal return. The caller cannot get updated `alloc` from the normal
  returned runtime-state tuple. The callee's raise path must force current
  `alloc` into `x27`, or the exceptional edge must carry the updated value by
  some other mechanism.

  Prototype result: AArch64 exception entries snapshot `x0/x28/x27/x26` into a
  stack slot at the exception-entry boundary and reload handler state from that
  slot. The inline asm must clobber `~{lr}` rather than `~{x30}` so handler
  values are not kept in the link register across the hidden exception edge.
  The `make_opcodes` reproducer and focused exception tests pass with this
  shape.

- [ ] **Statepoints and stackmaps may define runtime-observation points.**

  `gc_attr`, `call_operand_bundles`, `gc-live` bundles, and
  `RewriteStatepointsForGC` make OxCaml calls into LLVM GC statepoints.
  AArch64 lowers `TargetOpcode::STATEPOINT` in `AArch64AsmPrinter.cpp` and
  records stackmaps.

  If stackmap/frame-table consumers expect `x27` or `x28` to hold current
  runtime state at the statepoint PC, Design 1 must sync there. If stackmaps
  only need roots/relocations and the call ABI handles runtime state, then
  Design 1 can keep `alloc` as SSA until the actual statepoint call boundary.

- [ ] **C-call transitions rely on physical runtime registers and `Caml_state` fields.**

  `extcall` in `backend/llvm/llvmize.ml` lowers allocating C calls through
  `caml_c_call`, stack-arg calls through `caml_c_call_stack_args`, and some
  noalloc calls through wrappers/specialized lowering.

  In `runtime/arm64.S`, `caml_c_call` stores `ALLOC_PTR` to
  `Caml_state(young_ptr)` and `TRAP_PTR` to `Caml_state(exn_handler)`, calls C,
  then reloads `ALLOC_PTR` and `TRAP_PTR`. So for C/runtime boundaries, the
  current SSA `alloc` must be in physical `x27` before the stub runs. The
  calling convention should provide that if the call is modeled correctly.

- [ ] **`x27` and `Caml_state->young_ptr` are related but not identical at all times.**

  The native runtime stores `ALLOC_PTR` into `Caml_state(young_ptr)` on runtime
  transitions, for example in `runtime/arm64.S` around `SAVE_ALL_REGS` and
  `caml_c_call`.

  Design 1 should not assume updating SSA `alloc` or physical `x27`
  immediately updates `Caml_state->young_ptr`. That store happens at specific
  runtime boundaries. Any helper that reads `Caml_state->young_ptr` rather than
  physical `x27` must be classified carefully.

- [ ] **Signals, preemption, memprof, and callbacks run through poll/allocation/C boundaries.**

  `runtime/signals.c` documents pending asynchronous actions. Some delayed
  actions run OCaml code and may raise exceptions: signal handlers, finalisers,
  memprof callbacks, and forced systhread yield. Those are triggered by
  `young_limit` forcing a failed allocation/poll or by blocking-section
  transitions.

  This supports Design 1 only if all such entry points are treated as runtime
  boundaries where `x27`/`x28` and `Caml_state` are made consistent before
  runtime code can run callbacks or raise.

  The OS-level signal handler can be entered at an arbitrary instruction, but
  it does not run the OCaml signal handler. `handle_signal` only preserves
  `errno` and calls `caml_record_signal`; `caml_record_signal` atomically marks
  the signal pending and calls `caml_interrupt_all_signal_safe`. That function
  walks the fixed `all_domains` array and stores `UINTNAT_MAX` through each
  domain's `interrupt_word`, which is the domain state's `young_limit`. The
  comments explicitly avoid relying on `Caml_state` being available in the
  interrupted thread.

  The actual OCaml handler runs later in `caml_process_pending_signals_exn`,
  called from `caml_do_pending_actions_flags_exn` or the native
  `caml_garbage_collection` poll path. On arm64, generated code reaches that
  path through `caml_call_gc`, whose `SAVE_ALL_REGS` first stores physical
  `ALLOC_PTR` (`x27`) into `Caml_state(young_ptr)` and `TRAP_PTR` into
  `Caml_state(exn_handler)`, using `DOMAIN_STATE_PTR` (`x28`) as the base for
  those stores. Therefore the signal path does not require
  arbitrary-instruction validity of `x27`/`x28`; it requires that every poll or
  allocation slow-path call has synchronized the current logical domain-state
  and allocation-pointer values into `x28`/`x27` before entering
  `caml_call_gc`.

- [ ] **Blocking sections are runtime boundaries, not ordinary noalloc calls.**

  `runtime/signals.c` implements `caml_enter_blocking_section` and
  `caml_leave_blocking_section`. Entering checks pending actions and can raise
  async exceptions before releasing the domain lock; leaving reacquires the
  domain lock and may set action pending.

  Any LLVM lowering that calls into blocking-section machinery must be treated
  like a full runtime boundary. It is not safe to rely on arbitrary current
  allocation of `ds`/`alloc` virtual registers.

- [ ] **Call-preserved masks must still match the OxCaml GC story.**

  `CSR_AArch64_OxCaml_WithoutFP` currently saves only `LR`; normal OxCaml calls
  do not preserve general callee-save registers. `OxCaml_C_Call` subtracts
  `X27` and `X28` from the AAPCS preserved set because those are runtime
  registers, not ordinary C callee-saves.

  Design 1 should not accidentally turn `x27/x28` into ordinary preserved
  registers. They are argument/result locations for runtime state. The call
  preserved masks must continue to describe what ordinary values can live across
  a call.

- [ ] **Darwin/platform restrictions around `x28` must be checked.**

  `AArch64RegisterInfo.cpp` already reserves `W28` for Windows Arm64EC because
  asynchronous signals clobber it there. This branch is currently targeting
  Darwin/macOS arm64 in practice, but any broader support needs a platform
  check before making `x28` allocatable.

- [ ] **The existing post-RA cleanup is not a substitute for a real design.**

  `removeOxCamlRuntimeRegRoundTrips` in `AArch64ExpandPseudoInsts.cpp` removes
  a local `runtime -> temp -> same runtime` copy pair when nothing intervenes.
  It stops at calls, terminators, unmodeled side effects, or register uses.

  This can clean incidental copies, but it cannot fix the core issue when the
  ABI and reserved-register model force real virtual values to live somewhere
  other than `x27/x28`.

## Design 2: reserved registers plus explicit target operations

- [ ] **Inline asm is not a viable production representation.**

  Current code already uses inline asm to read `x26/x27/x28`, and prologue stack
  checks are emitted as a raw side-effecting inline asm string in
  `AArch64FrameLowering.cpp`.

  This works while the registers are globally reserved, but it hides exact
  register uses/defs from scheduling and register allocation. A production
  Design 2 needs target intrinsics or MachineInstr pseudos with explicit
  operands for `x27`/`x28`, not opaque asm strings.

- [ ] **Fast allocation must become a target-visible update of `x27`.**

  Today `heap_alloc` updates the logical `allocation_ptr` alloca/SSA value.
  Design 2 would instead need a target-visible operation like:

  ```text
  x27 := x27 - allocation_size
  compare x27 with [x28 + young_limit]
  ```

  If this is lowered too late, LLVM loses optimization opportunities. If it is
  lowered too early as opaque asm, LLVM loses scheduling and dataflow knowledge.

- [ ] **Domain-state loads need a first-class `x28` base.**

  `load_domainstate_addr` in `llvmize.ml` currently loads SSA `ds` and computes
  field addresses from that value. Native AArch64 helpers use `x28` directly;
  `backend/arm64/dsl_helpers.ml` defines the domain-state base as register 28.

  Design 2 should represent domain-state loads as target-aware operations using
  fixed `x28`, or at least as ordinary loads whose base is a well-modeled
  physical-register value. Raw inline asm should not be the main abstraction.

- [ ] **Statepoint integration must know about physical runtime state.**

  If `x27/x28` are always physical runtime state, GC statepoint lowering and
  stackmap emission must not treat them as ordinary clobbered registers or
  hidden values. `RewriteStatepointsForGC`, `StatepointOpers`, and AArch64
  `LowerSTATEPOINT` are the places to audit.

  This is probably manageable, but it is not free. The contract must say where
  the GC finds runtime state and whether roots can remain in registers across
  the statepoint.

- [ ] **Explicit operations need precise scheduling barriers.**

  Design 2 operations must not move across boundaries where the runtime observes
  state, but they also should not block unrelated scheduling. That means each
  pseudo needs accurate memory effects and register operands.

  For example, "write current alloc to `x27`" should define `x27` but should not
  claim `~{memory}` unless it stores to memory. "call runtime" should be the
  barrier, not every runtime-state move.

- [ ] **Reserved `x27/x28` reduce register-allocation freedom.**

  This is the main performance downside of Design 2. It matches native's fixed
  runtime-register model, but LLVM cannot use those registers for ordinary
  values even in code regions where runtime state is not immediately observed.

  The benchmark question is whether this loss is smaller than the current
  runtime-state round-trip cost. Numerical loops, allocation loops, and compiler
  source-file benchmarks should all be checked.

## Shared complications

- [ ] **Tail calls and self-tail calls need separate treatment.**

  `call` in `llvmize.ml` uses `musttail` for tail calls and returns the call
  result directly. Tail calls are sensitive to exact function signatures and
  calling conventions. Any change to runtime-state arguments/results must check
  tail-call verification and generated assembly separately.

- [ ] **Domain-state-passed arguments/results depend on `ds`.**

  `reg_list_for_call` filters out locations in `Domainstate`; `alloca_regs`
  maps `Stack (Domainstate idx)` registers to addresses computed from `ds`.

  Design 1 keeps `ds` as an SSA value, so this remains natural. Design 2 needs
  a clear way to materialize domain-state slot addresses from fixed `x28`
  without hiding the dependency from LLVM.

- [ ] **Mixed native/LLVM calls must agree on the boundary ABI.**

  All exported function definitions are currently emitted with `~cc:Oxcaml`,
  runtime args from `make_arg_types`, and runtime results from `make_ret_type`.

  Any internal change must preserve the external ABI expected by native code,
  runtime assembly, closure entry points, and C stubs. Design 1 preserves this
  most directly because it keeps the current function signatures and calling
  conventions.

- [ ] **Debug/unwind/frame metadata is coupled to stack checks and statepoints.**

  `fun_attrs` adds OxCaml stack-check attributes and `Noinline` for safepoint
  reasons. AArch64 frame lowering has OxCaml-specific frame and stack-check
  paths. Statepoints feed stackmaps.

  Any design that changes where runtime state lives must be validated not just
  by executing code, but also by checking generated frametables/stackmaps for
  allocation, poll, stack growth, exception, and C-call cases.

- [ ] **The first experiment must distinguish "bad design" from "missing audit."**

  If Design 1 fails immediately, the cause matters. A failure from raw
  `read_allocation_pointer_register` after a fast allocation means the physical
  read needs rewriting or a sync point. A failure from the runtime observing
  `x27` asynchronously would be a more fundamental strike against Design 1.

  Therefore failures should be reduced and classified before abandoning the
  design.
