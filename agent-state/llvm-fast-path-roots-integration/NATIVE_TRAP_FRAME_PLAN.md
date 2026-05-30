# Native-shaped trap frame plan

## Goal

Make AArch64 LLVM backend `try`/trap lowering use the native-like trap model:

- `Pushtrap` links a small trap frame into the OCaml trap chain.
- `Poptrap` unlinks that trap frame cheaply on normal exit.
- `Raise_notrace` restores `sp` from the trap pointer, pops the trap frame, and
  branches to the recovery target.
- Handler entry has the runtime-entered recovery ABI, not ordinary protected
  path live values.
- Stack slots and GC root locations remain correct while a trap frame is active.

The important goal is the machine-code semantics. The LLVM representation does
not need to mirror native compiler internals exactly, but it must make the
control-flow, ABI, and stack-pointer facts explicit enough for optimization,
register allocation, frame-index elimination, stackmaps, verification, and
tests.

## Why this is needed

Native AArch64 uses a two-word trap frame on the OCaml stack:

```asm
adr x16, handler
stp x26, x16, [sp, #-16]!
mov x26, sp
```

Normal exit:

```asm
ldr x26, [sp], #16
```

Dynamic raise:

```asm
mov sp, x26
ldp x26, x16, [sp], #16
br x16
```

Current LLVM AArch64 uses an alloca-backed trap block plus recovery stack repair.
That is correct enough for the current model, but it makes hot `try` paths and
some expected-control-flow exception paths slower than native.

Manual assembly experiments show that editing only the recovery prefix or label
materialization is not enough. Replacing the full function body with native-like
trap-frame code removes most of the remaining exception-specific slowdown. That
points at the representation, not just one bad instruction sequence.

## LLVM features we can reuse

LLVM already has useful pieces:

- Frame indexes keep stack slots abstract until prologue/epilogue insertion.
- Prologue/epilogue insertion already has a notion of temporary SP adjustment
  for call-frame setup and destroy pseudos.
- AArch64 frame-index lowering already centralizes the decision of whether a
  stack slot is addressed from SP, FP, or BP.
- Stackmaps/statepoints record final root locations after machine lowering.
- The existing OxCaml GC printer consumes stackmap locations to build OCaml
  frame tables.
- The current runtime-entered MachineBasicBlock bit is the right kind of
  machine-level concept for recovery entries.

These pieces should be reused where their semantics match.

## LLVM features that do not directly solve this

Call-frame setup/destroy pseudos are not the right direct carrier for OCaml trap
frames. They model short-lived outgoing call argument space, and LLVM's verifier
expects setup/destroy pairing and consistent stack state around those call
sequences.

AArch64 currently assumes the generic `SPAdj` passed into normal frame-index
elimination is zero. The concept exists in LLVM, but AArch64 is not generally
prepared to lower arbitrary stack slots under nonzero `SPAdj`.

Forcing a frame pointer or base pointer is also not the right default answer.
OxCaml AArch64 intentionally avoids forcing a platform frame pointer when
possible, because the OCaml stack walker expects return-address information from
the current stack pointer. The usual AArch64 base pointer is X19, and OxCaml
calls do not preserve X19.

LLVM exception handling is useful as a reference for representing non-local
control flow, but it does not model the OCaml runtime trap chain or the recovery
ABI directly. We should not try to pretend OCaml trap recovery is ordinary LLVM
EH if that hides the actual machine semantics.

## Proposed design

Use target-owned AArch64/OxCaml trap pseudos and a target-owned active-trap
stack model.

### Trap pseudos

Introduce machine pseudos for the native-shaped operations:

- `OXCAML_PUSH_TRAP recovery-target`
- `OXCAML_POP_TRAP`
- `OXCAML_RAISE_NOTRACE recovery-target`

The push pseudo owns the publication of the recovery target and the update of
`x26`. The pop pseudo owns the normal unlink. The raise pseudo owns the dynamic
runtime transfer through `x26`.

The final expansion should be equivalent to the native sequences above, unless a
specific target constraint requires a different but equivalent sequence.

The pseudos must describe their machine effects precisely:

- `OXCAML_PUSH_TRAP` defines `sp` and `x26`, stores the previous trap pointer and
  recovery target at `[sp, #-16]!`, materializes the recovery target, and is
  marked as storing and having side effects.
- `OXCAML_POP_TRAP` defines `sp` and `x26`, loads the previous trap pointer, and
  is marked as loading and having side effects.
- `OXCAML_RAISE_NOTRACE` defines `sp` and `x26`, uses the exception value in
  `x0`, loads the previous trap pointer and recovery target from the active trap
  frame, and is a terminator/barrier with an explicit Machine CFG successor to
  the active recovery block.

The current alloca-backed trap publication shape is not enough. This plan should
replace that representation on AArch64, not merely evolve the existing publish
operation while leaving the trap block off the real stack.

Preliminary experiment:

- A direct same-function `raise_notrace` with a published recovery block but no
  explicit control-flow edge lets LLVM delete the recovery block and fold the
  `blockaddress` to a dummy constant. Therefore direct `Raise_notrace` inside an
  active trap must be represented with an explicit successor before ordinary
  optimization and machine liveness.

### Recovery blocks

Recovery blocks should be marked `runtime-entered`.

The recovery ABI is:

- `x0`: exception bucket/value expected by the handler path
- `x26`: previous trap pointer after the trap frame has been popped
- `x27`: allocation pointer
- `x28`: domain state pointer

A recovery block must not use ordinary protected-path SSA values. It may use
only the recovery ABI values, constants, and values explicitly reloaded or
recomputed after entry.

The verifier or a target validation pass should reject malformed recovery
shapes:

- extra allocatable physical live-ins;
- PHIs in runtime-entered recovery blocks;
- recovery ABI snapshot not first when snapshotting is required;
- runtime-entered recovery blocks without a matching trap-region owner;
- protected-path values used directly from recovery code.

### Active trap stack

Compute the active trap stack for each machine instruction after trap pseudos
exist in the Machine CFG and before frame-index elimination.

Track the active trap stack, not only its integer depth. The byte adjustment is
derived from the stack length, but correctness at joins depends on the actual
trap identities.

The stack changes by:

- pushing the trap identity after `OXCAML_PUSH_TRAP`;
- popping and checking the matching trap identity after `OXCAML_POP_TRAP`;
- recovery entry starts after the runtime raise sequence has popped the trap
  frame, so the recovered handler path has one fewer active trap than the
  protected path that raised.

The analysis must reject CFG joins reached with incompatible active trap stacks.
Equal depth is not sufficient: two paths at depth one but protected by different
trap frames cannot be merged as if they had equivalent stack state.

### Stack-slot addressing

When a stack slot is lowered relative to SP, include the active trap bytes:

```text
effective offset = normal SP-relative offset + active_trap_depth * 16
```

Example:

```asm
ldr x0, [sp, #40]
```

inside one active trap becomes:

```asm
ldr x0, [sp, #56]
```

FP-relative and BP-relative references should not receive this adjustment,
because the trap frame changes SP, not FP/BP. The implementation must be careful
around cases where AArch64 chooses FP/BP for a particular frame index.

LLVM already has the right phase boundary for this. Frame indexes remain
abstract until `PrologEpilogInserter` asks the target to eliminate them. AArch64
then decides whether a given frame index is SP-, FP-, or BP-relative.

The preferred implementation is:

1. compute active trap stacks in
   `AArch64FrameLowering::processFunctionBeforeFrameIndicesReplaced`, while trap
   pseudos are still present in the Machine CFG and immediately before PEI
   replaces frame indexes;
2. store the per-MBB entry trap stack and the per-instruction derived active
   trap byte count in AArch64 function info keyed by `MachineInstr *`;
3. during AArch64 frame-index elimination, after the base register has been
   chosen, add active trap bytes only when the chosen base is SP.

Current prototype status:

- `OXCAML_PUSH_TRAP`, `OXCAML_POP_TRAP`, and `OXCAML_RAISE_NOTRACE_EDGE` exist as
  AArch64 pseudos.
- Push/pop lower to the native-shaped AArch64 sequences.
- PEI computes a per-instruction active trap byte count from the machine CFG and
  target recovery operands.
- SP-based frame-index, stackmap, patchpoint, and statepoint locations are
  adjusted by the active trap byte count after base-register selection.
- Joins with incompatible active trap stacks are rejected.

Remaining design work:

- Replace the current alloca-backed OxCaml trap lowering with these pseudos.
- Decide the exact IR-level construct used to carry the push recovery edge
  before machine lowering.
- Add full GC table tests that inspect emitted stackmap/frame-table locations,
  not only MIR after PEI.
- Add CFI/unwind handling for trap push/pop, or deliberately gate the affected
  unwind mode while that is not implemented.

Do not directly reuse LLVM call-frame setup/destroy pseudos for trap frames.
Those pseudos model short-lived outgoing call argument space, and LLVM verifier
rules expect call-frame setup/destroy pairing. OCaml trap frames are long-lived
dynamic handler registrations and can span arbitrary code.

AArch64 currently assumes the generic `SPAdj` passed into normal frame-index
elimination is zero. Reusing the idea is fine; depending on that existing
`SPAdj` path as-is is not.

Do not compute active trap stacks in an earlier pre-regalloc or post-regalloc
pass and then expect instruction identities to remain stable. The PEI hook above
is the intended ownership boundary: the CFG and final machine instructions are
late enough to be meaningful, and frame-index operands still exist.

Outgoing call-stack areas need special handling. AArch64 normally reserves the
maximum outgoing call frame in the function's static frame when there are no
variable-sized stack objects. In that mode, a call with one stack argument stores
the outgoing argument at `$sp, #0` immediately before the call. If a native trap
frame is active, `$sp, #0` is the trap frame, so that store would collide with
the trap frame.

The preferred first implementation is to make native-trap OxCaml functions avoid
reserved call frames when they contain calls with outgoing stack arguments. Then
PEI lowers the call sequence as a dynamic call-frame adjustment around the call,
placing outgoing stack arguments below the current SP and therefore below any
active trap frames. This is a better first implementation than trying to patch
raw stack-argument stores after the fact.

This policy should be implemented with a target-owned predicate, for example
making `AArch64FrameLowering::hasReservedCallFrame` return false for OxCaml
functions that contain native trap pseudos and have a nonzero max call frame
size. The exact predicate can be tightened after source lowering, but the
invariant is fixed: an outgoing stack-argument store must not address the active
trap frame.

Preliminary experiment:

- Existing OxCaml AArch64 calls with stack arguments use a reserved call frame
  and store the stack argument at `$sp, #0`.
- Adding a dynamic alloca makes AArch64 avoid reserved call frames; PEI then
  emits `sub sp, sp, #16`, stores the stack argument at `$sp, #0`, calls, and
  restores `sp`. That shape composes with active native trap frames.

### GC stack scan tables

GC root locations must describe locations relative to the actual machine SP at
the safepoint.

Therefore statepoint/stackmap stack locations inside active trap regions need
the same active-trap-byte adjustment as ordinary SP-relative stack-slot
accesses. Trap frame words themselves are not roots.

There are two distinct GC-table quantities to keep correct:

- root offsets: runtime scans stack roots as `sp + live_offset`;
- frame size: runtime advances to the caller with `sp += frame_size`.

Native AArch64 handles both by updating the emitter stack offset at `Pushtrap`
and `Poptrap`. The LLVM backend must produce equivalent frame descriptors when a
statepoint occurs inside active traps.

For root offsets, the preferred implementation is to make statepoint
frame-index lowering use the same active-trap-byte adjustment as ordinary
SP-relative frame indexes. `PrologEpilogInserter` has a special path for
STATEPOINT frame-index operands; add the smallest hook needed so AArch64/OxCaml
can add active trap bytes there. A more local fallback is a target pass after
PEI that rewrites SP-relative statepoint stackmap operands, but that is more
fragile because it must parse the finalized statepoint operand format.

The hook must run after the statepoint stack location base register is known.
Statepoint `TargetFrameIndex` operands are lowered through stackmap/patchpoint
machinery, not only through ordinary load/store frame-index elimination. The
implementation must handle direct and indirect stackmap memory references and
must test SP-based, FP-based, and BP-based root locations separately. Only
SP-based root locations receive the active-trap-byte adjustment.

Preliminary experiment:

- A normal AArch64 statepoint stack root is lowered by PEI's special STATEPOINT
  frame-index path to an SP-based stackmap memory location.
- With a dynamic alloca, the same kind of statepoint root lowers to an FP-based
  stackmap memory location.

This confirms that the adjustment must be applied after the final base register
is known. A blanket statepoint-offset adjustment would be wrong.

For frame size, keep using the existing OxCaml statepoint metadata path if it can
carry the needed callsite quantities, but change the AArch64
native-shaped-trap interpretation. Once trap frames are real stack frames instead
of alloca-backed trap blocks, the active native trap bytes and real `Stackoffset`
bytes should be added to the AArch64 frame size for that callsite. The current
LLVM-side subtraction for static alloca trap blocks is an artifact of the old
representation and should be removed.

After switching to native-shaped traps, the statepoint metadata must distinguish
two callsite quantities:

- active native trap bytes;
- real `Stackoffset` bytes.

The GC frame-size contribution for a callsite is their sum, but they should not
be collapsed too early. Keeping active trap bytes separate lets the GC printer
reject CSR root maps only for statepoints that are actually inside active traps;
a nonzero `Stackoffset` alone does not mean a trap is active.

Neither quantity may include:

- static LLVM frame bytes already represented by `MachineFrameInfo`;
- old alloca trap-block bytes;
- old recovery metadata bytes.

On AArch64, `Proc.trap_size_in_bytes` should return 16 for LLVM native-shaped
traps. Remove the current LLVM-specific 32-byte trap-size model and remove the
current LLVM-side subtraction of `32 * active_trap_depth` from statepoint stack
offsets.

Root offsets and frame size are fixed in different places:

- root offsets are fixed during STATEPOINT frame-index lowering by adding active
  trap bytes to SP-relative statepoint locations;
- frame size is fixed in the OxCaml GC printer by adding active native trap bytes
  and real `Stackoffset` bytes to the AArch64 frame size;
- the GC printer must not adjust root offsets a second time.

The invariant for a statepoint inside one active native-shaped trap is:

```text
frame size += 16
SP-relative root offsets += 16
```

Nested traps add another 16 bytes each.

CSR root maps are function-level in LLVM stackmap metadata, not callsite-specific.
The common OxCaml calling conventions preserve only LR or LR/FP, so the map is
usually empty. Active trap state is callsite-specific, so function-level CSR root
offsets are not sufficient inside active traps.

Initial implementation policy: reject CSR root maps in active traps. If any
statepoint occurs with active trap bytes greater than zero and the function has a
non-empty CSR root map, fail loudly. Add a negative LLVM test. Supporting this
later requires callsite-specific CSR-root handling.

### Calls inside active trap regions

Calls inside a trap-protected region are the hard correctness case.

If a call may raise and dynamically enter the active recovery block, the Machine
CFG must expose that recovery successor where register allocation and liveness
need it. A single edge from trap publication to recovery is not enough.

Primary policy: keep using IR `invoke` for every operation that can dynamically
enter the active handler. This gives LLVM IR optimizers and machine codegen the
control-flow edge before liveness, PHI elimination, live intervals, and register
allocation.

Operations that can transfer to the active handler include:

- ordinary OxCaml calls;
- external calls that may raise;
- allocation slow paths and GC calls;
- polls;
- stack checks/realloc-stack calls;
- regular raise/reraise helpers;
- any other runtime helper that can raise or enter the trap chain.

Direct `Raise_notrace` should not become a plain call. It should be represented
as a target terminator or call-like pseudo with an explicit recovery successor,
then expanded to the native dynamic branch sequence.

A call followed by `unreachable`, or an intrinsic with no Machine CFG successor,
is not an acceptable representation for direct `Raise_notrace` inside an active
trap. The IR/MIR lowering must carry the known active recovery block to the
target pseudo, or use an IR construct that reliably lowers to the same Machine
CFG edge before liveness and register allocation.

Tail calls inside active traps need an explicit policy. Either lower them as
non-tail invokes while a trap is active, or prove and test that the tail call
cannot dynamically return through the current handler. Do not silently emit a
plain tail call with no recovery edge inside an active trap.

A late machine pass that adds hidden recovery successors is only acceptable for
operations introduced after IR lowering. Such a pass must use explicit target
metadata to know the current active recovery target.

### CFI and unwinding

Trap push/pop expansion changes SP in the middle of the function. Native AArch64
emits corresponding CFA adjustments. LLVM native-shaped trap pseudos should do
the same unless a platform-specific object/unwind format rejects it.

Initial implementation policy:

- expanding `OXCAML_PUSH_TRAP` emits CFI equivalent to CFA offset `+16`;
- expanding `OXCAML_POP_TRAP` emits CFI equivalent to CFA offset `-16`;
- focused MIR/assembly tests check both the machine sequence and emitted CFI.

This is not only a local instruction-printing problem. CFI is emitted as a
linear stream, while trap recovery is non-local control flow. The backend must
track the expected CFA/trap depth at MachineBasicBlock entry and ensure that a
block label is emitted under the correct CFA state. If block layout would place a
recovery block after code with a different CFI state, the backend must emit the
needed restore/adjust directives at the block boundary or use another tested
layout/CFI strategy.

LLVM has a generic `CFIInstrInserter` pass that computes incoming/outgoing CFA
state for each MachineBasicBlock and inserts block-entry CFI when layout order
does not match CFG state. AArch64 does not currently run that pass. LLVM also
has `CFIFixup`, but that pass is narrower: it is designed around prologue and
epilogue frame-state transitions and explicitly does not handle arbitrary SP
adjustments split across basic blocks.

Therefore the native-trap implementation must either enable/adapt
`CFIInstrInserter` for AArch64/OxCaml trap CFI, add an equivalent AArch64/OxCaml
block-entry CFI repair pass, or explicitly gate unsupported unwind/debug modes
until this is implemented. Do not assume current AArch64 CFI fixup is sufficient
for trap push/pop CFI.

If a target platform cannot support this mid-function CFI shape, gate that
behavior explicitly and document the platform-specific fallback. Do not leave
debugging/unwinding behavior implicit.

## Implementation stages

### Stage 1: MIR prototype

Prototype the machine-level model in MIR before changing OxCaml lowering.

Current prototype status:

- A MIR-only `OXCAML_RAISE_NOTRACE_EDGE` pseudo proves that a direct dynamic
  raise can carry an explicit recovery Machine CFG successor while still
  lowering to the native raise sequence.
- A `hasReservedCallFrame` prototype proves that OxCaml functions containing a
  native-trap pseudo can opt out of reserved outgoing call frames, causing
  stack-argument calls to use dynamic call-frame adjustment instead of storing
  into `$sp, #0`.

Tests to build:

- one active trap, one spill/reload inside the active region;
- nested active traps;
- branch join with equal trap stack;
- branch join with incompatible trap stack, expected rejection;
- call inside active trap with recovery successor;
- direct `OXCAML_RAISE_NOTRACE` with an explicit recovery successor;
- statepoint inside active trap with a stack root addressed from SP;
- statepoint inside active trap with a stack root addressed from FP or BP;
- C or runtime call with outgoing stack arguments inside an active trap;
- native-trap function with outgoing stack arguments, expected to avoid reserved
  call frames or otherwise prove that stack-argument stores cannot collide with
  active trap frames;
- recovery block layout reached under a nontrivial CFI state;
- recovery block with only x0/x26/x27/x28 live-ins;
- negative recovery block with an extra live-in;
- negative recovery block using protected-path value directly.

Expected result:

- frame-index offsets change by `active_trap_depth * 16`;
- stackmap root offsets and frame sizes change consistently;
- verifier catches malformed runtime-entered blocks;
- CSR root maps in active traps are rejected until callsite-specific CSR support
  exists;
- push/pop trap CFI is emitted or explicitly gated with a documented reason;
- recovery block labels have the correct CFI state even under nontrivial block
  layout;
- stack-argument calls inside native-trap functions do not write into the active
  trap frame;
- no unnecessary frame pointer or base pointer is introduced for ordinary cases.

### Stage 2: LLVM AArch64 implementation

Add the target pseudos and active-trap-stack analysis.

Teach AArch64 frame-index elimination, or a tightly adjacent lowering step, to
apply active trap bytes to SP-relative references.

Teach statepoint/stackmap lowering to record root locations with the same
effective SP-relative offsets.

Use `processFunctionBeforeFrameIndicesReplaced` to compute the active-trap-stack
table immediately before PEI frame-index replacement. Store per-MBB entry trap
stacks and per-instruction active trap byte counts. Add a small PEI/target hook
for STATEPOINT frame-index lowering if needed so statepoint operands consume the
same active trap byte count as ordinary frame indexes after their base register
is known.

Keep active trap bytes separate from real `Stackoffset` bytes in statepoint
metadata or equivalent callsite metadata consumed by the GC printer.

Reject CSR root maps in active traps.

Add the machine validation for runtime-entered recovery blocks.

Reuse the existing MachineVerifier runtime-entered checks where they already
match the needed rule. It already rejects PHIs in runtime-entered blocks,
invalid runtime-entered live-ins, and missing required ABI live-ins. Add only the
missing trap-specific checks, such as matching trap-region ownership and
protected-path virtual registers reaching recovery code.

Teach AArch64 call-frame lowering how native trap frames interact with outgoing
stack arguments. The preferred first implementation is to disable reserved call
frames for OxCaml functions that contain native trap pseudos and need outgoing
stack argument space, so `ADJCALLSTACKDOWN`/`ADJCALLSTACKUP` lower to real SP
adjustments around those calls.

Add or enable the CFI block-entry repair mechanism required by native trap CFI.
The likely choices are adapting `CFIInstrInserter` for AArch64/OxCaml or adding
an equivalent target-owned pass. If this is not implemented initially, gate the
affected unwind/debug mode explicitly.

Keep the implementation local to AArch64/OxCaml unless a generic LLVM hook is
clearly the better abstraction.

### Stage 3: OxCaml LLVM lowering

Replace alloca-backed AArch64 trap blocks with the native-shaped trap-region
representation.

Remove recovery stack repair that only exists because the current trap block is
not the real stack top.

Change AArch64 LLVM trap-size accounting from the old 32-byte alloca-backed
trap-block model to the native 16-byte trap-frame model. Remove the old
statepoint stack-offset subtraction for alloca trap blocks.

Lower real `try`/`raise` constructs to the new representation, including:

- non-raising hot path;
- dynamic `Raise_notrace`;
- local handler recovery;
- nested handlers;
- calls inside active handlers;
- reraise/regular raise paths that use runtime helpers.

Update `Proc.trap_size_in_bytes` handling for the LLVM AArch64 backend once the
native-shaped frame is used.

### Stage 4: Code-shape tests

Add focused tests that check:

- pushtrap emits native-shaped trap publication;
- poptrap emits native-shaped unlink;
- raise-notrace emits the direct dynamic branch;
- no `_wrap_try` helper call remains on the hot path;
- no recovery SP repair prefix remains for native-shaped recovery;
- stack slot offsets inside active traps include the trap-frame adjustment;
- stackmap root locations inside active traps are correct for SP roots and are
  not over-adjusted for FP/BP roots;
- frame descriptor sizes inside active traps include active trap bytes;
- outgoing stack-argument calls inside native-trap functions use dynamic
  call-frame adjustment or another proven non-colliding representation;
- push/pop trap CFI is emitted or explicitly gated;
- recovery block labels have the correct CFI state under nontrivial layout;
- CSR root maps in active traps are rejected based on active trap bytes, not
  merely nonzero total stack offset.

### Stage 5: Source tests

Add or update source-level tests for:

- single `try`/`raise_notrace`;
- nested `try`;
- raising across a call;
- non-raising hot path inside `try`;
- GC/safepoint inside an active trap;
- stack spill/reload inside an active trap;
- statepoint inside an active trap with both a stack root and a frame-size check;
- C call inside an active trap if it can interact with stack arguments or
  raising behavior.

### Stage 6: Performance validation

Use installed compilers, not boot compilers.

Benchmark at least:

- `try_raise_cross_function_caught`;
- `try_raise_inline_caught`;
- `layered_try_raise_hit_only`;
- `env_find_same_layered_hit`;
- the existing representative try/trap microbenchmarks.

Compare:

- native compiler;
- current LLVM backend before the native-shaped trap change;
- LLVM backend after the native-shaped trap change.

Expected outcome: the exception-specific slowdown should move materially closer
to native. Any remaining slowdown should be attributable to separate issues,
such as string compare lowering, not trap representation.

### Stage 7: Full validation

After focused tests and benchmarks pass:

- run the regular OxCaml test suite with the LLVM backend;
- build self-stage2;
- run the stage2 test suite;
- record results and remaining risks in `PROGRESS.md`.

## Main risks

### Frame-index correctness

The active-trap-byte adjustment must be applied exactly once. Missing it breaks
loads/stores and GC roots inside active traps. Applying it twice also breaks
them.

### Stackmap correctness

GC tables are correctness-critical. They must describe roots relative to the
actual SP seen by the runtime at each safepoint.

### Hidden recovery edges

If LLVM does not see all possible recovery entries from raising operations
inside an active trap region, optimization or register allocation can preserve
values that are not actually live after dynamic recovery.

### Calls with stack arguments

C calls or other calls with outgoing stack argument areas may interact with the
active trap SP adjustment. This needs focused tests. Do not assume ordinary
reserved-call-frame behavior is enough.

### Verifier coverage

Runtime-entered recovery blocks must be hard to misuse. A design that relies
only on comments or hand-written lowering discipline is too fragile.

The validation should be a real post-isel or machine verifier pass. It should
reject non-ABI live-ins, machine PHIs in recovery blocks, and virtual registers
that are live into recovery from protected predecessors.

### Trap pseudo expansion

Expand the trap pseudos after PEI has resolved frame indexes and after the
active-trap metadata has been consumed, but before final assembly emission.
Prefer expansion to real machine instructions plus `CFI_INSTRUCTION`s rather
than only printing bytes in `AsmPrinter`. This keeps CFI visible to later
machine-level checks and makes the final code shape easier to test in MIR.

## Decision criteria

Keep this approach if the MIR prototype proves:

- active trap stacks can be computed reliably over realistic Machine CFGs;
- SP-relative frame-index and stackmap offsets can be corrected locally;
- runtime-entered block verification catches the important invalid shapes;
- calls inside active trap regions can expose recovery successors without
  excessive target complexity.

Abandon or redesign if:

- correct stackmap emission requires broad invasive changes outside the AArch64
  and OxCaml stackmap path;
- hidden recovery edges cannot be represented in Machine CFG without fighting
  core LLVM invariants;
- the implementation forces frame pointers/base pointers broadly enough to lose
  the performance benefit;
- focused source tests reveal unavoidable semantic mismatch with the native
  trap model.
