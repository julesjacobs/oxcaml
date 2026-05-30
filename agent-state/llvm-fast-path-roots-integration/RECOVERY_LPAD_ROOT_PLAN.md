# Recovery landingpad and handler-live value plan

## Goal

Make OxCaml AArch64 LLVM trap recovery use one real LLVM unwind target per
runtime recovery entry, with no skipped intermediary block, while still letting
LLVM optimize ordinary handler-live values normally.

The intended model is:

- each protected may-raise operation has an explicit unwind edge to the active
  recovery landingpad;
- the trap frame stores the address of that same recovery landingpad;
- the runtime enters the same block LLVM thinks the exceptional edge enters;
- `trap.recover` snapshots only runtime ABI state;
- ordinary source values used by the handler are modeled by LLVM values on the
  exceptional edge;
- GC pointers used by the handler are represented by the most precise safe
  mechanism available for that edge shape.

## Review findings

### The direct landingpad target is required

The old trampoline shape is unsound for anything that can become a real
instruction in the skipped block:

```llvm
invoke ... unwind label %lpad_trampoline

lpad_trampoline:
  %lp = landingpad token cleanup
  %reloc = gc.relocate ...
  br label %recover

recover:
  %rec = trap.recover()
```

The runtime jumps to the published trap target, not through arbitrary LLVM
trampoline code. Therefore the published target and the LLVM unwind target must
be the same recovery landingpad whenever values or relocates live on that edge.

### PHIs in recovery are viable

The earlier "reject all PHIs in runtime-entered blocks" rule is too strong
before machine PHI elimination.

The experiment with a two-predecessor recovery landingpad showed:

- machine PHI elimination already treats `runtime-entered` successors like
  hidden-transfer successors;
- PHI copies are inserted before the possibly-raising call;
- regalloc spills those values across the all-caller-saved call when needed;
- the recovery block reloads the ordinary value and combines it with the
  runtime ABI value from `x0`.

The verifier invariant should be:

- runtime-entered machine PHIs are allowed while the machine function is still
  SSA;
- runtime-entered machine PHIs are rejected once the machine function has
  `NoPHIs`.

### GC relocates need a split rule

LLVM has a real exceptional statepoint model. On an invoke statepoint's unwind
path, an exceptional `gc.relocate` is tied to the landingpad token, not directly
to the invoke token. That works when the landingpad token identifies the
statepoint through a unique unwind predecessor.

This means the right rule is not "never use exceptional `gc.relocate`".

The right rule is:

- use exceptional `gc.relocate` when LLVM's statepoint model can identify the
  statepoint for that landingpad;
- use explicit handler-live root slots when the handler shape is shared or
  merged in a way that prevents a landingpad token from identifying one
  statepoint cleanly.

Shared OCaml handlers can have multiple static recovery entries and multiple
raising sites. For those cases, a single landingpad token cannot describe all
possible statepoints. The frontend must either keep separate recovery entries
until after per-edge relocates/root stores are done, or use explicit slots for
handler-live GC pointers.

### Broad volatile slots are still the wrong default

Volatile trap-live slots solve correctness by blocking LLVM, but they are the
reason hot `try` code stays slow. The new design should keep addressable root
slots only where GC metadata or shared-handler semantics require them. Ordinary
non-GC values should remain SSA values and let PHI elimination/regalloc decide
whether to spill.

## Revised plan

### 1. Canonical IR shape

For every active trap region, emit direct recovery landingpads:

```llvm
call void @llvm.aarch64.oxcaml.trap.publish(...,
  blockaddress(@f, %recover))

%r = invoke ... @callee(...)
     to label %normal
     unwind label %recover

recover:
  %edge_value = phi ...
  %lp = landingpad token cleanup
  %rec = call @llvm.aarch64.oxcaml.trap.recover()
  ...
```

No branch-only trampoline may contain `landingpad`, PHIs, `gc.relocate`, loads,
stores, or other real work that the runtime can skip.

### 2. Runtime-entered block invariant

Keep the generic `runtime-entered` machine block concept.

For AArch64 OxCaml recovery blocks:

- required live-ins are exactly `x0`, `x26`, `x27`, and `x28`;
- `trap.recover` snapshots those ABI values;
- normal handler code may use ordinary SSA values after the recovery snapshot;
- no ordinary protected-path value may be consumed before the recovery snapshot;
- machine PHIs are allowed only before PHI elimination.

### 3. Pass pipeline

Move the runtime-entry clobber insertion after machine PHI elimination and
before register allocation.

Do this through a small target hook or pass insertion point, not by copying the
whole target-independent optimized regalloc pipeline.

The pass order must guarantee:

1. ISel may produce PHIs in runtime-entered blocks.
2. PHI elimination lowers those PHIs onto predecessor edges.
3. Runtime-entry clobbers mark calls with runtime-entered successors as
   clobbering allocatable caller-saved registers.
4. Regalloc spills/reloads ordinary handler-live values as needed.

### 4. Handler-live non-GC values

Represent non-GC handler-live values as ordinary SSA/alloca-promoted values.

Expected lowering:

- PHIs on the recovery landingpad are okay in LLVM IR;
- machine PHI elimination places copies before the raising call;
- regalloc spills across the call if the value cannot legally remain in a
  register.

Do not use volatile slots for these values.

### 5. Handler-live GC pointers

Use two representations.

Use exceptional `gc.relocate` when:

- the recovery landingpad has a unique invoke statepoint predecessor, or LLVM's
  verifier/statepoint lowering can otherwise identify the exact statepoint from
  the landingpad token;
- the relocated value is consumed in that recovery entry before merging with
  other recovery entries.

Use explicit root slots when:

- multiple invokes unwind to one recovery landingpad and the landingpad token
  cannot identify one statepoint;
- multiple recovery entries branch to a shared OCaml handler and the handler
  needs one merged GC value;
- LLVM optimization would otherwise have to merge exceptional relocates from
  different statepoints through a PHI of landingpad tokens.

Explicit root slots should be narrow:

- only for handler-live GC pointers that need them;
- not volatile as the main correctness mechanism;
- represented in OxCaml frame/root metadata in a way that survives generic
  optimization;
- loaded in the handler after `trap.recover`.

The statepoint experiments found that a plain alloca root slot in `"gc-live"`
before `opt -O2` is not optimization-stable. Generic optimization can remove
the slot from `"gc-live"`; escaping or volatile accesses preserve memory traffic
but still do not preserve the GC root location. If this path is needed, it must
be a real OxCaml root/frame-table mechanism or a late-lowered slot, not just an
alloca that happens to be live at the statepoint in initial IR.

### 6. Shared handler lowering

For shared OCaml handler blocks, keep per-trap recovery entries separate.

Each recovery entry should:

1. snapshot `trap.recover`;
2. perform any edge-local exceptional relocates or root-slot loads/stores;
3. branch to the shared handler block with ordinary SSA values.

Only merge after the recovery-entry-specific work is complete.

### 7. `raise_notrace`

Lower direct raise as a call-like or terminator-like AArch64 pseudo with the same
modeled exceptional successor discipline as an invoking call.

The final asm should remain native-shaped:

```asm
mov x0, <bucket>
mov sp, x26
ldp x26, x16, [sp], #16
br x16
```

The pseudo must expose that control continues at the active recovery block, not
straight-line after the raise.

### 8. Remove non-native recovery register writes

`trap.recover` should model the runtime ABI values directly. In particular,
the recovery-side `write_trap_pointer_register` inline asm is not part of the
native-shaped model. The runtime enters recovery with the previous trap pointer
in `x26`; LLVM should see that as the `trap.recover` result, not as a side
effectful rewrite in the handler.

### 9. Required tests

LLVM IR / MIR tests:

- direct recovery landingpad with one invoke;
- direct recovery landingpad with two invokes and a scalar PHI;
- machine verifier allows runtime-entered PHIs before PHI elimination;
- machine verifier rejects runtime-entered PHIs after `NoPHIs`;
- runtime-entry clobber pass runs after PHI elimination and before regalloc;
- scalar handler-live PHI spills before the call and reloads in recovery;
- exceptional `gc.relocate` on a unique invoke recovery path;
- shared-handler GC value through an explicit root slot;
- negative test for skipped trampoline with real work;
- negative test for protected-path value used before `trap.recover`;
- negative test for invalid runtime-entered live-ins.

OCaml source tests:

- direct call in try, no raise;
- direct call in try, raise caught;
- closure call in try;
- nested try;
- shared handler from multiple static traps;
- handler uses an integer local from before the raising call;
- handler uses a GC pointer from before the raising call;
- forced allocation/GC before raising, proving handler-live GC pointer handling;
- direct `raise_notrace` caught by current handler.

Validation:

- focused LLVM `llc`/`opt` tests;
- focused `testsuite/tests/llvm-codegen`;
- full LLVM backend testsuite;
- self-stage2 testsuite;
- representative microbenchmarks;
- native-built compiler vs LLVM-built compiler benchmark.

## Stabilized decision

This approach remains viable. The complexity is not accidental; it comes from
real control-flow cases:

- runtime recovery is an external entry into a handler block;
- LLVM must see the same edge the runtime can take;
- ordinary values can use LLVM's existing PHI/regalloc machinery;
- GC values need either LLVM's exceptional statepoint relocate model or explicit
  root slots when handlers are shared.

The plan should not keep broad volatile trap slots as the main representation.
It should use direct recovery landingpads, allow PHIs until PHI elimination, and
handle GC values with the narrowest representation that is valid for each edge
shape.
