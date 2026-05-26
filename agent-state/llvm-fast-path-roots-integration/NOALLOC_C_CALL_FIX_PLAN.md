# Plan: first-class noalloc C calls

Date: 2026-05-26.

This is the reviewed plan for reducing LLVM backend overhead on register-only
noalloc C calls.

Status after three review loops and one brainstorming round: a first-class
noalloc C-call contract is still the right area to investigate, but the default
implementation route should change. Do not start with the full
`TargetOpcode::STATEPOINT` mode. First prove whether the motivating calls are
true GC leaves and, if so, use a narrow non-statepoint leaf path as a diagnostic
prototype. Do not treat the allowlist as the whole answer unless profiling shows
those exact calls explain the compiler-binary gap. The broader goal is a general
low-overhead call-boundary contract for mechanically safe calls.

## Goal

Reduce the LLVM backend overhead for register-only noalloc C calls such as:

- `caml_obj_tag`
- `caml_string_equal`
- `caml_string_compare`
- other cheap `[@@noalloc]` externals with no stack arguments

Measured bad cases from `c_call_slowdown`:

```text
noalloc_add1_loop    LLVM 1.710x slower
obj_tag_loop         LLVM 2.307x slower
string_equal_loop    LLVM 1.236x slower
string_compare_loop  LLVM 1.236x slower
```

The intended fix is not expected to solve all compiler-binary slowdown by
itself. Local handler lowering (`_wrap_try` / `recover_rbp`) is a separate
known contributor.

Important evidence gap:

- The microbenchmarks prove that register-only noalloc C-call wrappers are
  expensive when the callee is cheap.
- They do not prove that `caml_obj_tag`, `caml_string_equal`, or
  `caml_string_compare` specifically explain the remaining 8-11%
  compiler-binary slowdown.
- Compiler-source reductions suggest C-call wrappers matter in string-lookup
  code, but the compiler-binary gap is broader and also includes local-handler
  call boundaries and ordinary OCaml call runtime-register traffic.

So the allowlist path is a way to test the runtime/LLVM contract cheaply. It is
not the final design unless measurement shows the allowlisted calls cover the
real profile. The final design should generalize to a class of mechanically
proven safe call boundaries.

## Current lowering

In `backend/llvm/llvmize.ml`, external calls split as follows:

- `stack_ofs > 0`: call `caml_c_call_stack_args`.
- `alloc = true`: call `caml_c_call`.
- `alloc = false && stack_ofs = 0`: generate and call a private
  `c_call_wrapper.*` function.

The wrapper switches from the OCaml stack to the C stack, calls the real C
function, then switches back:

```asm
sub sp, sp, #16
str x30, [sp, #8]
mov x19, sp
ldr x8, [x28, #104]  ; Domain_c_stack
mov sp, x8
bl  _caml_string_compare
mov sp, x19
ldr x30, [sp, #8]
add sp, sp, #16
ret
```

The caller pays a wrapper call, wrapper prologue/epilogue, real C call,
wrapper return, and extra caller save/restore pressure around the wrapper call.

Native arm64 emits the stack switch inline at the call site.

## Reviewed design choice

Do not implement a late pass that recognizes `c_call_wrapper.*` bodies and
rewrites calls to them.

The reviewers rejected that as the primary design for three reasons:

1. `llvmize.ml` already knows the real C symbol, argument types, result types,
   `alloc=false`, and `stack_ofs=0`. Rediscovering that fact from generated
   wrapper machine code is a brittle peephole.
2. A naive late rewrite is not statepoint-safe. On AArch64,
   `LowerSTATEPOINT` emits the `BL`, then the asm printer records the
   statepoint label immediately after the `BL`. If `sp` is restored by a normal
   instruction after that label, the frametable can observe the C stack instead
   of the OCaml stack.
3. A post-register-allocation rewrite cannot improve the caller's register
   allocation quality. The call has already been modeled as an opaque OxCaml
   wrapper call.

Therefore the main plan is an explicit noalloc C-call contract that survives to
AArch64 lowering. It should be non-statepoint when the call is mechanically
proven to be a GC leaf, and statepoint-aware only as a fallback.

## Brainstorming conclusion

Five reviewers challenged the previous preferred route. The consensus was:

1. The full `TargetOpcode::STATEPOINT` noalloc-C-call mode is probably too
   heavy as the first implementation. It touches `RewriteStatepointsForGC`,
   SelectionDAG statepoint lowering, `StatepointOpers`, `StackMaps`, AArch64
   emission, register masks, CFI, and metadata tests.
2. Native arm64 is evidence that the motivating noalloc C calls are intended to
   be GC leaves: in `backend/arm64/emit.ml`, the `alloc=false && stack_ofs=0`
   path switches to `Domain_c_stack`, calls the real C symbol, restores `sp`,
   and does not call `record_frame`. CFG also treats `alloc=false` external
   calls as non-raising.
3. The current LLVM wrapper path is more conservative than native: it calls the
   generated wrapper through `call_simple` with `can_call_gc=true` and live-root
   bundles. That may be unnecessary for true noalloc leaf stubs and may be a
   major source of the overhead.
4. If a noalloc C call can callback, raise through OCaml, allocate, or otherwise
   need runtime stack walking as an OCaml safepoint, then the current noalloc
   wrapper is not a sufficient safe runtime transition either; that call should
   not use the optimized noalloc path.

Recommended ranking:

1. **Audited GC-leaf inline prototype.** Optimize a narrow allowlist such as
   `caml_obj_tag`, `caml_string_equal`, and `caml_string_compare` with a
   non-statepoint inline stack-switch call. This is primarily a proof of the
   contract and an upper-bound measurement for the wrapper overhead.
2. **General safe-call contract.** If the prototype works, replace the allowlist
   with a propagated `no_gc/no_callback/no_raise/no_unwind` style property, and
   apply the lower-overhead boundary to every matching register-only noalloc
   C call. This is the likely real fix for the expensive-call problem.
3. **Cheaper-wrapper experiment.** Before major LLVM surgery, measure how much
   is recovered by making the existing noalloc wrappers GC-leaf-like: no
   live-root bundle and less runtime-register result pressure where sound.
4. **Full `TargetOpcode::STATEPOINT` mode.** Use this only if the GC-leaf proof
   fails but a statepoint-preserving inline stack switch is still clearly worth
   the implementation cost.

## Implementation gates

Resolve these before writing the full implementation:

1. Quantify which call boundaries explain the compiler-binary gap. Collect
   dynamic counts/profile evidence for generated noalloc C-call wrappers,
   `caml_c_call`, `caml_c_call_stack_args`, local-handler `_wrap_try` calls,
   and ordinary OCaml calls with runtime-register round trips while compiling
   representative compiler files such as `typecore.ml`.
2. Prove or disprove the GC-leaf contract for the audited allowlist. The proof
   must cover allocation, callbacks into OCaml, OCaml exception machinery,
   blocking sections, pending actions, memprof, async signals, preemption,
   backtraces, and stack walking.
3. Decide the mechanical source of "no callback and no OCaml raise". Current
   CFG external-call information gives `alloc`, `effects`, argument/result
   types, and stack layout. It does not obviously carry a separate callback
   exclusion fact. The production implementation must add and propagate such a
   fact or use an equivalently mechanical source. The audited allowlist is only
   acceptable for experiments or a deliberately narrow first PR.
4. If the audited calls are true GC leaves, prototype a non-statepoint AArch64
   noalloc-C-call operation with an inline stack switch, normal C ABI
   arguments/results, dedicated regmask, and CFI matching native arm64.
5. Decide whether the prototype should become a general noalloc-C-call
   operation. Do not stop at the allowlist if compiler-binary evidence shows
   expensive call boundaries are broader than those symbols.
6. If the calls are not true GC leaves,
   write down the runtime invariant that makes the post-restore statepoint PC
   valid, including what async profiling, signal handling, preemption, CFI, and
   stack walking can observe while the real C call is running on
   `Domain_c_stack`.
7. Only after that, choose and prototype the exact LLVM statepoint
   representation. The fallback statepoint route is to keep the final machine
   instruction as LLVM's real `TargetOpcode::STATEPOINT`, with an OxCaml
   noalloc-C-call mode recognized by AArch64 statepoint lowering/emission. Do
   not invent an unrelated target pseudo unless it is also taught to every
   statepoint consumer that matters: `StatepointOpers`, `StackMaps`, machine
   verification, call-preserved masks, and AArch64 asm emission.
8. Pick the saved-OCaml-stack register deliberately. Native arm64 uses the
   frame-pointer register in this stack-switch shape. The LLVM design should
   first test whether `x29` can be used under the current no-frame-pointer and
   Darwin/frame-chain constraints. Use `x19` only with a written reason and a
   modeled clobber/mask contract.

## Decision gate: statepoint or GC leaf

Before implementing the full fix, decide whether register-only noalloc C calls
must remain statepoints.

`alloc=false` says the C function does not allocate through the OCaml runtime,
but the implementation plan must not infer too much from that. The decision
needs evidence for:

- whether `alloc=false` also excludes callbacks into OCaml;
- whether `alloc=false` also excludes raising through OCaml exception
  machinery;
- whether async/signal/preemption stack walking can ever observe a return PC
  inside such a call;
- what stack/CFI state such an observer would see while the call is executing
  on `Domain_c_stack`;
- whether live roots across the call need a frametable entry for any runtime
  path;
- whether the non-allocating/no-callback/no-raise property is mechanically
  available from Cmm/external-call attributes.

If the answer is "these calls are true GC leaves", then the best contract is a
non-statepoint noalloc C-call operation with no live-root bundle.

If not, keep the statepoint-aware operation described below. Do not choose
`gc-leaf` just because today's tests happen to pass.

The rest of this plan describes both routes. The non-statepoint GC-leaf route
is now the first route to prove and prototype. The conservative statepoint route
is fallback design material, not the default implementation plan. The production
goal is not "inline three runtime stubs"; it is "make the safe call-boundary
class cheap."

## Desired contract

Add a first-class LLVM/OxCaml operation for register-only noalloc C calls.
Call the source-level operation `oxcaml.noalloc_c_call` in this note.

For the GC-leaf route, this is an OxCaml noalloc-C-call operation that is not a
statepoint. For the fallback conservative route, it is an OxCaml noalloc-C-call
statepoint mode. A new LLVM calling-convention spelling such as
`oxcaml_c_noalloccc` may be useful as an IR routing marker, but the calling
convention is not the core abstraction. The core abstraction owns:

- stack switch to `Domain_c_stack`;
- real C call;
- stack restore;
- statepoint/stackmap PC placement only in the fallback statepoint route;
- explicit register mask and scratch-register contract.

Semantics:

- It calls a real C function directly.
- It runs the C function on `Domain_c_stack`.
- It restores the OCaml stack pointer before control returns to ordinary OCaml
  code. In the fallback statepoint route, it also restores the OCaml stack
  pointer before the statepoint/stackmap call-site PC is recorded.
- It uses the default C ABI for the real C arguments and result.
- It keeps the OxCaml runtime registers available across the call as
  live-through state. The real C callee does not return them.
- In the GC-leaf route, it has no live-root bundle and no stackmap entry for the
  C call.
- In the fallback statepoint route, it preserves the current
  statepoint/deopt/live-root contract.
- It does not apply to C calls that allocate, pass stack arguments, unwind, or
  call back into OCaml.

This is a codegen contract, not a source-level semantic change.

## Proposed implementation

### 1. Add a separate noalloc-C-call emission path

Do not reuse `call_simple` for this path.

`call_simple` prepends runtime-register arguments and expects the callee to
return an OxCaml result struct containing updated runtime registers plus real
results. That is correct for OCaml-shaped calls and current wrappers, but it is
the wrong contract for a real C callee.

Add a dedicated `call_noalloc_c` path in `llvmize.ml`:

- operands:
  - real C symbol;
  - current domain-state value;
  - current allocation-pointer value;
  - current C-stack pointer value;
  - real C arguments only;
- result:
  - real C result only;
  - zero-result calls return no value;
  - one-result scalar calls are the first supported case;
  - multiple-result calls remain on the old wrapper path until explicitly
    designed.

The dedicated path must not synthesize fake runtime-register return values from
the real C callee. `x27` and `x28` are live-through values modeled by the
statepoint mode/register mask, not values returned by the C function.

Load the C-stack pointer in `llvmize.ml` using the existing domain-state helper
for `Domain_c_stack`, and pass it as an explicit hidden operand. Do not hard-code
the `Domain_c_stack` offset in vendored LLVM target lowering.

Hidden operands are not C ABI call arguments. They must not consume `x0..x7` or
`d0..d7`, and they must not shift the real C arguments. The lowering must peel
them off before C ABI assignment, or carry them through a statepoint operand
bundle / target operand that is explicitly excluded from real call-argument
classification.

### 2. Change the `alloc=false && stack_ofs=0` path

For `alloc=false && stack_ofs=0`, use the new path only when the full safety
contract below holds.

In the conservative statepoint version, keep the existing deopt/live-root
operand bundles. This makes the first implementation behavior-preserving with
respect to the current frametable contract only if the runtime-observer and CFI
questions in the implementation gates are answered.

Do not change:

- `alloc=true`: still use `caml_c_call`.
- `stack_ofs > 0`: still use `caml_c_call_stack_args`.
- calls with unwind edges: keep the conservative wrapper path until explicitly
  designed and tested.

### 3. Define AArch64 argument, result, and ABI eligibility

The operation needs hidden OxCaml values plus normal C arguments.

Hidden operands:

- domain-state value is constrained to `x28`;
- allocation-pointer value is constrained to `x27`;
- C-stack pointer is an explicit hidden operand loaded by `llvmize.ml`.

Real C argument assignment:

- Real C arguments use the normal AArch64 C ABI locations:
  `x0..x7`, `d0..d7`, then stack if ever allowed.

Initial eligibility forbids stack arguments, so the first implementation only
accepts C signatures whose real arguments fit in C registers.

Result assignment:

- Real C result uses the normal C ABI result location.
- Runtime values are logically preserved across the call, not produced by the C
  callee. The lowering may model them as live-through values in `x28` and
  `x27`, but the real emitted C call must not depend on the callee writing
  them.

Initial supported type/result set for the first PR:

- direct symbol only;
- no unwind edge;
- no stack arguments;
- zero or one real result;
- integer/pointer register ABI argument/result types only;
- at most eight integer/pointer arguments;
- no float, double, aggregate, byval, sret, vararg, vector, or multiple-result
  shapes.

Float/double support is a follow-up. The motivating measured cases
`caml_obj_tag`, `caml_string_equal`, `caml_string_compare`, and the local
integer noalloc microbenchmark do not need it.

Important ABI rule:

Do not assume the current OxCaml value locations are already the C ABI
locations. The new operation's job is to pass the real C operands using the
default C ABI. The eligibility check is about whether the C signature is within
the small supported register-only subset, not whether the source values already
sit in the final physical registers.

The implementation should not grow a broad hand-written ABI classifier in
`llvmize.ml`. For the first cut, use the narrow integer/pointer predicate above.
If broader signatures are needed, add a target-side query/helper or reuse the
AArch64 TableGen assignment logic rather than duplicating AAPCS in OCaml.
Boundary tests must cover the accepted and rejected cases.

### 4. Preferred route: lower as a non-statepoint GC leaf

Use this route only for the audited allowlist once the GC-leaf contract is
proved.

The operation should:

- use normal C ABI locations for the real C arguments and result;
- carry the C-stack pointer and runtime-register liveness as non-C-ABI operands
  or fixed-register uses;
- emit a native-like stack switch:

```asm
mov saved_sp_reg, sp
<CFI state matching native arm64 if required>
mov x16, c_stack_operand
mov sp, x16
bl  real_c_symbol
mov sp, saved_sp_reg
<restore CFI state if changed>
```

It must not produce a frametable/stackmap entry for the C call. That is the
point of proving the leaf contract.

### 5. Fallback route: lower as a real LLVM statepoint

The key correctness requirement is the stackmap PC.

Do not lower to an unrelated target pseudo and hope it behaves like a
statepoint. The final instruction reaching the stackmap recorder should remain
LLVM's real `TargetOpcode::STATEPOINT`, or a new opcode must be explicitly
taught to every generic statepoint consumer. The fallback statepoint design is:

- `RewriteStatepointsForGC` still produces a normal explicit
  `gc.statepoint`;
- SelectionDAG statepoint lowering still produces `TargetOpcode::STATEPOINT`;
- the statepoint carries an OxCaml noalloc-C-call mode plus non-C-ABI hidden
  operands for the C-stack pointer and runtime-register liveness;
- AArch64 `LowerSTATEPOINT` recognizes that mode, emits save-SP,
  switch-to-C-stack, `BL`, restore-SP, then records the stackmap label;
- the machine instruction has the right implicit uses/defs and regmask before
  register allocation / machine verification, even though the final textual
  sequence is emitted by AArch64 statepoint emission.

The statepoint-mode instruction must carry:

- real C call target;
- hidden domain-state operand constrained to `x28`;
- hidden allocation-pointer operand constrained to `x27`;
- hidden C-stack-pointer operand;
- real C argument operands;
- real C result defs;
- the normal statepoint/deopt/live-root operands if using the conservative
  statepoint contract;
- implicit def/clobber of the chosen saved-stack register;
- scratch use of `x16`;
- LR/SP call effects;
- explicit call-preserved register mask.

The AArch64 statepoint emission for this mode emits:

```asm
mov saved_sp_reg, sp
mov x16, c_stack_operand
mov sp, x16
bl  real_c_symbol
mov sp, saved_sp_reg
<record statepoint / stackmap label here>
```

The statepoint label must be after `mov sp, saved_sp_reg`, not immediately
after the `BL`.

Do not implement this as a normal `BL real_c_symbol` followed by ordinary
instructions. Do not hide the stack switch in a way that register allocation
and machine verification cannot see. The final implementation must include a
MIR/codegen check showing the statepoint opcode, hidden operands, implicit
scratch/clobber registers, and regmask before final emission.

### 6. Saved-stack register rule

Do not assume `x19` is the right saved-OCaml-stack register.

Native arm64 uses `x29` in this stack-switch shape. For LLVM, test `x29` first:
it may be the least surprising choice when LLVM OCaml functions use the
no-frame-pointer calling convention. Reject `x29` only if the current
frame-chain, CFI, Darwin, or stack-scanner contracts make it unsafe.

If the first implementation uses `x19`, that is only safe if the noalloc C-call
statepoint explicitly models `x19` as clobbered or reserved across the call. It
must not rely on normal C-call preservation of `x19`.

Concretely:

- the statepoint uses/defines the chosen saved-stack register;
- register allocation must not keep a live value in that register across the
  statepoint;
- the emitted real C call can rely on the C callee preserving the chosen
  saved-stack register while it holds the old OCaml stack pointer.

Do not use `x16` or `x17` to hold the old stack pointer across the C call.
Those registers are C/linker scratch registers and may be clobbered by the
callee or a veneer.

### 7. Clobber contract

The visible clobber contract must be explicit.

The real C callee follows the C ABI, but the noalloc C-call operation is not
just a normal C call because it also clobbers the saved-stack scratch register
while switching stacks.

The conservative first contract:

- preserve `x27` and `x28`;
- clobber the chosen saved-stack register (`x29` if validated, otherwise
  `x19`);
- otherwise preserve exactly the AAPCS C-call callee-saved set, minus that
  chosen saved-stack register;
- model `x16`, `x17`, LR, and ordinary C-call caller-saved registers as
  clobbered as required by the ABI and linker veneer rules;
- keep no live-root/statepoint metadata in the GC-leaf route;
- keep the same live-root/statepoint metadata as the current wrapper call only
  in the fallback statepoint route.

Do not reuse `CSR_AArch64_OxCaml_C_Call` blindly: that mask is for
`caml_c_call` and explicitly has different runtime-register behavior. Add a
dedicated mask for the noalloc C-call statepoint mode.

If this still leaves too much caller save/restore pressure, refine later.
Correctness comes first.

### 8. LLVM plumbing checklist

If a new calling-convention spelling is used as the IR marker, wire it
deliberately through the same places as the existing OxCaml conventions:

- IR calling-convention enum;
- IR parser/printer;
- verifier/tail-call restrictions if needed;
- RewriteStatepointsForGC OxCaml-statepoint classification;
- AArch64 call lowering;
- AArch64 return lowering;
- AArch64 register masks / CSR definitions;
- AArch64 frame-lowering predicates that special-case OxCaml conventions;
- non-AArch64 fallback behavior that rejects or keeps the old wrapper path.

The chosen first prototype should prefer one representation, not keep a fork in
the implementation. Current preference after brainstorming: a normal
real-C-symbol call with a new OxCaml noalloc-C marker for an audited GC-leaf
allowlist. If the GC-leaf route fails, record why before switching to an
intrinsic or `TargetOpcode::STATEPOINT` mode.

## Safety contract

Apply the new lowering only when all are true:

- target is AArch64;
- external call has `alloc=false`;
- external call has `stack_ofs=0`;
- call has no unwind edge;
- call target is direct;
- real C arguments and result fit the first-cut integer/pointer register-only
  subset;
- callee is mechanically known to be a true noalloc C function: no allocation,
  no callback into OCaml, no raising through OCaml exception machinery.

Until that last fact is propagated through CFG, use a small audited allowlist of
runtime stubs for experiments rather than treating all `alloc=false` externals
as safe.

Keep the old wrapper path for anything outside this contract.

## Preparatory experiments

Before the full implementation, run these experiments and record the decision.

1. Add profile/counter instrumentation for representative compiler-file
   compilations. Record dynamic counts and time attribution for:
   - generated `c_call_wrapper.*` calls by target symbol;
   - `caml_c_call` and `caml_c_call_stack_args`;
   - `_wrap_try` / `recover_rbp` local-handler calls;
   - ordinary OCaml calls that return unchanged runtime registers but force
     explicit runtime-register traffic.

2. Audit `caml_obj_tag`, `caml_string_equal`, `caml_string_compare`, and a small
   set of rejected noalloc externals. Check for `CAMLparam`, allocation,
   callbacks, OCaml raises, `caml_process_pending_actions`, blocking sections,
   memprof, and any runtime path that could need an OCaml frame while the C call
   runs.

3. Compare three local variants on the same installed compiler:
   - current wrapper;
   - cheaper wrapper with GC-leaf-like call metadata where sound;
   - audited non-statepoint inline stack-switch call.

4. Stress the audited GC-leaf prototype with signals, memprof, preemption,
   backtraces, and stack walking. Compare behavior with native arm64.

5. Generalization experiment: replace the allowlist with a mechanical property
   on external calls, initially accepted only for the same audited stubs. Check
   that the code shape and tests do not depend on symbol names.

6. Prototype the fallback `TargetOpcode::STATEPOINT` route on one synthetic
   signature, such as `int -> int`. Verify that the final stackmap record is
   attached to the post-restore label and that MachineVerifier accepts the
   implicit operands and regmask.

7. Check runtime observation/CFI behavior. Either prove no async/signal/
   preemption path can observe execution inside these noalloc C calls, or emit
   CFI/runtime-visible state equivalent to what the native arm64 backend relies
   on while it switches to the C stack.

8. Compare `x29` and `x19` as the saved-OCaml-stack register. Record why the
   chosen register is compatible with frame pointers, the stack scanner, CFI,
   and Darwin/linker constraints.

9. Decide the first-cut eligibility source: propagated attribute vs audited
   allowlist. Do not start with a broad `alloc=false` rewrite.

## Tests

Add focused tests before relying on benchmarks.

Positive codegen tests:

- zero-real-argument noalloc C call uses no wrapper and does not shift hidden
  runtime operands into `x0`;
- zero-result noalloc C call uses the same general path;
- one-int noalloc C call uses no wrapper and emits inline stack switch;
- zero, one, and many register-argument calls all use the same general
  machinery, with no special `n=0` or `n=1` lowering path;
- two-string noalloc C call uses no wrapper and emits inline stack switch;
- GC-leaf route emits no stackmap/frametable record for the C call;
- fallback statepoint route records the statepoint/stackmap label after the
  stack restore;
- fallback statepoint route's emitted stackmap/frametable record corresponds to
  the post-restore PC;
- the chosen saved-stack register is not live across the operation except as
  the saved stack pointer;
- IR/codegen does not return/extract `{x28,x27}` as fake call results.
- MIR/codegen checks show the chosen operation mode, hidden operands, implicit
  scratch/clobber registers, and dedicated register mask before final emission.

Negative codegen tests:

- `alloc=true` still uses `caml_c_call`;
- `stack_ofs > 0` still uses `caml_c_call_stack_args`;
- calls with unwind edges are not rewritten;
- calls not mechanically proven no-callback/no-raise are not rewritten;
- signatures outside the supported integer/pointer register-only subset are not
  rewritten;
- exactly-at-register-limit cases are accepted when otherwise supported;
- just-over-register-limit cases stay on the old path;
- float/double, vector, aggregate, and multiple-result shapes stay on the old
  path in the first PR.

Metadata/observer tests:

- for the GC-leaf route, inspect object metadata to prove the C call does not
  create a stackmap/frametable record;
- for the fallback statepoint route, inspect the emitted stackmap/frametable
  record or MIR/object metadata, not only nearby assembly text, to prove the
  recorded PC is the post-restore PC;
- run with machine verification enabled for the new operation mode;
- if CFI changes are required, add an object-level check for the emitted CFI
  around the stack switch.

Validation:

- LLVM codegen tests;
- `llvm-stack-checks`;
- full `make llvm-test-no-rebuild`;
- self-stage LLVM compiler build;
- compiler-binary benchmark against a native-built compiler.

Benchmark methodology:

- rerun `c_call_slowdown` before and after with the same installed compiler
  flags and same `LLVM_PATH`;
- rerun representative compiler-source compile benchmarks with both compilers
  compiling through the normal native backend;
- collect profile or call-count evidence showing whether the optimized symbols
  are actually material in the compiler-binary benchmark;
- separately track the remaining general call-boundary costs after the noalloc
  C-call change, especially local handlers and ordinary OCaml call
  runtime-register traffic.

## Expected outcome

Expected microbenchmark direction:

- `obj_tag_loop`: large improvement;
- `string_equal_loop`: material improvement;
- `string_compare_loop`: material improvement;
- `noalloc_add1_loop`: large improvement;
- `int_hash_loop`: small improvement;
- `float_sin_loop`: small improvement;
- `int_of_string_loop`: unchanged;
- `noalloc_sum10_loop`: unchanged.

Expected compiler-binary direction:

- the allowlist prototype should improve compiler-binary performance only if
  those symbols are dynamically material in the representative compile profile;
- if the slowdown remains broad, use the profile evidence to generalize from
  symbol allowlist to safe call-boundary contract;
- not full parity unless local-handler lowering and ordinary OCaml call
  runtime-register traffic are also addressed where they are material.

## Rejected first draft

The first draft proposed a late AArch64 pass that would recognize
`c_call_wrapper.*` bodies and splice an inline stack switch at each call.

The first review loop rejected that as the primary plan. Accepted critiques:

- explicit contract is cleaner than structural wrapper recognition;
- statepoint PC placement is a correctness requirement, not a risk note;
- scratch-register ownership must be defined before implementation;
- post-register-allocation rewriting cannot improve caller register allocation;
- tests must cover statepoint/frametable correctness and negative cases.

Rejected critiques:

- none of the major correctness critiques were rejected. The only kept part of
  the first draft is the performance goal and the native-like target code shape.

## Second review loop

A second review loop accepted the first-class direction but found that this plan
still needed sharper contracts. Accepted second-loop critiques:

- the new path must not reuse `call_simple` or fake runtime-register returns;
- the pseudo/intrinsic is the real abstraction, not the calling-convention name;
- `Domain_c_stack` should be loaded in `llvmize.ml` and passed as an operand,
  rather than hard-coding the domain-state offset in target lowering;
- the register mask must be a dedicated AAPCS-minus-`x19` style mask preserving
  `x27`/`x28`, not the existing `OxCaml_C_Call` mask;
- the GC-leaf experiment must be a decision gate with runtime criteria;
- tests need zero-argument, boundary, fake-runtime-return, and stackmap-record
  checks.

## Third review loop

A third review loop found that the revised plan was still too vague in the
hardest places. Accepted third-loop critiques:

- an independent AArch64 pseudo is not automatically an LLVM statepoint. The
  preferred prototype must keep the final machine instruction on the real
  `TargetOpcode::STATEPOINT` path, or explicitly teach a new opcode to all
  statepoint consumers;
- hidden runtime/C-stack operands must not be real C ABI arguments. They must
  be peeled off before C ABI assignment or carried out-of-band in a statepoint
  mode;
- `alloc=false` alone is not a mechanical proof of no callback. The first
  implementation needs a propagated attribute or audited allowlist;
- the plan must account for runtime observers and CFI while the real C call is
  executing on `Domain_c_stack`;
- `x19` is not automatically the right saved-stack register. Test the
  native-like `x29` choice first, then justify any fallback;
- the first ABI subset should be narrower: integer/pointer register-only calls.
  Float/double support is a follow-up;
- stackmap tests must inspect the recorded metadata, not only instruction
  order in assembly;
- if this note is turned into PR-facing design text, remove process-heavy
  review-loop history and keep only the technical decisions.

## Brainstorming round

A brainstorming round asked whether the `TargetOpcode::STATEPOINT` route is
really the right approach. Accepted critiques:

- the statepoint route should not be the default until GC-leaf semantics are
  disproved. It is too invasive for calls that native arm64 already appears to
  treat as leaves;
- native arm64 evidence matters: the `alloc=false && stack_ofs=0` path switches
  to `Domain_c_stack`, calls the real C symbol, restores `sp`, and does not
  record a frame;
- current LLVM wrappers are likely over-conservative because the wrapper call is
  emitted with `can_call_gc=true` and live-root bundles;
- if a noalloc C call can callback, raise, allocate, or otherwise require a
  full runtime transition, then the current noalloc wrapper is not the right
  safe mechanism either. Such calls should stay outside the optimized noalloc
  path;
- the best first implementation is an audited GC-leaf allowlist with a
  non-statepoint inline stack-switch operation;
- a cheaper-wrapper experiment is useful to split statepoint/root overhead from
  extra call/return overhead before committing to deeper LLVM surgery;
- the full `TargetOpcode::STATEPOINT` noalloc-C-call mode is now ranked as the
  fallback path, not the preferred first path.
