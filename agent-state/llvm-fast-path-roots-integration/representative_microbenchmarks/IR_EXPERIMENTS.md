# IR Experiments: Tiny Non-Inlined Variant Dispatch

This note explains the 10x slowdown in
`variant_dispatch_with_int_payload` and records IR-level experiments that
separate the possible causes.

## Case

The benchmark is:

```ocaml
type t = Attr of int | Prim of int | Other of int

let[@inline never] eval = function
  | Attr x -> if x = 17 then 1 else 2
  | Prim x -> if x = 42 then 3 else 4
  | Other i -> i land 7
```

The loop calls `eval` in the inner loop. Native takes about `0.107s`; LLVM takes
about `1.100s`. If `eval` is allowed to inline, LLVM takes about `0.066s`.

## Raw And Optimized IR

The raw backend IR has the direct call as an OxCaml call returning domain state,
allocation state, and the actual result:

```llvm
%214 = call oxcaml_nofpcc { { i64, i64 }, { i64 } }
  @"\01_camlVariant_dispatch_with_int_payload__eval_4_10_code"(
    i64 %212, i64 %213, ptr addrspace(1) %211)
  "statepoint-id"="0"
  [ "deopt"(...), "gc-live"(ptr %19) ]
```

After standard LLVM optimization, most allocas are gone, but the live root
alloca remains because the call becomes a statepoint:

```llvm
%4 = alloca ptr addrspace(1), align 8
...
%statepoint_token = call oxcaml_nofpcc token (...) @llvm.experimental.gc.statepoint.p0(...)
  [ "deopt"(...), "gc-live"(ptr %4) ]
%62 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result...
```

So the root alloca is not a pre-mem2reg artifact. It survives normal cleanup.

## Experiments

All timings below use the same loop arguments as the benchmark:
`4000000 20`, median of five runs, compiled with the branch-local custom LLVM.

| Experiment | Eval call shape | Median | Result |
| --- | --- | ---: | --- |
| baseline | `oxcaml_nofpcc`, statepoint, `gc-live`, returns `{ds, alloc, result}` | 1.084s | bad |
| remove `gc-live` only | still rewritten as statepoint, no live root | 1.118s | no improvement |
| remove statepoint metadata | still becomes a statepoint with synthetic id | 1.101s | no improvement |
| nonvolatile root + no statepoint metadata | root alloca removed, call still statepoint | 1.095s | no improvement |
| mark call as `gc-leaf-function` | direct call, not rewritten as statepoint | 1.189s | worse |
| mark callee as `gc-leaf-function` | still statepoint at call | 1.095s | no improvement |
| scalar `oxcaml_nofpcc` leaf clone | returns `i64`, no statepoint | 1.215s | worse |
| scalar `ccc` leaf clone | returns `i64`, no statepoint | 0.081s | fixes issue |
| scalar `fastcc` leaf clone | returns `i64`, no statepoint | 0.079s | fixes issue |

## What This Shows

The statepoint live-root machinery is real, but it is not the main cause of the
10x in this benchmark. Removing `gc-live`, removing root volatility, and even
forcing the hot call to stay a direct non-statepoint call did not improve the
runtime.

The dominant cost is the OxCaml direct-call ABI for a tiny function:

- the call takes domain state and allocation state;
- the call returns domain state and allocation state;
- LLVM must preserve loop state around the call;
- LLVM must repair pinned OxCaml state registers around the call.

For this tiny callee, that boundary work is much larger than the useful work.

The scalar `ccc` / `fastcc` experiments eliminate the slowdown without inlining
the callee. The call remains a real call, but the ABI is just:

```llvm
call fastcc i64 @eval_fast(ptr addrspace(1) %arg)
```

The resulting inner loop keeps loop state in registers and has no OxCaml
domain/allocation state repair around the call. It runs near the fully-inlined
LLVM case.

The scalar `oxcaml_nofpcc` experiment is worse because the OxCaml convention
uses pinned/special registers for arguments/results. With a scalar return, LLVM
ends up passing the payload through `x28`, which collides with the runtime-state
role of that register and makes the loop spill even more.

## Assembly Shape

Baseline LLVM inner loop:

```asm
stp x0, x10, [sp, #40]
...
mov x28, x8
mov x27, x2
bl  _camlVariant_dispatch_with_int_payload__eval_4_10_code
ldr x10, [sp, #48]
ldr x13, [sp, #32]
mov/movk x12, ...
mov/movk x11, ...
mov x2, x27
mov x8, x28
ldr x9, [sp, #40]
...
```

Scalar `ccc` leaf-call clone:

```asm
ubfx  x8, x21, #62, #1
sbfx  x9, x21, #0, #63
smulh x9, x9, x20
add   x8, x8, x9, lsr #1
msub  x8, x8, x22, x25
ldr   x9, [sp, #56]
add   x8, x9, x8
ldur  x0, [x8, #-4]
bl    _camlVariant_dispatch_with_int_payload__eval_fast_scalar_ccc_code
add   x8, x19, x0
sub   x19, x8, #1
...
```

The scalar call still has a call instruction, but it does not force the
domain/allocation state round trip.

## Design Implication

Inlining is one solution, but it is not the only one. A separate leaf direct-call
contract would also fix this class:

- only for statically known direct calls;
- callee must not allocate;
- callee must not poll;
- callee must not call into code that can allocate or poll;
- callee must not raise, unless the ABI also carries a compatible exception
  contract;
- result should be scalar/normal values, not `{ds, alloc, result}`;
- caller does not treat the call as a GC statepoint.

This is not a general replacement for OxCaml calls. It is a specialized ABI for
tiny proven-leaf helpers. The experiment shows that such an ABI can recover the
lost performance without requiring full inlining.
