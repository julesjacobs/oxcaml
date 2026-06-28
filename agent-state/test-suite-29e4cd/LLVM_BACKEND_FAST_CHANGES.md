# LLVM Backend Fast Changes

This file records the main changes that made the OxCaml LLVM backend faster and
more correct. It is intentionally about the current useful design, not every
experiment that was tried.

## Active Performance-Critical Changes

### OxCaml Calling Conventions

LLVM now knows about the OxCaml calling conventions instead of treating all
calls as generic C-like calls. The important distinction is that OxCaml has
fixed runtime-state registers such as the domain state and trap pointer, and no
normal callee-save convention for OCaml values.

The LLVM lowering now models those fixed registers as call inputs and results
where appropriate. This lets register allocation use those physical registers
for ordinary values between calls instead of globally reserving them or forcing
them through memory.

Relevant call families include:

- normal OxCaml calls;
- OxCaml allocation calls;
- OxCaml C wrapper calls;
- OxCaml direct C calls;
- C calls with stack arguments;
- GC/runtime calls.

Useful commits include `5ac8b7ddf3` and `769051107e`.

### Trap Intrinsics And Native-Shaped Recovery

We added LLVM/AArch64 surface area for OxCaml trap operations:

- pushtrap;
- poptrap;
- trap recovery;
- runtime-entered recovery blocks.

The goal was to model OxCaml's pushtrap/poptrap runtime control transfer
directly, rather than forcing it into ordinary zero-cost C++ EH structure. This
matters because OxCaml handlers are reached through the runtime trap stack, and
multiple protected calls may share the same recovery target.

Useful commits include `8d453458f1`, `37f32ea920`, `9b58121bc8`,
`0db98a2190`, and `769051107e`.

### Active Trap Stack Adjustment

Pushtrap temporarily changes the physical stack layout. LLVM frame lowering now
tracks active trap bytes through the machine CFG and adjusts OxCaml stack
accesses that are relative to the stack pointer.

This is both a correctness and performance issue: without it, either stack
accesses can be wrong on paths with active traps, or lowering has to avoid
native-shaped trap code.

Useful commit: `769051107e`.

### RS4GC Support For OxCaml Exception Handlers

`RewriteStatepointsForGC` was extended for OxCaml's exception model. The
standard statepoint model did not directly match OxCaml recovery blocks, shared
handlers, and values live into handlers.

The current active direction keeps the normal RS4GC/gc.relocate path, with
OxCaml-specific handling for:

- recovery landingpad/trampoline boundaries;
- handler-live roots;
- duplicated edges into a shared recovery block;
- roots that must remain visible at recovery boundaries;
- coalescing compatible exception root slots.

Useful commits include `6df0e4136b`, `1bd5362369`, and `57e9764b3c`.

### Derived Pointer Rematerialization

OxCaml's GC does not support arbitrary derived pointers as roots. RS4GC was
therefore taught to rematerialize selected derived pointer values as:

- a base GC pointer, which is a real root;
- a non-GC offset or address computation, which can be recomputed after the
  statepoint.

This avoids putting hidden interior pointers in frame tables. It also keeps
value-arithmetic roots self-based when the derived/base distinction is not
semantically real.

Useful commits include `38ee7a44bc`, `34670f68d`, `0e5ff10d`,
`7d9745170b`, `564ead43d3`, and `5e0e515eca`.

### Statepoint Root Location Improvements

Statepoint lowering was changed so roots can use better locations instead of
always creating poor forced spill patterns.

Important pieces:

- allocation statepoint roots can stay in registers when the runtime preserves
  the relevant registers;
- unscannable physical register assignments are forced to spill before frame
  table emission;
- stable root homes are reused for statepoint PHIs, so equivalent incoming
  roots do not create unnecessary independent slots;
- compatible exception roots are coalesced.

Useful commits include `05f2a82f51`, `ce97b150b9`, `b5972718c6`,
`20f5368ce1`, and `57e9764b3c`.

### Regalloc Call-Site Splitting

LLVM's existing greedy register allocator produced bad code for OxCaml's
no-callee-save call model. Values live across calls were often kept on the
stack too broadly.

We enabled local splitting of call-clobbered remainders at call sites. This
lets the allocator keep values in registers in regions where that is profitable
and only split around the actual call pressure.

This started as a flag/experiment and is now intended to be default behavior for
OxCaml.

Useful commits include `29e4cd4210`, `2b789140bd`, and `5ca28d4f8e`.

### Suppress Bad AArch64 Stack Pairing

AArch64 `stp`/`ldp` formation near call boundaries caused large performance
regressions in the OxCaml LLVM backend. Pairing stores around calls interacted
badly with stack slots, roots, and scheduling.

We changed the AArch64 load/store optimizer to suppress those stack pairs in the
problematic OxCaml call-boundary shapes. This is more precise than disabling all
pairing globally, but the practical effect for those hot paths is that LLVM
avoids the bad `stp` shapes.

Useful commit: `1cf9c81aac`.

Empirical check, 2026-06-08:

We compared the default compiler, where OxCaml SP-based stack-pair suppression
is enabled, against the same compiler with:

```text
-mllvm -oxcaml-suppress-statepoint-stack-pairs=false
```

The representative microbenchmark suite strongly favored keeping suppression
enabled:

```text
44-case representative micro suite:
  default LLVM total median sum: 12.5929s
  suppression-off total median sum: 13.2200s
  suppression-off / default: 1.0498
  suppression-off / default geomean: 1.0882
```

The focused rerun on the most affected cases used 9 samples and showed the
same direction:

```text
variant_dispatch_with_int_payload       off/default 3.684x
try_raise_inline_caught                 off/default 2.494x
closure_call_in_nested_try_hit          off/default 1.364x
direct_call_in_try_hit                  off/default 1.355x
closure_call_in_try_hit                 off/default 1.061x
higher_order_fold_string_keys           off/default 1.007x
```

The larger minibench suite was roughly neutral to slightly positive:

```text
10-case minibench suite:
  default LLVM total median sum: 2.2296s
  suppression-off total median sum: 2.2406s
  suppression-off / default: 1.0050
  suppression-off / default geomean: 1.0071
```

We also checked the generated assembly for `direct_call_in_try_hit`. With the
default suppressor, only the runtime helper save/restore pair remained:

```text
default:
  stp=1 ldp=1
```

With suppression disabled, LLVM formed extra folded stack spill/reload pairs:

```text
suppression off:
  stp=3 ldp=3
  stp x11, x10, [sp, #8]    ; 16-byte Folded Spill
  stp x9,  x8,  [sp, #24]   ; 16-byte Folded Spill
  ldp x8,  x9,  [sp, #32]   ; 16-byte Folded Reload
  ldp x11, x10, [sp, #8]    ; 16-byte Folded Reload
```

Manual ablations on the original `variant_dispatch_with_int_payload` slowdown
showed that the paired store itself was the main problem, not only the later
reloads:

```text
original unaligned stp at sp+40:       ~1.07s
split that stp into scalar str/str:    ~0.076s
keep stp but bypass both reloads:      ~0.62s
move stp to 16-byte-aligned offset:    ~0.29s
aligned stp + bypass reloads:          ~0.078s
```

This points to two separate costs: the original pair is at an address that is
8 mod 16, and the later reloads do not reload the same pair. Scalar stores avoid
both issues in this shape.

Conclusion: this should stay default-on. It prevents severe regressions in
small exception and dispatch shapes, and there was no compensating win in the
larger minibenches.

### Avoid Bad i64 Load Narrowing

The OxCaml LLVM driver passes `-oxcaml-avoid-i64-load-narrowing`. This prevents
LLVM from narrowing some 64-bit loads into smaller loads in ways that are bad
for OxCaml tagged-value code shape.

This is currently wired through `backend/llvm/llvmize.ml`.

### Stack Check And Runtime Entry Shape

LLVM stack checks and runtime-entry frames needed to match OxCaml runtime
expectations without creating bloated prologues or broken async-stack-overflow
frames.

Changes here include:

- stack check slack/size contract work;
- improved stack-check preamble shape;
- fixed async stack overflow frame shape;
- runtime-entry pass integration;
- avoiding stale frame-pointer assumptions in LLVM runtime frames.

Useful commits include `31f1af6e7f`, `1ec71b0c9a`, `09b57caedc`,
`fa9baa4744`, and `e5e1c6c2fe`.

### C-Call Root Filtering

RS4GC originally treated some OxCaml C-call arguments as roots when that was not
the right model. This created extra root traffic and could keep values live for
the wrong reason.

The current model distinguishes roots needed by the caller from arguments owned
by the C/runtime call protocol.

Useful commits include `5ead8f582e` and `50da5aff3f`.

### Provenance Preservation From OxCaml Values

Lowering now preserves more information about which LLVM values came from
OxCaml `Val` values. This matters for root classification and for avoiding
cases where values lose their GC/non-GC distinction before statepoint lowering.

Useful commit: `2889fd4c95`.

### Sanitizing Undefined GC Pointers

Some LLVM IR shapes can contain undef/poison GC pointer values. Those are not
valid frame-table roots. RS4GC sanitizes OxCaml undefined GC pointers to a
non-moving immediate value before root processing.

This prevents undefined roots from escaping into statepoint metadata or machine
frame tables.

Useful commit: `9f83e8d96b`.

### Avoid Comballoc In The LLVM Backend

Comballoc was disabled for the LLVM backend because it produced root and code
generation shapes that the LLVM backend did not handle well enough.

Useful commit: `bdf3884def`.

## Experiments That Were Useful But Are Not The Current Default

### Global Volatile Root Allocas

We tried an RS4GC mode that materialized live GC pointers through volatile root
allocas instead of relying on gc.relocate/statepoint root lowering. This made
some exception-heavy examples better, but the global mode regressed total
microbench time and had unsafe exception-root merging variants.

The active branch reverted the unsafe merge and returned to the normal
RS4GC/gc.relocate path.

Useful commits for context: `527ca9bcbd`, `685d252ac0`, and `9f38c181d9`.

### Late Root Discovery / No-Relocate Direction

We explored tracking addrspace(1) provenance through instruction selection and
register allocation, then discovering actual root locations late. This remains
a promising conceptual direction, but it is not the current validated default.

The current validated path is still RS4GC/gc.relocate based.

Useful context commits include `ce6f94167a`, `2491e45cfd`, `846abfdfac`,
`70b09eca6d`, `d44328fe7f`, and `35b9d0adb6`.

## Other Useful Infrastructure

### Focused LLVM Tests

We added many focused LLVM IR/MIR tests for:

- trap recovery;
- statepoint roots;
- exception roots;
- derived pointer rematerialization;
- unscannable register roots;
- stable PHI root homes;
- C-call argument/root behavior.

These tests let us red-green individual lowering issues before rerunning full
self-stage2.

### Benchmark Harnesses

We built local microbench, minibench, compiler-benchmark, and Godbolt-style
inspection tools. These were important because several changes looked good on a
single microbenchmark but regressed totals.

The current useful headline from the progress note after rollback:

- micro total: LLVM/native `0.971072`, about `2.98%` faster;
- minibench total: LLVM/native `0.927656`, about `7.80%` faster;
- compiler module medians: LLVM/native `0.965285`, about `3.60%` faster.

### Build Hygiene

We fixed several scripts so benchmark and self-stage runs use the intended
compiler and custom LLVM:

- local `_install` prefix instead of `/usr/local`;
- clean native install separate from LLVM self-stage install;
- explicit `LLVM_EXTRA_FLAGS` support where needed;
- wrapper logging for fresh LLVM IR compilations;
- checks to avoid accidentally timing identical compilers.

This did not directly make generated code faster, but it stopped misleading
numbers from stale or mixed native/LLVM builds.

## What Else To Remember

- `addrspace(1)` tracks GC pointer-ness, not object alignment.
- The fastest string-hash-like OCaml source used `int32#`; this let LLVM
  combine byte loads into a 32-bit load in the hot loop.
- Native still often emits four byte loads for that shape.
- A raw string/bytes `get_uint32_ne` primitive would be a cleaner way to expose
  that load than relying on LLVM to rediscover it from byte operations.
- Handler-live roots remain one of the important hard cases: performance wants
  coalesced, stable root homes, while correctness requires frame tables to name
  exactly the roots the runtime can scan.
- The volatile-root-allocation idea should not be resurrected wholesale without
  revalidating self-stage2 and total benchmarks.
