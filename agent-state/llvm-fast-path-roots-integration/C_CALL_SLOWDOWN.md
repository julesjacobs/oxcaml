# C-call slowdown investigation

Date: 2026-05-26.

This note investigates which C calls are slower under the LLVM backend and why.
The short version is:

- The bad case is register-only noalloc C calls.
- The slowdown is largest when the C callee is cheap.
- The slowdown is not all C calls: allocating calls and noalloc calls with
  stack arguments do not show the same problem in these reductions.

## Where the lowering differs

In `backend/llvm/llvmize.ml`, external calls are split by `stack_ofs` and
`alloc`:

- `stack_ofs > 0`: call `caml_c_call_stack_args`.
- `alloc = true`: call `caml_c_call`.
- `alloc = false` and `stack_ofs = 0`: call a generated
  `c_call_wrapper.*` function.

The wrapper exists for a real reason. The wrapper switches from the OCaml stack
to the C stack, calls the target C function, then switches back. Isolating this
in a wrapper avoids changing the stack pointer inside a normal LLVM function
and then continuing with ordinary LLVM frame accesses.

Native arm64 emits the noalloc stack switch inline at the call site:

```asm
add fp, sp, #0
ldr x16, [x28, #104]  ; Domain_c_stack
add sp, x16, #0
bl  _caml_obj_tag
add sp, fp, #0
```

LLVM instead emits a hot-path call to the generated wrapper:

```asm
mov x28, x8
mov x27, x9
bl  _c_call_wrapper.caml_obj_tag...
mov x9, x27
mov x8, x28
```

The wrapper then does the stack switch and the real C call:

```asm
sub sp, sp, #16
str x30, [sp, #8]
mov x19, sp
ldr x8, [x28, #104]  ; Domain_c_stack
mov sp, x8
bl  _caml_obj_tag
mov sp, x19
ldr x30, [sp, #8]
add sp, sp, #16
ret
```

So a direct noalloc C call becomes two calls plus wrapper prologue/epilogue.
The LLVM call site also tends to save/restore more live values around the
wrapper call because the boundary is opaque to the caller.

## Benchmark harness

Harness:

```sh
PAIRS=5 \
LLVM_PATH=/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper \
agent-state/llvm-fast-path-roots-integration/c_call_slowdown/run.sh
```

Latest log:

```text
agent-state/llvm-fast-path-roots-integration/c_call_slowdown/run_full_20260526_164641.log
```

Latest JSON:

```text
agent-state/llvm-fast-path-roots-integration/c_call_slowdown/.build/summary.json
```

## Results

```text
case                 native    llvm      ratio   LLVM wrapper refs
noalloc_add1_loop    0.1491s   0.2551s   1.710x  5
noalloc_sum10_loop   0.2710s   0.2495s   0.921x  0
obj_tag_loop         0.1673s   0.3860s   2.307x  4
string_equal_loop    0.1674s   0.2069s   1.236x  5
string_compare_loop  0.1885s   0.2330s   1.236x  5
int_hash_loop        0.3462s   0.3500s   1.011x  5
float_sin_loop       0.0607s   0.0655s   1.080x  5
int_of_string_loop   0.0852s   0.0832s   0.977x  0
```

Interpretation:

- `noalloc_add1_loop` isolates the boundary cost with a tiny local C stub.
  LLVM is 1.7x slower, and the assembly has a generated wrapper.
- `obj_tag_loop` is the strongest real-runtime example found so far. The C
  function is cheap, so the wrapper cost dominates.
- `string_equal_loop` and `string_compare_loop` match the compiler-profile
  suspicion around string-heavy lookup paths. The callee does enough work that
  the slowdown is smaller than `Obj.tag`, but it is still material.
- `int_hash_loop` still uses a wrapper, but it is near parity because
  `caml_hash_exn` does enough work to mostly amortize the boundary cost.
- `float_sin_loop` still uses a wrapper, but `sin` is expensive enough that the
  boundary cost is a small fraction of the loop.
- `int_of_string_loop` is an allocating C call. It goes through `caml_c_call`,
  not a generated wrapper, and is not slower here.
- `noalloc_sum10_loop` is a noalloc C call with stack arguments. It goes
  through `caml_c_call_stack_args`, not a generated wrapper, and is not slower
  here. This is a useful control: the problem is not noalloc-ness by itself.

## `caml_obj_tag` Inline Follow-up

`caml_obj_tag` has now been specialized in the LLVM lowering instead of going
through a generated noalloc C-call wrapper.

Focused rerun:

```text
agent-state/llvm-fast-path-roots-integration/c_call_slowdown/run_obj_tag_inline_20260526_212617.log
```

Result:

```text
obj_tag_loop: native=0.1974s llvm=0.0992s ratio=0.5026 wrappers=0
```

Interpretation:

- The previous `obj_tag_loop` result was `2.307x` slower with 4 wrapper
  references in the LLVM assembly.
- The new lowering removes those wrapper references and makes the LLVM version
  faster than native on this focused benchmark.
- This is a good proof point for tiny helper inlining, but the dynamic
  compiler-file profile only had about 2.9M `caml_obj_tag` calls in
  `typecore.ml`, so the compiler-binary impact should be much smaller than the
  microbenchmark speedup.

## LLVM IR evidence

Register-only noalloc call:

```llvm
call oxcaml_nofpcc ... @"\01_c_call_wrapper.bench_noalloc_add1..."
```

The wrapper is defined in the same IR module:

```llvm
define private oxcaml_nofpcc ... @"\01_c_call_wrapper.bench_noalloc_add1..." noinline {
  ...
  call ... @"\01_bench_noalloc_add1"(...)
  ...
}
```

Noalloc call with stack arguments:

```llvm
call oxcaml_c_stackcc ... @"\01_caml_c_call_stack_args"(...)
```

Allocating call:

```llvm
call oxcaml_ccc ... @"\01_caml_c_call"(...)
```

This confirms that the bad cases share one lowering shape: generated noalloc
C-call wrappers.

## Why native does better

The native backend owns its frame layout and stack-pointer convention directly.
It can emit the C-stack switch inline and then continue because its own emitter
knows exactly what it did.

LLVM generally expects the stack pointer inside a function to obey LLVM's frame
model. If we set `sp` to `Domain_c_stack` in the middle of a normal LLVM
function and then keep using LLVM-generated frame accesses, we are fighting
LLVM's assumptions. The current wrapper isolates the stack-pointer violation in
a tiny function. That is correct and simple, but it is expensive for cheap hot
C calls.

## Proper fix direction

The best fix is probably not to special-case individual runtime calls. The
proper contract is a lower-overhead noalloc C-call boundary for LLVM.

Potential designs:

1. Add a backend-supported inline noalloc C-call pseudo operation in LLVM.
   It would lower late, after normal frame layout decisions, to the native-like
   stack switch sequence. This preserves LLVM's stack model during optimization
   and emits the unsafe stack-pointer manipulation only after LLVM is done
   reasoning about frame accesses.

2. Teach the LLVM backend about an `oxcaml_c_noalloccc` call convention or
   intrinsic that represents "call this C function on the C stack, no GC, no
   allocation". The AArch64 backend would lower it directly without an
   out-of-line wrapper.

3. Keep wrappers as a fallback, but whitelist very common cheap runtime calls
   with hand-written inline lowering. This is the fastest incremental route but
   creates a growing list of special cases.

The experiments say the first high-value microbenchmark targets are cheap
register-only noalloc calls such as `caml_obj_tag`, `caml_string_equal`, and
`caml_string_compare`. Expensive noalloc calls and allocating calls are much
lower priority in this harness.

Important limitation: this does not prove that those exact symbols explain the
compiler-binary slowdown. The compiler-binary benchmarks still need dynamic
counts or profiles for generated C-call wrappers, `_wrap_try`/`recover_rbp`, and
ordinary OCaml call runtime-register traffic. The right implementation strategy
should use the audited hot stubs as a diagnostic first step, then generalize to
a mechanical safe-call-boundary contract if the expensive call problem is broad.
