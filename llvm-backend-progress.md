# LLVM Backend Progress

Last updated: 2026-05-21.

Goal: make ordinary arm64 programs pass with the normal-built compiler plus
`-llvm-backend`, then make an LLVM-built arm64 compiler self-host repeatedly.
Near-term strategy: get useful test-suite slices passing with `-llvm-backend`
using a normal-built compiler, then run those slices with an LLVM-built
compiler, and only then target full self-hosting.

## Current State

The normal-built driver plus `-llvm-backend` passes targeted arm64 llvmize
tests and many small manual programs: allocation in `try`, external calls in
`try`, stack growth, caught stack overflow, small multi-module programs, and
small effect/continuation tests.

The LLVM-built compiler is not self-host useful yet, but it now builds and runs
small programs with both the normal backend and `-llvm-backend`. Do not rebuild
stage 2 from an already LLVM-built compiler when trying to isolate bugs; seed it
from the clean normal-built compiler to avoid preserving old broken generated
code.

Always verify real LLVM use by checking the wrapper log for `-x ir` and the
fixed arm64 runtime registers:

```sh
/tmp/oxcaml-clang-wrapper ... -x ir ... \
  -ffixed-x15 -ffixed-x26 -ffixed-x27 -ffixed-x28
```

Progress is steady. The remaining work is still runtime/exception/GC contract
work, so reduced experiments and targeted test-suite slices are more useful
than hill-climbing on full self-hosting.

## Known Good Setup

- Normal runtime stdlib: `_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib`.
- Normal stage-1 compiler:
  `_normal_stage1_fpfix_build/install/main/bin/ocamlopt.opt`.
- Stage-2 workspace: `/tmp/oxcaml-llvm-stage2-fpfix.ws`.
- LLVM-built compiler:
  `_llvm_stage2_fpfix_build/main/oxcaml_main_native.exe`.
- Its build used real local LLVM; the wrapper log had 1596 clang calls including
  `-x ir`.

Important setup lesson: `duneconf/runtime_stdlib.ws` must also have empty
`OCAMLPARAM` for normal stage-1 builds. A stale LLVM-built runtime stdlib made
normal generated tools look broken and produced confusing `simdgen` crashes.

## Verified Tests

Normal-built compiler plus `-llvm-backend`:

- `@runtest-llvmize` on arm64 passes.
- Passing arm64 llvmize tests include `arm64_many_args`,
  `arm64_exception_root_refresh`, `arm64_specific_ops`, `arm64_c_call_gc`,
  `arm64_input_channel_loop`, and `arm64_stack_overflow_trap`.
- Reduced repros in `/tmp/oxcaml-six-args-repro` and
  `/tmp/oxcaml-llvm-refill-repro` now pass.

LLVM-built compiler:

- Builds and runs hello world with both the normal backend and `-llvm-backend`.
- Runs `_llvm_stage2_fpfix_build/main/tools/make_opcodes.exe -opcodes <
  runtime/caml/instruct.h`.
- Runs `_llvm_stage2_fpfix_build/main/tools/simdgen/simdgen.exe` enough to
  generate `tools/simdgen/amd64_simd_instrs.ml`.
- Dune-driven `@runtest-llvmize` for arm64 passes when
  `OXCAML_LLVM_TEST_OCAMLOPT` points at the LLVM-built compiler. This covered
  the six arm64 llvmize tests and produced 28 fresh clang wrapper calls.
- Generic llvmize output-only checks passed in a temporary directory for
  `const_val`, `int_ops`, `gcd`, `array_rev`, `many_args`, `multi_ret`,
  `indirect_call`, `extcalls`, `exn`, `alloc`, `tailcall`, `switch`, and
  `csel`, with all OCaml modules compiled by the LLVM-built compiler using
  `-llvm-backend`.
- `float_ops` only differed from the checked-in amd64-oriented expected output
  in the spelling/sign of printed NaNs; the normal arm64 backend prints the same
  output as the LLVM run.

## Key Findings

- The refill/parser failures came from forcing `"frame-pointer"="all"` on
  AArch64 in a no-frame-pointer compiler. Normal no-FP OxCaml callees may use
  `x29` as scratch, so LLVM code must not reload locals from `x29` unless the
  compiler is actually configured with frame pointers.
- AArch64 `Pushtrap` originally used dynamic allocas for trap blocks. Moving to
  preallocated entry-frame trap blocks and restoring `sp` in the exception-entry
  shim avoids LLVM introducing an unsafe frame-pointer dependency.
- Static entry-frame trap blocks must not also contribute their old dynamic
  trap-block stack offset to statepoint frame-size metadata.
- Trap recovery must store a trap-block-relative restore-SP delta, not an
  absolute restore-SP, because stack growth rewrites the trap-block chain.
- Protected OCaml calls, protected external calls, `raise_notrace`,
  allocation/poll safepoints, and stack checks need explicit unwind successors
  when a trap is active.
- Local LLVM emits an inline AArch64 fast-path stack check for
  `"oxcaml-stack-check"="true"`. This is still a prototype: Darwin-specific
  inline asm, hardcoded current-stack offset and guard threshold, and a
  diagnostic `CHECK_SP_IN_STACK` runtime guard.
- A previous apparent stage-2 stall on `parser.pp.ml` was caused by temporary
  runtime diagnostics left in hot GC paths, not by LLVM. After removing those
  diagnostics, a parser typing probe completed in about 18s.

## Next Checks

1. Expand repeatable LLVM-built compiler tests beyond the arm64-only Dune alias,
   preferably as output-only checks that avoid amd64 IR expectations.
2. Add or adapt test harness support so the generic llvmize output tests can run
   on arm64 without checking amd64-specific IR.
3. If an LLVM-built compiler test fails, reduce from that test-suite case rather
   than returning directly to broad self-hosting.
4. Keep checking wrapper logs on forced rebuilds so cached Dune successes are
   not mistaken for fresh LLVM executions.
