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
- The arm64 llvmize alias now includes 20 output checks. The generic output
  checks compile every OCaml module and executable startup with
  `-llvm-backend`; IR checks stay amd64-only because their expected output is
  target-specific.
- The normal-built compiler run of that alias produced 116 fresh wrapper calls.
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
  the expanded 20 output checks and produced 116 fresh wrapper calls.
- `float_ops` uses `float_ops_arm64.output` for the arm64 NaN spelling/sign;
  the normal arm64 backend prints the same output.
- A direct `testsuite/tests/basic-more` slice passes with the LLVM-built
  compiler and `-llvm-backend`: 21 runtime/reference tests covering bounds
  checks, division exceptions, evaluation order, pattern matching, structural
  constants, buffers, and formatting. The main slice produced 96 fresh wrapper
  calls; the larger pattern-matching slice produced 20; `safer_matching` with
  its required `-safer-matching` flag produced 4.
- Direct `testsuite/tests/misc` reference tests also pass with the LLVM-built
  compiler and `-llvm-backend`: `fib`, `taku`, `takc`, `bdd`, `boyer`,
  `nucleic`, `hamming`, `gcwords`, `gctweaks`, `gc_mark_stack_overflow`,
  `darkening_work`, `gpr1370`, `pr7168`, `sorts`, and `sieve`. The first batch
  produced 40 fresh wrapper calls; the second produced 20.
- Direct single-domain `testsuite/tests/weak-ephe-final` tests pass with the
  LLVM-built compiler and `-llvm-backend`: `weaktest`, `weaklifetime`,
  `weaklifetime2`, `finaliser`, `ephetest`, `ephetest2`, `ephetest3`,
  `ephe_infix`, and `pr12001`. This produced 44 fresh wrapper calls before the
  two multidomain tests were reached.
- The current direct test setup is single-domain: `OCAMLRUNPARAM=d=4` reports
  that `max_domains` cannot exceed 1. Multidomain weak/finaliser tests need a
  proper multidomain runtime/test harness before they give useful LLVM signal.
- Direct `testsuite/tests/basic` reference tests pass with the LLVM-built
  compiler and `-llvm-backend`: 36 single-module executables produced 144 fresh
  wrapper calls. Correctly harnessed adjacent cases also pass:
  `basic-modules/main`, `basic-modules/recursive_module_init`,
  four `basic-float` reference tests, `basic-io/wc`, and `basic-io-2/io`.
  Those corrected adjacent runs produced 20 fresh wrapper calls for the
  module/I/O cases.
- Direct pure-OCaml callback/GC probes pass with the LLVM-built compiler and
  `-llvm-backend`: `callback/test_finaliser_gc` and
  `callback/callback_effects_gc`. These produced 8 fresh wrapper calls.
- Direct C-stub callback probes pass with the LLVM-built compiler and
  `-llvm-backend`: `callback/test1`, `test2`, `test4`, `test5`, `test6`, and
  `test_local_bug`. These cover OCaml-to-C-to-OCaml callbacks, callback
  arguments, exception unwinding through C, and the local-argument repro; they
  produced 24 fresh wrapper calls.
- Direct non-C-stub `testsuite/tests/asmcomp` probes pass with the LLVM-built
  compiler and `-llvm-backend`: `bind_tuples`, `compare`, `evaluation_order`,
  `lift_mutable_let_flambda`, `obj_dup_primitive`, `prevent_fma`,
  `register_typing`, `register_typing_switch`, `regression_value_kinds`,
  `select_addr`, `try_checkbound`, `unrolling_flambda`, and
  `unrolling_flambda2`. These produced 52 fresh wrapper calls.
- `asmcomp/optargs` is not useful as a direct probe yet: it fails its
  no-allocation assertion even with the normal-built compiler and the normal
  backend, so it needs the real testsuite invocation or exact optimization
  setup before it can classify an LLVM issue.

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

1. Convert the direct `basic-more`/`misc` probes into repeatable checks, or move
   to the next small runtime-heavy testsuite slice if that gives better signal.
2. If an LLVM-built compiler test fails, reduce from that test-suite case rather
   than returning directly to broad self-hosting.
3. Keep checking wrapper logs on forced rebuilds so cached Dune successes are
   not mistaken for fresh LLVM executions.
