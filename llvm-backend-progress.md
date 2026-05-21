# LLVM Backend Progress

Last updated: 2026-05-21.

Goal: make ordinary arm64 programs pass with the normal-built compiler plus
`-llvm-backend`, then make an LLVM-built arm64 compiler self-host repeatedly.
Near-term strategy: get useful test-suite slices passing with `-llvm-backend`
using a normal-built compiler, then run those slices with an LLVM-built
compiler, and only then target full self-hosting.

## Current State

The normal-built driver plus `-llvm-backend` can compile and run many ordinary
arm64 programs: hello world, allocation in `try`, external calls in `try`, stack
growth, caught stack overflow, small multi-module programs, and small
effect/continuation tests.

For every LLVM-backend test, verify actual local LLVM use by checking the
wrapper log for commands like:

```sh
.toolchains/llvm-build-host-aarch64/bin/clang-16 ... -x ir \
  -ffixed-x15 -ffixed-x26 -ffixed-x27 -ffixed-x28
```

The LLVM-built compiler is not self-host useful yet, but it now builds and runs
small programs with both the normal backend and `-llvm-backend`. Always seed
stage-2 tests from the clean normal-built compiler; rebuilding with an
already-LLVM-built compiler can preserve old broken generated code inside the
next compiler.

## Known Good Setup

- Normal runtime stdlib rebuilt without LLVM:
  `_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib`.
- Fresh normal stage-1 compiler:
  `_normal_stage1_fpfix_build/install/main/bin/ocamlopt.opt`.
- Stage-2 workspace for current checks: `/tmp/oxcaml-llvm-stage2-fpfix.ws`.
- LLVM-built compiler:
  `_llvm_stage2_fpfix_build/main/oxcaml_main_native.exe`. Its build used real
  local LLVM: the wrapper log had 1596 clang calls including `-x ir`.
- LLVM stage-2 tool smoke test:
  `_llvm_stage2_fpfix_build/main/tools/make_opcodes.exe` builds with real local
  LLVM and now runs `-opcodes < runtime/caml/instruct.h` successfully.
- LLVM-built `tools/simdgen/simdgen.exe` now generates
  `tools/simdgen/amd64_simd_instrs.ml` successfully.

## Current Test Frontier

The former `make_opcodes.exe` lexer crash is fixed by the frame-pointer change
below. The former `tools/simdgen/amd64_simd_instrs.ml` blocker is also fixed:
an LLVM-built `simdgen.exe` now generates the file successfully.

The useful reduced tests are in `/tmp/oxcaml-six-args-repro`:

- `filter_try.ml`: local `try` around the parser, no exception raised.
- `filter_try_raise.ml`: local `try`, an actual `Unsupported` raise is caught.
- `filter_no_try.ml`: same parser shape without the local `try`.

All three now pass with the normal-built compiler plus `-llvm-backend`. The
wrapper log confirms real local LLVM use with `-x ir`, fixed arm64 runtime
registers, and `-fomit-frame-pointer`.

Root cause: arm64 `Pushtrap` used a dynamic alloca for the trap block. LLVM then
forced `x29` as a frame pointer in the enclosing function, but normal no-FP
OCaml callees may clobber `x29`. In the reduced parser, after
`String.split_on_char`, a reload from `[x29, ...]` read the wrong list tail and
passed `encs = []` to `parse_args`. The fix direction is to preallocate aligned
arm64 trap blocks in the function entry frame and restore `sp` explicitly in the
exception-entry shim.

The refill reproducer in `/tmp/oxcaml-llvm-refill-repro` is now a passing
regression test candidate:

- `lexer.mll` has a simple digit token.
- `main.ml` builds a lexbuf with `Lexing.from_function` that returns one byte
  per refill.
- Normal backend output: `12345`.
- LLVM backend output after the fix: `12345`.
- The wrapper log confirms real local LLVM use with `-x ir`, fixed arm64
  runtime registers, and `-fomit-frame-pointer`.

The refill crash is now explained more directly: LLVM-generated AArch64 code
was forcing `"frame-pointer"="all"` even in a no-frame-pointer compiler, so it
used `x29` for local reloads across ordinary OCaml calls. Normal no-FP OxCaml
callees may use `x29` as scratch. In the repro, `Lexing.lex_refill` returns with
`x29` pointing at its own frame, and the LLVM caller reloads the lexbuf and
start state from the wrong frame.

Manual IR validation: removing only the generated `"frame-pointer"="all"`
attribute from `lexer.ll`, while keeping `"oxcaml-stack-check"="true"` and even
keeping clang's `-fno-omit-frame-pointer`, makes the refill repro print `12345`.
The assembly then reloads locals from `sp` offsets instead of `x29` offsets.

Source change in progress: `backend/llvm/llvmize.ml` now emits
`Frame_pointer_all` on AArch64 only when `Config.with_frame_pointers` is true,
while still emitting `Oxcaml_stack_check`. It also stops passing
`-fno-omit-frame-pointer` unless `Config.with_frame_pointers` is true. This is
validated by the refill repro and by the now-passing LLVM-built
`make_opcodes.exe`.

Important setup lesson: `duneconf/runtime_stdlib.ws` must also have empty
`OCAMLPARAM` for normal stage-1 builds. A stale LLVM-built runtime stdlib made
normal generated tools look broken and produced confusing `simdgen` crashes.

This is steady progress, but the remaining work is still runtime/exception/GC
contract work. Keep using reduced experiments and targeted test-suite slices,
not broad self-hosting builds, until the targeted suites are stable.

Targeted `@runtest-llvmize` on arm64 with the normal-built compiler plus
`-llvm-backend` now passes. Individual forced rebuilds of the arm64 tests used
real local LLVM; the wrapper log showed `-x ir`, fixed arm64 runtime registers,
and `-fomit-frame-pointer`.

Passing arm64 LLVM-backend tests now include:

- `arm64_many_args`: prints `66`.
- `arm64_exception_root_refresh`: prints `1`.
- `arm64_specific_ops`: matches expected arithmetic/float output.
- `arm64_c_call_gc`: prints `160` after fixing the C-stub include path in the
  test harness.
- `arm64_input_channel_loop`: prints `5200`.
- `arm64_stack_overflow_trap`: prints the expected stack-overflow/backtrace
  result.

Two important AArch64 trap fixes made this pass:

- Static entry-frame trap blocks must not also contribute their old dynamic
  trap-block stack offset to statepoint frame-size metadata. The bad descriptor
  in `arm64_input_channel_loop` was 320 bytes and made the stack scanner read a
  heap pointer as a return address; after subtracting active static trap-block
  bytes it is 288 bytes and the test passes.
- The trap recovery shim now stores a trap-block-relative restore-SP delta, not
  an absolute restore-SP. Stack growth rewrites the trap-block chain but cannot
  rewrite extra absolute slots; the relative delta survives stack copying and
  fixes `arm64_stack_overflow_trap`.

A broad stage-2 compiler build is not a good next iteration target. A previous
build made real LLVM progress (274 clang wrapper calls with `-x ir`) and then
appeared to stall on `parser.pp.ml`, but the stall was caused by temporary
runtime diagnostics left in hot GC paths: every `caml_darken` scanned major
heap pools. After removing those diagnostics, a fresh normal compiler target
built in about two minutes and the same parser typing probe completed in 18s.
Use build-dir-qualified Dune targets for manual compiler probes, e.g.
`_some_build/main/oxcaml_main_native.exe`; plain `./oxcaml_main_native.exe`
does not force the intended context target.

This was steady progress, not a hard LLVM design problem. The hard design work
is still the runtime/exception/GC contract; keep using reduced experiments and
targeted test-suite slices rather than hill-climbing on full self-hosting.

Stage-2 LLVM-built compiler checks now also pass on selected llvmize tests. Each
manual compile used `OCAMLPARAM=_,llvm-backend=1,llvm-path=/tmp/oxcaml-clang-wrapper`
and the wrapper log showed fresh `-x ir` clang calls:

- hello world compiled and ran with both the normal backend and `-llvm-backend`.
- `arm64_many_args`: prints `66`.
- `arm64_exception_root_refresh`: prints `1`.
- `arm64_input_channel_loop`: prints `5200`.
- `arm64_stack_overflow_trap`: prints the expected stack-overflow/backtrace
  result.

## Prior Fix Direction

Earlier failures were hidden exceptional edges and stale trap-state restoration
after arm64 stack reallocation. Protected OCaml calls, protected external calls,
`raise_notrace`, allocation/poll safepoints, and stack checks now expose unwind
successors when a trap is active. AArch64 `Poptrap` now reloads the live trap
block from `x26` instead of restoring stale absolute stack addresses from LLVM
allocas.

Local LLVM emits an inline AArch64 fast-path stack check before frame setup for
functions marked `"oxcaml-stack-check"="true"`. The slow helper calls
`caml_try_realloc_stack` and returns to the inline prologue. This remains a
prototype: Darwin-specific inline asm, hardcoded current-stack offset and guard
threshold, and a diagnostic `CHECK_SP_IN_STACK` runtime guard.

## Next Checks

1. Add a small Dune-driven stage-2 llvmize slice so these manual checks become
   repeatable without depending on cached outputs.
2. Extend the LLVM-built compiler checks to more `llvmize` cases, especially
   external-call and multi-module cases.
3. If LLVM-built compiler tests fail, reduce from the failing test-suite case
   rather than returning to broad self-hosting.
4. Keep checking wrapper logs on forced rebuilds so cached dune successes are
   not mistaken for fresh LLVM executions.
