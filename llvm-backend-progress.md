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

The LLVM-built compiler is not self-host useful yet. Always seed stage-2 tests
from the clean normal-built compiler; rebuilding with an already-LLVM-built
compiler can preserve old broken generated code inside the next compiler.

## Known Good Setup

- Normal runtime stdlib rebuilt without LLVM:
  `_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib`.
- Fresh normal stage-1 compiler:
  `_normal_stage1_fpfix_build/install/main/bin/ocamlopt.opt`.
- Stage-2 workspace for current checks: `/tmp/oxcaml-llvm-stage2-fpfix.ws`.
- LLVM stage-2 tool smoke test:
  `_llvm_stage2_fpfix_build/main/tools/make_opcodes.exe` builds with real local
  LLVM and now runs `-opcodes < runtime/caml/instruct.h` successfully.
- LLVM-built `tools/simdgen/simdgen.exe` now generates
  `tools/simdgen/amd64_simd_instrs.ml` successfully.

## Current Blocker

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

This is steady progress. The current hard problems are still design-level
runtime/exception/GC integration issues; they should keep being handled by
reduced experiments rather than by hill-climbing around generated tools.

A broad stage-2 `dune build --only-package=ocaml @install` is not a good next
iteration target. It was stopped after ~20 minutes: dune had four child
compilers burning CPU on `parser.ml` / `parser.pp.ml`, with no recent output
files for those modules and no clang wrapper activity for them. A `sample` of
one process showed time in typing / warning-scope recursion plus debug heap
checks, so this is too coarse and should be replaced by targeted test-suite
slices.

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

1. Choose a targeted test-suite slice and run it with the normal-built compiler
   plus `-llvm-backend`.
2. Keep checking wrapper logs so every claimed LLVM test actually uses local
   LLVM.
3. If the next failure involves exceptions or stack growth, reduce it before
   changing the runtime/LLVM contract again.
