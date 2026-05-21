# LLVM Backend Progress

Last updated: 2026-05-21.

Goal: make ordinary arm64 programs pass with the normal-built compiler plus
`-llvm-backend`, then make an LLVM-built arm64 compiler self-host repeatedly.

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

## Current Blocker

The former `make_opcodes.exe` lexer crash is fixed by the frame-pointer change
below. The next blocker is `tools/simdgen/amd64_simd_instrs.ml`: the
LLVM-built `simdgen.exe` still fails while parsing `amd64/amd64.csv`.

A temporary instrumented copy in `/tmp/oxcaml-simdgen-repro` shows a smaller
failure shape: while parsing the CSV, `first_word` calls
`String.split_on_char ' ' str` with the separator register correct but the
string register holding a bad non-value. A tiny standalone `first_word` program
compiled with the same LLVM backend works, so the bug needs the larger recursive
`simdgen` parse path.

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

This is steady progress. The next hard problem is the `simdgen` recursive parse
miscompile; it should be reduced by experiment, not patched around.

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

1. Reduce the `simdgen` failure from `/tmp/oxcaml-simdgen-repro`: bad string
   argument to `String.split_on_char` in the recursive parse path.
2. After fixing that, build `tools/simdgen/amd64_simd_instrs.ml` in
   `_llvm_stage2_fpfix_build`.
3. Then resume broader stage-2 compiler test-suite targets with the LLVM flag.
