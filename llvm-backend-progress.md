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

- Clean normal runtime stdlib: `_normal_runtime_stdlib_install`.
- Clean normal stage-1 compiler:
  `_normal_stage1_clean_build/install/main/bin/ocamlopt.opt`.
- Stage-2 workspace: `/tmp/oxcaml-llvm-stage2-from-clean.ws`.
- Small stage-2 target passed:
  `_llvm_stage2_from_clean_build/main/.ocamlcommon.objs/native/parser.cmx`.
  The wrapper log had 54 LLVM invocations and included `parser.ll`.
- Larger stage-2 build genuinely used LLVM: the wrapper log had 366 entries and
  compiled many `.ocamlcommon` modules before failing in generated tools.

## Current Blocker

The larger normal-seeded LLVM stage-2 build fails when running generated tools:

- `tools/make_opcodes.exe -opcodes < runtime/caml/instruct.h` segfaults.
- `tools/simdgen/simdgen.exe amd64` fails with `Failure(_)`.

`make_opcodes.exe` crashes in `caml_new_lex_engine` with bad arguments. The
crashing call site should pass:

- `x0`: lexer table, which is correct.
- `x1`: lexer state, but it is a stack-like address.
- `x2`: lexbuf, but it is a code pointer.

A tiny ocamllex program compiled with the clean normal compiler plus
`-llvm-backend` does use local LLVM and prints the expected result, so lexers and
`caml_c_call` are not universally broken.

There is now a small reproducer in `/tmp/oxcaml-llvm-refill-repro`:

- `lexer.mll` has a simple digit token.
- `main.ml` builds a lexbuf with `Lexing.from_function` that returns one byte
  per refill.
- Normal backend output: `12345`.
- LLVM backend behavior: crash in `caml_lex_engine`.
- The wrapper log confirms real local LLVM use with `-x ir` and the fixed
  arm64 runtime registers.
- An lldb breakpoint at `caml_lex_engine` entry shows the first engine call has
  correct arguments. The second engine call, after the generated lexer calls
  `Lexing.lex_refill`, already has a stack-like value for `start_state` and the
  `Lexing.lex_refill` code pointer where the lexbuf should be.

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
`-fno-omit-frame-pointer` unless `Config.with_frame_pointers` is true.

Validation blocker: rebuilding a fresh normal compiler to test this source
change currently hits a separate bootstrap/generated-tool crash:
`tools/simdgen/simdgen.exe amd64` segfaults in `Stdlib.Fun.protect`. This also
appears in a fresh `_normal_stage1_fpfix_build`, so the next step is to decide
whether this is caused by the dirty runtime/prologue work or by a contaminated
bootstrap setup.

This is steady progress on the refill failure, but the remaining work should be
driven by focused experiments. Avoid piling on fixes until the bootstrap crash
and the frame-pointer ABI contract are separated cleanly.

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

1. Separate the fresh normal compiler rebuild from dirty runtime/prologue
   changes, because the current rebuild crashes running `simdgen.exe`.
2. Once a fresh normal compiler can be built, rerun the refill repro and confirm
   the generated `lexer.ll` has no `"frame-pointer"="all"` attribute in no-FP
   mode and still invokes local LLVM with `-x ir`.
3. If that passes, rerun the stage-2 generated-tool target and then decide
   whether `CSR_AArch64_OxCaml_WithoutFP` should also stop listing `FP`.
