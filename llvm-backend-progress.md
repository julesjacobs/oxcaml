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

Current evidence points at frame roots around the generated lexer's refill path:

- The `caml_c_call` trampoline in the linked binary preserves `x0`/`x1`/`x2`.
- The AArch64 prologue stack-growth helper in the linked binary saves and
  restores the argument registers.
- The generated lexer frame has the bad values in its frame slots before the
  crashing `caml_new_lex_engine` call.
- The relevant refill call has a frametable entry that scans both outgoing
  arguments and frame slots. This may still be wrong, but the simple
  frame-size conversion for the `caml_new_lex_engine` call itself looks
  consistent in the generated assembly.

This is a design/debugging problem, not a good place for hill climbing. The next
step is to use the small refill reproducer to inspect the exact stack-map roots
at the call that first corrupts the lexbuf slot.

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

1. Watch the generated lexer's lexbuf frame slot and identify the first write
   or GC/frame scan that changes it.
2. Compare the failing call's LLVM stack-map locations with the emitted
   frametable offsets and the actual AArch64 frame layout.
3. Only after the root contract is clear, patch either LLVM stack-map emission
   or `llvmize.ml` root selection and rerun the stage-2 generated-tool target.
