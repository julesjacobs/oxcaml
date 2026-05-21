# LLVM Backend Progress

Last updated: 2026-05-21.

Goal: make ordinary arm64 programs pass with the boot-built compiler plus
`-llvm-backend`, then make an LLVM-built arm64 compiler self-host repeatedly.

## Current State

The boot-built driver plus `-llvm-backend` has compiled and run many ordinary
arm64 programs, including allocation in `try`, external calls in `try`, stack
growth, caught stack overflow, small multi-module programs, and small
effect/continuation tests. For every LLVM-backend test, verify the wrapper log
or verbose output shows local LLVM:

```sh
.toolchains/llvm-build-host-aarch64/bin/clang-16 ... -x ir
```

The LLVM-built compiler is still not self-host useful. It compiles and runs a
tiny `hello` program through local LLVM, but richer compile jobs still fail
inside the compiler before their source files reach clang.

Focused check on 2026-05-21:

- `/tmp/hello_llvm.ml` compiles with `_build/install/main/bin/ocamlopt.opt
  -llvm-backend -llvm-path /tmp/oxcaml-clang-wrapper`, the wrapper log contains
  local `clang-16 -x ir` plus `-ffixed-x15`, and the executable prints `hello`.
- `/tmp/llvm_exn.ml` and `/tmp/llvm_stack.ml` still fail with exit 133 while
  the LLVM-built compiler is typechecking them. The wrapper log has no entries
  for those files, so the failure is in the compiler process, not in clang or
  the generated test-program IR.

## Latest Finding

The current blocker is stack-reallocation/trap-state consistency on arm64. This
needs deliberate design experiments; more local crash-site patching is likely to
hide the real contract problem.

Verified with lldb:

- The immediate trap is the temporary `CHECK_SP_IN_STACK` guard in
  `caml_c_call`.
- At the trap, `Caml_state->current_stack` points to a newer stack, but `sp`
  and `x26`/`TRAP_PTR` still point into the previous stack.
- The second stack realloc observed by lldb rewrites from an old stack to a new
  stack, but later execution has returned to the old stack around
  `Misc.try_finally`/`Profile.create`.
- A tentative helper change that restored `sp` from
  `Caml_state->current_stack->sp` after `caml_try_realloc_stack` did not change
  the focused failure.

This is not yet evidence that LLVM is missing an exceptional edge for the small
program being compiled. It may still be related to exceptional control in the
LLVM-built compiler itself, because the stale value is the active trap pointer.

## Prologue Contract

Local LLVM now emits an inline AArch64 fast-path stack check before any frame
write for functions marked `"oxcaml-stack-check"="true"`. The fast path uses
`x16`, `x17`, and `x30`, branches over the slow helper when there is enough
stack, and restores `x30` before LLVM's ordinary frame setup.

The slow helper saves OCaml registers, calls `caml_try_realloc_stack`, and
returns to the inline prologue. Because the helper currently uses `x15` before
saving all registers, LLVM calls clang with `-ffixed-x15` on arm64.

This is still a prototype:

- The inline asm is Darwin-specific.
- The current-stack offset and guard-page threshold are hardcoded in local LLVM.
- The `CHECK_SP_IN_STACK` runtime guard is diagnostic only.

## Exception Edges

Earlier real bugs were hidden exceptional edges. Protected OCaml calls,
protected external calls, `raise_notrace`, allocation/poll safepoints, and stack
checks now expose unwind successors when a trap is active. A guarded
`-keep-llvmir` compile of `utils/misc.ml` showed protected calls lowered as
`invoke ... unwind label ...`, and small hidden-edge stress tests printed the
expected results.

The next question is narrower: after stack realloc, why can the LLVM-built
compiler resume with `sp`/`TRAP_PTR` from the old stack while
`Caml_state->current_stack` points at the new stack?

## Next Checks

1. Instrument or breakpoint the second `caml_try_realloc_stack` path to compare
   `Caml_state->exn_handler`, `TRAP_PTR`, `current_stack->sp`, and the C-stack
   link before/after rewrite.
2. Determine whether stale `TRAP_PTR` comes from runtime stack rewriting,
   generated LLVM trap-state restoration, or an exception edge that still lacks
   the right state.
3. Only after the focused compiler examples pass, add expect tests for local
   LLVM usage, startup stack growth, exception edges, and stack realloc.
