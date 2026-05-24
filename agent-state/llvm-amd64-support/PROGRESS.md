# Progress

Last updated: 2026-05-24.

## Current Claim

First AMD64 implementation patch is in progress. The vendored LLVM X86
prologue now recognizes OxCaml stack-check functions and emits an AMD64
prologue stack-growth slow path, and the AMD64 runtime now provides the
matching no-OCaml-stack helper.

## Evidence

- OxCaml branch: `jujacobs/llvm-amd64-support`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/10
- Initial state commit: `7965927f6af4`
- Agent PR-record commit: `25e06ddb58`
- Base integration branch: `jujacobs/llvm-backend-integration`
- Base integration commit at worktree creation: `95443659432c`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-amd64-support`
- Makefile workflow audit recorded in `GOAL.md`. Main finding:
  `Makefile.common-ox` provides `llvm-*` targets that set `LLVM_BACKEND=1`,
  use `LLVM_PATH`, require frame pointers, and support explicit `ARCH=amd64`.
- Full-validation scripts:
  `tools/build-llvm-self-stage-install.sh`,
  `tools/build-llvm-stage5-install.sh`, and
  `tools/run-llvm-stage5-ocamltest.sh`. They support `DUNE_BUILD_FLAGS`,
  `LLVM_TESTSUITE_PARALLEL`, `LLVM_TESTSUITE_JOBS`, `LIST`, and
  `EXCLUDE_REGEX`.
- Implemented source changes:
  - `vendor/llvm-project/llvm/lib/Target/X86/X86FrameLowering.cpp` disables
    red-zone use for `oxcaml-stack-check` functions and emits an inline
    AMD64 stack check that jumps to `caml_llvm_prologue_realloc_stack` without
    pushing on the OCaml stack.
  - `runtime/amd64.S` defines `caml_llvm_prologue_realloc_stack`, preserving
    OCaml registers in a `gc_regs` bucket, calling `caml_try_realloc_stack`
    from the C stack, restoring registers on success, and raising
    `Stack_overflow` via the no-push internal raise path on failure.
- Validation done this turn:
  - `git diff --check` passed.
  - Runtime assembly was preprocessed and assembled with temporary configured
    `m.h`/`s.h` shims because `./configure` cannot complete in the current
    opam switch:
    `gcc -E -x assembler-with-cpp -I /tmp/oxcaml-agent-llvm-amd64-support/include -I /tmp/oxcaml-agent-llvm-amd64-support/include/caml -I runtime -DSYS_linux runtime/amd64.S >/tmp/oxcaml-agent-llvm-amd64-support/amd64.S.pp`
    then
    `gcc -c -x assembler /tmp/oxcaml-agent-llvm-amd64-support/amd64.S.pp -o /tmp/oxcaml-agent-llvm-amd64-support/amd64.o`.
  - `nm -g /tmp/oxcaml-agent-llvm-amd64-support/amd64.o` shows
    `caml_llvm_prologue_realloc_stack`.
  - A standalone AMD64 assembly snippet using the LLVM inline stack-check
    sequence assembled and produced the expected `R_X86_64_PLT32`
    relocation to `caml_llvm_prologue_realloc_stack`.
- Validation attempted but blocked:
  - `./configure --enable-frame-pointers` fails because the active compiler is
    OCaml 4.14.2, while this tree requires OCaml 5.4.x.
  - Direct `g++ -fsyntax-only` for `X86FrameLowering.cpp` cannot run from this
    unconfigured checkout because generated LLVM headers such as
    `X86GenRegisterInfo.inc` are unavailable.

## Current Blocker

Focused compiler/test validation is blocked until the checkout is configured
with an OCaml 5.4.x compiler and generated LLVM build headers are available.
This is not a blocker for source work; it is the current blocker for running
`llvm-install`/`llvm-test-one` in this checkout.

## Next Step

Install or select an OCaml 5.4.x opam switch, rerun
`./configure --enable-frame-pointers`, then build enough vendored LLVM/OxCaml
state to run:
`DUNE_BUILD_FLAGS=-j1 ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-install`,
followed by
`ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-test-one TEST=llvm-codegen/arithmetic`.
If that exposes an AMD64 lowering/runtime failure, reduce it before expanding
the implementation.
