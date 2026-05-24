# Progress

Last updated: 2026-05-24.

## Current Claim

Implemented the focused LLVM AArch64 stack-check preamble improvement. The fast
path now loads the current stack limit from the domain state and adds the fixed
64-bit OxCaml stack context plus the required frame/threshold words directly,
instead of loading `_caml_plat_pagesize` through the GOT and adding two pages.
Stack checks remain in the function preamble; no CFG sinking was attempted.

## Evidence

- OxCaml branch: `jujacobs/llvm-stack-check-preamble`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/3
- Initial PR commit: `87e729dc7ac2`
- Vendored LLVM change:
  `vendor/llvm-project/llvm/lib/Target/AArch64/AArch64FrameLowering.cpp`
  computes `LimitOffsetBytes = (StackContextWords + RequiredWords) * 8`
  and emits:
  - a single `add x16, x16, #imm` when the byte offset fits the AArch64 add
    immediate;
  - a shifted `add` for page-multiple offsets that fit that encoding;
  - otherwise a `movz`/`movk` load into `x30` followed by `add x16, x16, x30`.
- Assembly-level expect updates:
  - `testsuite/tests/llvm-codegen/allocation.ml` now expects
    `add x16, x16, #392` instead of the old GOT/page-size sequence.
  - `testsuite/tests/llvm-codegen/store_modify.ml` now expects
    `add x16, x16, #376` instead of the old GOT/page-size sequence.
- Agent state path: `agent-state/llvm-stack-check-preamble`
- Goal recorded in `agent-state/llvm-stack-check-preamble/GOAL.md`.
- Validation:
  - Reconfigured the agent checkout with
    `./configure --prefix=$(pwd)/_local_install --enable-frame-pointers`
    under opam switch `oxcaml-5.4.0+oxcaml`.
  - Temporarily mirrored the vendored AArch64 change into the separate
    `/Users/julesjacobs/git/jujacobs/oxcaml-llvm/llvm-project` checkout,
    rebuilt `/tmp/oxcaml-clang-wrapper` with `ninja -C .../llvm-build clang`,
    then restored that external checkout to a clean source state after
    validation.
  - `make llvm-test-one TEST=testsuite/tests/llvm-codegen/allocation.ml
    LLVM_BOOT_BACKEND=0 LLVM_PATH=/tmp/oxcaml-clang-wrapper`: passed
    (`3 passed`, `0 failed`).
  - `make llvm-test-one TEST=testsuite/tests/llvm-codegen/store_modify.ml
    LLVM_BOOT_BACKEND=0 LLVM_PATH=/tmp/oxcaml-clang-wrapper`: passed
    (`3 passed`, `0 failed`).
  - `make llvm-test-one DIR=testsuite/tests/llvm-codegen
    LLVM_BOOT_BACKEND=0 LLVM_PATH=/tmp/oxcaml-clang-wrapper`: passed
    (`52 passed`, `0 failed`).
  - `/tmp/oxcaml-clang-wrapper.log` from the directory run contained `419`
    `-x ir` wrapper invocations and `423` fixed-register flag matches.
  - `git diff --check`: passed.

## Current Blocker

None.

## Next Step

Review the PR, or run broader self-stage validation if desired.
