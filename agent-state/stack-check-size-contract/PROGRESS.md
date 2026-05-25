# Progress

Last updated: 2026-05-24.

## Current Claim

Implemented locally. The LLVM backend now emits an explicit function-local
`"oxcaml-stack-check-bytes"="<n>"` attribute for AArch64 LLVM functions that
also receive `"oxcaml-stack-check"="true"`. The value is exactly the maximum
`Cfg.Stack_check.max_frame_size_bytes` in the CFG, or `0` when the function has
no CFG `Stack_check`. When `Config.no_stack_checks` is true, neither attribute
is emitted.

This PR does not change the AArch64 LLVM prologue sizing or omission policy.
The byte-count contract exposes the later-rule input
`cfg_required_stack_check_bytes`; the later rule can combine it with
`final_static_llvm_frame_bytes` as:

```text
required_bytes = max(final_static_llvm_frame_bytes, cfg_required_stack_check_bytes)
```

## Evidence

- OxCaml branch: `jujacobs/stack-check-size-contract`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/11
- Initial state commit: `106d76c3c37f`
- Goal clarification commit: `8145da3b9b`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/stack-check-size-contract`
- Focused test added: `testsuite/tests/llvm-codegen/stack_check_size_contract.ml`
  compares each LLVM IR attribute against the post-`Cfg_stack_checks` CFG dump.
- Observed contract values in the focused test:
  - leaf allocation-only function: CFG `0`, LLVM IR attribute `"0"`;
  - non-tail call: CFG `16`, LLVM IR attribute `"16"`;
  - noalloc extcall with many outgoing stack args: CFG `336`, LLVM IR
    attribute `"336"`, with final assembly showing a `320` byte outgoing stack
    adjustment before `_caml_c_call_stack_args`.
- No-stack-checks source-level check added as
  `testsuite/tests/llvm-codegen/stack_check_size_contract_no_stack_checks.ml`.
  In this local AArch64 stack-checks build it is skipped by the
  `no-stack-checks` predicate; that is the smallest available harness check
  here because this local configuration has stack checks enabled.
- Validation run:
  - `make -s compiler LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`
  - `make -s install LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`
  - `cmake --build "$OXCAML_AGENT_TMP/llvm-build" --target clang -- -j8`
  - direct focused testsuite run:
    `make one TEST=tests/llvm-codegen/stack_check_size_contract.ml` passed
  - direct no-stack-checks test file run:
    `make one TEST=tests/llvm-codegen/stack_check_size_contract_no_stack_checks.ml`
    skipped at the `no-stack-checks` predicate, with no failures
  - full-suite environment:
    `opam exec --switch=oxcaml-5.4.0+oxcaml -- make -s boot-compiler
    LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"` passed after
    `eval "$(../../../scripts/agent-tmp-env)"` and an agent-local clang wrapper.
  - full-suite LLVM-codegen directory rerun:
    `make -s test-one DIR=llvm-codegen LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0
    LLVM_PATH="$LLVM_PATH"` passed: 59 passed, 1 skipped, 0 failed.
  - full suite rerun:
    `make -s test LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"`
    reached the final report with 6666 passed, 281 skipped, 7 failed, 0 not
    started, and 0 unexpected errors. The branch-related LLVM-codegen oracle
    failures were fixed. The remaining failures were existing/non-branch
    failures in six `tests/frame-pointers` native tests and
    `tests/typing-small-numbers/test_matching_native.ml` in `ocamlnat`, which
    exits with signal 5.
  - review follow-up:
    `make -s test-one DIR=llvm-codegen LLVM_BACKEND=1 LLVM_BOOT_BACKEND=0
    LLVM_PATH="$LLVM_PATH"` passed after tightening the focused test for the
    legacy boolean attribute and leaf CFG `stack_check` absence: 59 passed, 1
    skipped, 0 failed.
  - follow-up review for `-no-cfg-stack-checks`:
    the LLVM backend now omits `"oxcaml-stack-check-bytes"` when CFG stack-check
    insertion did not run, while preserving the legacy boolean request. The
    focused LLVM-codegen directory passed with the added regression test:
    64 passed, 1 skipped, 0 failed.
  - dump-label review follow-up:
    the LLVM path now emits the `After cfg_stack_checks` CFG dump only when
    `Cfg_stack_checks.cfg` actually ran. The no-CFG-stack-checks regression test
    asserts that the heading is absent in `-no-cfg-stack-checks` mode. The
    focused LLVM-codegen directory passed again: 64 passed, 1 skipped, 0 failed.

## Current Blocker

None.

## Next Step

The implementation and focused tests are on the PR branch. Remaining full-suite
failures are outside this stack-check byte-contract change.
