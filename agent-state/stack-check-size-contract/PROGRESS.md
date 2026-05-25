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
- Goal clarification commit: `afb853a6361b`
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

## Current Blocker

None.

## Next Step

Review the diff, then commit and push the implementation plus tests to the
existing PR branch.
