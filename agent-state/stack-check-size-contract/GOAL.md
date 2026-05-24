# Goal

Introduce an explicit stack-check size contract from the OCaml LLVM backend to
AArch64 LLVM frame lowering.

Today the LLVM backend effectively gives AArch64 LLVM frame lowering a binary
request: this function should get an OxCaml stack check. That is too vague for
the next optimization. The backend needs to pass the CFG-required stack-check
size in bytes, so LLVM frame lowering can later distinguish:

- functions where CFG requires no checked stack space;
- functions where CFG requires a check because of a non-tail call;
- functions where CFG requires a large check because of outgoing stack
  arguments;
- functions where CFG requires no check, but LLVM later creates a large static
  frame.

This agent should add and test that byte-count contract. It should not make the
broad policy change that removes stack checks from normal codegen yet.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Expected output: a focused OxCaml PR that adds the stack-check byte-count
  contract and focused tests for it.

The change may edit both OxCaml sources and vendored LLVM sources, but should
stay limited to the LLVM backend stack-check attribute plumbing and the focused
tests needed to prove it.

## Required Behavior

1. In `backend/llvm/llvmize.ml`, compute the maximum CFG-required stack-check
   size for each generated function from `Cfg.Stack_check
   { max_frame_size_bytes }` instructions.

2. Emit a numeric LLVM function attribute carrying that size, for example:

   ```text
   "oxcaml-stack-check-bytes"="<n>"
   ```

   The exact attribute name may change if there is a better local convention,
   but it must be numeric and function-local.

3. Preserve the existing stack-check emission policy as much as possible. This
   PR should establish the data contract, not remove stack checks broadly.

4. Respect `Config.no_stack_checks`: when stack checks are disabled, do not emit
   the new stack-check byte-count attribute or any equivalent stack-check
   request.

5. In AArch64 LLVM frame lowering, parse the numeric attribute and consume it in
   a way that tests can prove. If the implementation keeps the current emitted
   sequence unchanged, add enough test visibility to prove LLVM read the value.
   If the implementation uses the value in the existing prologue check size,
   keep behavior conservative and document why it is safe.

## Correctness Tests

Add focused tests proving the contract carries the right value.

The tests must cover:

1. Small allocation-only leaf function
   - Expected CFG-required size: `0`.
   - The test must prove the new byte-count contract represents this as zero or
     absent, according to the chosen representation.

2. Function with a non-tail call
   - Expected CFG-required size: nonzero.
   - The test must prove the numeric value is emitted and consumed.

3. Noalloc extcall with many outgoing stack arguments
   - Expected CFG-required size includes the outgoing stack-argument area.
   - The test must prove the numeric value is large enough to cover that area.

4. `-no-stack-checks`
   - The test must prove the new attribute is absent and disabled stack-check
     behavior is preserved.

These tests should fail if the backend only tracks a boolean. They should also
fail if outgoing stack arguments are not reflected in the CFG-required byte
count.

## Helpfulness Test

The PR must make the later optimization mechanically possible.

In either tests, comments, or a short note in `PROGRESS.md`, show that the new
contract exposes the inputs needed for the later rule:

```text
required_bytes = max(final_static_llvm_frame_bytes, cfg_required_stack_check_bytes)

if cfg_required_stack_check_bytes == 0
and final_static_llvm_frame_bytes < stack_threshold_size
then omit the prologue stack check
else emit a prologue stack check for required_bytes
```

The PR does not need to implement that rule. It does need to prove the
`cfg_required_stack_check_bytes` input is available and has the distinctions the
rule depends on.

## Validation

Before completion, run the focused LLVM backend tests added or updated by this
PR.

Do not run broad benchmarking. Full LLVM backend suite and stage2 validation are
not required for this first slice unless the change grows beyond attribute
plumbing and focused frame-lowering consumption.

## Non-goals

- Do not broadly remove stack checks from small leaf functions yet.
- Do not repeat the unsafe PR 8 behavior of gating checks only on whether CFG
  inserted a `Stack_check`.
- Do not change the runtime helper ABI.
- Do not remove the `_caml_plat_pagesize` load or two-page margin.
- Do not attempt CFG-level stack-check sinking.

## Branches

- OxCaml: `jujacobs/stack-check-size-contract`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/11
