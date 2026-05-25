# Goal

Make OxCaml LLVM stack checks efficient without changing the stack-check policy
yet.

Today the LLVM backend mostly tells AArch64 LLVM frame lowering only whether a
function wants an OxCaml stack check. That is not enough information for later
optimization. LLVM needs a trustworthy way to tell the difference between:

- a function whose CFG proves no checked stack space is needed;
- a function whose CFG requires a nonzero stack-check size;
- a function where that CFG-derived size is unknown because the relevant CFG
  pass did not run;
- a function whose final LLVM frame may still require a check even if CFG did
  not require one.

This PR should add the smallest useful plumbing and focused tests for that
information flow. It should not implement the later optimization that removes
or resizes stack checks.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Main implementation area: `backend/llvm/` and the LLVM backend path in
  `asmcomp/`
- Expected output: a focused OxCaml PR that makes the relevant CFG stack-check
  information visible to LLVM IR and proves the observable contract with tests.

## Desired Shape

- Preserve the existing legacy stack-check request behavior unless stack checks
  are explicitly disabled.
- Add a truthful CFG-derived byte-count signal for LLVM when that CFG result is
  known.
- Do not encode an unknown CFG byte requirement as `0`; `0` should mean known
  zero.
- Keep AArch64 LLVM frame-lowering behavior unchanged in this PR.
- Keep the tests focused on the information contract, not on brittle assembly
  spellings.

## Validation

Run the focused LLVM backend tests added or updated by this PR.

Do not run broad benchmarking. Full LLVM backend suite and stage2 validation are
not required for this first slice unless the change grows beyond attribute
plumbing and focused tests.

## Non-goals

- Do not broadly remove stack checks from small leaf functions yet.
- Do not change the runtime helper ABI.
- Do not remove the `_caml_plat_pagesize` load or two-page margin.
- Do not attempt CFG-level stack-check sinking.

## Branches

- OxCaml: `jujacobs/stack-check-size-contract`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/11
