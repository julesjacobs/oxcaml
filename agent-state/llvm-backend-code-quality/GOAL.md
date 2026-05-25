# Goal

Iteratively improve LLVM backend code quality through small, reviewable cleanup
PRs.

First propose a short list of candidate cleanup targets in `PROGRESS.md`, then
use human-like review to pick a target before implementing it. After
implementation, run human-like review on the diff before finalizing.

Use `PLAN.md` as a guide, not a rigid checklist.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Expected output: one small, well-reviewed cleanup that improves maintainability
  without intended behavior changes, plus focused validation.

## Branches

- OxCaml: `jujacobs/llvm-backend-code-quality`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/16
