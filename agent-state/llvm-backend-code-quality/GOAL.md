# Goal

Iteratively improve LLVM backend code quality through small, reviewable cleanup
PRs.

Use either path:

- Ask human-like review for candidate cleanup targets, then pick a reviewed
  target before implementing it.
- Self-propose and implement a small candidate cleanup, then use human-like
  review to decide whether the diff should be kept, revised, or dropped.

In both paths, run human-like review on the diff before finalizing.

Use `PLAN.md` as a guide, not a rigid checklist.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Expected output: one small, well-reviewed cleanup that improves maintainability
  without intended behavior changes, plus focused validation.

Each commit must have a written testing story in `PROGRESS.md`: what was run,
why that validation is enough for the commit's risk, and what broader validation
is intentionally deferred.

Before finalizing any implementation commit, the default validation is:

```sh
eval "$(../../../scripts/agent-tmp-env)"
make llvm-test LLVM_PATH="$LLVM_PATH"
make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"
```

If a commit does not run both, `PROGRESS.md` must say exactly why that is safe
for this commit.

## Branches

- OxCaml: `jujacobs/llvm-backend-code-quality`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/16
