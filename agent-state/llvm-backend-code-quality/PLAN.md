# Plan

Use a mixed review loop:

1. Inspect `backend/llvm/`, LLVM-codegen tests, and nearby integration code.
2. Propose 3-5 small cleanup targets in `PROGRESS.md`.
3. Run human-like review on the target list before editing.
4. Pick one reviewed target.
5. Implement the smallest useful change.
6. Run focused tests.
7. Run human-like review on the diff.
8. Verify review findings before applying them.
9. Keep or drop the change based on whether it remains obviously reviewable.

Prefer changes that clarify existing contracts, reduce local coupling, remove
misleading names, improve focused tests, or make future LLVM backend work safer.

Avoid broad refactors, formatting churn, speculative redesign, golden-output
noise, and behavior changes unless a concrete bug is identified.

Quality bar:

- The diff should be easy for an expert maintainer to review.
- The reason for the cleanup should be concrete, not taste-based.
- Tests should prove the contract being cleaned up.
- `PROGRESS.md` should record the chosen target, review result, validation, and
  any rejected review findings.
