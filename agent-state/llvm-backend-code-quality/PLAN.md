# Plan

Use a mixed review loop:

1. Inspect `backend/llvm/`, LLVM-codegen tests, and nearby integration code.
2. Propose 3-5 small cleanup targets in `PROGRESS.md`.
3. Run human-like review on the target list before editing.
4. Pick one reviewed target.
5. Implement the smallest useful change.
6. Write a testing story for the planned commit in `PROGRESS.md`.
7. Run the tests named by that testing story, adjusting the story if the diff
   changes.
8. Run human-like review on the diff.
9. Verify review findings before applying them.
10. Keep or drop the change based on whether it remains obviously reviewable.

Prefer changes that clarify existing contracts, reduce local coupling, remove
misleading names, improve focused tests, or make future LLVM backend work safer.

Avoid broad refactors, formatting churn, speculative redesign, golden-output
noise, and behavior changes unless a concrete bug is identified.

Quality bar:

- The diff should be easy for an expert maintainer to review.
- The reason for the cleanup should be concrete, not taste-based.
- Tests should prove the contract being cleaned up.
- Every commit should have a clear testing story: exact commands or checks,
  expected coverage, why that scope is proportionate, and any validation that is
  deliberately deferred.
- `PROGRESS.md` should record the chosen target, review result, testing story,
  validation result, and any rejected review findings.

Testing story template:

```text
## Testing Story for Next Commit

Change:
Risk:
Validation:
Why this is enough:
Deferred validation:
```
