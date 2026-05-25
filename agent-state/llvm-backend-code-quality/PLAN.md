# Plan

Use a mixed review loop. Choose one path per iteration:

End state:

- Complete 10 iterations, then stop and summarize the committed cleanups,
  dropped ideas, and remaining promising targets in `PROGRESS.md`.
- Count an iteration when it ends in either a committed cleanup or a discarded
  idea with a recorded reason.
- Do not keep working just to make 10 commits. The goal is 10 reviewed
  decisions, not 10 diffs.

Path A: review-selected target.

1. Inspect `backend/llvm/`, LLVM-codegen tests, and nearby integration code.
2. Propose 3-5 small cleanup targets in `PROGRESS.md`.
3. Run human-like review on the target list before editing.
4. Pick one reviewed target.
5. Implement the smallest useful change.

Path B: self-proposed candidate.

1. Inspect `backend/llvm/`, LLVM-codegen tests, and nearby integration code.
2. Record the candidate cleanup and why it is likely useful in `PROGRESS.md`.
3. Implement the smallest useful change.
4. Run human-like review on the diff specifically asking whether the change
   should be kept, revised, or dropped.

Shared finalization:

1. Write a testing story for the planned commit in `PROGRESS.md`.
2. Run the tests named by that testing story, adjusting the story if the diff
   changes.
3. Before finalizing an implementation commit, run the full LLVM-backend
   testsuite, then the stage2 LLVM verification run:

   ```sh
   eval "$(../../../scripts/agent-tmp-env)"
   make llvm-test LLVM_PATH="$LLVM_PATH"
   make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"
   ```

4. Run human-like review on the diff if it has not already been reviewed in its
   final form.
5. Verify review findings before applying them.
6. Decide keep, revise, or drop. If dropping, discard the diff, record why in
   `PROGRESS.md`, and return to target selection.

Prefer changes that clarify existing contracts, reduce local coupling, remove
misleading names, improve focused tests, or make future LLVM backend work safer.

Avoid broad refactors, formatting churn, speculative redesign, golden-output
noise, and behavior changes unless a concrete bug is identified.

Quality bar:

- The diff should be easy for an expert maintainer to review.
- The reason for the cleanup should be concrete, not taste-based.
- If the agent self-proposes a change, review must explicitly answer whether
  the completed diff is worth committing at all.
- Do not commit a cleanup just because it was implemented. If review shows it
  is weak, noisy, too risky, or not useful enough, drop it and pick a new idea.
- Tests should prove the contract being cleaned up.
- Every commit should have a clear testing story: exact commands or checks,
  expected coverage, why that scope is proportionate, and any validation that is
  deliberately deferred.
- Implementation commits should run `make llvm-test LLVM_PATH="$LLVM_PATH"` and
  then `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` before finalizing.
  If either is skipped, record the exact reason in `PROGRESS.md`.
- `PROGRESS.md` should record the chosen target, review result, testing story,
  validation result, any rejected review findings, and any dropped ideas.
- `PROGRESS.md` should track the iteration count, committed cleanups, dropped
  ideas, and current stop condition.

Testing story template:

```text
## Testing Story for Next Commit

Change:
Risk:
Validation:
Full LLVM validation:
Stage2 verification:
Why this is enough:
Deferred validation:
```
