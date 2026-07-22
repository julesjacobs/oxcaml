# AUTO flow/quotation integration freeze

Status: **focused integration gates pass; ready for the separately reviewed
imposition delta and final combined suites; not committed, pushed, or
deployed**.

The worktree is based on Git commit
`570b3630055ad09b62ed1e00f4bfac2aa7a4886e`.  It contains the synchronized
contract-AUTO foundation, the dual-confirmed flow fix7 candidate, the reviewed
expression-boundary handling, and the dual-confirmed conservative quotation
boundary.  It deliberately contains no source from the imposition-fix lane.

## Frozen identity

- `typing/vox_verify.ml`:
  `a9c60fd4f106d90b0805cb5c373182e42c4150c1cc8347068373545923c1a847`
- `typing/vox_vc.mli`:
  `43d1c51d4201b46538e373bb8b2a644c3297ea9edbee604057695bda260d5c75`
- installed `ocamlc.byte`:
  `03a254d150c7b4e0713de6c4ecdaabfc5c0f977a2e84a934cb7b9666f401d2cd`
- installed `ocamlc.opt`:
  `3d69f2537ea750ad0d20ff46adb9ea9e11c2af300e6e93bdce6da2b43a1837c0`
- installed `ocamlopt.opt`:
  `45ea87bf985fb14c1460ff16b22221a25858b5a99580e6bb86ae103d6a7c91c0`
- flow permanent-test manifest: 17 files,
  `6e6de2d5948aa53ea0367bb25d9df58fe6ef8d1ec522d428c062475232c746d7`
- boundary permanent-test manifest: 8 files,
  `850f031b5a00818cf33789bbf69e6687ecbfdd40e201c4892b8e619df0086558`
- conservative quotation-test manifest, including the combined control:
  41 files,
  `ceb123119ae69b8523414ce5005e6e658552dc2372a7393cb3de45b57a846950`

Each test-manifest digest is the SHA-256 of the `sha256sum` output for the
lexicographically sorted relative filenames in that family.  No correction
file is part of a manifest or present in the candidate source tree.

The compiler delta against the base commit is 1,137 insertions and 714
deletions in `typing/vox_verify.ml`, plus the reviewed four-line
`Facts.restrict` interface exposure in `typing/vox_vc.mli`.  Relative to the
frozen flow-fix7 source, the semantic merge adds 332 lines and removes 23.

## Semantic merge

The merge was performed from the reviewed behavior, not by selecting one side
of a textual conflict:

- ordinary applications retain flow fix7's complete application arm;
- `%sequand` and `%sequor` retain the separate short-circuit path, so the
  conditionally evaluated right operand is not treated as an unordered
  sibling;
- all specialized control, aggregate, delayed, assertion, and quotation forms
  bypass the refinement-free fast path through the centralized boundary
  classifier;
- eager aggregate children are checked from a common entry environment, and
  only normally completing children contribute postfacts;
- nonrecursive `let ... and ...` right-hand sides use the same common-entry
  rule, while the body retains valid outward postfacts through
  `Facts.restrict`; an incomplete right-hand side prevents unreachable body
  postfacts and inherited result marks from escaping;
- optional-argument defaults, loops, comprehensions, lazy values, probes, and
  class declarations retain their reviewed conservative boundaries;
- match, try, exception, effect, guard, and mixed-arm completion remain the
  flow-fix7 implementation, including independent value/exception/effect
  chains and guard-aware handler fallthrough;
- let-operator bodies use `walk_case_facts`, so their local facts do not leak;
- quotation handling adds no field to the ordinary walker state.  It checks
  each current-stage splice from the same construction entry, discards every
  splice postfact, checks future code from an empty fresh state, and discards
  that state.  Quotation completion remains conservatively `true`; there is no
  stale traversal into the quoted body as the result of a surrounding try
  summary.

The combined quotation/application control specifically checks the overlap:
an impossible application domain around a quotation is still checked when the
quotation contains a nonreturning current-stage splice.  It rejects at the
quotation leaf with the intended fail-closed unsupported-subject diagnostic;
the nonreturning splice cannot suppress the outer obligation.

## Verification

The following passed with the lane-local compiler:

- `make -s boot-compiler`;
- `CCACHE_DISABLE=1 make -s install_for_test` (only the known same-file
  compiler-libs copy warnings);
- flow fixtures: `normal_exit_facts`, `normal_exit_scope`,
  `quotation_completion`, `direct_if_total_calls`, direct-if CMI, and the
  exact direct-if VC reference;
- boundary fixtures: `aggregate_sibling_facts`, `evaluation_boundaries`,
  `evaluation_boundaries_noassert`, and `evaluation_boundaries_quotation`;
- conservative `quotation_staging`, including the combined control;
- upstream `quotation/typing/quotes_splices`: 6/6;
- the frozen flow reviewer matrix a0 through e5: all seven intended
  acceptances accepted, and all twelve intended rejections failed at their
  intended leaf.  Local false leaves were `disproved`; the remaining negative
  controls were `not-proved`.

Every new or changed rejection was read at its source span.  No generated
output was accepted merely because it came from an expect test.  A discarded
early `normal_exit_facts` run overlapped an accidentally duplicated harness and
failed only because the shared temporary directory was removed; after
confirming no stale process remained, an isolated rerun passed.

The wider `DIR=refinement` diagnostic run completed 55/62.  Its seven failures
are all pre-integration or environmental rather than a new acceptance:

- three fixtures retain stale diagnostic spans after branch/result obligations
  moved to their enclosing expression;
- the omitted-labeled-argument fixture retains the known fail-closed
  unsupported case;
- one recursive negative now points at the failing recursive call rather than
  the whole match.  The frozen flow-fix7 compiler reproduces this exact output;
- the VC dump retains a stale exact hypothesis list after the closed
  proposition `1 > 0` was removed from a later call's environment.  This is
  not goal-variable or transitive relevance pruning.  The proposition was the
  already-discharged domain obligation for the earlier call `positive 1`;
  satisfying a callee precondition does not establish a runtime postcondition
  for later program points.  It mentions no returned value or mutable state,
  and the solver can re-derive the closed arithmetic tautology if a later goal
  ever needs it.  The later VC still contains its actual path fact `y > 0` and
  proves the same goal;
- the first Z3 subcase reports `unavailable` because the default test
  environment has no `z3` executable on `PATH`.  Rerunning `vc_dump_smt` with
  the verification-only Z3 directory explicitly prepended to `PATH` passes
  1/1, including all regular/nonregular JSON checks.

All negative programs above remain rejected.  No correction was accepted.
The full acceptance, Lean, example/backend, and repository suites were not
started here after the integration manager requested avoiding duplicate
resource-heavy runs while the imposition lane is active.

## Imposition insertion point

`typing/typecore.ml` is byte-identical to the contract-AUTO foundation at
`50edc2fa46ca4768b46939559fb8471e07bb88bf5190ff53ce6231f028b3eaf1`.
The reviewed imposition delta can therefore be applied and reviewed as a
separate, auditable change before the final combined suite.  No integration
decision in `typing/vox_verify.ml` depends on that pending implementation.

`git diff --check` passes.  The integrated compiler files, AUTO fixtures, and
this report contain none of the prohibited internal-path patterns.  No commit,
push, or deployment was performed.
