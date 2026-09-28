title: Myers diff
blurb: An edit script between two integer lists, proved to turn the first into the second at the minimum number of insertions and deletions.
status: owner-review
date: 27 September 2026
sources:
  - verification/library/vox_diff_spec.mli — Edit scripts and the edit distance, specified by equations
  - verification/library/vox_diff.mli — Public interface: `diff`, optimality and inversion
  - verification/library/vox_diff_spec.ml — Implementation of the equations
  - verification/library/vox_diff.ml — Frontier search and its proofs
  - testsuite/tests/vox/diff_public_client.ml — Client using only the public interfaces
  - testsuite/tests/vox/diff_rejected.ml — Rejected programs
  - testsuite/tests/vox/diff.ml — Runtime tests against a dynamic-programming oracle
  - testsuite/tests/vox/diff_boundary.ml — Erasure audit, public-only compile of the client and a run of the demo
  - verification/demos/diff_demo.ml — Command-line demo, also run by `scripts/run-diff-demo`
---
`Vox_diff.diff old fresh` computes an edit script between two `int` lists with Myers' frontier search. A script is a list of `Keep x`, `Delete x` and `Insert x`. `Vox_diff_spec` specifies, by equations, its source (the kept and deleted elements), its target (the kept and inserted elements), its cost (the number of deletions and insertions), `apply`, `invert` and the edit distance `minimum_cost`. When both inputs have at most 1,000,000 elements, `diff` returns `Ok s`, where `s` has source `old`, target `fresh` and cost `minimum_cost old fresh`; otherwise it returns `Error Input_too_large`. `optimal_at` proves that this cost is optimal: every script that `apply` turns from `old` into `fresh` costs at least as much as `s`. `invert` exchanges source and target and keeps the cost.

`minimum_cost` is specified by its recurrence, so it is the edit distance with insertions and deletions only: a substitution costs two. Which of several optimal scripts `diff` returns is described in a comment in `vox_diff.mli` but not specified. Running time and memory are not proved; the 1,000,000-element cap bounds an integer position kept by the search, and inputs within it are not guaranteed to fit in memory or finish quickly.

## Client example

From the public client. `(x : t) -> ...` names an argument so that later types can mention it, and `{r : t | p}` is `t` refined by the predicate `p`. `===` is logical equality. `ghost_ (...)` is proof code, checked and then erased. `@ total` marks a total function, one that terminates without raising or touching mutable state. The ascription `computed : {s : script | ...} = s` makes the checker prove, from `diff`'s result, the premise that `optimal_at` needs.

@code testsuite/tests/vox/diff_public_client.ml "let (verified_client @ total)" "    result"

## A rejected program

A script that deletes and reinserts `97` does not have minimum cost, so it cannot be offered to `optimal_at` as one. The lemma calls give the checker the equations for these concrete lists, from which the script costs 2 and `minimum_cost [97] [97]` is 0. The test `diff_rejected.ml` requires this error.

@code testsuite/tests/vox/diff_rejected.ml "let nonminimal () =" "Vox_diff.optimal_at [97] [97] computed [Keep 97];;"

@text testsuite/tests/vox/diff_rejected.ml "Line 10, characters 4-10:" "The refinement is stated here."

The same test rejects a false claim about the patched result, a conclusion drawn from `optimal_at` without proving that the other script applies, and references to a hidden module and a hidden function.

## Interface

@code verification/library/vox_diff_spec.mli

@code verification/library/vox_diff.mli

`[@@inductive]` declares a datatype the checker reasons about by cases and induction; the kind `immutable_data mod total` says that values of type `operation` are immutable data that total code may use. `@@ total` on a declaration marks a total function, which may appear in refinements. Each `_def` or `_equation` lemma states the defining equation of the function before it; `apply_characterization` defines `apply` through `source` and `target`. `Bigint.t` is unbounded integers, and `0Z`, `1Z` and `1000000Z` are `Bigint` literals.

## Trusted base

- `diff_boundary.ml` is the only check that `diff` runs no proof code. It compiles the library with `-drawlambda` and requires, for each function `diff` runs (listed by hand in `diff_boundary_check.ml`), that every application in its body is one of a fixed set of calls, and that it mentions neither `minimum_cost` nor `Bigint`. Since each listed function calls only listed functions, the list covers everything `diff` runs. The one function of the `Proof` module that runs is `reverse_into`, which `finished` calls to build the result script; for it the check looks only at calls by name (it calls only itself) and at references to other modules, not at every application.

## Scope

- Operations: `diff`, `apply` and `invert`. There is no three-way merge, no line splitting and no output format; the command-line demo encodes bytes as integers.
- Elements are compared with `int` equality.
- `diff` returns `Error Input_too_large` exactly when an input has more than 1,000,000 elements. The cap is a constant in `vox_diff.ml`.
- `optimal_at` takes the computed script as a refined argument `{s : script | cost s = minimum_cost old fresh}`. `diff` supplies such a script only within the cap; beyond it a client must prove the premise itself. The lower bound for an arbitrary script, `minimum_cost (source s) (target s) <= cost s`, is proved inside `vox_diff.ml` but not exported.
- `diff`'s result also states `apply old s === Some fresh`, `0Z <= cost s` and a bound on the script's length; these follow from the other three facts and the equations of `Vox_diff_spec`.
- The tie rule (insertion wins when the old-input positions are equal) is tested, not proved: `diff.ml` checks that `a` to `b` gives `Delete 97; Insert 98`.
- Every public function is declared `total`: it terminates without raising, except by running out of memory or stack. Time, memory and stack depth are not bounded.
- `optimal_at`, `invert_correct` and `inverse_patch` are ordinary functions that run their proofs if called outside `ghost_`; the client calls them inside it.

## Reproduce

After `make install` and `./dev init`, from the repository root:

```
./dev test vox/diff.ml vox/diff_rejected.ml vox/diff_boundary.ml
```

`diff.ml` compiles the library and the public client, then compares `diff` with a dynamic-programming edit distance on all 3,969 pairs of binary words of length at most 5, 200 random pairs and other cases, and checks the million-element cap on both sides. `diff_rejected.ml` checks the five rejected programs. `diff_boundary.ml` compiles the library with both compilers, checks in its Lambda which functions each runtime function of `vox_diff.ml` calls (no metric or big-integer code, no proof function but `reverse_into`, and no runtime fuel in the search), compiles `diff_public_client.ml` with only the two public `.cmi` files available and checks that `verified_client` and `accepted` each make one call, to `Vox_diff.diff`, and runs the demo on `ABCABBA` and `CBABAC` against its expected output. `scripts/run-diff-demo OLD NEW` runs the demo on other inputs.
