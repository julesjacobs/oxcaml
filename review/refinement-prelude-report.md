# vox2 verified prelude + examples report

**The prelude is quantifier-free by ruling.**  Per the user ruling (reflected in
`pub/vox2/plan.html`, "The predicate representation"), refinement predicates are
quantifier-free bool terms for now, so `forall_`/`exists_` are OUT of scope: the
prelude carries no quantifier combinators and there is no quantifier example.  If
quantifiers return later they come back as ordinary total spec combinators, so
nothing here forecloses them.

The executable corpus is in `testsuite/tests/refinement-examples/`.  Its final
green run is **6/6 passed** (the five examples plus the prelude signature test;
`vox_spec.ml` is a compiled support module).

| Example | Final behavior | Today | Unlocking stage |
|---|---|---|---|
| `abs_nonnegative` | ACCEPT: prove the selected result is nonnegative | ACCEPT: body skeleton-checked, refined type assigned, no VC stored yet | total comparisons + verification |
| `max_upper_bound` | ACCEPT: prove the result is at least both inputs | ACCEPT: body skeleton-checked, refined type assigned, no VC stored yet | total comparisons + verification |
| `fib_nonnegative` | ACCEPT using refined recursive results as induction hypotheses | REJECT at the bare zero branch against the refined result | total comparisons + verification |
| `list_length_measure` | ACCEPT using the recursive result as the length induction hypothesis | REJECT at the bare nil-branch zero against the refined result | recursive totality + modes + verification |
| `seal_square_nonnegative` | ACCEPT by the directed implication from `result = x * x` to `result >= 0` | REJECT with rigid signature mismatch | total comparisons + seals + verification |

## Prelude

`implies` and `conjunction` are `@ total` functions over plain boolean
operations.  There are no quantifier combinators (see the ruling above).

Two DISTINCT reasons keep prelude entries partial, and they must not be
conflated:

- **Ruled partial (not compiler-forbidden).** The four monomorphic integer
  ordering wrappers (`int_lt`/`int_le`/`int_gt`/`int_ge`) would compile with an
  `@ total` annotation on this base — int comparison is on the pure-primitive
  allowlist at this tip — but `TASK-SPEC.md` (Ops) and the canonical plan
  ("Comparisons in specs") RULE all comparison primitives partial for now (the
  immediate-comparison question is deferred; making comparison total safely
  needs either a banned-application-site rule or future kind-constrained
  declarations). So the prelude deliberately keeps them partial to match the
  ruled end-state. Annotating them `@ total` would be forward-wrong: the
  modes-integration step tightens the totality allowlist to the ruling and such
  an annotation would then break. `VOX2_AWAITS_TOTAL_COMPARISONS` marks the
  single substitution point for when total comparisons land.
- **Genuinely not total yet.** `list_length` wraps the recursive stdlib
  `List.length`, whose structural recursive totality the current mode checker
  cannot establish, so it is partial as a fact about today's checker (not by
  ruling). Polymorphic comparison would be the other case here — it is off the
  pure-primitive totality allowlist entirely — but the prelude uses only the
  monomorphic int wrappers, so no polymorphic-comparison entry exists.

`implies`/`conjunction` are genuinely `@ total` (plain boolean operations). The
promoted `vox_spec_signatures.ml` expect block pins the inferred surface and the
harness explicitly compiles and loads `vox_spec.ml` for every client example.

## Verification commands

Every shell used:

```sh
export TMPDIR=/usr/local/home/jujacobs/tmp
export PATH="/home/jujacobs/.opam/5.4.0/bin:$PATH"
export CCACHE_DIR=/usr/local/home/jujacobs/tmp/ccache
```

The relevant build, promotion, and final green commands were:

```sh
make -s boot-compiler -j8
make test-one DIR=refinement-examples
make -s promote-one TEST=testsuite/tests/refinement-examples/<file>.ml
make -s test-one-no-rebuild TEST=testsuite/tests/refinement-examples/<file>.ml
make -s test-one-no-rebuild DIR=refinement-examples
make -s test
```

Final directory result (after the quantifier-free scope change; the stale
`_runtest` copy of the deleted `forall.ml` was pruned before the final run):

```text
6 tests passed
0 tests failed
6 tests considered
```

An earlier run (before the ruling) had 7/7 with a sixth `forall` example; that
example was deleted, not merely marked, because the combinators are out of
scope.  The repository-wide `make -s test` gate could not complete: the managed
sandbox forbids socket `bind`, so pre-existing `lib-unix/unix-socket` tests fail
with `Unix.EPERM` and the run then hung -- an environment artifact unrelated to
this change (which touches only new files under `testsuite/tests/refinement-examples/`).
