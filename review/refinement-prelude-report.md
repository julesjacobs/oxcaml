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
| `abs_nonnegative` | ACCEPT: prove the selected result is nonnegative | ACCEPT with an undischarged marked annotation | total comparisons + verification |
| `max_upper_bound` | ACCEPT: prove the result is at least both inputs | ACCEPT with an undischarged marked annotation | total comparisons + verification |
| `fib_nonnegative` | ACCEPT using refined recursive results as induction hypotheses | REJECT at the bare zero branch against the refined result | total comparisons + verification |
| `list_length_measure` | ACCEPT using the recursive result as the length induction hypothesis | REJECT at the bare nil-branch zero against the refined result | recursive totality + modes + verification |
| `seal_square_nonnegative` | ACCEPT by the directed implication from `result = x * x` to `result >= 0` | REJECT with rigid signature mismatch | total comparisons + seals + verification |

## Prelude

`implies` and `conjunction` are `@ total` functions over plain boolean
operations.  There are no quantifier combinators (see the ruling above).

The four typed integer ordering wrappers are deliberately partial today.
`VOX2_AWAITS_TOTAL_COMPARISONS` marks their single future substitution point.
`list_length` is likewise partial because the current mode checker cannot
express structural recursive totality.  The promoted `vox_spec_signatures.ml`
expect block pins the inferred surface and the harness explicitly compiles and
loads `vox_spec.ml` for every client example.

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
