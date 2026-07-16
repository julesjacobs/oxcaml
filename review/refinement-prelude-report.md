# vox2 verified prelude + examples report

The executable corpus is in `testsuite/tests/refinement-examples/`.  Its final
green run is **7/7 passed** (the six examples plus the prelude signature test;
`vox_spec.ml` is a compiled support module).

| Example | Final behavior | Today | Unlocking stage |
|---|---|---|---|
| `abs_nonnegative` | ACCEPT: prove the selected result is nonnegative | ACCEPT with an undischarged marked annotation | total comparisons + verification |
| `max_upper_bound` | ACCEPT: prove the result is at least both inputs | ACCEPT with an undischarged marked annotation | total comparisons + verification |
| `fib_nonnegative` | ACCEPT using refined recursive results as induction hypotheses | REJECT at the bare zero branch against the refined result | total comparisons + verification |
| `list_length_measure` | ACCEPT using the recursive result as the length induction hypothesis | REJECT at the bare nil-branch zero against the refined result | recursive totality + modes + verification |
| `seal_square_nonnegative` | ACCEPT by the directed implication from `result = x * x` to `result >= 0` | REJECT with rigid signature mismatch | total comparisons + seals + verification |
| `forall_unique_identity` | ACCEPT after backend recognition of ordinary `forall_` application | REJECT because the stubbed predicate context leaves the hole partial inside a required-total lambda | modes + verification |

## Prelude

`forall_` and `exists_` are ordinary `@ total` functions accepting total
single-argument predicate functions.  They have executable sentinel bodies;
their logical quantifier meanings belong to backend recognition, not to a
formula layer or surface syntax.  `implies` and `conjunction` are also total
and use plain boolean operations.

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

Final directory result:

```text
7 tests passed
0 tests failed
7 tests considered
```

The exact cold-entry `make test-one DIR=refinement-examples` invocation also
finished with 7/7 passing.  The repository-wide `make -s test` gate was run,
but the managed sandbox forbids socket `bind`: the existing
`lib-unix/unix-socket/recvfrom_linux.ml` and `recvfrom_unix.ml` tests failed
with `Unix.EPERM`.  The run then stopped producing output after
`mixed-modules/multi-file` for more than six minutes and was interrupted.
