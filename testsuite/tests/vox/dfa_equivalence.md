# DFA equivalence and minimization

`dfa_equivalence_core.ml` implements finite DFAs over machine-integer letters.
Each state has explicit labelled transitions and a default transition.
`regex_core.ml` contains the extracted regex implementation;
`regex_dfa_bridge_core.ml` proves that successful lowering preserves the
compiled regex language.

## Ordinary APIs

- `compare left right limit` returns `Equivalent`, `Inequivalent`, or
  `Comparison_limit`. `compare_equal` proves agreement on every supplied word;
  `comparison_witness` supplies an erased distinguishing word for `Inequivalent`.
- `reduce source limit` returns a minimized machine or `None`.
  `reduce_preserves` proves language preservation. `reduce_minimum` proves that
  its state count is no greater than that of any valid DFA agreeing with the
  source on every word, using the supplied universally quantified agreement.

Comparison is guaranteed to avoid `Comparison_limit` for valid machines with
at most 64 explicit labels per state, when the product of their state counts
is at most `limit` and `0 < limit <= 65_536`. Smaller budgets may succeed.
The implementation caps larger comparison budgets at 65,536 and rejects
nonpositive budgets.

Minimization requires a valid source, at most 64 explicit labels per state,
and `state_size source <= limit` with `0 < limit <= 64`.
`reduce_complete` proves success within these bounds. The label bound is per
state; the global alphabet may exceed 64 letters.

`diagnose_comparison` and `diagnose_reduction` are optional diagnostic APIs
that construct runtime witnesses and certificates. Ordinary APIs erase them.
The ordinary search queues and visited sets remain runtime algorithm state.
Resource bounds establish checked termination, not practical memory limits.

## Reproduce the checks

From a configured worktree, run these sequentially:

```sh
./dev init
./dev test vox/dfa_equivalence.ml
./dev test vox/regex_dfa_bridge.ml
./dev test vox/regex.ml
```

The DFA test checks bytecode and native expect modes, with and without
`-principal`, including ordinary and diagnostic results, exact and insufficient
budgets, unreachable states, default transitions, and 66 global labels spread
across two states.

To inspect proof erasure with the installed compiler:

```sh
make -s install
dfa_audit_dir=$(mktemp -d /tmp/vox-dfa-audit.XXXXXX)
_install/bin/ocamlopt.opt -principal -extension refinement_types \
  -drawlambda -dcanonical-ids \
  -c testsuite/tests/vox/dfa_equivalence_core.ml \
  -o "$dfa_audit_dir/dfa_equivalence_core.cmx" \
  > "$dfa_audit_dir/stdout" 2> "$dfa_audit_dir/lambda"
```

Inspect function bodies and their transitive calls, starting at `compare` and
`reduce`. Expected ordinary paths:

- `compare` calls `comparison_proved`, `search_pairs_loop`,
  `expand_search_pairs`, and `push_search_pair`. Its `append` combines state
  labels; no runtime word is appended or stored.
- `reduce` calls `minimize_proved`, the `reachable_states_*` search,
  `refine_to_stable`, and quotient construction. Partition refinement retains
  its current partition, with the prior partition and decreasing measure erased.
- Neither path calls `candidate`, `search_product`, `append_word`,
  `quotient_relation`, `quotient_access`, `cover_rows`, `copy_access`,
  `copy_separations`, or `same_class_pairs`.
- `comparison_witness` has an empty product result. `compare_complete`,
  `compare_equal`, `reduce_complete`, `reduce_preserves`, and `reduce_minimum`
  contain no runtime function calls. Ghost record fields have empty product
  layout; ghost histories and fuel are constant placeholders.

The diagnostic APIs intentionally retain their certificate-producing paths.
