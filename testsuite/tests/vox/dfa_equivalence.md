# DFA equivalence and minimization

## Human-review surface

Read these files in order; all paths are relative to the repository root:

1. `testsuite/tests/vox/dfa_semantics.ml`, module `Dfa_semantics`: the
   complete DFA model and definitions of every DFA predicate/observation used
   in the public contracts. A machine is an initial integer state plus a finite
   list of state rows. Each row contains an accepting bit, explicit labelled
   transitions, and a default target. Validity requires unique state/label keys,
   an existing initial state, and existing transition targets. The model defines
   behavior for invalid tables too; sufficient-budget claims require validity.
   State counts use unbounded integers. The label-length counter saturates at
   129, which preserves the public threshold of 64.
2. `testsuite/tests/vox/regex_semantics.ml`, modules `Regex_semantics` and
   `Membership`: regex syntax, finite membership evidence, its word, and its
   validity. These are the complete inductive regular-language rules.
3. `testsuite/tests/vox/dfa_equivalence_core.mli`, module `Dfa_equivalence`:
   executable comparison/minimization and all semantic contracts, including
   sufficient-budget completeness. Minimality quantifies over any valid DFA
   with pointwise language agreement, not over certificates or partitions.
4. `testsuite/tests/vox/regex_language.mli`, module `Regex_language`:
   executable matching and lowering. `sound` and `complete` completely
   characterize `matches` using the visible membership relation. `lower_matches`
   connects successful lowering to that language. Lowering may return `None`;
   no unconditional lowering-completeness claim is made.
5. `stdlib/bigint.mli`: unbounded integer semantics for `add`, `mul`, `of_int`,
   and numeric `compare`; `stdlib/ghost.mli`: the erased `Ghost.t` field.
   Other semantic primitives are ordinary finite lists/tuples, machine-integer
   equality/order/addition, Boolean operations, and Vox logical equality.
   Vox's refinement checker, checked total recursion, ghost erasure, and its
   solver/runtime primitive correspondence form the existing trusted base.
   The demo adds no assumptions or external primitives.

For DFA comparison/minimization alone, read items 1 and 3 plus the primitive
contracts in item 5. Regex matching and lowering additionally require items 2
and 4. The combined route needs their union; separating their presentation does
not remove a semantic dependency or shrink the combined claim.

This is the transitive specification surface. The concrete DFA table is the
public semantic input/output model; search queues, partitions, processed
histories, and certificates are implementation details. The four demo files
above have no dependencies on a proof module. Checked `.mli` files prevent the
public implementation from exporting auxiliary helpers. The implementation
modules rely on those interfaces instead of repeating their entire signatures.

`dfa_equivalence_proof.ml` contains the sealed `Dfa_proof` implementation and
its auxiliary interface for diagnostic tests and bridge proofs. `regex_core.ml`
and `regex_dfa_bridge_core.ml` contain the regex/DFA implementations and their
proofs. Their raw helper interfaces and certificate lemmas are not part of the
ordinary public API. `dfa_equivalence_core.ml` and `regex_language.ml` prove the
public contracts from those implementations and retain no runtime proof work.

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

`Dfa_proof.diagnose_comparison` and `Dfa_proof.diagnose_reduction` are optional diagnostic APIs
that construct runtime witnesses and certificates. Ordinary APIs erase them.
The ordinary search queues and visited sets remain runtime algorithm state.
Resource bounds establish checked termination. No allocation-success or
stack-space guarantee is claimed.

## Reproduce the checks

These sources require implicit refinement introduction and elimination. The
migration was checked with the compiler from commit
`9d5d8fca7a3261a3b06c293fe7aa30a38b9a2ef0`; the original #133 base is too old.
PR #200 is stacked on the shared implicit-refinement dependency, PR #207.

From a worktree configured with a compatible compiler, run these sequentially:

```sh
./dev init
./dev test vox/dfa_equivalence.ml
./dev test vox/regex_dfa_bridge.ml
./dev test vox/regex.ml
```

The DFA test runs its phrases once in the expect toplevel and covers
ordinary and diagnostic results, exact and insufficient
budgets, unreachable states, default transitions, and 66 global labels spread
across two states.

To reproduce separate compilation, the public-only client, rejection checks,
and the erasure audit, use the installed compiler:

```sh
make -s install
python3 testsuite/tests/vox/dfa_boundary_check.py
```

An existing compatible installation can be used without rebuilding this
checkout: pass `--compiler /path/to/installation/bin/ocamlopt.opt` for native
checks or `--compiler /path/to/installation/bin/ocamlc.opt` for bytecode checks. All generated
files and rejection fixtures remain in the script's private output directory.

The script prints its output directory. It compiles the public `.mli` files,
then compiles `dfa_public_client.ml` with only `Dfa_semantics` and the public
DFA interface available. `regex_public_client.ml` additionally gets the regex
semantic module and public regex interface. No proof-module CMI is available
to either client; the public interfaces are compiled with `-opaque`.
The DFA client derives equality, a ghost distinguishing word, completion, and
minimum state count. The regex client derives membership/matching and language
preservation through lowering and minimization. The script links and runs
smoke cases, rejects hidden certificate/definition helpers and proof modules,
and rejects an unsupported universal equality claim.

The script saves Lambda dumps for the selected compiler and checks transitive local calls starting
at ordinary `compare` and `reduce`. Public theorem bodies must contain no
runtime calls. Inspect the retained dumps as well, including indirect calls to
the semantic modules. Expected ordinary paths:

- `compare` calls `comparison_proved`, `search_pairs_loop`,
  `expand_search_pairs`, and `push_search_pair`. Its `append` combines state
  labels; no runtime word is appended or stored.
- `reduce` calls `minimize_proved`, the `reachable_states_*` search,
  `refine_to_stable`, and quotient construction. Partition refinement retains
  its current partition, with the prior partition and decreasing measure erased.
- Neither path calls `candidate`, `search_product`, `append_word`,
  `quotient_relation`, `quotient_access`, `cover_rows`, `copy_access`,
  `copy_separations`, `same_class_pairs`, `copy_word`, `copy_relation`,
  `copy_table`, or `copy_machine`.
- `comparison_witness` and regex `sound` have empty product results.
  `compare_complete`, `compare_equal`, `reduce_complete`, `reduce_preserves`,
  `reduce_minimum`, regex `complete`, and `lower_matches` contain no runtime
  function calls. Ghost record fields have empty product layout; ghost histories
  and fuel are constant placeholders.

The diagnostic APIs intentionally retain their certificate-producing paths.

The alphabet/class accumulators require total lists, and alphabet, partition,
class and quotient producers return total values directly. This removes four
identity traversals from ordinary minimization and the partition traversal
from each refinement round. Producer equations and public claims are unchanged.
Diagnostic access/separation witness copies retain their existing conversion
to total data; they are absent from ordinary execution.

`state_size` counts table rows directly. Private `state_ids_length` and
`state_size_via_ids` prove equality with the former ID-list count for every raw
table, including malformed tables. The audit checks that the emitted
`state_size` calls only `big_length` and constructs no list.

The independent regex membership model and optional-lowering contract remain
unchanged. Reachable-state construction is deferred: it would add a worklist,
closure and termination invariants, and change the implementation of lowering
without making the semantic review surface smaller. The derivative matcher is
already small. Proof wrappers that connect private operations to the public
entrypoints remain because direct function aliases do not satisfy the checked
public refinement interface.

### Minimization proof placement

`minimize_proved` calls the private `minimization_certificate` helper once,
inside `ghost_`. The helper contains the relation-respect proof, distinguishing
word construction, and separation cases. Its input refinements state the
source validity, access and closure facts, refinement result identity, and
budget and quotient facts required to produce the complete
`check_reduction` certificate. The ordinary pipeline retains its small local
facts and contains no certificate construction block.

This extraction improves the source layout, not runtime allocation. On both
backends, the emitted bodies of `minimize_proved`, `refine_to_stable`, `reduce`,
and `compare` match the accepted pre-extraction bodies modulo identifier
stamps and whitespace. The private `proved` and `stable_partition` wrappers
still appear in emitted code. The boundary gate rejects any ordinary call
graph that reaches `minimization_certificate`.
