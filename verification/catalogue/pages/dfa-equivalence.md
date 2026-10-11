title: DFA equivalence and minimization
blurb: Language equivalence of two DFA tables, with an erased counterexample word, and minimization of tables of at most 64 rows, proved to preserve the language and to give the fewest states.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/dfa_semantics.ml — DFA tables, `run`, validity and sizes
  - testsuite/tests/vox/dfa_equivalence_core.mli — Public interface
  - testsuite/tests/vox/dfa_equivalence_core.ml — Proofs of the public contracts from the implementation
  - testsuite/tests/vox/dfa_equivalence_proof.ml — Comparison search, partition refinement and their proofs
  - testsuite/tests/vox/dfa_public_client.ml — Client using only the public interface
  - testsuite/tests/vox/dfa_equivalence.ml — Test that checks the proofs and runs examples
  - testsuite/tests/vox/dfa_boundary.ml — The only compile of the public interface and client, and the erasure check
  - testsuite/tests/vox/dfa_boundary_check.ml — The erasure check's list of certificate-building functions
---
`Dfa_equivalence` compares two deterministic automata and minimizes one. A machine is a plain table: an initial state id and a list of rows, each with a state id, an accepting bit, a list of `(letter, target)` edges and a default target for every other letter. Letters and state ids are `int`s. `Dfa_semantics.run m w` follows the table from the initial state over the word `w` and returns the accepting bit of the last state. Lookup uses the first matching row or edge. A missing row has accepting bit `false` and sends every letter to state 0; `valid` excludes missing rows, duplicate state ids and duplicate edge labels.

`compare left right limit` returns `Equivalent`, `Inequivalent` or `Comparison_limit`. If it returns `Equivalent`, the two machines give the same answer on every word; if `Inequivalent`, an erased word on which they differ is available to proofs. These two facts hold for any tables. `compare` does not return `Comparison_limit` when both machines are `valid`, no row has more than 64 edges, `0 < limit <= 65,536`, and the product of the two row counts is at most `limit`.

`reduce source limit` returns a machine or `None`. If it returns `Some c`, then `c` is valid, gives the same answer as `source` on every word, and has no more rows than any valid machine that gives the same answers as `source`. `reduce` returns `Some` of a valid machine when `source` is valid, no row has more than 64 edges, and `source` has at most `limit` rows with `0 < limit <= 64`.

`reduce`'s cap counts table rows, reachable or not: it returns `None` for any table of more than 64 rows, including one whose other rows are unreachable, and for any table with a row of more than 64 edges, reachable or not. `compare`'s `limit` bounds the number of state pairs it visits; the product of row counts is only the condition under which an answer is guaranteed. The result of `reduce` can also fall outside those premises: each of its rows has an edge for every letter used by the reachable part of the source, so two states with 33 different letters each reduce to rows of 66 edges, and `compare` of that result with itself returns `Comparison_limit`. Running time and memory are not proved.

## Interface

Read `dfa_equivalence_core.mli` for the operations and laws, then
`dfa_semantics.ml` for the complete table model, execution, validity and
size limits. Search relations, reduction certificates and partition
refinement belong to `dfa_equivalence_proof.ml`.

@code testsuite/tests/vox/dfa_equivalence_core.mli

`@@ total` on a declaration marks a total function, which may appear in refinements; `@ total` on a result type marks a value at the `total` mode, which total code may consume. `===` is logical equality. `Ghost.t` wraps a value that is erased at run time; `witness.ghost` reads it in specifications. `Bigint` is unbounded integers, used for row counts. The interface declares a module `Dfa_equivalence`, and `dfa_semantics.ml` a module `Dfa_semantics`, because the tests load the files into the toplevel with `#use`. The contracts use these definitions:

@code testsuite/tests/vox/dfa_semantics.ml

Language equality means agreement of `run` on every word. Table equality also compares state identifiers and row representation; the public client checks that renaming the single state from 0 to 7 preserves the language and yields `Equivalent`. A minimized table is specified by validity, language preservation and minimum state count; its particular identifiers and row order are permitted choices.

`let[@def]` defines a total function that refinements may mention, together with a lemma stating its defining equation. A missing state reads as a non-accepting row whose letters all lead to state 0; `valid` rules this out by requiring that the initial state and every target and default exist, and that state ids and the letters of each row are unique. On a valid table, `labels_bounded` requires at most 64 edges per row, counted by `list_size`, whose count saturates at 129. `state_size` is the number of rows. `has_state` is not used by the public contracts.

The proofs are in `dfa_equivalence_proof.ml`, and `dfa_equivalence_core.ml` restates them against the interface. `compare` runs one search over pairs of states. Along with its answer it builds, in erased code, either a relation that contains the pair of initial states and is closed under steps (related states agree on acceptance, and their successors on every letter are related), which gives `compare_equal`, or a word on which the two machines differ, which gives `comparison_witness`. `reduce` collects the reachable states, refines a partition of them until it is stable, and builds the quotient table. Its proofs use an erased certificate: such a closed relation between the source and the result, a word reaching each state of the result, and a word separating each pair of its states. The separating words come from the same pair search, run in erased code on two states of the source. `reduce_minimum` follows: in a machine with the same language, the words reaching two distinct states of the result must reach states that the separating word tells apart, so that machine has at least as many rows.

## Trusted base

- The test `dfa_boundary.ml` is the only one that compiles `dfa_equivalence_core.mli` against its implementation, and the only one that compiles `dfa_public_client.ml`. The test `dfa_equivalence.ml` checks the implementation and the proofs in `dfa_equivalence_core.ml`, without the `.mli`.
- `dfa_equivalence_proof.ml` declares three `external` aliases of the primitives `%equal` and `%greaterequal` at `int` (`equal_int`, `same_int` and `>=`). The checker gives them the meaning of `=` and `>=`.

## Scope

- Operations: `compare` and `reduce`. There is no construction API (tables are plain data), no product, complement or determinization, and no reachability or emptiness query.
- Caps: `reduce` handles at most 64 rows and `limit <= 64`; `compare` treats a `limit` above 65,536 as 65,536 and returns `Comparison_limit` for `limit <= 0`. The completeness of both requires at most 64 edges per row.
- On an invalid table, `reduce` returns `None`, and `compare` may return any result, but `Equivalent` and `Inequivalent` are still correct.
- The minimal machine is compared with valid machines only. The erased word from `comparison_witness` is not available at run time.
- Every operation and proof function is declared `total`: it terminates without raising, except by running out of memory or stack. Time, memory and stack depth are not bounded.

## Client example

From the public client. `(x : t) -> ...` names an argument so that later types can mention it, and `{u : unit | p}` is `unit` refined by the predicate `p`: returning it proves `p`. `agreement` is a proof passed as a function: for each word it returns a `unit` refined by the fact that `source` and `other` agree on that word. `ghost_ (...)` is proof code, checked and then erased, and `@ total` marks a total function, one that terminates without raising or touching mutable state.

@code testsuite/tests/vox/dfa_public_client.ml "let (minimum @ total)" "  ()"

## A rejected program

Claiming that any two machines agree on a word is a type error. The test `dfa_boundary.ml` checks this program against the public interfaces only and requires this error:

@code testsuite/tests/vox/dfa_boundary.ml "(* Two machines need not agree on a word. *)" "|}]"

## Reproduce

After `make install` and `./dev init`, from the repository root:

```
./dev test vox/dfa_equivalence.ml vox/dfa_boundary.ml
```

The test checks the implementation and its proofs and runs comparison and minimization examples with exact and insufficient limits and default edges, and checks hand-written reduction certificates with `Dfa_proof.check_reduction`. `dfa_boundary.ml` compiles all DFA and regex modules with `-opaque` with both compilers, compiles `dfa_public_client.ml` against the public DFA interface only, links and runs it, and requires five programs to fail with their exact errors: this one, three that name hidden functions or modules, and one from the regex demo. It also follows the named calls in the `-drawlambda` output to check that `compare` and `reduce` do not reach a fixed list of certificate-building functions, and checks that the public proof functions make no calls.
