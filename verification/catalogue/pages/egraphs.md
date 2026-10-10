title: Rule-relative e-graphs
blurb: An e-graph for a small typed expression language whose `Equal` answers carry a derivation from the caller's rewrite rules, and whose `Fixed_point` result is closed under the rules and congruence.
status: owner-review
date: 4 October 2026
sources:
  - verification/library/vox_egraph_rule_handle.mli — Public interface
  - verification/library/vox_egraph_language_spec.ml — Expressions, sorts and evaluation
  - verification/library/vox_egraph_rule_spec.ml — Patterns, rules, rule validity and instantiation
  - verification/library/vox_egraph_derivation_spec.ml — Derivations and their validity
  - verification/library/vox_egraph_match_spec.ml — The graph model, class labels and pattern matching
  - verification/library/vox_egraph_snapshot_spec.ml — The expression an id stands for (`origin`)
  - verification/library/vox_egraph_preservation_spec.ml — Preservation of existing origins (`extends`)
  - verification/library/vox_egraph_closure_spec.ml — Typed assignments and closure at every root
  - verification/library/vox_egraph_quantifier.mli — Closure over all assignments
  - verification/library/vox_egraph_saturation_spec.ml — Closure under every rule
  - verification/library/vox_egraph_congruence_spec.ml — Congruence closure
  - verification/library/vox_egraph_fixedpoint_spec.ml — The meaning of `Fixed_point`
  - verification/library/vox_egraph_interpret_wrapping.mli — From derivations to equal evaluations
  - verification/library/vox_egraph_rule_handle.md — Reading order, resource limits and fuel
  - testsuite/tests/vox/egraph_rule_public.ml — Public-only client
  - testsuite/tests/vox/egraph_boundary.ml — Public-only compile of the client, rejected programs, erasure check and the declaration inventory
  - testsuite/tests/vox/egraph_rule_rejected.ml — Rejected derivations
---
`Vox_egraph_rule_handle` stores typed integer and boolean expressions under
rewrite rules supplied by the caller. `admit` returns an id for an expression;
`query` reports whether the graph proves two expressions equal. An `Equal`
answer carries an erased derivation from those rules. The rules need not
preserve evaluation: evaluation equality requires the additional interpretation
premise described below.

`saturate` applies rules and restores congruence. `Fixed_point` means the
returned graph is closed under the rules and congruence. Other results report
limits; `Not_proved` says nothing about whether an equality is derivable.
Every operation preserves the rules and the expression represented by each
existing id. These contracts hold on normal return.

## Interface

The primary contracts are followed by two observation lemmas:

@code verification/library/vox_egraph_rule_handle.mli

The graph observation stores a node and a class label per id. `same` compares
class labels. Labels may change when classes merge; `origin` reconstructs an
id's expression independently of those labels.

@code verification/library/vox_egraph_match_spec.ml "type node =" "| _ -> false"

The complete [origin definition](src:verification/library/vox_egraph_snapshot_spec.ml)
recurses through each node's children, which must have smaller ids.
`extends` preserves every old origin while allowing more ids:

@code verification/library/vox_egraph_preservation_spec.ml "let[@def] rec (origins @ total)" "before.count <= after.count && origins before after before.count)"

Equality uses familiar proof trees: reflexivity, symmetry, transitivity,
congruence and a rule instance.

@code verification/library/vox_egraph_derivation_spec.ml "type evidence =" "[@@inductive]"

The [endpoint equations](src:verification/library/vox_egraph_derivation_spec.ml)
compute the expressions at the two ends. The validity check ties each rule
step to the actual caller-supplied rule and a well-sorted substitution:

@code verification/library/vox_egraph_derivation_spec.ml "let[@def] rec (valid @ total)" "R.rule_valid rule && R.subst_valid rule.vars subst)"

[Expressions, sorts and evaluation](src:verification/library/vox_egraph_language_spec.ml)
and [patterns, rule validity and substitution](src:verification/library/vox_egraph_rule_spec.ml)
complete the equality specification. In particular, a rule's two sides must
have the same sort and every right-side variable must occur on the left.
[The interpretation theorem](src:verification/library/vox_egraph_interpret_wrapping.mli)
turns a valid derivation into equal evaluations when every rule instance
preserves evaluation.

A fixed point combines two closure properties:

@code verification/library/vox_egraph_fixedpoint_spec.ml "module R =" "R.valid rules && C.closed_rules graph rules && G.closed graph"

Rule closure says that each left-side match has a right-side match in the
same class, for every well-sorted assignment and root:

@code verification/library/vox_egraph_closure_spec.ml "let[@def] rec (closed_roots @ total)" "[@@decreases if count > 0 then count else 0]"

The complete definitions of [pattern matching](src:verification/library/vox_egraph_match_spec.ml),
[assignment validity](src:verification/library/vox_egraph_closure_spec.ml),
[all assignments](src:verification/library/vox_egraph_quantifier.mli)
and [all rules](src:verification/library/vox_egraph_saturation_spec.ml)
make these quantifiers explicit. [Congruence closure](src:verification/library/vox_egraph_congruence_spec.ml)
says that nodes with the same constructor and children in the same classes
share a class. The specification does not require the least such closure.

## Trusted base

- The node index is the flat hash table's implementation (`Vox_table_implementation.Make`), so the e-graph trusts what the [flat hash table](flat-hash-table.html) trusts: the SIMD group matching in `runtime/vox_control.c` and `backend/cmm_builtins.ml`, the storage contracts of `verification/library/vox_table_storage.mli`, and count-trailing-zeros returning 63 for zero.
- `Vox_iarray.get` and `Vox_iarray.set` (`verification/library/vox_iarray.mli`) are `external`s whose meaning is built into the checker (`verification/vox_vc.ml`); `set` is implemented in `runtime/borrow.c`.
- `Iarray.length` and `Iarray.Refined.get` (`stdlib/iarray.mli`), and the checker's built-in rule that `Iarray.init n f` has length `n`.
- `verification/library/vox_egraph_rule_handle.spec.json` records the exact text of every top-level declaration of the thirteen files a reader must trust (the specification and interface files listed above, from `vox_egraph_rule_handle.mli` to `vox_egraph_interpret_wrapping.mli`), and the trusted standard-library primitives. `egraph_boundary.ml` regenerates it from the sources and fails if it differs.

## Scope

- Operations: `create`, `admit`, `query`, `saturate`, `same_class`, the ghost observations `model` and `rules`, and the ghost theorem `model_bounds`. There is no operation that reads back a smallest or cheapest expression, no deletion and no rule added after `create`.
- At most 512 nodes, as stated by `model_bounds`. `saturate` also stops on its round limit, its rebuild-pass limit, or when its search fuel runs out; fuel counts steps of the rule search, not matching within one candidate, time or allocation, and `vox_egraph_rule_handle.md` lists what it charges.
- `Not_proved`, `Rebuild_limit` and `Round_limit` carry no information; `Invalid_input` says an operand is ill-typed (an `admit` or `query` that fails may still have added nodes for well-sorted subexpressions), and `Node_limit`, `Saturation_node_limit` and `Search_limit` say only which limit was hit.
- `Fixed_point` is closure of the returned graph under the rules and congruence. That `Not_proved` means "not derivable" is not stated.
- Integer addition in the evaluator is 63-bit and wraps.
- The search is exhaustive, not e-matching: for each rule it enumerates every assignment of node ids (or -1) to the rule's variables, tries every id as the root, and stops at the first rule application that changes the graph. A round is one rebuild followed by one such search, so each round applies at most one rewrite, and one search tries up to (count + 1)^n × count matches for a rule with n variables. Rebuilding compares every pair of nodes, and the union-find has neither path compression nor union by rank.
- The node, sort and union-find arrays are immutable arrays updated by copying, so adding a node or merging two classes copies 512-element arrays.
- The implementation and proofs are 59 `.ml` and `.mli` files, about 7,600 lines. Every file has a header comment saying what it does and where it fits.

## Client example

From the public-only client. `{proved : bool | p}` is `bool` refined by the predicate `p`, and `===` is logical equality. `@ ghost` marks an argument that is erased after checking, and `ghost_ (...)` is proof code, checked and then erased. The `interpretation` argument is a ghost function proving that both sides of every well-sorted instance of a rule evaluate to the same value in `env`. The graph is `@ unique`: `admit`, `query`, `saturate` and `same_class` consume it and return it in an unboxed record `#{...}`; the `proof` field of `query` and `same_class` results is ghost. `saturate state 20 20 100000` allows 20 rounds, 20 rebuild passes per round and 100,000 units of search fuel; a round applies at most one rewrite (see Scope).

@code testsuite/tests/vox/egraph_rule_public.ml "let interpreted_query :" "    | _ -> false"

## A rejected program

A derivation may use only the rules it is checked against. This ghost function claims that, with no rules at all, rule 0 rewriting `0` to `1` gives a valid derivation. `E.valid_def` and `R.lookup_rule_def` give the solver the definitions of `valid` and `lookup_rule` at these arguments.

@code testsuite/tests/vox/egraph_rule_rejected.ml "let (foreign_rule @ total) () :" "  ());;"

@text testsuite/tests/vox/egraph_rule_rejected.ml "Line 8, characters 2-4:" "Error: Refinement could not be proved"

The same test rejects a transitivity step whose two halves do not meet (`0 = 0` followed by `1 = 1`).

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/egraph_rule_public.ml vox/egraph_rule_rejected.ml vox/egraph_rule_saturate.ml vox/egraph_derivation.ml vox/egraph_boundary.ml
```

`egraph_rule_public.ml` compiles the library modules the e-graph needs, including the hash table, and the client, as bytecode and native code; it also checks that the graph handle has two runtime fields in native code (bytecode keeps an empty slot for the erased ghost field, so three there). `egraph_rule_rejected.ml` holds the two rejected derivations. `egraph_rule_saturate.ml` reaches a fixed point and the search, rebuild and round limits, calling the saturation module below the public interface. The public client reaches the 512-node limit. `egraph_derivation.ml` proves the interpretation premise for a concrete rule and applies `sound`.

`egraph_boundary.ml` compiles the library, compiles the public client with only the thirteen public interfaces visible, requires three programs to fail with their exact errors (forging a graph handle, creating a graph from a rule whose sides have different sorts, and claiming a fixed point after a saturation that stopped at a limit), searches the emitted Lambda of five modules for direct calls to five proof modules (a text search, not a proof of erasure), and checks the declaration inventory.
