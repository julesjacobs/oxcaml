title: Rule-relative e-graphs
blurb: An e-graph for a small typed expression language whose `Equal` answers carry a derivation from the caller's rewrite rules, and whose `Fixed_point` result is closed under the rules and congruence.
status: review-pending
date: 27 September 2026
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
`Vox_egraph_rule_handle` is an e-graph: a union-find over hash-consed expression nodes. The expressions are those of a small typed language with integer and boolean literals, one integer and one boolean input, addition, integer equality and conditionals. The graph is created with a list of rewrite rules, each a pair of patterns over typed variables, and "equal" means derivable from these rules by reflexivity, symmetry, transitivity and congruence. The rules need not agree with the language's evaluator. The interface proves that `admit` returns an id whose reconstructed expression is exactly the admitted one, and returns `None` only for an ill-typed expression or when the graph has reached 512 nodes; that `query` answers `Equal` only together with an erased derivation, valid for the rules, from the left expression to the right one; that `saturate` answers `Fixed_point` only when, for every rule and every assignment of existing nodes of the right sorts to its variables (`-1` stands for no node), a class matched by the left-hand side is also matched by the right-hand side, and all congruent nodes share a class; and that `same_class` answers exactly whether two ids have the same class label and, when they do, supplies a valid derivation between their expressions. Every operation keeps the rules and the expressions of existing ids. A separate theorem, `Vox_egraph_interpret_wrapping.sound`, turns a valid derivation into equality of evaluations, given a proof that every instance of every rule preserves evaluation.

`Not_proved`, `Rebuild_limit` and `Round_limit` promise nothing. A `query` that always answers `Not_proved` satisfies the interface, and so does a `saturate` that returns its argument with `Round_limit`. The contracts do not say that classes equal before an operation remain equal after it, nor that every id below the node count has a class label, so a client cannot prove that `same_class state a a` is true. `create`, `admit`, `query` and `saturate` are not declared total; their contracts hold on normal return. A fixed point describes the returned graph only: a later `admit` or `query` can add nodes and end it.

## Client example

From the public-only client. `{proved : bool | p}` is `bool` refined by the predicate `p`, and `===` is logical equality. `@ ghost` marks an argument that is erased after checking, and `ghost_ (...)` is proof code, checked and then erased. The `interpretation` argument is a ghost function proving that both sides of every rule instance evaluate to the same value in `env`. The graph is `@ unique`: `admit`, `query`, `saturate` and `same_class` consume it and return it in an unboxed record `#{...}`; the `proof` field of `query` and `same_class` results is ghost. `saturate state 20 20 100000` allows 20 rounds, 20 rebuild passes per round and 100,000 units of search fuel.

@code testsuite/tests/vox/egraph_rule_public.ml "let interpreted_query :" "    | _ -> false"

## A rejected program

A derivation may use only the rules it is checked against. This ghost function claims that, with no rules at all, rule 0 rewriting `0` to `1` gives a valid derivation. `E.valid_def` and `R.lookup_rule_def` give the solver the definitions of `valid` and `lookup_rule` at these arguments.

@code testsuite/tests/vox/egraph_rule_rejected.ml "let (foreign_rule @ total) () :" "  ());;"

@text testsuite/tests/vox/egraph_rule_rejected.ml "Line 8, characters 2-4:" "Error: Refinement could not be proved"

The same test rejects a transitivity step whose two halves do not meet (`0 = 0` followed by `1 = 1`).

## Interface

@code verification/library/vox_egraph_rule_handle.mli

`Q.graph` is the model: a node count and arrays of nodes and class labels. `Snapshot.origin graph id` rebuilds the expression that `id` stands for from the nodes below it. `Preserves.extends before after` says the count has not decreased and every existing id keeps its origin. `E.valid rules proof` checks a derivation, and `E.left` and `E.right` are its two ends. `Fixed.fixed` is the closure property described above. `R.valid` requires each rule's sides to have the same sort and every right-hand-side variable to occur on the left. `@ local immutable` on `model` and `rules` means they read a borrowed graph, and `@@ aliased` on `id` means that field is shared. In `preserved_origin` and in the interpretation theorem below, an argument of type `{u : unit | p}` is a premise: the caller passes `()` and must prove `p`.

@code verification/library/vox_egraph_interpret_wrapping.mli

## Trusted base

- The node index is the flat hash table's implementation (`Vox_table_implementation.Make`), so the e-graph trusts what the [flat hash table](flat-hash-table.html) trusts: the SIMD group matching in `runtime/vox_control.c` and `backend/cmm_builtins.ml`, the storage contracts of `verification/library/vox_table_storage.mli`, and count-trailing-zeros returning 63 for zero.
- `Vox_iarray.get` and `Vox_iarray.set` (`verification/library/vox_iarray.mli`) are `external`s whose meaning is built into the checker (`verification/vox_vc.ml`); `set` is implemented in `runtime/borrow.c`.
- `Iarray.length` and `Iarray.Refined.get` (`stdlib/iarray.mli`), and the checker's built-in rule that `Iarray.init n f` has length `n`.
- `verification/library/vox_egraph_rule_handle.spec.json` records the exact text of every top-level declaration of the thirteen files a reader must trust (the eleven semantic and interface files listed above and `vox_egraph_closure_spec.ml` and `vox_egraph_quantifier.mli`), and the trusted standard-library primitives. `egraph_boundary.ml` regenerates it from the sources and fails if it differs.

## Scope

- Operations: `create`, `admit`, `query`, `saturate`, `same_class`, and the ghost observations `model` and `rules`. There is no operation that reads back a smallest or cheapest expression, no deletion and no rule added after `create`.
- At most 512 nodes; this is an implementation limit, and the interface does not state that the model's count stays at or below 512. `saturate` also stops on its round limit, its rebuild-pass limit, or when its search fuel runs out; fuel counts steps of the rule search, not matching within one candidate, time or allocation, and `vox_egraph_rule_handle.md` lists what it charges.
- `Not_proved`, `Rebuild_limit` and `Round_limit` carry no information; `Invalid_input` says an operand is ill-typed, and `Node_limit`, `Saturation_node_limit` and `Search_limit` say only which limit was hit.
- `Fixed_point` is closure of the returned graph under the rules and congruence. That `Not_proved` means "not derivable" is not stated.
- Integer addition in the evaluator is 63-bit and wraps.
- The node, sort and union-find arrays are immutable arrays updated by copying, so adding a node or merging two classes copies 512-element arrays.
- The implementation and proofs are 59 `.ml` and `.mli` files, about 7,100 lines, with one comment.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/egraph_rule_public.ml vox/egraph_rule_rejected.ml vox/egraph_rule_saturate.ml vox/egraph_derivation.ml vox/egraph_boundary.ml
```

`egraph_rule_public.ml` compiles the library modules the e-graph needs, including the hash table, and the client, as bytecode and native code; it also checks that the graph handle has two runtime fields in native code. `egraph_rule_rejected.ml` holds the two rejected derivations. `egraph_rule_saturate.ml` reaches a fixed point and the search, rebuild and round limits, calling the saturation module below the public interface. The public client reaches the 512-node limit. `egraph_derivation.ml` proves the interpretation premise for a concrete rule and applies `sound`.

`egraph_boundary.ml` compiles the library, compiles the public client with only the thirteen public interfaces visible, requires three programs to fail with their exact errors (forging a graph handle, creating a graph from a rule whose sides have different sorts, and claiming a fixed point after a saturation that stopped at a limit), searches the emitted Lambda of five modules for calls to five proof modules, and checks the declaration inventory.
