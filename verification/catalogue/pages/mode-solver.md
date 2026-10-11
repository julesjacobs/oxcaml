title: Quantifier elimination over a three-element chain
blurb: Quantifier elimination by enumeration for formulas over Global < Regional < Local, proved to preserve their meaning in every environment; it is not connected to OxCaml's mode solver.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/mode_solver_public.mli — Public interface
  - testsuite/tests/vox/mode_solver_semantics.ml — The chain, terms, formulas, their evaluation and scoping
  - testsuite/tests/vox/mode_solver_semantics.mli — The same definitions as checked equations
  - testsuite/tests/vox/mode_solver_graph_semantics.ml — Constraint graphs and existential projection of them
  - testsuite/tests/vox/mode_solver_graph_semantics.mli — The same definitions as checked equations
  - testsuite/tests/vox/mode_solver_guarded_semantics.ml — Guarded quantifier prefixes
  - testsuite/tests/vox/mode_solver_guarded_semantics.mli — The same definitions as checked equations
  - testsuite/tests/vox/mode_solver_three_qe_proof.ml — Substitution, `eliminate` and its proofs
  - testsuite/tests/vox/mode_solver_graph_qe_proof.ml — Graph projection and graph subsumption
  - testsuite/tests/vox/mode_solver_retained_symbolic.ml — Guarded projection
  - testsuite/tests/vox/mode_solver_public.ml — The public operations, defined from the three modules above
  - testsuite/tests/vox/mode_solver_public_rejected.ml — Rejected public claims
  - testsuite/tests/vox/mode_solver_public_client.ml — Public-only client
---
The formulas are first-order formulas over the three-element chain Global < Regional < Local, the values of OxCaml's regionality mode axis. Terms are constants, variables (de Bruijn indices), join, meet, the map that sends Regional to Global, and arbitrary lookup tables from the chain to itself; atoms are `≤` between terms. `Mode_solver_public.eliminate` turns a formula with quantifiers into a quantifier-free one, and `eliminate_exact` proves that the two have the same truth value in every environment. The other operations are built on it: deciding closed formulas, projecting variables out of a list of inequalities, reducing a subsumption check `∀x. guard → ∃y. obligation` to a quantifier-free formula, and projecting a guarded prefix of quantifiers. Each comes with a theorem stating its result's truth value exactly in terms of the definitions in the three semantics modules.

This is a reference procedure, not OxCaml's mode solver: nothing in the compiler uses it, and no theorem relates it to `typing/solver.ml`. It handles one axis. Elimination is by enumeration: a quantifier over a formula becomes the disjunction (or conjunction) of three copies with the variable replaced by each value, so the output is exponential in the nesting depth of quantifiers, and nothing simplifies it. Lookup tables need not be monotone, so the terms are more general than mode morphisms. Unbound variables evaluate to `Global`; the decision procedures return `None` for a formula with an unbound variable.

## Interface

The specification consists of `mode_solver_public.mli` and the three
semantics modules below. Their `.mli` files expose the same definitions as
checked equations. Quantifier-elimination algorithms, substitution lemmas
and the proofs of these laws stay in the implementation modules.

The meaning of terms and formulas:

@code testsuite/tests/vox/mode_solver_semantics.ml

`scoped depth f` in the same file holds when every variable of `f` is bound by a quantifier in `f` or is one of `depth` outer variables, and `subsumption_formula guard obligation` is `∀x. ¬guard ∨ ∃y. obligation`. `Mode_solver_graph_semantics` defines a graph as a list of inequalities between terms, `models env graph` as all of them holding, and `models_exists count env graph` as `models` holding for some values of `count` further variables; `subsumes` means that every value satisfying the guard has a value satisfying the obligations; `count` is a `unit list` used as a natural number. `Mode_solver_guarded_semantics` defines guarded quantifier prefixes.
`admissible` asks whether any assignment satisfies the guard. At each move,
`game` considers only values that leave the guard satisfiable; a universal
move with no admissible values wins vacuously. With an empty prefix, `game`
is just the witness's truth value. `normalized_game` additionally requires
`admissible`, so an impossible guard makes it false.

@code testsuite/tests/vox/mode_solver_graph_semantics.ml

@code testsuite/tests/vox/mode_solver_guarded_semantics.ml

The public interface, which opens the three semantics modules. The operations are abstract: the interface exports none of their definitions, so a client knows each result only through its theorems. `@@ total` declares each function total, which lets the specifications below it apply the function (the specifications of `eliminate_exact` and `eliminate_scoped` mention `eliminate`, for example).

@code testsuite/tests/vox/mode_solver_public.mli

## Trusted base

Nothing beyond the shared base.

## Scope

- Nine operations: `eliminate`, `decide_checked`, `project_graph`, `decide_graph_checked`, `subsumption_residual`, `assert_subsumption`, `project_guarded`, `project_admissible` and `assert_graph_subsumption`. Each has an exact truth-value theorem. Equivalent output formulas may have different syntax; the client checks `p` and `And (p, p)` as an allowed alternative. `project_scopes_compose` additionally promises syntax equality for its two constructions. `regionality_adjunction` is a fact about the chain, not about an operation.
- Output size is exponential in the nesting depth of quantifiers; running time is not stated.
- The final interface section collects the scoping and composition theorems. Scoping bounds the free variables of elimination, graph projection, subsumption residuals, graph assertions and admissible projection.
- `project_scopes_compose` states that two ways of building a `project_guarded` result give the same syntax tree (`===`), not just the same truth value.
- The theorems return their refined `unit` `@ ghost`: their bodies are erased, though each still compiles to a small function that returns a placeholder.
- `mode_solver_public_rejected.ml` rejects a fixed-syntax claim for elimination and a true answer for a closed false inequality.
- The directory also holds 22 other `mode_solver_*.ml` tests from earlier experiments, some with their own `elt` and `term` types. They are not part of this interface.

## Client example

The public-only client. `{answer : bool | p}` is `bool` refined by the predicate `p`, `ghost_ (...)` is proof code, checked and then erased, and `@ total` declares a function that the checker proves terminates without raising. `formula_value` evaluates a formula by eliminating its quantifiers, and its type states that the answer is the formula's truth value. In the assertions, `Var 0` is the innermost bound variable, so `equality` says that the two innermost variables are equal: every value has an equal value, but no value is equal to every value.

@code testsuite/tests/vox/mode_solver_public_client.ml "open Mode_solver_semantics" "assert (not (guarded_value"

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/mode_solver_public_client.ml vox/mode_solver_public_rejected.ml vox/mode_solver_retained_symbolic.ml
```

`./dev test` first builds the modules that the tests list as prebuilt (the three semantics modules and their interfaces, the three proof modules, and the public interface and implementation) with both compilers, checking every proof. `mode_solver_public_client.ml` then compiles the client against them as native code and runs the assertions. `mode_solver_retained_symbolic.ml` checks the guarded-projection proofs on their own, also as native code.
