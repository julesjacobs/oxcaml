title: Quantifier elimination over a three-element chain
blurb: Quantifier elimination by enumeration for formulas over Global < Regional < Local, proved to preserve their meaning in every environment; it is not connected to OxCaml's mode solver.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/mode_solver_public.mli — Public interface
  - testsuite/tests/vox/mode_solver_semantics.ml — The chain, terms, formulas, their evaluation and scoping
  - testsuite/tests/vox/mode_solver_graph_semantics.ml — Constraint graphs and existential projection of them
  - testsuite/tests/vox/mode_solver_guarded_semantics.ml — Guarded quantifier prefixes
  - testsuite/tests/vox/mode_solver_three_qe_proof.ml — Substitution, `eliminate` and its proofs
  - testsuite/tests/vox/mode_solver_graph_qe_proof.ml — Graph projection and graph subsumption
  - testsuite/tests/vox/mode_solver_retained_symbolic.ml — Guarded projection
  - testsuite/tests/vox/mode_solver_public.ml — The public operations, defined from the three modules above
  - testsuite/tests/vox/mode_solver_public_client.ml — Public-only client
---
The formulas are first-order formulas over the three-element chain Global < Regional < Local, the values of OxCaml's regionality mode axis. Terms are constants, variables (de Bruijn indices), join, meet, the map that sends Regional to Global, and arbitrary lookup tables from the chain to itself; atoms are `≤` between terms. `Mode_solver_public.eliminate` turns a formula with quantifiers into a quantifier-free one, and `eliminate_exact` proves that the two have the same truth value in every environment. The other operations are built on it: deciding closed formulas, projecting variables out of a list of inequalities, reducing a subsumption check `∀x. guard → ∃y. obligation` to a quantifier-free formula, and projecting a guarded prefix of quantifiers. Each comes with a theorem stating its result's truth value exactly in terms of the definitions in the three semantics modules.

This is a reference procedure, not OxCaml's mode solver: nothing in the compiler uses it, and no theorem relates it to `typing/solver.ml`. It handles one axis. Elimination is by enumeration: a quantifier over a formula becomes the disjunction (or conjunction) of three copies with the variable replaced by each value, so the output is exponential in the nesting depth of quantifiers, and nothing simplifies it. Lookup tables need not be monotone, so the terms are more general than mode morphisms. Unbound variables evaluate to `Global`; the decision procedures return `None` for a formula with an unbound variable.

## Client example

The public-only client. `{answer : bool | p}` is `bool` refined by the predicate `p`, `ghost_ (...)` is proof code, checked and then erased, and `@ total` declares a function that the checker proves terminates without raising. `formula_value` evaluates a formula by eliminating its quantifiers, and its type states that the answer is the formula's truth value. In the assertions, `Var 0` is the innermost bound variable, so `equality` says that the two innermost variables are equal: every value has an equal value, but no value is equal to every value.

@code testsuite/tests/vox/mode_solver_public_client.ml "open Mode_solver_semantics" "assert (not (guarded_value"

## Interface

The meaning of terms and formulas:

@code testsuite/tests/vox/mode_solver_semantics.ml "type term : immutable_data =" "&& eval (Local :: env) a"

`scoped depth f` in the same file holds when every variable of `f` is bound by a quantifier in `f` or is one of `depth` outer variables, and `subsumption_formula guard obligation` is `∀x. ¬guard ∨ ∃y. obligation`. `Mode_solver_graph_semantics` defines a graph as a list of inequalities between terms, `models env graph` as all of them holding, and `models_exists count env graph` as `models` holding for some values of `count` further variables; `count` is a `unit list` used as a natural number. `Mode_solver_guarded_semantics` defines the two-player reading of a quantifier prefix whose variables must satisfy a guard (`admissible`, `game`, `normalized_game`).

The public interface. It is written in the form the compiler prints: names from other modules carry their module path, and `@ total stateful` on each theorem is a mode annotation on its partial applications, which does not change the statement.

@code testsuite/tests/vox/mode_solver_public.mli

## Trusted base

Nothing beyond the shared base.

## Scope

- Nine operations: `eliminate`, `decide_checked`, `project_graph`, `decide_graph_checked`, `subsumption_residual`, `assert_subsumption`, `project_guarded`, `project_admissible` and `assert_graph_subsumption`. Each has an exact truth-value theorem. `regionality_adjunction` is a fact about the chain, not about an operation.
- Output size is exponential in the nesting depth of quantifiers; running time is not stated.
- `assert_subsumption` and `project_guarded` have no theorem about which variables their results mention; the other operations do.
- `project_scopes_compose` states that two ways of building a `project_guarded` result give the same syntax tree (`===`), not just the same truth value.
- `assert_graph_subsumption_exact` is stated as a nine-case expansion instead of through a named predicate.
- The theorems return their refined `unit` `@ ghost`: their bodies are erased, though each still compiles to a small function that returns a placeholder.
- There is no rejection test: no test checks that a false claim about these operations fails to check.
- The directory also holds 22 other `mode_solver_*.ml` tests from earlier experiments, some with their own `elt` and `term` types. They are not part of this interface.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/mode_solver_public_client.ml vox/mode_solver_retained_symbolic.ml
```

`mode_solver_public_client.ml` compiles the three semantics modules, the three proof modules, the public interface and implementation, and the client, as native code, and runs the assertions. `mode_solver_retained_symbolic.ml` checks the guarded-projection proofs on their own, also as native code.
