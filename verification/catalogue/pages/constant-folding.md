title: Constant folding
blurb: A constant folder for integer expressions proved to preserve evaluation, with `int` addition wrapping on overflow.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/expression_folding.mli — Public interface
  - testsuite/tests/vox/expression_folding.ml — Implementation and proofs
  - testsuite/tests/vox/folding_semantics_client.ml — Public-only client
  - verification/library/vox_machine_semantics.mli — `int` addition and subtraction restated over unbounded integers
  - verification/library/vox_machine_semantics.ml — Proofs of those statements
  - testsuite/tests/vox/expression_folding_rejected.ml — Rejected programs, against the interface
---
`Expression_folding` folds constants in expressions built from integer literals, one input variable and addition. The interface defines the evaluator `eval` by its recursive equation and proves `fold_correct`: for every expression and every input, `eval (fold expression) input` equals `eval expression input`. Addition is OCaml's 63-bit `int` addition, which wraps, so the theorem includes overflow: folding `Add (Lit max_int, Lit 1)` gives `Lit min_int`, which is also what evaluation gives. `eval`, `fold` and `eval_folded` are declared `total`, so the checker also proves that they terminate without raising. `eval_folded e i` evaluates `fold e`; its contract says only that the result equals `eval e i`.

The theorem `fold_is_folded` states that the result also satisfies `folded`: every subexpression contains no `Add` of two literals and no addition of `Lit 0` on either side. The interface defines this predicate by its recursive equation. The implementation folds bottom up and does not reassociate, so `Add (Lit 1, Add (Input, Lit 2))` is left as it is. The exact result among expressions satisfying preservation and `folded` is unspecified. For example, `Add (Input, Lit 1)` and `Add (Lit 1, Input)` are distinct folded trees with identical evaluation for every input; the public client proves this allowed alternative.

## Interface

This interface is the complete specification: the expression datatype, evaluation and `folded` equations, and the guarantees of preservation and a folded result. Folding rules and induction proofs are in `expression_folding.ml`.

@code testsuite/tests/vox/expression_folding.mli

`[@@inductive]` declares a variant whose values are finite trees, so `total` functions may recurse on their subterms. `@@ total` on a value declaration states that the function is total. The implementation carries no mode annotations: the checker infers that its functions are total and checks that against the interface.

## Trusted base

Nothing beyond the shared base. `Vox_machine_semantics` adds no assumption: its lemmas are proved from the checker's built-in meaning of `int` and `Bigint`, and only restate it.

## Scope

- Expressions have literals, a single input and addition. There is no subtraction, multiplication, variable environment or `let`.
- `fold` preserves `eval` and leaves no literal addition or addition of zero to fold. No size bound, idempotence or reassociation rule is stated.
- Overflow is covered: `eval` uses wrapping `int` addition, and the client checks `max_int + 1 = min_int` and `min_int + (-1) = max_int` through `eval_folded`.
- Stack depth is not bounded; `eval` and `fold` recurse on the expression tree, so a deep enough expression overflows the stack even though both are `total`.

## Client example

The public-only client, compiled against `expression_folding.mli`. `{result : int | p}` is `int` refined by the predicate `p`, `===` is logical equality, and `ghost_ (...)` is proof code, checked and then erased. `eval_def` is the equation of `eval` for one expression; calling it gives that equation to the solver. `M.add` is a lemma from `Vox_machine_semantics` stating that `int` addition is the mathematical sum wrapped into [-2^62, 2^62), and `Bigint` is unbounded integers. `@ total` declares a function that the checker proves terminates without raising. The assertions run when the test runs.

@code testsuite/tests/vox/folding_semantics_client.ml "open Expression_folding" "assert (eval_folded expression 39 = 42)"

## A rejected program

A wrong folding rule is a type error. This function claims that `Add (Lit a, Lit b)` may be folded to `Lit (a - b)`; the equations of `eval` do not imply it, and the solver finds a counterexample. Returning `()` asks the checker to prove the refinement of the result type. The test compiles only `expression_folding.mli`, so the function is checked against the interface's equation `eval_def`.

@code testsuite/tests/vox/expression_folding_rejected.ml "let bad_fold" "  ()"

@text testsuite/tests/vox/expression_folding_rejected.ml "Line 15, characters 2-4:" "Error: Refinement could not be proved"

The same test also rejects a `total` evaluator over `Expression_folding.t` that recurses on its own argument instead of a subexpression, and a claimed folded result containing `Add (Lit 1, Lit 2)`.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/folding_semantics_client.ml vox/expression_folding_rejected.ml
```

`folding_semantics_client.ml` compiles `vox_machine_semantics`, `expression_folding` and the client, as bytecode and as native code, and runs the assertions. `expression_folding_rejected.ml` is an expect test that compiles `expression_folding.mli` and checks the three rejections.
