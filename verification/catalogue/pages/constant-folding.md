title: Constant folding
blurb: A constant folder for integer expressions proved to preserve evaluation, with `int` addition wrapping on overflow.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/expression_folding.mli — Public interface
  - testsuite/tests/vox/expression_folding.ml — Implementation and proofs
  - testsuite/tests/vox/folding_semantics_client.ml — Public-only client
  - verification/library/vox_machine_semantics.mli — `int` addition and subtraction restated over unbounded integers
  - verification/library/vox_machine_semantics.ml — Proofs of those statements
  - testsuite/tests/vox/expressions.ml — A copy of the module, with two rejected programs
---
`Expression_folding` folds constants in expressions built from integer literals, one input variable and addition. The interface defines the evaluator `eval` by its recursive equation and proves `fold_correct`: for every expression and every input, `eval (fold expression) input` equals `eval expression input`. Addition is OCaml's 63-bit `int` addition, which wraps, so the theorem includes overflow: folding `Add (Lit max_int, Lit 1)` gives `Lit min_int`, which is also what evaluation gives. `eval`, `fold` and `eval_folded` are declared `total`, so the checker also proves that they terminate without raising.

The interface specifies only that `fold` preserves the value. It says nothing about the result's shape: a `fold` that returns its argument unchanged satisfies it. The implementation, which a client cannot see, replaces an `Add` of two literals by their sum and drops an added `Lit 0`, bottom up; it does not reassociate, so `Add (Lit 1, Add (Input, Lit 2))` is left as it is.

## Client example

The public-only client, compiled against `expression_folding.mli`. `{result : int | p}` is `int` refined by the predicate `p`, `===` is logical equality, and `ghost_ (...)` is proof code, checked and then erased. `eval_def` is the equation of `eval` for one expression; calling it gives that equation to the solver. `M.add` is a lemma from `Vox_machine_semantics` stating that `int` addition is the mathematical sum wrapped into [-2^62, 2^62), and `Bigint` is unbounded integers. `@ total` declares a function that the checker proves terminates without raising. The assertions run when the test runs.

@code testsuite/tests/vox/folding_semantics_client.ml "open Expression_folding" "assert (eval_folded expression 39 = 42)"

## A rejected program

A wrong folding rule is a type error. This function claims that `Add (Lit a, Lit b)` may be folded to `Lit (a - b)`; the equations of `eval` do not imply it, and the solver finds a counterexample. Returning `()` asks the checker to prove the refinement of the result type. The test runs it against `Expr`, a copy of `Expression_folding` in the same file.

@code testsuite/tests/vox/expressions.ml "let bad_fold" "  ()"

@text testsuite/tests/vox/expressions.ml "Line 13, characters 2-4:" "Error: Refinement could not be proved"

The same file also rejects an `eval` that recurses on its own argument instead of a subexpression, because it cannot be proved to terminate.

## Interface

@code testsuite/tests/vox/expression_folding.mli

`[@@inductive]` declares a variant whose values are finite trees, so `total` functions may recurse on their subterms. `@@ total` on a value declaration states that the function is total. The implementation carries no mode annotations: the checker infers that its functions are total and checks that against the interface.

## Trusted base

Nothing beyond the shared base. `Vox_machine_semantics` adds no assumption: its lemmas are proved from the checker's built-in meaning of `int` and `Bigint`, and only restate it.

## Scope

- Expressions have literals, a single input and addition. There is no subtraction, multiplication, variable environment or `let`.
- The only theorem about `fold` is preservation of `eval`. That the result is smaller or contains no foldable `Add` is not stated.
- Overflow is covered: `eval` uses wrapping `int` addition, and the client checks `max_int + 1 = min_int` and `min_int + (-1) = max_int` through `eval_folded`.
- `expressions.ml` repeats the whole implementation as `Expr` instead of using `Expression_folding`. Its rejected programs therefore run against the copy.
- Stack depth is not bounded; `eval` and `fold` recurse on the expression tree, so a deep enough expression overflows the stack even though both are `total`.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/folding_semantics_client.ml vox/expressions.ml
```

`folding_semantics_client.ml` compiles `vox_machine_semantics`, `expression_folding` and the client, as bytecode and as native code, and runs the assertions. `expressions.ml` is an expect test containing the copy and the two rejected programs.
