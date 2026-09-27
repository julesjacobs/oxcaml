(* TEST
 has-z3;
 readonly_files = "expression_folding.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types";
 module = "expression_folding.mli";
 ocamlc.opt;
 expect;
*)

(* Programs rejected against the public interface of [Expression_folding].
   Only the interface is compiled, so each phrase must fail to type-check. *)

#directory "ocamlc.opt";;

(* A wrong folding rule: the equations of [eval] do not give it. *)
let bad_fold (a : int) (b : int) input :
    {u : unit |
      Expression_folding.eval (Expression_folding.Lit (a - b)) input
      === Expression_folding.eval
            (Expression_folding.Add
              (Expression_folding.Lit a, Expression_folding.Lit b)) input} =
  let left = Expression_folding.Lit a in
  let right = Expression_folding.Lit b in
  let original = Expression_folding.Add (left, right) in
  let result = Expression_folding.Lit (a - b) in
  Expression_folding.eval_def left input;
  Expression_folding.eval_def right input;
  Expression_folding.eval_def original input;
  Expression_folding.eval_def result input;
  let u = () in
  refine_ u
;;
[%%expect{|
Line 16, characters 2-11:
16 |   refine_ u
       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 3-6, characters 6-73:
3 | ......Expression_folding.eval (Expression_folding.Lit (a - b)) input
4 |       === Expression_folding.eval
5 |             (Expression_folding.Add
6 |               (Expression_folding.Lit a, Expression_folding.Lit b)) input...
  The refinement is stated here.
|}]

(* A total evaluator must recurse on a proper subexpression. *)
module No_descent = struct
  let rec (eval @ total) expression input =
    match expression with
    | Expression_folding.Lit n -> n
    | Expression_folding.Input -> input
    | Expression_folding.Add (_, _) -> eval expression input
end
;;
[%%expect{|
Line 6, characters 39-60:
6 |     | Expression_folding.Add (_, _) -> eval expression input
                                           ^^^^^^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
