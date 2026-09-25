(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "expression_folding.mli expression_folding.ml";
 readonly_files = "expressions.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   binary_modules = "expression_folding";
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   binary_modules = "expression_folding";
   run-expectnat;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   binary_modules = "expression_folding";
   run-expect;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   binary_modules = "expression_folding";
   run-expectnat;
   check-program-output;
 }
*)

module Expr = Expression_folding
;;
[%%expect{|
module Expr = Expression_folding
|}]

let () =
  let open Expr in
  let expression = Add (Input, Add (Lit 2, Lit 3)) in
  Format.printf "input=4 result=%d; input=10 result=%d@."
    (eval expression 4) (eval expression 10);
  let input = 4 in
  let result = eval_folded expression input in
  Format.printf "folded=%d@." result;
  let overflow = Add (Lit max_int, Lit 1) in
  let input = 0 in
  let result = eval_folded overflow input in
  Format.printf "wrapping addition preserved=%b@."
    (result = min_int)
;;
[%%expect{|
input=4 result=9; input=10 result=15
folded=9
wrapping addition preserved=true
|}]

let bad_fold (a : int) (b : int) input :
    {u : unit |
      Expr.eval (Expr.Lit (a - b)) input
      === Expr.eval (Expr.Add (Expr.Lit a, Expr.Lit b)) input} =
  let left = Expr.Lit a in
  let right = Expr.Lit b in
  let original = Expr.Add (left, right) in
  let result = Expr.Lit (a - b) in
  Expr.eval_def left input;
  Expr.eval_def right input;
  Expr.eval_def original input;
  Expr.eval_def result input;
  let u = () in
  u
;;
[%%expect{|
Line 14, characters 2-3:
14 |   u
       ^
Error: Refinement could not be proved (counterexample)
|}]

module No_descent = struct
  let rec (eval @ total) expression input =
    match expression with
    | Expr.Lit n -> n
    | Expr.Input -> input
    | Expr.Add (_, _) -> eval expression input
end
;;
[%%expect{|
Line 6, characters 25-46:
6 |     | Expr.Add (_, _) -> eval expression input
                             ^^^^^^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
