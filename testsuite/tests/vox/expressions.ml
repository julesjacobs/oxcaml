(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Expr = struct
  type t = Lit of int | Input | Add of t * t [@@inductive]

  let rec (eval @ total) expression input =
    match expression with
    | Lit n -> n
    | Input -> input
    | Add (left, right) -> eval left input + eval right input
end
;;
[%%expect{|
module Expr :
  sig
    type t = Lit of int | Input | Add of t * t
    [@@inductive]
    val eval : t -> int -> int
  end
|}]

let () =
  let open Expr in
  let expression = Add (Input, Add (Lit 2, Lit 3)) in
  Format.printf "input=4 result=%d; input=10 result=%d@."
    (eval expression 4) (eval expression 10)
;;
[%%expect{|
input=4 result=9; input=10 result=15
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
