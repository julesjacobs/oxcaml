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

  let[@def] rec (eval @ total) expression input =
    match expression with
    | Lit n -> n
    | Input -> input
    | Add (left, right) -> eval left input + eval right input

  let[@def] add (left @ total) (right @ total) : t @ total =
    match left, right with
    | Lit a, Lit b -> Lit (a + b)
    | Lit 0, _ -> right
    | _, Lit 0 -> left
    | _ -> Add (left, right)

  let (add_correct @ total) (left @ total) (right @ total) input :
      {u : unit |
        eval (add left right) input === eval left input + eval right input} =
    let result = add left right in
    let refine_ equation = add_def left right in
    let refine_ equation = eval_def left input in
    let refine_ equation = eval_def right input in
    let refine_ equation = eval_def result input in
    let u = () in
    match left, right with
    | Lit _, Lit _ -> refine_ u
    | Lit 0, _ -> refine_ u
    | _, Lit 0 -> refine_ u
    | _ -> refine_ u

  let[@def] rec fold (expression @ total) : t @ total =
    match expression with
    | Lit _ | Input -> expression
    | Add (left, right) -> add (fold left) (fold right)

  let rec (fold_correct @ total) :
      (expression : t) -> (input : int) ->
      {u : unit | eval (fold expression) input === eval expression input}
        @ immutable contended =
    fun expression input ->
    let refine_ equation = fold_def expression in
    let refine_ equation = eval_def expression input in
    let u = () in
    match expression with
    | Lit _ | Input -> refine_ u
    | Add (left, right) ->
      let refine_ left_proof = fold_correct left input in
      let refine_ right_proof = fold_correct right input in
      let left = fold left in
      let right = fold right in
      let refine_ local_proof = add_correct left right input in
      refine_ u

  let (eval_folded @ total) (expression @ total) input :
      {result : int | result === eval expression input} =
    let (result @ total) = (eval (fold expression) input : int @ total) in
    let refine_ proof = ghost_ (fold_correct expression input) in
    refine_ result
end
;;
[%%expect{|
module Expr :
  sig
    type t = Lit of int | Input | Add of t * t
    [@@inductive]
    val eval : t -> int -> int
    val eval_def :
      (expression : t) ->
      (input : int) ->
      {u : unit
        | (eval expression input) ===
            (match expression with
             | Lit n -> n
             | Input -> input
             | Add (left, right) -> (eval left input) + (eval right input))}
    val add : t @ total -> t @ total -> t @ total
    val add_def :
      (left : t) ->
      (right : t) ->
      {u : unit
        | (add left right) ===
            (match (left, right) with
             | (Lit a, Lit b) -> Lit (a + b)
             | (Lit 0, _) -> right
             | (_, Lit 0) -> left
             | _ -> Add (left, right) : t)}
    val add_correct :
      (left : t) ->
      (right : t) ->
      (input : int) ->
      {u : unit
        | (eval (add left right) input) ===
            ((eval left input) + (eval right input))}
    val fold : t @ total -> t @ total
    val fold_def :
      (expression : t) ->
      {u : unit
        | (fold expression) ===
            (match expression with
             | Lit _ | Input -> expression
             | Add (left, right) -> add (fold left) (fold right) : t)}
    val fold_correct :
      (expression : t) ->
      ((input : int) ->
       {u : unit
         | (eval (fold expression) input) === (eval expression input)} @ immutable) @ total
      stateful
    val eval_folded :
      (expression : t) ->
      (input : int) -> {result : int | result === (eval expression input)}
  end
|}]

let () =
  let open Expr in
  let expression = Add (Input, Add (Lit 2, Lit 3)) in
  Format.printf "input=4 result=%d; input=10 result=%d@."
    (eval expression 4) (eval expression 10);
  let input = 4 in
  let refine_ result = eval_folded expression input in
  Format.printf "folded=%d@." result;
  let overflow = Add (Lit max_int, Lit 1) in
  let input = 0 in
  let refine_ result = eval_folded overflow input in
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
  let refine_ equation = Expr.eval_def left input in
  let refine_ equation = Expr.eval_def right input in
  let refine_ equation = Expr.eval_def original input in
  let refine_ equation = Expr.eval_def result input in
  let u = () in
  refine_ u
;;
[%%expect{|
Line 14, characters 2-11:
14 |   refine_ u
       ^^^^^^^^^
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
