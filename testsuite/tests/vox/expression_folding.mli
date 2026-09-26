type t = Lit of int | Input | Add of t * t [@@inductive]

val eval : t -> int -> int @@ total
val eval_def : (expression : t) -> (input : int) ->
  {u : unit | eval expression input === (match expression with
    | Lit n -> n | Input -> input
    | Add (left, right) -> eval left input + eval right input)} @@ total

val fold : t @ total -> t @ total @@ total
val fold_correct : (expression : t) -> (input : int) ->
  {u : unit | eval (fold expression) input === eval expression input}
  @ immutable contended @@ total
val eval_folded : (expression : t) -> (input : int) ->
  {result : int | result === eval expression input} @@ total
