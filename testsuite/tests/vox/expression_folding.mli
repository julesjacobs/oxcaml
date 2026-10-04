type t = Lit of int | Input | Add of t * t [@@inductive]

val eval : t -> int -> int @@ total
val eval_def : (expression : t) -> (input : int) ->
  {u : unit | eval expression input === (match expression with
    | Lit n -> n | Input -> input
    | Add (left, right) -> eval left input + eval right input)} @@ total

val folded : t -> bool @@ total
val folded_def : (expression : t) ->
  {u : unit | folded expression === (match expression with
    | Lit _ | Input -> true
    | Add (left, right) -> folded left && folded right &&
      (match left, right with
       | Lit _, Lit _ | Lit 0, _ | _, Lit 0 -> false
       | _ -> true))} @@ total

val fold : t -> t @@ total
val fold_is_folded : (expression : t) ->
  {u : unit | folded (fold expression)} @@ total
val fold_correct : (expression : t) -> (input : int) ->
  {u : unit | eval (fold expression) input === eval expression input}
  @@ total
val eval_folded : (expression : t) -> (input : int) ->
  {result : int | result === eval expression input} @@ total
