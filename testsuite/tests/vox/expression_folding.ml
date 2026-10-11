type t = Lit of int | Input | Add of t * t [@@inductive]

let[@def] rec eval expression input =
  match expression with
  | Lit n -> n
  | Input -> input
  | Add (left, right) -> eval left input + eval right input

let[@def] add left right : t =
  match left, right with
  | Lit a, Lit b -> Lit (a + b)
  | Lit 0, _ -> right
  | _, Lit 0 -> left
  | _ -> Add (left, right)

let add_correct left right input :
    {u : unit |
      eval (add left right) input === eval left input + eval right input} =
  let result = add left right in
  add_def left right;
  eval_def left input;
  eval_def right input;
  eval_def result input

let[@def] rec folded expression =
  match expression with
  | Lit _ | Input -> true
  | Add (left, right) -> folded left && folded right &&
    (match left, right with
     | Lit _, Lit _ | Lit 0, _ | _, Lit 0 -> false
     | _ -> true)

let add_folded left right :
    {u : unit | if folded left && folded right then
      folded (add left right) else true} =
  add_def left right;
  folded_def (add left right)

let[@def] rec fold expression : t =
  match expression with
  | Lit _ | Input -> expression
  | Add (left, right) -> add (fold left) (fold right)

let rec fold_is_folded : (expression : t) ->
    {u : unit | folded (fold expression)} =
  fun expression ->
  fold_def expression;
  match expression with
  | Lit _ | Input -> folded_def expression
  | Add (left, right) ->
    fold_is_folded left;
    fold_is_folded right;
    add_folded (fold left) (fold right)

let rec fold_correct :
    (expression : t) -> (input : int) ->
    {u : unit | eval (fold expression) input === eval expression input} =
  fun expression input ->
  fold_def expression;
  eval_def expression input;
  match expression with
  | Lit _ | Input -> ()
  | Add (left, right) ->
    fold_correct left input;
    fold_correct right input;
    add_correct (fold left) (fold right) input

let eval_folded expression input :
    {result : int | result === eval expression input} =
  let result = eval (fold expression) input in
  ghost_ (fold_correct expression input);
  result
