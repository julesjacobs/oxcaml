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
  add_def left right;
  eval_def left input;
  eval_def right input;
  eval_def result input;
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
  fold_def expression;
  eval_def expression input;
  let u = () in
  match expression with
  | Lit _ | Input -> refine_ u
  | Add (left, right) ->
    fold_correct left input;
    fold_correct right input;
    let left = fold left in
    let right = fold right in
    add_correct left right input;
    refine_ u

let (eval_folded @ total) (expression @ total) input :
    {result : int | result === eval expression input} =
  let (result @ total) = (eval (fold expression) input : int @ total) in
  ghost_ (fold_correct expression input);
  refine_ result
