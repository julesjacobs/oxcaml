open Vox_egraph_language_spec

let[@def] rec (same_expr @ total) (a : expr @ immutable)
    (b : expr @ immutable) =
  match a, b with
  | Int_lit x, Int_lit y -> x = y
  | Bool_lit x, Bool_lit y -> x = y
  | Int_input, Int_input | Bool_input, Bool_input -> true
  | Add (a1, a2), Add (b1, b2)
  | Eq_int (a1, a2), Eq_int (b1, b2) ->
    same_expr a1 b1 && same_expr a2 b2
  | Int_if (a1, a2, a3), Int_if (b1, b2, b3)
  | Bool_if (a1, a2, a3), Bool_if (b1, b2, b3) ->
    same_expr a1 b1 && same_expr a2 b2 && same_expr a3 b3
  | _ -> false

let rec (same_expr_correct @ total) :
    (a : expr) @ immutable -> (b : expr) @ immutable ->
    {u : unit | same_expr a b = (a === b)} @ ghost = fun a b -> ghost_ (
  same_expr_def a b;
  match a, b with
  | Add (a1, a2), Add (b1, b2)
  | Eq_int (a1, a2), Eq_int (b1, b2) ->
    same_expr_correct a1 b1;
    same_expr_correct a2 b2;
    ()
  | Int_if (a1, a2, a3), Int_if (b1, b2, b3)
  | Bool_if (a1, a2, a3), Bool_if (b1, b2, b3) ->
    same_expr_correct a1 b1;
    same_expr_correct a2 b2;
    same_expr_correct a3 b3;
    ()
  | _ -> ())

let[@def] (integer @ total) (expr : expr @ immutable) =
  match sort expr with Some Integer -> true | _ -> false

let[@def] (boolean @ total) (expr : expr @ immutable) =
  match sort expr with Some Boolean -> true | _ -> false

let (typed_eval @ total) (expr : expr @ immutable)
    (env : env @ immutable) :
    {u : unit |
      match sort expr, eval expr env with
      | Some Integer, Bool_value _ -> false
      | Some Boolean, Int_value _ -> false
      | _ -> true} =
  sort_def expr;
  eval_def expr env;
  match expr with
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> ()
  | Add _ | Eq_int _ | Int_if _ | Bool_if _ -> ()

let (add_zero_right @ total) (expr : expr @ immutable)
    (env : env @ immutable) (value : int) :
    {u : unit | eval expr env === Int_value value} ->
    {u : unit | eval (Add (expr, Int_lit 0)) env === eval expr env} =
  fun premise ->
  eval_def (Add (expr, Int_lit 0)) env;
  eval_def (Int_lit 0) env;
  as_int_def (eval expr env);
  as_int_def (eval (Int_lit 0) env);
  as_int_def (Int_value value);
  as_int_def (Int_value 0);
  ()

let (add_zero_left @ total) (expr : expr @ immutable)
    (env : env @ immutable) (value : int) :
    {u : unit | eval expr env === Int_value value} ->
    {u : unit | eval (Add (Int_lit 0, expr)) env === eval expr env} =
  fun premise ->
  eval_def (Add (Int_lit 0, expr)) env;
  eval_def (Int_lit 0) env;
  as_int_def (eval expr env);
  as_int_def (eval (Int_lit 0) env);
  as_int_def (Int_value value);
  as_int_def (Int_value 0);
  ()

let (literal_add @ total) (left : int) (right : int)
    (env : env @ immutable) :
    {u : unit |
      eval (Add (Int_lit left, Int_lit right)) env ===
        eval (Int_lit (left + right)) env} =
  eval_def (Add (Int_lit left, Int_lit right)) env;
  eval_def (Int_lit left) env;
  eval_def (Int_lit right) env;
  eval_def (Int_lit (left + right)) env;
  as_int_def (Int_value left);
  as_int_def (Int_value right);
  ()

let (add_associative @ total) (left : expr @ immutable)
    (middle : expr @ immutable) (right : expr @ immutable)
    (env : env @ immutable) (a : int) (b : int) (c : int) :
    {u : unit | eval left env === Int_value a &&
      eval middle env === Int_value b &&
      eval right env === Int_value c} ->
    {u : unit |
      eval (Add (Add (left, middle), right)) env ===
        eval (Add (left, Add (middle, right))) env} =
  fun premise ->
  let lm = Add (left, middle) in
  let mr = Add (middle, right) in
  eval_def (Add (lm, right)) env;
  eval_def (Add (left, mr)) env;
  eval_def lm env;
  eval_def mr env;
  as_int_def (eval left env);
  as_int_def (eval middle env);
  as_int_def (eval right env);
  as_int_def (Int_value a);
  as_int_def (Int_value b);
  as_int_def (Int_value c);
  as_int_def (eval lm env);
  as_int_def (eval mr env);
  ()

let (eq_reflexive @ total) (expr : expr @ immutable)
    (env : env @ immutable) :
    {u : unit | eval (Eq_int (expr, expr)) env === Bool_value true} =
  eval_def (Eq_int (expr, expr)) env;
  ()

let (int_if_true @ total) (yes : expr @ immutable)
    (no : expr @ immutable) (env : env @ immutable) (value : int) :
    {u : unit | eval yes env === Int_value value} ->
    {u : unit |
      eval (Int_if (Bool_lit true, yes, no)) env === eval yes env} =
  fun premise ->
  eval_def (Int_if (Bool_lit true, yes, no)) env;
  eval_def (Bool_lit true) env;
  as_bool_def (Bool_value true);
  as_int_def (eval yes env);
  as_int_def (Int_value value);
  ()

let (int_if_false @ total) (yes : expr @ immutable)
    (no : expr @ immutable) (env : env @ immutable) (value : int) :
    {u : unit | eval no env === Int_value value} ->
    {u : unit |
      eval (Int_if (Bool_lit false, yes, no)) env === eval no env} =
  fun premise ->
  eval_def (Int_if (Bool_lit false, yes, no)) env;
  eval_def (Bool_lit false) env;
  as_bool_def (Bool_value false);
  as_int_def (eval no env);
  as_int_def (Int_value value);
  ()

let (int_if_same @ total) (condition : expr @ immutable)
    (branch : expr @ immutable) (env : env @ immutable) (value : int) :
    {u : unit | eval branch env === Int_value value} ->
    {u : unit |
      eval (Int_if (condition, branch, branch)) env ===
        eval branch env} =
  fun premise ->
  eval_def (Int_if (condition, branch, branch)) env;
  as_int_def (eval branch env);
  as_int_def (Int_value value);
  ()

let (bool_if_true @ total) (yes : expr @ immutable)
    (no : expr @ immutable) (env : env @ immutable) (value : bool) :
    {u : unit | eval yes env === Bool_value value} ->
    {u : unit |
      eval (Bool_if (Bool_lit true, yes, no)) env === eval yes env} =
  fun premise ->
  eval_def (Bool_if (Bool_lit true, yes, no)) env;
  eval_def (Bool_lit true) env;
  as_bool_def (Bool_value true);
  as_bool_def (eval yes env);
  as_bool_def (Bool_value value);
  ()

let (bool_if_false @ total) (yes : expr @ immutable)
    (no : expr @ immutable) (env : env @ immutable) (value : bool) :
    {u : unit | eval no env === Bool_value value} ->
    {u : unit |
      eval (Bool_if (Bool_lit false, yes, no)) env === eval no env} =
  fun premise ->
  eval_def (Bool_if (Bool_lit false, yes, no)) env;
  eval_def (Bool_lit false) env;
  as_bool_def (Bool_value false);
  as_bool_def (eval no env);
  as_bool_def (Bool_value value);
  ()

let (bool_if_same @ total) (condition : expr @ immutable)
    (branch : expr @ immutable) (env : env @ immutable) (value : bool) :
    {u : unit | eval branch env === Bool_value value} ->
    {u : unit |
      eval (Bool_if (condition, branch, branch)) env ===
        eval branch env} =
  fun premise ->
  eval_def (Bool_if (condition, branch, branch)) env;
  as_bool_def (eval branch env);
  as_bool_def (Bool_value value);
  ()
