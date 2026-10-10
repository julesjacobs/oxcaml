(* The expression language of the e-graph; the first file of the trusted
   specification (vox_egraph_rule_handle.md gives the reading order). Two
   sorts, integers and booleans. [expr] has literals, one integer and one
   boolean input ([Int_input] and [Bool_input], read from [env]), addition,
   integer equality and a conditional at each sort. [sort] is an
   expression's sort, or [None] if it is ill-sorted. [eval] is total on all
   expressions: [as_int] and [as_bool] turn a value of the wrong sort into
   0 and [false], so most lemmas about [eval] assume that a subexpression
   has a value of the right sort. [+] is OCaml's 63-bit addition and wraps.
   The e-graph's equality is derivability from the caller's rules
   (Vox_egraph_derivation_spec), not equality under [eval]; [eval] enters
   only through [Vox_egraph_interpret_wrapping.sound]. *)

type sort = Integer | Boolean
type value = Int_value of int | Bool_value of bool
type expr =
  | Int_lit of int
  | Bool_lit of bool
  | Int_input
  | Bool_input
  | Add of expr * expr
  | Eq_int of expr * expr
  | Int_if of expr * expr * expr
  | Bool_if of expr * expr * expr
[@@inductive]

type env = { int_input : int; bool_input : bool }

let[@def] (as_int @ total) (value : value @ immutable) =
  match value with Int_value x -> x | Bool_value _ -> 0

let[@def] (as_bool @ total) (value : value @ immutable) =
  match value with Bool_value x -> x | Int_value _ -> false

let[@def] rec (sort @ total) (expr : expr @ immutable) =
  match expr with
  | Int_lit _ | Int_input -> Some Integer
  | Bool_lit _ | Bool_input -> Some Boolean
  | Add (left, right) ->
    (match sort left, sort right with
     | Some Integer, Some Integer -> Some Integer
     | _ -> None)
  | Eq_int (left, right) ->
    (match sort left, sort right with
     | Some Integer, Some Integer -> Some Boolean
     | _ -> None)
  | Int_if (condition, yes, no) ->
    (match sort condition, sort yes, sort no with
     | Some Boolean, Some Integer, Some Integer -> Some Integer
     | _ -> None)
  | Bool_if (condition, yes, no) ->
    (match sort condition, sort yes, sort no with
     | Some Boolean, Some Boolean, Some Boolean -> Some Boolean
     | _ -> None)

let[@def] rec (eval @ total) (expr : expr @ immutable)
    (env : env @ immutable) =
  match expr with
  | Int_lit value -> Int_value value
  | Bool_lit value -> Bool_value value
  | Int_input -> Int_value env.int_input
  | Bool_input -> Bool_value env.bool_input
  | Add (left, right) ->
    Int_value (as_int (eval left env) + as_int (eval right env))
  | Eq_int (left, right) ->
    Bool_value (as_int (eval left env) = as_int (eval right env))
  | Int_if (condition, yes, no) ->
    Int_value (if as_bool (eval condition env)
      then as_int (eval yes env) else as_int (eval no env))
  | Bool_if (condition, yes, no) ->
    Bool_value (if as_bool (eval condition env)
      then as_bool (eval yes env) else as_bool (eval no env))
