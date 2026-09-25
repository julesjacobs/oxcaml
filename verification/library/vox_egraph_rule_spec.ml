module L = Vox_egraph_language_spec

type pat =
  | Var of int
  | Int_lit of int
  | Bool_lit of bool
  | Int_input
  | Bool_input
  | Add of pat * pat
  | Eq_int of pat * pat
  | Int_if of pat * pat * pat
  | Bool_if of pat * pat * pat
[@@inductive]

type rule = { vars : L.sort list; lhs : pat; rhs : pat }
type t = No_rules | Rule_cons of rule * t [@@inductive]
type subst = L.expr list

let[@def] rec (lookup_sort @ total) (vars : L.sort list @ immutable)
    (index : int) =
  match vars with
  | [] -> None
  | sort :: rest ->
    if index < 0 then None
    else if index = 0 then Some sort
    else lookup_sort rest (index - 1)

let[@def] rec (lookup_expr @ total) (subst : subst @ immutable)
    (index : int) =
  match subst with
  | [] -> L.Int_lit 0
  | expr :: rest ->
    if index < 0 then L.Int_lit 0
    else if index = 0 then expr
    else lookup_expr rest (index - 1)

let[@def] rec (pat_sort @ total) (vars : L.sort list @ immutable)
    (pat : pat @ immutable) =
  match pat with
  | Var index -> lookup_sort vars index
  | Int_lit _ | Int_input -> Some L.Integer
  | Bool_lit _ | Bool_input -> Some L.Boolean
  | Add (a, b) ->
    (match pat_sort vars a, pat_sort vars b with
     | Some L.Integer, Some L.Integer -> Some L.Integer
     | _ -> None)
  | Eq_int (a, b) ->
    (match pat_sort vars a, pat_sort vars b with
     | Some L.Integer, Some L.Integer -> Some L.Boolean
     | _ -> None)
  | Int_if (c, y, n) ->
    (match pat_sort vars c, pat_sort vars y, pat_sort vars n with
     | Some L.Boolean, Some L.Integer, Some L.Integer -> Some L.Integer
     | _ -> None)
  | Bool_if (c, y, n) ->
    (match pat_sort vars c, pat_sort vars y, pat_sort vars n with
     | Some L.Boolean, Some L.Boolean, Some L.Boolean -> Some L.Boolean
     | _ -> None)

let[@def] rec (occurs @ total) (index : int) (pat : pat @ immutable) =
  match pat with
  | Var other -> index = other
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> false
  | Add (a, b) | Eq_int (a, b) -> occurs index a || occurs index b
  | Int_if (c, y, n) | Bool_if (c, y, n) ->
    occurs index c || occurs index y || occurs index n

let[@def] rec (vars_in @ total) (pat : pat @ immutable)
    (lhs : pat @ immutable) =
  match pat with
  | Var index -> occurs index lhs
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> true
  | Add (a, b) | Eq_int (a, b) -> vars_in a lhs && vars_in b lhs
  | Int_if (c, y, n) | Bool_if (c, y, n) ->
    vars_in c lhs && vars_in y lhs && vars_in n lhs

let[@def] (rule_valid @ total) (rule : rule @ immutable) =
  (match pat_sort rule.vars rule.lhs, pat_sort rule.vars rule.rhs with
   | Some L.Integer, Some L.Integer | Some L.Boolean, Some L.Boolean ->
     true
   | _ -> false) && vars_in rule.rhs rule.lhs

let[@def] rec (valid @ total) (rules : t @ immutable) =
  match rules with
  | No_rules -> true
  | Rule_cons (rule, rest) -> rule_valid rule && valid rest

let[@def] rec (subst_valid @ total) (vars : L.sort list @ immutable)
    (subst : subst @ immutable) =
  match vars, subst with
  | [], [] -> true
  | sort :: vars, expr :: subst ->
    (match sort, L.sort expr with
     | L.Integer, Some L.Integer | L.Boolean, Some L.Boolean -> true
     | _ -> false) && subst_valid vars subst
  | _ -> false

let[@def] rec (instantiate @ total) (pat : pat @ immutable)
    (subst : subst @ immutable) =
  match pat with
  | Var index -> lookup_expr subst index
  | Int_lit n -> L.Int_lit n
  | Bool_lit b -> L.Bool_lit b
  | Int_input -> L.Int_input
  | Bool_input -> L.Bool_input
  | Add (a, b) -> L.Add (instantiate a subst, instantiate b subst)
  | Eq_int (a, b) -> L.Eq_int (instantiate a subst, instantiate b subst)
  | Int_if (c, y, n) ->
    L.Int_if (instantiate c subst, instantiate y subst, instantiate n subst)
  | Bool_if (c, y, n) ->
    L.Bool_if (instantiate c subst, instantiate y subst, instantiate n subst)

let[@def] rec (lookup_rule @ total) (rules : t @ immutable)
    (index : int) =
  match rules with
  | No_rules -> None
  | Rule_cons (rule, rest) ->
    if index < 0 then None
    else if index = 0 then Some rule
    else lookup_rule rest (index - 1)
