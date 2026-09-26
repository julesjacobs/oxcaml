open Vox_egraph_rule_spec

let rec (lookup_sorted @ total) :
    (vars : L.sort list) @ immutable ->
    (subst : subst) @ immutable -> (index : int) ->
    {u : unit | subst_valid vars subst &&
      not (lookup_sort vars index === None)} ->
    {u : unit | L.sort (lookup_expr subst index) === lookup_sort vars index}
    @ ghost = fun vars subst index premise -> ghost_ (
  subst_valid_def vars subst;
  lookup_sort_def vars index;
  lookup_expr_def subst index;
  match vars, subst with
  | _ :: rest_vars, _ :: rest_subst ->
    if index > 0 then lookup_sorted rest_vars rest_subst (index - 1) ()
    else ()
  | _ -> ())

let rec (instantiate_sorted @ total) :
    (vars : L.sort list) @ immutable -> (pat : pat) @ immutable ->
    (subst : subst) @ immutable ->
    {u : unit | subst_valid vars subst &&
      not (pat_sort vars pat === None)} ->
    {u : unit | L.sort (instantiate pat subst) === pat_sort vars pat}
    @ ghost = fun vars pat subst premise -> ghost_ (
  pat_sort_def vars pat;
  instantiate_def pat subst;
  L.sort_def (instantiate pat subst);
  match pat with
  | Var index -> lookup_sorted vars subst index ()
  | Int_lit _ | Bool_lit _ | Int_input | Bool_input -> ()
  | Add (a, b) | Eq_int (a, b) ->
    instantiate_sorted vars a subst ();
    instantiate_sorted vars b subst ();
    ()
  | Int_if (c, y, n) | Bool_if (c, y, n) ->
    instantiate_sorted vars c subst ();
    instantiate_sorted vars y subst ();
    instantiate_sorted vars n subst ();
    ())

let (instance_sorted @ total) :
    (rule : rule) @ immutable -> (subst : subst) @ immutable ->
    {u : unit | rule_valid rule && subst_valid rule.vars subst} ->
    {u : unit |
      not (L.sort (instantiate rule.lhs subst) === None) &&
      L.sort (instantiate rule.lhs subst) ===
        L.sort (instantiate rule.rhs subst)} @ ghost =
    fun rule subst premise -> ghost_ (
  rule_valid_def rule;
  instantiate_sorted rule.vars rule.lhs subst ();
  instantiate_sorted rule.vars rule.rhs subst ();
  ())
