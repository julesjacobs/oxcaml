module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec

let[@def] (node_sort @ total) (node : Q.node @ immutable) =
  match node with
  | Q.Int_lit _ | Q.Int_input | Q.Add _ | Q.Int_if _ -> L.Integer
  | Q.Bool_lit _ | Q.Bool_input | Q.Eq_int _ | Q.Bool_if _ -> L.Boolean

let[@def] rec (binding_valid @ total) (graph : Q.graph @ immutable)
    (vars : L.sort list @ immutable) (bindings : int list @ immutable) =
  match vars, bindings with
  | [], [] -> true
  | sort :: vars, id :: bindings ->
    (if id < 0 then true else
      match Q.node graph id with
      | None -> false
      | Some node ->
        match sort, node_sort node with
        | L.Integer, L.Integer | L.Boolean, L.Boolean -> true
        | _ -> false) && binding_valid graph vars bindings
  | _ -> false

let[@def] rec (closed_roots @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (bindings : int list @ immutable) (count : int) =
  if count <= 0 then true
  else (not (binding_valid graph rule.vars bindings) ||
    not (Q.matches graph rule.lhs bindings (count - 1)) ||
    Q.matches graph rule.rhs bindings (count - 1)) &&
    closed_roots graph rule bindings (count - 1)
  [@@decreases if count > 0 then count else 0]

let[@def] rec (snoc @ total) (prefix : int list @ immutable) (id : int) =
  match prefix with [] -> [id] | head :: rest -> head :: snoc rest id

