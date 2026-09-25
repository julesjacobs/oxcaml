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

let[@def] (closed_instance @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (bindings : int list @ immutable) (root : int) =
  not (binding_valid graph rule.vars bindings) ||
  not (Q.matches graph rule.lhs bindings root) ||
  Q.matches graph rule.rhs bindings root

let[@def] rec (closed_roots @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (bindings : int list @ immutable) (count : int) =
  if count <= 0 then true
  else closed_instance graph rule bindings (count - 1) &&
    closed_roots graph rule bindings (count - 1)
  [@@decreases if count > 0 then count else 0]

type cases = End | Case of int list * cases [@@inductive]

let[@def] rec (append @ total) (first : cases @ immutable) (second : cases @ immutable) =
  match first with End -> second | Case (ids, rest) -> Case (ids, append rest second)

let[@def] rec (prepend @ total) (id : int) (suffixes : cases @ immutable) =
  match suffixes with
  | End -> End
  | Case (ids, rest) -> Case (id :: ids, prepend id rest)

let[@def] rec (choices @ total) (count : int) (suffixes : cases @ immutable) =
  if count <= 0 then prepend (-1) suffixes
  else append (prepend (count - 1) suffixes) (choices (count - 1) suffixes)
  [@@decreases if count > 0 then count else 0]

let[@def] rec (assignments @ total) (count : int) (vars : L.sort list @ immutable) =
  match vars with
  | [] -> Case ([], End)
  | _ :: vars -> choices count (assignments count vars)

let[@def] rec (closed_cases @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (cases : cases @ immutable) =
  match cases with
  | End -> true
  | Case (ids, rest) -> closed_roots graph rule ids graph.count && closed_cases graph rule rest

let[@def] (closed_rule @ total) (graph : Q.graph @ immutable) (rule : R.rule @ immutable) =
  closed_cases graph rule (assignments graph.count rule.vars)

let[@def] rec (closed_rules @ total) (graph : Q.graph @ immutable) (rules : R.t @ immutable) =
  match rules with
  | R.No_rules -> true
  | R.Rule_cons (rule, rest) -> closed_rule graph rule && closed_rules graph rest
