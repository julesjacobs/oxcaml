(* Closure under one rule for one assignment of its variables; part of the
   trusted specification. Vox_egraph_quantifier ranges over the
   assignments. An assignment ([bindings]) gives each variable of the rule
   an id, or a negative id for no node. [binding_valid graph vars
   bindings]: there is one id per variable and each nonnegative id has a
   node of its variable's sort ([node_sort]). [closed_roots graph rule
   bindings count]: for every root id below [count], if the assignment is
   valid and the left side of [rule] matches at the root, so does the right
   side.

   A variable bound to a negative id matches nothing
   ([Vox_egraph_match_spec.classes]), so such an assignment holds trivially
   when the variable occurs in the left side. The negative case matters for
   a variable that occurs in neither side: without it, a variable of a sort
   that no node has would leave no valid assignment, and the rule would be
   closed vacuously. [snoc] appends an id to an assignment. *)

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

