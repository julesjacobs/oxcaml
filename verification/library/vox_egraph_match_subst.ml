module V = Vox_egraph_rule_store
module P = Vox_egraph_match_evidence
module S = Vox_egraph_rule_semantics
module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec

let (default @ total) (sort : L.sort) :
    {e : L.expr | L.sort e === Some sort} @ immutable =
  match sort with
  | L.Integer ->
    ghost_ (L.sort_def (L.Int_lit 0));
    (L.Int_lit 0)
  | L.Boolean ->
    ghost_ (L.sort_def (L.Bool_lit false));
    (L.Bool_lit false)

let[@def] rec (accepts @ total) (state : V.t @ immutable)
    (vars : L.sort list @ immutable) (bindings : int list @ immutable) = ghost_ (
  match vars, bindings with
  | [], [] -> true
  | sort :: vars, id :: ids ->
    id < state.semantic.union.count &&
    (if id < 0 then true else
      L.sort (S.origin state.semantic.origins id) === Some sort) &&
    accepts state vars ids
  | _ -> false)

module I = Vox_iarray
module N = Vox_egraph_rule_node

type result = #{accepted : bool; subst : R.subst option @@ ghost}

let rec (build @ total) : (state : {s : V.t | V.valid s}) @ immutable ->
    (vars : L.sort list) @ immutable -> (bindings : int list) @ immutable ->
    {r : result | r.#accepted = accepts state vars bindings &&
      (match r.#subst with
       | None -> not r.#accepted
       | Some subst -> r.#accepted && R.subst_valid vars subst &&
         P.agrees state.semantic.origins bindings subst)} = fun state vars bindings ->
  ghost_ (accepts_def state vars bindings);
  match vars, bindings with
  | [], [] ->
    let empty = ghost_ ([] : R.subst) in
    ghost_ (
      R.subst_valid_def vars empty;
      P.agrees_def state.semantic.origins bindings empty);
    #{accepted = true; subst = ghost_ (Some empty)}
  | sort :: rest_vars, id :: ids ->
    if id >= state.semantic.union.count then #{accepted = false; subst = ghost_ None}
    else
      let typed = id < 0 ||
        (match sort, I.at state.sorts id with
         | L.Integer, Some L.Integer | L.Boolean, Some L.Boolean -> true
         | _ -> false) in
      ghost_ (
        if id >= 0 then (
          V.valid_def state;
          V.nodes_valid_def state state.semantic.union.count;
          V.node_at_data state.nodes state.sorts state.semantic.origins state.semantic.union.count id ();
          V.node_ok_data_def state.nodes state.sorts state.semantic.origins id));
      if not typed then #{accepted = false; subst = ghost_ None}
      else
        let #{accepted; subst} = build state rest_vars ids in
        if not accepted then #{accepted = false; subst = ghost_ None}
        else
          let subst = ghost_ (
            let expr = if id < 0 then default sort else S.origin state.semantic.origins id in
            match subst with
            | None -> None
            | Some rest ->
              R.subst_valid_def vars (expr :: rest);
              P.agrees_def state.semantic.origins bindings (expr :: rest);
              Some (expr :: rest)) in
          #{accepted = true; subst}
  | _ -> #{accepted = false; subst = ghost_ None}
