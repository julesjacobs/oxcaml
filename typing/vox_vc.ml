type fact =
  { expression : Types.refinement_expression;
    location : Location.t option;
  }

type t =
  { location : Location.t;
    facts : fact list;
    goal : Types.refinement_expression;
  }

let create ~loc ~facts ~goal = { location = loc; facts; goal }

let instantiate ~(refinement : Types.refinement_desc) ~with_ =
  Types.Refinement.subst ~id:refinement.ref_view.rb_id ~by:with_
    refinement.ref_pred

type scope_error =
  { location : Location.t;
    escaped : Ident.t list;
  }

module Fact_env = struct
  type nonrec fact = fact
  type vc = t

  type t =
    { facts_rev : fact list;
      scope : Ident.Set.t;
    }

  let empty = { facts_rev = []; scope = Ident.Set.empty }

  let enter id env = { env with scope = Ident.Set.add id env.scope }

  let enter_many ids env = List.fold_left (fun env id -> enter id env) env ids

  let in_scope id env = Ident.Set.mem id env.scope

  let expression_in_scope scope expression =
    Ident.Set.subset
      (Types.Refinement.free_bound_identifiers expression)
      scope

  let restrict scope env =
    { facts_rev =
        List.filter
          (fun fact -> expression_in_scope scope fact.expression)
          env.facts_rev;
      scope;
    }

  let leave id env = restrict (Ident.Set.remove id env.scope) env

  let leave_many ids env =
    List.fold_left (fun env id -> leave id env) env ids

  let add ?loc expression env =
    if expression_in_scope env.scope expression then
      { env with facts_rev = { expression; location = loc } :: env.facts_rev }
    else env

  let facts env = List.rev env.facts_rev
  let scope env = env.scope

  let snapshot ~loc ~goal env =
    let escaped =
      Ident.Set.diff
        (Types.Refinement.free_bound_identifiers goal)
        env.scope
      |> Ident.Set.elements
    in
    match escaped with
    | _ :: _ -> Error { location = loc; escaped }
    | [] ->
      let facts =
        facts env
        |> List.filter (fun fact ->
          expression_in_scope env.scope fact.expression)
      in
      Ok { location = loc; facts; goal }
end
