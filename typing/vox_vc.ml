type fact_origin =
  { kind : string;
    name : string option;
    span : Location.t option;
  }

type fact =
  { expression : Types.refinement_expression;
    location : Location.t option;
    scope : Location.t option;
    origin : fact_origin;
  }

type t =
  { location : Location.t;
    facts : fact list;
    goal : Types.refinement_expression;
  }

module Recursive_binding = struct
  let defeq_locations : Location.t list ref = ref []

  let memq loc locations =
    List.exists (fun recorded -> recorded == loc) !locations

  let request_defeq loc = defeq_locations := loc :: !defeq_locations
  let defeq_requested loc = memq loc defeq_locations
end

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

  let same_expression left right =
    (* Refinement copies may have distinct internal carrier nodes while
       denoting the same resolved proposition. *)
    Types.Refinement.strict_equal ~equal_type:(fun _ _ -> true) left right

  (* [Stdlib.( = )].  Recognised by path so that a user's own [=] is never
     mistaken for the modeled structural equality. *)
  let is_stdlib_equality = function
    | Path.Pdot (Path.Pident root, "=") ->
      Ident.same root (Ident.create_persistent "Stdlib")
    | Path.Pdot _ | Path.Pident _ | Path.Papply _ | Path.Pextra_ty _ -> false

  (* Structural equality is reflexive at every type this model reaches except
     float, where [x = x] additionally says that [x] is not a NaN and so
     carries content worth keeping. *)
  let mentions_float type_ =
    let found = ref false in
    Types.with_type_mark (fun mark ->
      let super = Btype.type_iterators mark in
      let iterator =
        { super with
          Btype.it_type_expr =
            (fun self type_ ->
              (match Types.get_desc type_ with
               | Types.Tconstr (path, _, _)
                 when Path.same path Predef.path_float -> found := true
               | _ -> ());
              super.Btype.it_type_expr self type_);
        }
      in
      iterator.Btype.it_type_expr iterator type_);
    !found

  (* A hypothesis of the form [a = a] holds at every instantiation, so it
     constrains nothing.  Dropping it keeps solver input, proof-pane lines and
     hydration payload proportional to the facts that carry content. *)
  let trivially_reflexive expression =
    match expression.Types.rexp_desc with
    | Types.Rexp_apply
        ({ Types.rexp_desc =
             Types.Rexp_ident (Types.Rfree (Types.Rapp path)); _ },
         [ (Types.Nolabel, left); (Types.Nolabel, right) ])
      when is_stdlib_equality path ->
      same_expression left right
      && not (mentions_float left.Types.rexp_type)
    | Types.Rexp_ident _ | Types.Rexp_constant _ | Types.Rexp_let _
    | Types.Rexp_function _ | Types.Rexp_apply _ | Types.Rexp_tuple _
    | Types.Rexp_construct _ | Types.Rexp_field _ | Types.Rexp_ifthenelse _
    | Types.Rexp_match _ -> false

  let add ~origin ?loc ?scope expression env =
    if
      expression_in_scope env.scope expression
      && not (trivially_reflexive expression)
      && not
           (List.exists
              (fun fact -> same_expression expression fact.expression)
              env.facts_rev)
    then
      { env with
        facts_rev =
          { expression; location = loc; scope; origin } :: env.facts_rev;
      }
    else env

  let facts env = List.rev env.facts_rev
  let scope env = env.scope

  let intersect left right =
    let scope = Ident.Set.inter left.scope right.scope in
    { facts_rev =
        List.filter
          (fun fact ->
            expression_in_scope scope fact.expression
            && List.exists
                 (fun other ->
                   same_expression fact.expression other.expression)
                 right.facts_rev)
          left.facts_rev;
      scope;
    }

  let union left right =
    { facts_rev = right.facts_rev @ left.facts_rev;
      scope = Ident.Set.union left.scope right.scope;
    }

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
