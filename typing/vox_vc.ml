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

module Decreases = struct
  type measure =
    { parameters : Ident.t list list;
      components : Types.refinement_expression list;
      group : Ident.t list;
      loc : Location.t;
    }

  (* Typecore checks a termination measure while the parameters it is written
     over are still in scope; the verifier states the obligation at each
     recursive call.  The two phases meet here, keyed on the identifier the
     binding introduces, which is the name both of them hold. *)
  let measures : measure Ident.Tbl.t = Ident.Tbl.create 16

  let record id measure = Ident.Tbl.replace measures id measure
  let find id = Ident.Tbl.find_opt measures id
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

  (* [Stdlib.( = )], the modeled structural equality.  Recognised by path: the
     primitive its value description carries would be the sharper test, but a
     standard library that rebinds [=] to another primitive is not reachable
     from ordinary source, and mistaking one would only retain or drop a
     hypothesis, never change what a sound proof establishes. *)
  let is_stdlib_equality path =
    match path with
    | Path.Pdot (Path.Pident root, "=") ->
      Ident.same root (Ident.create_persistent "Stdlib")
    | Path.Pdot _ | Path.Pident _ | Path.Papply _ | Path.Pextra_ty _ -> false

  (* Equality is reflexive at every type this model reaches whose values
     cannot contain a float, because at float [x = x] additionally says that
     [x] is not a NaN and so carries content.  Manifests are expanded and
     record and variant declarations are looked through, since a type that
     merely names float still contains one.  Anything opaque -- an abstract
     type, a variable, a function -- is treated as possibly containing a
     float, so its facts are retained. *)
  let reflexive_at env type_ =
    (* These carry no float and have no declaration to look through: the
       built-in scalars are abstract, so the declaration walk below would
       otherwise reject them. *)
    let scalar path =
      List.exists (Path.same path)
        [ Predef.path_int; Predef.path_bool; Predef.path_char;
          Predef.path_string; Predef.path_bytes; Predef.path_unit;
          Predef.path_int32; Predef.path_int64; Predef.path_nativeint ]
      || Vox_builtin.is_bigint_type path
    in
    (* Two cycles have to terminate here: a named type whose declaration
       mentions itself, tracked by path, and a structurally recursive type
       under [-rectypes], which has no name to track and is instead marked
       node by node.  Re-reaching either contributes no new float. *)
    let rec float_free mark visited type_ =
      let type_ = Ctype.expand_head env type_ in
      if not (Types.try_mark_node mark type_) then true
      else
      match Types.get_desc type_ with
      | Types.Ttuple labelled ->
        List.for_all (fun (_, type_) -> float_free mark visited type_) labelled
      | Types.Tconstr (path, arguments, _) ->
        (not (Path.same path Predef.path_float))
        && (not (Path.same path Predef.path_floatarray))
        && List.for_all (float_free mark visited) arguments
        && (scalar path
            || Path.Set.mem path visited
            || let visited = Path.Set.add path visited in
               match Env.find_type path env with
               | exception Not_found -> false
               | declaration -> float_free_kind mark visited declaration)
      | Types.Trefine refinement ->
        (* A refinement expression carries the refined type; reflexivity is a
           property of the underlying carrier. *)
        float_free mark visited refinement.Types.ref_skeleton
      | Types.Tvar _ | Types.Tunivar _ | Types.Tarrow _ | Types.Tobject _
      | Types.Tfield _ | Types.Tnil | Types.Tlink _ | Types.Tsubst _
      | Types.Tvariant _ | Types.Tpoly _ | Types.Tpackage _
      | Types.Tof_kind _ | Types.Tunboxed_tuple _ | Types.Tbox _
      | Types.Tquote _ | Types.Tsplice _
      | Types.Tquote_eval _ | Types.Trepr _ -> false
    and float_free_kind mark visited declaration =
      match declaration.Types.type_kind with
      | Types.Type_record (labels, _, _) ->
        List.for_all
          (fun label -> float_free mark visited label.Types.ld_type) labels
      | Types.Type_variant (constructors, _, _) ->
        List.for_all
          (fun constructor ->
            match constructor.Types.cd_args with
            | Types.Cstr_tuple arguments ->
              List.for_all
                (fun argument ->
                  float_free mark visited argument.Types.ca_type)
                arguments
            | Types.Cstr_record labels ->
              List.for_all
                (fun label -> float_free mark visited label.Types.ld_type)
                labels)
          constructors
      | Types.Type_abstract _ | Types.Type_open
      | Types.Type_record_unboxed_product _ -> false
    in
    Types.with_type_mark (fun mark -> float_free mark Path.Set.empty type_)

  (* A hypothesis of the form [a = a] holds at every instantiation, so it
     constrains nothing.  Dropping it keeps solver input, proof-pane lines and
     hydration payload proportional to the facts that carry content. *)
  let trivially_reflexive env expression =
    match env with
    | None -> false
    | Some env ->
      (match expression.Types.rexp_desc with
       | Types.Rexp_apply
           ({ Types.rexp_desc =
                Types.Rexp_ident (Types.Rfree (Types.Rapp path)); _ },
            [ (Types.Nolabel, left); (Types.Nolabel, right) ]) ->
         is_stdlib_equality path
         && same_expression left right
         && reflexive_at env left.Types.rexp_type
       | Types.Rexp_ident _ | Types.Rexp_constant _ | Types.Rexp_let _
       | Types.Rexp_function _ | Types.Rexp_apply _ | Types.Rexp_tuple _
       | Types.Rexp_construct _ | Types.Rexp_field _
       | Types.Rexp_ifthenelse _ | Types.Rexp_match _ -> false)

  let add ~origin ?loc ?scope ?typing_env expression env =
    if
      expression_in_scope env.scope expression
      && not (trivially_reflexive typing_env expression)
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
