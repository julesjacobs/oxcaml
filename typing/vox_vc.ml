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
    producers : fact_origin list;
  }

(* Two origins denote the same introduction site when their kind, name and
   span agree.  Spans are compared on their absolute byte offsets and file
   rather than physically: a fact re-inserted through a merge carries a copy,
   and a copied span must still count as the site it came from. *)
let same_span (left : Location.t) (right : Location.t) =
  String.equal left.loc_start.pos_fname right.loc_start.pos_fname
  && left.loc_start.pos_cnum = right.loc_start.pos_cnum
  && left.loc_end.pos_cnum = right.loc_end.pos_cnum
  && left.loc_ghost = right.loc_ghost

let same_origin left right =
  String.equal left.kind right.kind
  && (match left.name, right.name with
      | None, None -> true
      | Some left, Some right -> String.equal left right
      | Some _, None | None, Some _ -> false)
  && (match left.span, right.span with
      | None, None -> true
      | Some left, Some right -> same_span left right
      | Some _, None | None, Some _ -> false)

let merge_producers into from_ =
  List.fold_left
    (fun into origin ->
      if List.exists (fun existing -> same_origin existing origin) into
      then into
      else into @ [origin])
    into from_

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

  let add ~origin ?producers ?loc ?scope ?typing_env expression env =
    let producers =
      match producers with
      | None -> [origin]
      | Some producers -> merge_producers [origin] producers
    in
    if
      not (expression_in_scope env.scope expression)
      || trivially_reflexive typing_env expression
    then env
    else if
      List.exists
        (fun fact -> same_expression expression fact.expression)
        env.facts_rev
    then
      (* The proposition is already here.  Keeping the first entry's origin is
         what the pane shows, but the site being added introduced it too, so
         record it: otherwise that site reads as having introduced nothing. *)
      { env with
        facts_rev =
          List.map
            (fun fact ->
              if same_expression expression fact.expression
              then
                { fact with producers = merge_producers fact.producers producers }
              else fact)
            env.facts_rev;
      }
    else
      { env with
        facts_rev =
          { expression; location = loc; scope; origin; producers }
          :: env.facts_rev;
      }

  let facts env = List.rev env.facts_rev
  let scope env = env.scope

  let introduced_by origin env =
    List.exists
      (fun fact ->
        List.exists (fun producer -> same_origin producer origin)
          fact.producers)
      env.facts_rev

  (* The surviving entry keeps the left side's origin, so the right side's
     introduction sites have to be carried across: a proposition that both
     arms of a branch establish is read after the merge through this one
     entry, and crediting only the left arm would leave the right arm's site
     looking unread. *)
  let intersect left right =
    let scope = Ident.Set.inter left.scope right.scope in
    { facts_rev =
        List.filter_map
          (fun fact ->
            if not (expression_in_scope scope fact.expression) then None
            else
              let matching =
                List.filter
                  (fun other ->
                    same_expression fact.expression other.expression)
                  right.facts_rev
              in
              match matching with
              | [] -> None
              | matching ->
                Some
                  { fact with
                    producers =
                      List.fold_left
                        (fun producers other ->
                          merge_producers producers other.producers)
                        fact.producers matching;
                  })
          left.facts_rev;
      scope;
    }

  let union left right =
    { facts_rev = right.facts_rev @ left.facts_rev;
      scope = Ident.Set.union left.scope right.scope;
    }

  (* Nothing was added to, merged into or dropped from these facts between
     one environment and the other.  Identity of the list answers the common
     case, since a walk that touched nothing hands back what it was given.
     Content answers the rest, because ordinary walking rebuilds the list
     without changing it -- [merge_facts] re-inserts every fact -- and a
     caller that read a rebuild as a change would refuse to answer about any
     call with a compound argument.  Producers are compared and not only the
     propositions: a site whose proposition is already present leaves the
     list the same length, and the only trace of it is the producer it added
     to the entry that was there. *)
  let same_facts left right =
    let same_fact left right =
      left == right
      || (same_expression left.expression right.expression
          && List.length left.producers = List.length right.producers
          && List.for_all2 same_origin left.producers right.producers)
    in
    left.facts_rev == right.facts_rev
    || (List.length left.facts_rev = List.length right.facts_rev
        && List.for_all2 same_fact left.facts_rev right.facts_rev)

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
