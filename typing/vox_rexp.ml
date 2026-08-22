(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

(* Folding over interior types. *)

let fold_types_gen ~include_stored f acc rexp =
  let rec fold acc rexp =
    let acc =
      if include_stored
      then Option.fold ~none:acc ~some:(f acc) rexp.rexp_type
      else acc
    in
    match rexp.rexp_desc with
    | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> acc
    | Rexp_apply (fn, { rapp_source_args = args; rapp_completion = _ }) ->
        List.fold_left
          (fun acc (_, arg) -> fold acc arg)
          (fold acc fn) args
    | Rexp_format (_, expansion) -> fold acc expansion
    | Rexp_tuple components ->
        List.fold_left
          (fun acc (_, component) -> fold acc component)
          acc components
    | Rexp_construct (_, _, arg) ->
        Option.fold ~none:acc ~some:(fold acc) arg
    | Rexp_field (e, _, _, _) -> fold acc e
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        let acc = fold acc cond in
        let acc = fold acc ifso in
        Option.fold ~none:acc ~some:(fold acc) ifnot
    | Rexp_let ({ rb_ident = _; rb_expr }, body) ->
        fold (fold acc rb_expr) body
    | Rexp_fun (_, body) -> fold acc body
    | Rexp_match (scrutinee, cases) ->
        List.fold_left fold_case (fold acc scrutinee) cases
    | Rexp_constraint (e, ty) -> f (fold acc e) ty
  and fold_case acc { rc_lhs = _; rc_guard; rc_rhs } =
    (* Patterns carry no interior types. *)
    let acc = Option.fold ~none:acc ~some:(fold acc) rc_guard in
    fold acc rc_rhs
  in
  fold acc rexp

let fold_types f acc rexp =
  fold_types_gen ~include_stored:true f acc rexp

let iter_types f rexp = fold_types (fun () ty -> f ty) () rexp

(* Written constraint types only: stored node types may share structure
   with the enclosing type (own-domain binder instances), and consumers
   reasoning about type structure must not read those metadata edges. *)
let fold_written_types f acc rexp =
  fold_types_gen ~include_stored:false f acc rexp

let iter_written_types f rexp =
  fold_written_types (fun () ty -> f ty) () rexp

(* Rebuilding *)

let map ?(rename = Ident.Map.empty) ?(freshen = false) ?value_path
    ?type_path ?stored_type_expr ~type_expr rexp =
  let stored_type_expr =
    Option.value stored_type_expr ~default:type_expr
  in
  let rename_var rename id =
    match Ident.Map.find_opt id rename with Some id' -> id' | None -> id
  in
  let bind rename id =
    if freshen then
      match Ident.Map.find_opt id rename with
      | Some id' -> rename, id'
      | None ->
          let id' = Ident.rename id in
          Ident.Map.add id id' rename, id'
    else rename, id
  in
  let map_type_path path =
    match type_path with Some f -> f path | None -> path
  in
  (* A stored type on an outer function node is the full arrow and can contain
     refinements mentioning parameters bound by nested [Rexp_fun] nodes.
     Allocate every local rename before mapping any stored type; Ident stamps
     are unique, so mappings for binders outside a node's lexical scope cannot
     affect a well-formed occurrence there. *)
  let rec preallocate_rexp_binders rename rexp =
    match rexp.rexp_desc with
    | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> rename
    | Rexp_apply (fn, { rapp_source_args = args; rapp_completion = _ }) ->
        List.fold_left
          (fun rename (_, arg) -> preallocate_rexp_binders rename arg)
          (preallocate_rexp_binders rename fn) args
    | Rexp_format (_, expansion) ->
        preallocate_rexp_binders rename expansion
    | Rexp_tuple components ->
        List.fold_left
          (fun rename (_, component) ->
            preallocate_rexp_binders rename component)
          rename components
    | Rexp_construct (_, _, arg) ->
        Option.fold ~none:rename
          ~some:(preallocate_rexp_binders rename) arg
    | Rexp_field (e, _, _, _) -> preallocate_rexp_binders rename e
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        let rename = preallocate_rexp_binders rename cond in
        let rename = preallocate_rexp_binders rename ifso in
        Option.fold ~none:rename
          ~some:(preallocate_rexp_binders rename) ifnot
    | Rexp_let ({ rb_ident; rb_expr }, body) ->
        let rename = preallocate_rexp_binders rename rb_expr in
        let rename, _ = bind rename rb_ident in
        preallocate_rexp_binders rename body
    | Rexp_fun (param, body) ->
        let rename, _ = bind rename param in
        preallocate_rexp_binders rename body
    | Rexp_match (scrutinee, cases) ->
        List.fold_left preallocate_case_binders
          (preallocate_rexp_binders rename scrutinee) cases
    | Rexp_constraint (e, _) -> preallocate_rexp_binders rename e
  and preallocate_case_binders rename { rc_lhs; rc_guard; rc_rhs } =
    let rename = preallocate_pat_binders rename rc_lhs in
    let rename =
      Option.fold ~none:rename
        ~some:(preallocate_rexp_binders rename) rc_guard
    in
    preallocate_rexp_binders rename rc_rhs
  and preallocate_pat_binders rename pat =
    match pat.rpat_desc with
    | Rpat_any | Rpat_constant _ -> rename
    | Rpat_var id -> fst (bind rename id)
    | Rpat_tuple components ->
        List.fold_left
          (fun rename (_, component) ->
            preallocate_pat_binders rename component)
          rename components
    | Rpat_construct (_, _, arg) ->
        Option.fold ~none:rename
          ~some:(preallocate_pat_binders rename) arg
    | Rpat_alias (pat, id) ->
        let rename = preallocate_pat_binders rename pat in
        fst (bind rename id)
  in
  let rename =
    if freshen then preallocate_rexp_binders rename rexp else rename
  in
  let rec map_rexp rename rexp =
    let rexp_desc =
      match rexp.rexp_desc with
      | Rexp_hole -> Rexp_hole
      | Rexp_var id -> Rexp_var (rename_var rename id)
      | Rexp_ident (path, lid) ->
          let path =
            match value_path with Some f -> f path | None -> path
          in
          Rexp_ident (path, lid)
      | Rexp_constant _ as desc -> desc
      | Rexp_apply (fn, app) ->
          Rexp_apply
            ( map_rexp rename fn,
              { app with
                rapp_source_args =
                  List.map
                    (fun (lbl, arg) -> lbl, map_rexp rename arg)
                    app.rapp_source_args } )
      | Rexp_format (literal, expansion) ->
          Rexp_format (literal, map_rexp rename expansion)
      | Rexp_tuple components ->
          Rexp_tuple
            (List.map (fun (lbl, c) -> lbl, map_rexp rename c) components)
      | Rexp_construct (path, lid, arg) ->
          Rexp_construct
            (map_type_path path, lid, Option.map (map_rexp rename) arg)
      | Rexp_field (e, parent, name, lid) ->
          Rexp_field (map_rexp rename e, map_type_path parent, name, lid)
      | Rexp_ifthenelse (cond, ifso, ifnot) ->
          Rexp_ifthenelse
            ( map_rexp rename cond,
              map_rexp rename ifso,
              Option.map (map_rexp rename) ifnot )
      | Rexp_let ({ rb_ident; rb_expr }, body) ->
          let rb_expr = map_rexp rename rb_expr in
          let rename, rb_ident = bind rename rb_ident in
          Rexp_let ({ rb_ident; rb_expr }, map_rexp rename body)
      | Rexp_fun (param, body) ->
          let rename, param = bind rename param in
          Rexp_fun (param, map_rexp rename body)
      | Rexp_match (scrutinee, cases) ->
          Rexp_match (map_rexp rename scrutinee, List.map (map_case rename) cases)
      | Rexp_constraint (e, ty) ->
          Rexp_constraint (map_rexp rename e, type_expr rename ty)
    in
    (* The preallocated map includes every binder whose scope can be reflected
       in this node's stored type. *)
    let rexp_type = Option.map (stored_type_expr rename) rexp.rexp_type in
    { rexp with rexp_desc; rexp_type }
  and map_case rename { rc_lhs; rc_guard; rc_rhs } =
    let rename, rc_lhs = map_pat rename rc_lhs in
    { rc_lhs;
      rc_guard = Option.map (map_rexp rename) rc_guard;
      rc_rhs = map_rexp rename rc_rhs }
  and map_pat rename pat =
    let rename, rpat_desc =
      match pat.rpat_desc with
      | (Rpat_any | Rpat_constant _) as desc -> rename, desc
      | Rpat_var id ->
          let rename, id = bind rename id in
          rename, Rpat_var id
      | Rpat_tuple components ->
          let rename, components =
            List.fold_left_map
              (fun rename (lbl, p) ->
                let rename, p = map_pat rename p in
                rename, (lbl, p))
              rename components
          in
          rename, Rpat_tuple components
      | Rpat_construct (path, lid, arg) ->
          let path = map_type_path path in
          let rename, arg =
            match arg with
            | None -> rename, None
            | Some p ->
                let rename, p = map_pat rename p in
                rename, Some p
          in
          rename, Rpat_construct (path, lid, arg)
      | Rpat_alias (p, id) ->
          let rename, p = map_pat rename p in
          let rename, id = bind rename id in
          rename, Rpat_alias (p, id)
    in
    rename, { pat with rpat_desc }
  in
  map_rexp rename rexp

(* Alpha-equivalence.  Syntactic, over shape + written constraint types +
   identity keys; stored node types ([rexp_type]) are deliberately not
   compared — they are derived. *)

(* [Pconst_string] carries the location of the string contents inside the
   description; it is not part of the syntax and must not be part of type
   identity. *)
let constant_equal (c1 : Parsetree.constant) (c2 : Parsetree.constant) =
  match c1.pconst_desc, c2.pconst_desc with
  | Pconst_string (s1, _, d1), Pconst_string (s2, _, d2) ->
      String.equal s1 s2 && Option.equal String.equal d1 d2
  | desc1, desc2 -> desc1 = desc2

let application_arg_equal
    (arg1 : refinement_application_arg)
    (arg2 : refinement_application_arg) =
  arg1.rarg_label = arg2.rarg_label
  &&
  match arg1.rarg_desc, arg2.rarg_desc with
  | Rarg_source index1, Rarg_source index2
  | Rarg_optional_wrapper index1, Rarg_optional_wrapper index2 ->
      Int.equal index1 index2
  | Rarg_optional_default, Rarg_optional_default
  | Rarg_call_pos _, Rarg_call_pos _
  | Rarg_omitted_optional, Rarg_omitted_optional
  | Rarg_omitted_position, Rarg_omitted_position
  | Rarg_omitted_required, Rarg_omitted_required ->
      true
  | ( ( Rarg_source _ | Rarg_optional_wrapper _ | Rarg_optional_default
      | Rarg_call_pos _ | Rarg_omitted_optional | Rarg_omitted_position
      | Rarg_omitted_required ), _ ) ->
      false

let equal ~type_eq ~pairs rexp1 rexp2 =
  (* [pairs] pairs the binders of the left predicate with the binders of
     the right one, innermost first. *)
  let var_eq pairs id1 id2 =
    let rec find = function
      | [] -> Ident.same id1 id2
      | (l, r) :: rest ->
          if Ident.same id1 l then Ident.same id2 r
          else if Ident.same id2 r then false
          else find rest
    in
    find pairs
  in
  let rec eq pairs rexp1 rexp2 =
    match rexp1.rexp_desc, rexp2.rexp_desc with
    | Rexp_hole, Rexp_hole -> true
    | Rexp_var id1, Rexp_var id2 -> var_eq pairs id1 id2
    | Rexp_ident (p1, _), Rexp_ident (p2, _) -> Path.same p1 p2
    | Rexp_constant c1, Rexp_constant c2 -> constant_equal c1 c2
    | Rexp_apply (f1, app1), Rexp_apply (f2, app2) ->
        eq pairs f1 f2
        && List.compare_lengths app1.rapp_source_args app2.rapp_source_args = 0
        && List.for_all2
             (fun (l1, a1) (l2, a2) -> l1 = l2 && eq pairs a1 a2)
             app1.rapp_source_args app2.rapp_source_args
        && List.compare_lengths app1.rapp_completion app2.rapp_completion = 0
        && List.for_all2 application_arg_equal
             app1.rapp_completion app2.rapp_completion
    | Rexp_format (literal1, expansion1),
      Rexp_format (literal2, expansion2) ->
        constant_equal literal1 literal2 && eq pairs expansion1 expansion2
    | Rexp_tuple c1, Rexp_tuple c2 ->
        List.compare_lengths c1 c2 = 0
        && List.for_all2
             (fun (l1, e1) (l2, e2) -> l1 = l2 && eq pairs e1 e2)
             c1 c2
    | Rexp_construct (p1, _, arg1), Rexp_construct (p2, _, arg2) ->
        Path.same p1 p2
        && Option.equal (eq pairs) arg1 arg2
    | Rexp_field (e1, parent1, name1, _), Rexp_field (e2, parent2, name2, _)
      ->
        Path.same parent1 parent2
        && String.equal name1 name2
        && eq pairs e1 e2
    | Rexp_ifthenelse (c1, t1, e1), Rexp_ifthenelse (c2, t2, e2) ->
        eq pairs c1 c2 && eq pairs t1 t2 && Option.equal (eq pairs) e1 e2
    | Rexp_let (b1, body1), Rexp_let (b2, body2) ->
        eq pairs b1.rb_expr b2.rb_expr
        && eq ((b1.rb_ident, b2.rb_ident) :: pairs) body1 body2
    | Rexp_fun (p1, body1), Rexp_fun (p2, body2) ->
        eq ((p1, p2) :: pairs) body1 body2
    | Rexp_match (s1, cases1), Rexp_match (s2, cases2) ->
        eq pairs s1 s2
        && List.compare_lengths cases1 cases2 = 0
        && List.for_all2 (eq_case pairs) cases1 cases2
    | Rexp_constraint (e1, ty1), Rexp_constraint (e2, ty2) ->
        eq pairs e1 e2 && type_eq ~pairs ty1 ty2
    | ( ( Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _
        | Rexp_apply _ | Rexp_format _ | Rexp_tuple _ | Rexp_construct _
        | Rexp_field _
        | Rexp_ifthenelse _ | Rexp_let _ | Rexp_fun _ | Rexp_match _
        | Rexp_constraint _ ), _ ) ->
        false
  and eq_case pairs case1 case2 =
    match eq_pat pairs case1.rc_lhs case2.rc_lhs with
    | None -> false
    | Some pairs ->
        Option.equal (eq pairs) case1.rc_guard case2.rc_guard
        && eq pairs case1.rc_rhs case2.rc_rhs
  and eq_pat pairs pat1 pat2 =
    match pat1.rpat_desc, pat2.rpat_desc with
    | Rpat_any, Rpat_any -> Some pairs
    | Rpat_var id1, Rpat_var id2 -> Some ((id1, id2) :: pairs)
    | Rpat_constant c1, Rpat_constant c2 ->
        if constant_equal c1 c2 then Some pairs else None
    | Rpat_tuple c1, Rpat_tuple c2 ->
        if List.compare_lengths c1 c2 = 0 then
          List.fold_left2
            (fun pairs (l1, p1) (l2, p2) ->
              Option.bind pairs (fun pairs ->
                  if l1 = l2 then eq_pat pairs p1 p2 else None))
            (Some pairs) c1 c2
        else None
    | Rpat_construct (c1, _, arg1), Rpat_construct (c2, _, arg2) ->
        if Path.same c1 c2 then
          match arg1, arg2 with
          | None, None -> Some pairs
          | Some p1, Some p2 -> eq_pat pairs p1 p2
          | None, Some _ | Some _, None -> None
        else None
    | Rpat_alias (p1, id1), Rpat_alias (p2, id2) ->
        Option.map
          (fun pairs -> (id1, id2) :: pairs)
          (eq_pat pairs p1 p2)
    | ( ( Rpat_any | Rpat_var _ | Rpat_constant _ | Rpat_tuple _
        | Rpat_construct _ | Rpat_alias _ ), _ ) ->
        None
  in
  eq pairs rexp1 rexp2

(* Back to surface syntax *)

let untype ~var_name ~value_ident ~constructor_ident ~field_ident ~core_type
    rexp =
  let open Ast_helper in
  let lid_of_name name = Location.mknoloc (Longident.Lident name) in
  let rec untype_rexp rexp =
    let loc = rexp.rexp_loc in
    match rexp.rexp_desc with
    | Rexp_hole -> Exp.mk ~loc Pexp_hole
    | Rexp_var id -> Exp.ident ~loc (lid_of_name (var_name id))
    | Rexp_ident (path, _) ->
        (* Render from the resolved path: the source longident may not
           resolve at the printing site, and substitution rewrites only the
           path. *)
        Exp.ident ~loc (value_ident path)
    | Rexp_constant const -> Exp.constant ~loc const
    | Rexp_apply
        (fn, { rapp_source_args = []; rapp_completion = _ :: _ }) ->
        (* An omittable-function coercion is represented by the application
           completion it synthesized, but has no application in the source. *)
        untype_rexp fn
    | Rexp_apply (fn, { rapp_source_args = args; rapp_completion = _ }) ->
        Exp.apply ~loc (untype_rexp fn)
          (List.map (fun (lbl, arg) -> lbl, untype_rexp arg) args)
    | Rexp_format (literal, _) -> Exp.constant ~loc literal
    | Rexp_tuple components ->
        Exp.tuple ~loc
          (List.map (fun (lbl, c) -> lbl, untype_rexp c) components)
    | Rexp_construct (path, _, arg) ->
        Exp.construct ~loc (constructor_ident path)
          (Option.map untype_rexp arg)
    | Rexp_field (e, parent, name, _) ->
        Exp.field ~loc (untype_rexp e) (field_ident parent name)
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        Exp.ifthenelse ~loc (untype_rexp cond) (untype_rexp ifso)
          (Option.map untype_rexp ifnot)
    | Rexp_let ({ rb_ident; rb_expr }, body) ->
        Exp.let_ ~loc Immutable Nonrecursive
          [ Vb.mk
              (Pat.var (Location.mknoloc (var_name rb_ident)))
              (untype_rexp rb_expr) ]
          (untype_rexp body)
    | Rexp_fun (param, body) ->
        Exp.function_ ~loc
          [ { pparam_desc =
                Pparam_val
                  ( Asttypes.Nolabel, None,
                    Pat.var (Location.mknoloc (var_name param)) );
              pparam_loc = Location.none } ]
          { mode_annotations = [];
            ret_mode_annotations = [];
            ret_type_constraint = None }
          (Pfunction_body (untype_rexp body))
    | Rexp_match (scrutinee, cases) ->
        Exp.match_ ~loc (untype_rexp scrutinee) (List.map untype_case cases)
    | Rexp_constraint (e, ty) ->
        Exp.constraint_ ~loc (untype_rexp e) (Some (core_type ty)) []
  and untype_case { rc_lhs; rc_guard; rc_rhs } =
    Exp.case (untype_pat rc_lhs)
      ?guard:(Option.map untype_rexp rc_guard)
      (untype_rexp rc_rhs)
  and untype_pat pat =
    let loc = pat.rpat_loc in
    match pat.rpat_desc with
    | Rpat_any -> Pat.any ~loc ()
    | Rpat_var id -> Pat.var ~loc (Location.mknoloc (var_name id))
    | Rpat_constant const -> Pat.constant ~loc const
    | Rpat_tuple components ->
        Pat.tuple ~loc
          (List.map (fun (lbl, p) -> lbl, untype_pat p) components)
          Asttypes.Closed
    | Rpat_construct (path, _, arg) ->
        Pat.construct ~loc (constructor_ident path)
          (Option.map (fun p -> [], untype_pat p) arg)
    | Rpat_alias (p, id) ->
        Pat.alias ~loc (untype_pat p) (Location.mknoloc (var_name id))
  in
  untype_rexp rexp

(* Occurrence checks used by the printer *)

let exists_rexp pred rexp =
  let exception Found in
  let rec walk rexp =
    if pred rexp then raise Found;
    match rexp.rexp_desc with
    | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> ()
    | Rexp_apply (fn, { rapp_source_args = args; rapp_completion = _ }) ->
        walk fn;
        List.iter (fun (_, arg) -> walk arg) args
    | Rexp_format (_, expansion) -> walk expansion
    | Rexp_tuple components -> List.iter (fun (_, c) -> walk c) components
    | Rexp_construct (_, _, arg) -> Option.iter walk arg
    | Rexp_field (e, _, _, _) -> walk e
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        walk cond; walk ifso; Option.iter walk ifnot
    | Rexp_let ({ rb_expr; _ }, body) -> walk rb_expr; walk body
    | Rexp_fun (_, body) -> walk body
    | Rexp_match (scrutinee, cases) ->
        walk scrutinee;
        List.iter
          (fun { rc_guard; rc_rhs; _ } ->
            Option.iter walk rc_guard;
            walk rc_rhs)
          cases
    | Rexp_constraint (e, _) -> walk e
  in
  match walk rexp with () -> false | exception Found -> true

let find_value_path (f : Path.t -> 'a option) rexp : 'a option =
  let result = ref None in
  let check path =
    match f path with
    | Some _ as found ->
        result := found;
        true
    | None -> false
  in
  ignore
    (exists_rexp
       (fun r ->
         match r.rexp_desc with
         | Rexp_ident (path, _) | Rexp_construct (path, _, _)
         | Rexp_field (_, path, _, _) ->
             check path
         | Rexp_match (_, cases) ->
             let rec pat_path p =
               match p.rpat_desc with
               | Rpat_construct (path, _, arg) ->
                   check path
                   || Option.fold ~none:false ~some:pat_path arg
               | Rpat_alias (p, _) -> pat_path p
               | Rpat_tuple ps -> List.exists (fun (_, p) -> pat_path p) ps
               | Rpat_any | Rpat_var _ | Rpat_constant _ -> false
             in
             List.exists (fun c -> pat_path c.rc_lhs) cases
         | _ -> false)
       rexp
     : bool);
  !result

let rec promote_locals locals rexp =
  let promote = promote_locals locals in
  let rexp_desc =
    match rexp.rexp_desc with
    | Rexp_ident (Path.Pident id, _) when Ident.Set.mem id locals ->
        Rexp_var id
    | (Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _) as desc ->
        desc
    | Rexp_apply (fn, app) ->
        Rexp_apply
          ( promote fn,
            { app with
              rapp_source_args =
                List.map (fun (l, a) -> l, promote a) app.rapp_source_args } )
    | Rexp_format (literal, expansion) ->
        Rexp_format (literal, promote expansion)
    | Rexp_tuple components ->
        Rexp_tuple (List.map (fun (l, c) -> l, promote c) components)
    | Rexp_construct (path, lid, arg) ->
        Rexp_construct (path, lid, Option.map promote arg)
    | Rexp_field (e, parent, name, lid) ->
        Rexp_field (promote e, parent, name, lid)
    | Rexp_ifthenelse (c, t, e) ->
        Rexp_ifthenelse (promote c, promote t, Option.map promote e)
    | Rexp_let ({ rb_ident; rb_expr }, body) ->
        Rexp_let ({ rb_ident; rb_expr = promote rb_expr }, promote body)
    | Rexp_fun (param, body) -> Rexp_fun (param, promote body)
    | Rexp_match (scrutinee, cases) ->
        Rexp_match
          ( promote scrutinee,
            List.map
              (fun { rc_lhs; rc_guard; rc_rhs } ->
                { rc_lhs;
                  rc_guard = Option.map promote rc_guard;
                  rc_rhs = promote rc_rhs })
              cases )
    | Rexp_constraint (e, ty) -> Rexp_constraint (promote e, ty)
  in
  let rexp_type =
    match rexp_desc with
    | Rexp_var _ ->
        (* Bound variables are contextual: their binder or enclosing arrow
           supplies the type, so retaining a derived annotation here would
           create a metadata backedge. *)
        None
    | _ -> rexp.rexp_type
  in
  { rexp with rexp_desc; rexp_type }

let mentions_ident id rexp =
  exists_rexp
    (fun r ->
      match r.rexp_desc with
      | Rexp_var id' -> Ident.same id id'
      | _ -> false)
    rexp
