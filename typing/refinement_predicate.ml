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

(* Rebuilding *)

let map ?(rename = Ident.Map.empty) ?rename_bound ?bind_value ?free_var_path
    ?value_path
    ?constructor_path ?type_path ?(type_expr = Fun.id)
    ?(location = Fun.id) rexp =
  let map_constant (constant : Parsetree.constant) =
    let pconst_desc =
      match constant.pconst_desc with
      | Parsetree.Pconst_string (contents, loc, delimiter) ->
          Parsetree.Pconst_string (contents, location loc, delimiter)
      | desc -> desc
    in
    { Parsetree.pconst_desc;
      pconst_loc = location constant.pconst_loc
    }
  in
  let bind rename id =
    match rename_bound with
    | Some rename_bound ->
      let id' = rename_bound id in
      Ident.Map.add id id' rename, id'
    | None -> Ident.Map.add id id rename, id
  in
  let rec map_rexp rename rexp =
    let rexp_desc =
      match rexp.rexp_desc with
      | Rexp_var id -> begin
          match Ident.Map.find_opt id rename with
          | Some id -> Rexp_var id
          | None ->
              match Option.bind free_var_path (fun f -> f id) with
              | None -> Rexp_var id
              | Some (Path.Pident id) -> Rexp_var id
              | Some path -> Rexp_ident path
        end
      | Rexp_ident path -> begin
          match Option.bind bind_value (fun f -> f path) with
          | Some id -> Rexp_var id
          | None ->
              let path =
                match value_path with Some f -> f path | None -> path
              in
              Rexp_ident path
          end
      | Rexp_constant constant -> Rexp_constant (map_constant constant)
      | Rexp_apply (fn, args) ->
          Rexp_apply
            ( map_rexp rename fn,
              List.map (fun (lbl, arg) -> lbl, map_rexp rename arg) args )
      | Rexp_tuple components ->
          Rexp_tuple
            (List.map (fun (lbl, c) -> lbl, map_rexp rename c) components)
      | Rexp_construct (path, args) ->
          let path =
            match constructor_path with Some f -> f path | None -> path
          in
          Rexp_construct (path, List.map (map_rexp rename) args)
      | Rexp_record (fields, extended) ->
          Rexp_record
            (List.map
               (fun (path, label, e) ->
                  let path =
                    match type_path with Some f -> f path | None -> path
                  in
                  path, label, map_rexp rename e)
               fields,
             Option.map (map_rexp rename) extended)
      | Rexp_record_unboxed_product (fields, extended) ->
          Rexp_record_unboxed_product
            (List.map
               (fun (path, label, e) ->
                  let path =
                    match type_path with Some f -> f path | None -> path
                  in
                  path, label, map_rexp rename e)
               fields,
             Option.map (map_rexp rename) extended)
      | Rexp_array (mutability, elements) ->
          Rexp_array (mutability, List.map (map_rexp rename) elements)
      | Rexp_field (e, path, label) ->
          let path =
            match type_path with Some f -> f path | None -> path
          in
          Rexp_field (map_rexp rename e, path, label)
      | Rexp_ifthenelse (cond, ifso, ifnot) ->
          Rexp_ifthenelse
            ( map_rexp rename cond,
              map_rexp rename ifso,
              Option.map (map_rexp rename) ifnot )
      | Rexp_sequence (first, second) ->
          Rexp_sequence (map_rexp rename first, map_rexp rename second)
      | Rexp_let ({ rb_kind; rb_ident; rb_type; rb_expr }, body) ->
          let rb_expr = map_rexp rename rb_expr in
          let rb_type = type_expr rb_type in
          let rename, rb_ident = bind rename rb_ident in
          Rexp_let
            ({ rb_kind; rb_ident; rb_type; rb_expr }, map_rexp rename body)
      | Rexp_fun (param, param_type, body) ->
          let param_type = type_expr param_type in
          let rename, param = bind rename param in
          Rexp_fun (param, param_type, map_rexp rename body)
      | Rexp_match (scrutinee, cases) ->
          Rexp_match
            (map_rexp rename scrutinee, List.map (map_case rename) cases)
    in
    { rexp_desc;
      rexp_type = type_expr rexp.rexp_type;
      rexp_loc = location rexp.rexp_loc }
  and map_case rename { rc_lhs; rc_guard; rc_rhs } =
    let rename, rc_lhs = map_pat rename rc_lhs in
    { rc_lhs;
      rc_guard = Option.map (map_rexp rename) rc_guard;
      rc_rhs = map_rexp rename rc_rhs }
  and map_pat rename pat =
    let rename, rpat_desc =
      match pat.rpat_desc with
      | Rpat_any -> rename, Rpat_any
      | Rpat_constant constant ->
          rename, Rpat_constant (map_constant constant)
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
      | Rpat_construct (path, args) ->
          let path =
            match constructor_path with Some f -> f path | None -> path
          in
          let rename, args =
            List.fold_left_map
              (fun rename p -> map_pat rename p)
              rename args
          in
          rename, Rpat_construct (path, args)
      | Rpat_alias (p, id) ->
          let rename, p = map_pat rename p in
          let rename, id = bind rename id in
          rename, Rpat_alias (p, id)
    in
    rename,
    { rpat_desc;
      rpat_type = type_expr pat.rpat_type;
      rpat_loc = location pat.rpat_loc }
  in
  map_rexp rename rexp

let rec iter_pattern f pat =
  f pat;
  match pat.rpat_desc with
  | Rpat_any | Rpat_var _ | Rpat_constant _ -> ()
  | Rpat_tuple components ->
      List.iter (fun (_, pat) -> iter_pattern f pat) components
  | Rpat_construct (_, args) -> List.iter (iter_pattern f) args
  | Rpat_alias (pat, _) -> iter_pattern f pat

let iter ?(expression = ignore) ?(pattern = ignore) ?(type_expr = ignore)
    rexp =
  let rec walk rexp =
    expression rexp;
    type_expr rexp.rexp_type;
    match rexp.rexp_desc with
    | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> ()
    | Rexp_apply (fn, args) ->
        walk fn;
        List.iter (fun (_, arg) -> walk arg) args
    | Rexp_tuple components ->
        List.iter (fun (_, component) -> walk component) components
    | Rexp_construct (_, args) -> List.iter walk args
    | Rexp_record (fields, extended)
    | Rexp_record_unboxed_product (fields, extended) ->
        List.iter (fun (_, _, field) -> walk field) fields;
        Option.iter walk extended
    | Rexp_array (_, elements) -> List.iter walk elements
    | Rexp_field (record, _, _) -> walk record
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        walk cond; walk ifso; Option.iter walk ifnot
    | Rexp_sequence (first, second) -> walk first; walk second
    | Rexp_let ({ rb_type; rb_expr; _ }, body) ->
        type_expr rb_type;
        walk rb_expr;
        walk body
    | Rexp_fun (_, param_type, body) -> type_expr param_type; walk body
    | Rexp_match (scrutinee, cases) ->
        walk scrutinee;
        List.iter case cases
  and case { rc_lhs; rc_guard; rc_rhs } =
    iter_pattern (fun pat -> pattern pat; type_expr pat.rpat_type) rc_lhs;
    Option.iter walk rc_guard;
    walk rc_rhs
  in
  walk rexp

let fold_types f init rexp =
  let result = ref init in
  iter ~type_expr:(fun ty -> result := f !result ty) rexp;
  !result

(* Alpha-equivalence *)

(* [Pconst_string] carries the location of the string contents inside the
   description; it is not part of the syntax and must not be part of type
   identity. *)
let constant_equal (c1 : Parsetree.constant) (c2 : Parsetree.constant) =
  match c1.pconst_desc, c2.pconst_desc with
  | Pconst_string (s1, _, d1), Pconst_string (s2, _, d2) ->
      String.equal s1 s2 && Option.equal String.equal d1 d2
  | desc1, desc2 -> desc1 = desc2

let equal ~pairs rexp1 rexp2 =
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
    | Rexp_var id1, Rexp_var id2 -> var_eq pairs id1 id2
    | Rexp_ident p1, Rexp_ident p2 -> Path.same p1 p2
    | Rexp_var id1, Rexp_ident (Pident id2)
    | Rexp_ident (Pident id1), Rexp_var id2 -> var_eq pairs id1 id2
    | Rexp_constant c1, Rexp_constant c2 -> constant_equal c1 c2
    | Rexp_apply (f1, args1), Rexp_apply (f2, args2) ->
        eq pairs f1 f2
        && List.compare_lengths args1 args2 = 0
        && List.for_all2
             (fun (l1, a1) (l2, a2) -> l1 = l2 && eq pairs a1 a2)
             args1 args2
    | Rexp_tuple c1, Rexp_tuple c2 ->
        List.compare_lengths c1 c2 = 0
        && List.for_all2
             (fun (l1, e1) (l2, e2) -> l1 = l2 && eq pairs e1 e2)
             c1 c2
    | Rexp_construct (p1, args1), Rexp_construct (p2, args2) ->
        Path.same p1 p2
        && List.compare_lengths args1 args2 = 0
        && List.for_all2 (eq pairs) args1 args2
    | Rexp_record (f1, e1), Rexp_record (f2, e2)
    | ( Rexp_record_unboxed_product (f1, e1),
        Rexp_record_unboxed_product (f2, e2) ) ->
        List.compare_lengths f1 f2 = 0
        && List.for_all2
             (fun (p1, l1, e1) (p2, l2, e2) ->
               Path.same p1 p2
               && String.equal l1 l2
               && eq pairs e1 e2)
             f1 f2
        && Option.equal (eq pairs) e1 e2
    | Rexp_array (m1, es1), Rexp_array (m2, es2) ->
        m1 = m2
        && List.compare_lengths es1 es2 = 0
        && List.for_all2 (eq pairs) es1 es2
    | Rexp_field (e1, p1, l1), Rexp_field (e2, p2, l2) ->
        Path.same p1 p2
        && String.equal l1 l2
        && eq pairs e1 e2
    | Rexp_ifthenelse (c1, t1, e1), Rexp_ifthenelse (c2, t2, e2) ->
        eq pairs c1 c2 && eq pairs t1 t2 && Option.equal (eq pairs) e1 e2
    | Rexp_sequence (f1, s1), Rexp_sequence (f2, s2) ->
        eq pairs f1 f2 && eq pairs s1 s2
    | Rexp_let (b1, body1), Rexp_let (b2, body2) ->
        b1.rb_kind = b2.rb_kind
        && eq pairs b1.rb_expr b2.rb_expr
        && eq ((b1.rb_ident, b2.rb_ident) :: pairs) body1 body2
    | Rexp_fun (p1, _, body1), Rexp_fun (p2, _, body2) ->
        eq ((p1, p2) :: pairs) body1 body2
    | Rexp_match (s1, cases1), Rexp_match (s2, cases2) ->
        eq pairs s1 s2
        && List.compare_lengths cases1 cases2 = 0
        && List.for_all2 (eq_case pairs) cases1 cases2
    | ( ( Rexp_var _ | Rexp_ident _ | Rexp_constant _
        | Rexp_apply _ | Rexp_tuple _ | Rexp_construct _ | Rexp_record _
        | Rexp_record_unboxed_product _ | Rexp_array _ | Rexp_field _
        | Rexp_ifthenelse _ | Rexp_sequence _ | Rexp_let _ | Rexp_fun _
        | Rexp_match _ ), _ ) ->
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
    | Rpat_construct (c1, args1), Rpat_construct (c2, args2) ->
        if Path.same c1 c2 && List.compare_lengths args1 args2 = 0 then
          List.fold_left2
            (fun pairs p1 p2 ->
              Option.bind pairs (fun pairs -> eq_pat pairs p1 p2))
            (Some pairs) args1 args2
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

let untype ?(expression = fun _ exp -> exp)
    ?(function_label = fun _ -> Asttypes.Nolabel)
    ~var_name ~value_ident ~constructor_ident ~label_ident rexp =
  let open Ast_helper in
  let lid_of_name name = Location.mknoloc (Longident.Lident name) in
  let rec untype_rexp rexp =
    let loc = rexp.rexp_loc in
    expression rexp (untype_desc loc rexp)
  and untype_desc loc rexp =
    match rexp.rexp_desc with
    | Rexp_var id -> Exp.ident ~loc (lid_of_name (var_name id))
    | Rexp_ident path -> Exp.ident ~loc (value_ident path)
    | Rexp_constant const -> Exp.constant ~loc const
    | Rexp_apply (fn, args) ->
        Exp.apply ~loc (untype_rexp fn)
          (List.map (fun (lbl, arg) -> lbl, untype_rexp arg) args)
    | Rexp_tuple components ->
        Exp.tuple ~loc
          (List.map (fun (lbl, c) -> lbl, untype_rexp c) components)
    | Rexp_construct (path, args) ->
        let arg =
          match List.map untype_rexp args with
          | [] -> None
          | [arg] -> Some arg
          | args -> Some (Exp.tuple ~loc (List.map (fun arg -> None, arg) args))
        in
        Exp.construct ~loc (constructor_ident path)
          arg
    | Rexp_record (fields, extended) ->
        Exp.record ~loc
          (List.map
             (fun (path, label, e) ->
                label_ident path label, untype_rexp e)
             fields)
          (Option.map untype_rexp extended)
    | Rexp_record_unboxed_product (fields, extended) ->
        Exp.record_unboxed_product ~loc
          (List.map
             (fun (path, label, e) ->
                label_ident path label, untype_rexp e)
             fields)
          (Option.map untype_rexp extended)
    | Rexp_array (mutability, elements) ->
        Exp.array ~loc mutability (List.map untype_rexp elements)
    | Rexp_field (e, path, label) ->
        Exp.field ~loc (untype_rexp e) (label_ident path label)
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        Exp.ifthenelse ~loc (untype_rexp cond) (untype_rexp ifso)
          (Option.map untype_rexp ifnot)
    | Rexp_sequence (first, second) ->
        Exp.sequence ~loc (untype_rexp first) (untype_rexp second)
    | Rexp_let ({ rb_kind; rb_ident; rb_expr; _ }, body) ->
        let name = Location.mknoloc (var_name rb_ident) in
        begin match rb_kind with
        | Rbind_value ->
            Exp.let_ ~loc Immutable Nonrecursive
              [Vb.mk (Pat.var name) (untype_rexp rb_expr)]
              (untype_rexp body)
        | Rbind_refine ->
            Exp.let_refine ~loc name (untype_rexp rb_expr)
              (untype_rexp body)
        end
    | Rexp_fun (param, _, body) ->
        Exp.function_ ~loc
          [ { pparam_desc =
                Pparam_val
                  ( function_label rexp, None,
                    Pat.var (Location.mknoloc (var_name param)) );
              pparam_loc = Location.none } ]
          { mode_annotations = [];
            ret_mode_annotations = [];
            ret_type_constraint = None }
          (Pfunction_body (untype_rexp body))
    | Rexp_match (scrutinee, cases) ->
        Exp.match_ ~loc (untype_rexp scrutinee) (List.map untype_case cases)
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
    | Rpat_construct (path, args) ->
        let arg =
          match List.map untype_pat args with
          | [] -> None
          | [arg] -> Some ([], arg)
          | args ->
              Some
                ([], Pat.tuple ~loc
                       (List.map (fun arg -> None, arg) args) Asttypes.Closed)
        in
        Pat.construct ~loc (constructor_ident path)
          arg
    | Rpat_alias (p, id) ->
        Pat.alias ~loc (untype_pat p) (Location.mknoloc (var_name id))
  in
  untype_rexp rexp

(* Occurrence checks used by the printer *)

let exists_rexp pred rexp =
  let exception Found in
  match iter ~expression:(fun e -> if pred e then raise Found) rexp with
  | () -> false
  | exception Found -> true

let find_dependency_path (f : Path.t -> 'a option) rexp : 'a option =
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
         | Rexp_ident path | Rexp_construct (path, _) -> check path
         | Rexp_record (fields, _)
         | Rexp_record_unboxed_product (fields, _) ->
             List.exists (fun (path, _, _) -> check path) fields
         | Rexp_field (_, path, _) -> check path
         | Rexp_match (_, cases) ->
             let exception Found in
             let pattern p =
               match p.rpat_desc with
               | Rpat_construct (path, _) when check path -> raise Found
               | _ -> ()
             in
             begin match
               List.iter (fun c -> iter_pattern pattern c.rc_lhs) cases
             with
             | () -> false
             | exception Found -> true
             end
         | _ -> false)
       rexp
     : bool);
  !result

let find_ident ids rexp =
  match
    find_dependency_path (Path.find_free_opt (Ident.Set.elements ids)) rexp
  with
  | Some _ as found -> found
  | None ->
      let found = ref None in
      ignore
        (exists_rexp
           (fun rexp ->
             match rexp.rexp_desc with
             | Rexp_var id when Ident.Set.mem id ids ->
                 found := Some id;
                 true
             | _ -> false)
           rexp
          : bool);
      !found

let bound_idents rexp =
  let ids = ref Ident.Set.empty in
  let bind id = ids := Ident.Set.add id !ids in
  let pattern pat =
    match pat.rpat_desc with
    | Rpat_var id | Rpat_alias (_, id) -> bind id
    | Rpat_any | Rpat_constant _ | Rpat_tuple _ | Rpat_construct _ -> ()
  in
  let expression rexp =
    match rexp.rexp_desc with
    | Rexp_let ({ rb_ident; _ }, _) -> bind rb_ident
    | Rexp_fun (param, _, _) -> bind param
    | _ -> ()
  in
  iter ~expression ~pattern rexp;
  !ids
