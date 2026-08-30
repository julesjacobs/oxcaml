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
<<<<<<< HEAD
    ?value_path ?constructor_path ?type_path ?(type_expr = Fun.id)
    ?(location = Fun.id) rexp =
||||||| parent of 40c8375b60 (Automated commit: Import compiler changes from 31e6e0ed01ab17f8dead4c9c71786ac712a9fcc0)
    ?value_path
    ?constructor_path ?type_path ?(type_expr = Fun.id)
    ?(location = Fun.id) rexp =
=======
    ?value_path
    ?constructor_path ?type_path ?(type_expr = Fun.id)
    ?(location = Fun.id) ?(expression = Fun.id) rexp =
>>>>>>> 40c8375b60 (Automated commit: Import compiler changes from 31e6e0ed01ab17f8dead4c9c71786ac712a9fcc0)
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
              | Some path -> Rexp_ident path
              | None -> Rexp_var id
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
      | Rexp_logical_equal (left, right) ->
          Rexp_logical_equal (map_rexp rename left, map_rexp rename right)
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
      | Rexp_let
          ({ rb_kind; rb_ident; rb_type; rb_type_constraint; rb_expr }, body) ->
          let rb_expr = map_rexp rename rb_expr in
          let rb_type = type_expr rb_type in
          let rename, rb_ident = bind rename rb_ident in
          Rexp_let
            ({ rb_kind; rb_ident; rb_type; rb_type_constraint; rb_expr },
             map_rexp rename body)
      | Rexp_fun (param, param_type, constrained, body) ->
          let param_type = type_expr param_type in
          let rename, param = bind rename param in
          Rexp_fun (param, param_type, constrained, map_rexp rename body)
      | Rexp_match (scrutinee, cases) ->
          Rexp_match
            (map_rexp rename scrutinee, List.map (map_case rename) cases)
    in
<<<<<<< HEAD
    { rexp_desc;
      rexp_type = type_expr rexp.rexp_type;
      rexp_type_constraint = rexp.rexp_type_constraint;
      rexp_loc = location rexp.rexp_loc }
||||||| parent of 9cf9d2e29e (Automated commit: Import compiler changes from 31e6e0ed01ab17f8dead4c9c71786ac712a9fcc0)
    { rexp_desc;
      rexp_type = type_expr rexp.rexp_type;
      rexp_loc = location rexp.rexp_loc }
=======
    expression
      { rexp_desc;
        rexp_type = type_expr rexp.rexp_type;
        rexp_loc = location rexp.rexp_loc }
>>>>>>> 9cf9d2e29e (Automated commit: Import compiler changes from 31e6e0ed01ab17f8dead4c9c71786ac712a9fcc0)
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
      rpat_type_constraint = pat.rpat_type_constraint;
      rpat_loc = location pat.rpat_loc }
  in
  map_rexp rename rexp

let fold_types_gen ~constraints_only f init rexp =
  let f constrained init ty =
    if constrained || not constraints_only then f init ty else init
  in
  let rec expression init rexp =
    let init = f rexp.rexp_type_constraint init rexp.rexp_type in
    match rexp.rexp_desc with
    | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> init
    | Rexp_apply (fn, args) ->
        List.fold_left
          (fun init (_, arg) -> expression init arg)
          (expression init fn) args
    | Rexp_logical_equal (left, right) ->
        expression (expression init left) right
    | Rexp_tuple components ->
        List.fold_left
          (fun init (_, component) -> expression init component)
          init components
    | Rexp_construct (_, args) -> List.fold_left expression init args
    | Rexp_record (fields, extended)
    | Rexp_record_unboxed_product (fields, extended) ->
        let init =
          List.fold_left
            (fun init (_, _, field) -> expression init field)
            init fields
        in
        Option.fold ~none:init ~some:(expression init) extended
    | Rexp_array (_, elements) -> List.fold_left expression init elements
    | Rexp_field (record, _, _) -> expression init record
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        let init = expression (expression init cond) ifso in
        Option.fold ~none:init ~some:(expression init) ifnot
    | Rexp_sequence (first, second) ->
        expression (expression init first) second
    | Rexp_let ({ rb_type; rb_type_constraint; rb_expr; _ }, body) ->
        expression (expression (f rb_type_constraint init rb_type) rb_expr) body
    | Rexp_fun (_, param_type, constrained, body) ->
        expression (f constrained init param_type) body
    | Rexp_match (scrutinee, cases) ->
        List.fold_left case (expression init scrutinee) cases
  and case init { rc_lhs; rc_guard; rc_rhs } =
    let init = pattern init rc_lhs in
    let init = Option.fold ~none:init ~some:(expression init) rc_guard in
    expression init rc_rhs
  and pattern init pat =
    let init = f pat.rpat_type_constraint init pat.rpat_type in
    match pat.rpat_desc with
    | Rpat_any | Rpat_var _ | Rpat_constant _ -> init
    | Rpat_tuple components ->
        List.fold_left
          (fun init (_, pat) -> pattern init pat)
          init components
    | Rpat_construct (_, args) -> List.fold_left pattern init args
    | Rpat_alias (pat, _) -> pattern init pat
  in
  expression init rexp

let fold_types f = fold_types_gen ~constraints_only:false f

let fold_type_constraints f = fold_types_gen ~constraints_only:true f

let iter_scoped_dependencies ~bound ~ident ~type_expr rexp =
  let path bound path =
    List.iter
      (fun id -> if not (Ident.Set.mem id bound) then ident id)
      (Path.heads path)
  in
  let rec pattern bound pat =
    type_expr ~bound pat.rpat_type;
    match pat.rpat_desc with
    | Rpat_any | Rpat_constant _ -> bound
    | Rpat_var id -> Ident.Set.add id bound
    | Rpat_tuple components ->
        List.fold_left
          (fun bound (_, pat) -> pattern bound pat)
          bound components
    | Rpat_construct (constructor, args) ->
        path bound constructor;
        List.fold_left pattern bound args
    | Rpat_alias (pat, id) ->
        Ident.Set.add id (pattern bound pat)
  in
  let rec expression bound rexp =
    type_expr ~bound rexp.rexp_type;
    match rexp.rexp_desc with
    | Rexp_var id ->
        if not (Ident.Set.mem id bound) then ident id
    | Rexp_ident value -> path bound value
    | Rexp_constant _ -> ()
    | Rexp_apply (fn, args) ->
        expression bound fn;
        List.iter (fun (_, arg) -> expression bound arg) args
    | Rexp_tuple components ->
        List.iter (fun (_, component) -> expression bound component) components
    | Rexp_construct (constructor, args) ->
        path bound constructor;
        List.iter (expression bound) args
    | Rexp_record (fields, extended)
    | Rexp_record_unboxed_product (fields, extended) ->
        List.iter
          (fun (owner, _, field) ->
             path bound owner;
             expression bound field)
          fields;
        Option.iter (expression bound) extended
    | Rexp_array (_, elements) -> List.iter (expression bound) elements
    | Rexp_field (record, owner, _) ->
        path bound owner;
        expression bound record
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        expression bound cond;
        expression bound ifso;
        Option.iter (expression bound) ifnot
    | Rexp_sequence (first, second) ->
        expression bound first;
        expression bound second
    | Rexp_logical_equal (left, right) ->
        expression bound left;
        expression bound right
    | Rexp_let ({ rb_ident; rb_type; rb_expr; _ }, body) ->
        type_expr ~bound rb_type;
        expression bound rb_expr;
        expression (Ident.Set.add rb_ident bound) body
    | Rexp_fun (id, param_type, _, body) ->
        type_expr ~bound param_type;
        expression (Ident.Set.add id bound) body
    | Rexp_match (scrutinee, cases) ->
        expression bound scrutinee;
        List.iter (case bound) cases
  and case bound { rc_lhs; rc_guard; rc_rhs } =
    let bound = pattern bound rc_lhs in
    Option.iter (expression bound) rc_guard;
    expression bound rc_rhs
  in
  expression bound rexp

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
    | Rexp_logical_equal (l1, r1), Rexp_logical_equal (l2, r2) ->
        eq pairs l1 l2 && eq pairs r1 r2
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
    | Rexp_fun (p1, _, _, body1), Rexp_fun (p2, _, _, body2) ->
        eq ((p1, p2) :: pairs) body1 body2
    | Rexp_match (s1, cases1), Rexp_match (s2, cases2) ->
        eq pairs s1 s2
        && List.compare_lengths cases1 cases2 = 0
        && List.for_all2 (eq_case pairs) cases1 cases2
    | ( ( Rexp_var _ | Rexp_ident _ | Rexp_constant _
        | Rexp_apply _ | Rexp_logical_equal _ | Rexp_tuple _
        | Rexp_construct _ | Rexp_record _
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

let untype ?(type_constraint = fun _ -> None)
    ?(expression = fun _ exp -> exp)
    ?(function_label = fun _ -> Asttypes.Nolabel)
    ~var_name ~value_ident ~constructor_ident ~label_ident rexp =
  let open Ast_helper in
  let lid_of_name name = Location.mknoloc (Longident.Lident name) in
  let constrain_pattern constrained ty pat =
    match if constrained then type_constraint ty else None with
    | None -> pat
    | Some ty -> Pat.constraint_ ~loc:pat.Parsetree.ppat_loc pat (Some ty) []
  in
  let rec untype_rexp rexp =
    let loc = rexp.rexp_loc in
    let exp = match rexp.rexp_desc with
    | Rexp_var id -> Exp.ident ~loc (lid_of_name (var_name id))
    | Rexp_ident path -> Exp.ident ~loc (value_ident path)
    | Rexp_constant const -> Exp.constant ~loc const
    | Rexp_apply (fn, args) ->
        Exp.apply ~loc (untype_rexp fn)
          (List.map (fun (lbl, arg) -> lbl, untype_rexp arg) args)
    | Rexp_logical_equal (left, right) ->
        Exp.apply ~loc
          (Exp.ident ~loc (lid_of_name "==="))
          [ Nolabel, untype_rexp left; Nolabel, untype_rexp right ]
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
    | Rexp_let
          ({ rb_kind; rb_ident; rb_type; rb_type_constraint; rb_expr }, body) ->
        let name = Location.mknoloc (var_name rb_ident) in
        begin match rb_kind with
        | Rbind_value ->
            Exp.let_ ~loc Immutable Nonrecursive
              [Vb.mk
                 (constrain_pattern rb_type_constraint rb_type (Pat.var name))
                 (untype_rexp rb_expr)]
              (untype_rexp body)
        | Rbind_refine ->
            Exp.let_refine ~loc name (untype_rexp rb_expr)
              (untype_rexp body)
        end
    | Rexp_fun (param, param_type, constrained, body) ->
        Exp.function_ ~loc
          [ { pparam_desc =
                Pparam_val
                  ( function_label rexp, None,
                    constrain_pattern constrained param_type
                      (Pat.var (Location.mknoloc (var_name param))) );
              pparam_loc = Location.none } ]
          { mode_annotations = [];
            ret_mode_annotations = [];
            ret_type_constraint = None }
          (Pfunction_body (untype_rexp body))
    | Rexp_match (scrutinee, cases) ->
        Exp.match_ ~loc (untype_rexp scrutinee) (List.map untype_case cases)
    in
    let exp = match if rexp.rexp_type_constraint
          then type_constraint rexp.rexp_type else None with
    | None -> exp
    | Some ty -> Exp.constraint_ ~loc exp (Some ty) []
    in
    expression rexp exp
  and untype_case { rc_lhs; rc_guard; rc_rhs } =
    Exp.case (untype_pat rc_lhs)
      ?guard:(Option.map untype_rexp rc_guard)
      (untype_rexp rc_rhs)
  and untype_pat pat =
    let loc = pat.rpat_loc in
    let pattern = match pat.rpat_desc with
    | Rpat_any -> Pat.any ~loc ()
    | Rpat_var id ->
        Pat.var ~loc (Location.mknoloc (var_name id))
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
    constrain_pattern pat.rpat_type_constraint pat.rpat_type pattern
  in
  untype_rexp rexp

(* Occurrence checks used by the printer *)

let exists_rexp pred rexp =
  let exception Found in
  let rec walk rexp =
    if pred rexp then raise Found;
    match rexp.rexp_desc with
    | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> ()
    | Rexp_apply (fn, args) ->
        walk fn;
        List.iter (fun (_, arg) -> walk arg) args
    | Rexp_logical_equal (left, right) -> walk left; walk right
    | Rexp_tuple components -> List.iter (fun (_, c) -> walk c) components
    | Rexp_construct (_, args) -> List.iter walk args
    | Rexp_record (fields, extended)
    | Rexp_record_unboxed_product (fields, extended) ->
        List.iter (fun (_, _, e) -> walk e) fields;
        Option.iter walk extended
    | Rexp_array (_, elements) -> List.iter walk elements
    | Rexp_field (e, _, _) -> walk e
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        walk cond; walk ifso; Option.iter walk ifnot
    | Rexp_sequence (first, second) -> walk first; walk second
    | Rexp_let ({ rb_expr; _ }, body) -> walk rb_expr; walk body
    | Rexp_fun (_, _, _, body) -> walk body
    | Rexp_match (scrutinee, cases) ->
        walk scrutinee;
        List.iter
          (fun { rc_guard; rc_rhs; _ } ->
            Option.iter walk rc_guard;
            walk rc_rhs)
          cases
  in
  match walk rexp with () -> false | exception Found -> true

<<<<<<< HEAD
let iter_value_idents f rexp =
  ignore (exists_rexp (fun rexp ->
    begin match rexp.rexp_desc with
    | Rexp_var id | Rexp_ident (Path.Pident id) -> f id
    | _ -> ()
    end;
    false) rexp : bool)

||||||| parent of 40c8375b60 (Automated commit: Import compiler changes from 31e6e0ed01ab17f8dead4c9c71786ac712a9fcc0)
=======
let logical_definition_body rexp =
  map ~expression:(fun rexp ->
    match rexp.rexp_desc with
    | Rexp_let (binding, body) ->
        let used =
          exists_rexp
            (fun exp -> match exp.rexp_desc with
               | Rexp_var id -> Ident.same id binding.rb_ident
               | _ -> false)
            body
        in
        if not used then { rexp with rexp_desc = body.rexp_desc }
        else
          begin match binding.rb_kind with
          | Rbind_value -> rexp
          | Rbind_refine ->
              let binding =
                { binding with
                  rb_kind = Rbind_value;
                  rb_expr =
                    { binding.rb_expr with rexp_type = binding.rb_type } }
              in
              { rexp with rexp_desc = Rexp_let (binding, body) }
          end
    | _ -> rexp)
    rexp

>>>>>>> 40c8375b60 (Automated commit: Import compiler changes from 31e6e0ed01ab17f8dead4c9c71786ac712a9fcc0)
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
             let rec pat_path p =
               match p.rpat_desc with
               | Rpat_construct (path, args) ->
                   check path
                   || List.exists pat_path args
               | Rpat_alias (p, _) -> pat_path p
               | Rpat_tuple ps -> List.exists (fun (_, p) -> pat_path p) ps
               | Rpat_any | Rpat_var _ | Rpat_constant _ -> false
             in
             List.exists (fun c -> pat_path c.rc_lhs) cases
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
  let rec pattern ids pat =
    match pat.rpat_desc with
    | Rpat_any | Rpat_constant _ -> ids
    | Rpat_var id -> Ident.Set.add id ids
    | Rpat_tuple components ->
        List.fold_left
          (fun ids (_, pat) -> pattern ids pat)
          ids components
    | Rpat_construct (_, args) -> List.fold_left pattern ids args
    | Rpat_alias (pat, id) -> Ident.Set.add id (pattern ids pat)
  in
  let rec expression ids rexp =
    match rexp.rexp_desc with
    | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> ids
    | Rexp_apply (fn, args) ->
        List.fold_left
          (fun ids (_, arg) -> expression ids arg)
          (expression ids fn) args
    | Rexp_logical_equal (left, right) ->
        expression (expression ids left) right
    | Rexp_tuple components ->
        List.fold_left
          (fun ids (_, component) -> expression ids component)
          ids components
    | Rexp_construct (_, args) -> List.fold_left expression ids args
    | Rexp_record (fields, extended)
    | Rexp_record_unboxed_product (fields, extended) ->
        let ids =
          List.fold_left
            (fun ids (_, _, field) -> expression ids field)
            ids fields
        in
        Option.fold ~none:ids ~some:(expression ids) extended
    | Rexp_array (_, elements) -> List.fold_left expression ids elements
    | Rexp_field (record, _, _) -> expression ids record
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        let ids = expression (expression ids cond) ifso in
        Option.fold ~none:ids ~some:(expression ids) ifnot
    | Rexp_sequence (first, second) ->
        expression (expression ids first) second
    | Rexp_let ({ rb_ident; rb_expr; _ }, body) ->
        expression
          (Ident.Set.add rb_ident (expression ids rb_expr))
          body
    | Rexp_fun (param, _, _, body) ->
        expression (Ident.Set.add param ids) body
    | Rexp_match (scrutinee, cases) ->
        List.fold_left
          (fun ids { rc_lhs; rc_guard; rc_rhs } ->
             let ids = pattern ids rc_lhs in
             let ids =
               Option.fold ~none:ids ~some:(expression ids) rc_guard
             in
             expression ids rc_rhs)
          (expression ids scrutinee) cases
  in
  expression Ident.Set.empty rexp
