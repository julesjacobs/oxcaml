(* TEST
 readonly_files = "predicate_typing_defs.mli";
 setup-ocamlc.opt-build-env;
 module = "predicate_typing_defs.mli";
 ocamlc.opt;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 include ocamlcommon;
 expect;
*)

open Types

(* A failed reentry must restore payload links.  Reusing the identical fresh
   payload for a successful judgment distinguishes transaction rollback from
   ordinary failed-phrase recovery. *)
let failure_rolls_back failing_source =
  let payload =
    Ctype.newvar
      (Jkind.Builtin.value ~why:Jkind.History.Default_type_jkind)
  in
  let env = Lazy.force Env.initial in
  let type_predicate source =
    let predicate = Parse.expression (Lexing.from_string source) in
    match
      Typecore.type_refinement_predicate
        env ~loc:predicate.pexp_loc ~payload ~binders:[] predicate
    with
    | _ -> true
    | exception _ -> false
  in
  let failed = not (type_predicate failing_source) in
  let restored = match get_desc payload with Tvar _ -> true | _ -> false in
  let subsequent =
    type_predicate "match _ with 0 -> true | _ -> false"
  in
  let became_int =
    match get_desc payload with
    | Tconstr (path, [], _) -> Path.same path Predef.path_int
    | _ -> false
  in
  failed && restored && subsequent && became_int
;;

let typecore_failure_rolls_back =
  failure_rolls_back "if _ then true else 0"
;;

(* This predicate reaches mirror construction before [%apply] is rejected.
   Its payload links must be rolled back just as for an ordinary Typecore
   failure, so the identical variable remains usable by a later judgment. *)
let mirror_failure_rolls_back =
  failure_rolls_back "(fun n -> n > 0) @@ _"
;;

[%%expect{|
val failure_rolls_back : string -> bool = <fun>
val typecore_failure_rolls_back : bool = true
val mirror_failure_rolls_back : bool = true
|}]

let successful_reentry_discards_saved_types =
  let before = Cmt_format.get_saved_types () in
  let predicate = Parse.expression (Lexing.from_string "true") in
  ignore
    (Typecore.type_refinement_predicate
       (Lazy.force Env.initial)
       ~loc:predicate.pexp_loc ~payload:Predef.type_unit ~binders:[] predicate);
  let after = Cmt_format.get_saved_types () in
  List.length before = List.length after
  && List.for_all2 ( == ) before after
;;

[%%expect{|
val successful_reentry_discards_saved_types : bool = true
|}]

(* Application correspondence must not use locations as reusable keys.
   PPXs commonly erase every location; reordered labels pair structurally and
   equal-label arguments still consume their occurrences. *)
let duplicate_locations_preserve_application_correspondence =
  let env = Lazy.force Env.initial in
  let arrow label argument result =
    Ctype.newty
      (Tarrow
         ((label, None, Mode.Alloc.legacy, Mode.Alloc.legacy),
          Ctype.newmono argument, result, Types.commu_ok))
  in
  let fn_type =
    arrow (Types.Labelled "x") Predef.type_int
      (arrow (Types.Labelled "y") Predef.type_bool
         (arrow Types.Nolabel Predef.type_int
            (arrow Types.Nolabel Predef.type_string Predef.type_bool)))
  in
  let sort =
    match Ctype.type_sort ~why:Let_binding ~fixed:false env fn_type with
    | Ok sort -> sort
    | Error _ -> assert false
  in
  let fn_id = Ident.create_local "f" in
  let fn_desc =
    { val_type = fn_type;
      val_kind = Val_reg sort;
      val_lpoly = Lpoly.determined [];
      val_attributes = [];
      val_zero_alloc = Zero_alloc.default;
      val_modalities = Mode.Modality.undefined;
      val_loc = Location.none;
      val_uid = Uid.mk ~current_unit:(Env.get_current_unit ()) }
  in
  let env = Env.add_value ~mode:Mode.Value.legacy fn_id fn_desc env in
  let predicate =
    Parse.expression (Lexing.from_string "f ~y:true ~x:0 1 \"s\"")
  in
  let remove_locations =
    let open Ast_mapper in
    { default_mapper with
      location = (fun _mapper _location -> Location.none) }
  in
  let predicate = remove_locations.expr remove_locations predicate in
  let mirror =
    match
      Typecore.type_refinement_predicate
        env
        ~loc:predicate.pexp_loc ~payload:Predef.type_unit ~binders:[] predicate
    with
    | mirror -> Some mirror
    | exception exn ->
        Location.report_exception Format.std_formatter exn;
        None
  in
  let has_type path expression =
    match expression.rexp_type with
    | Some ty ->
        begin match get_desc ty with
        | Tconstr (actual, [], _) -> Path.same actual path
        | _ -> false
        end
    | None -> false
  in
  match mirror with
  | None -> false
  | Some { rexp_desc =
      Rexp_apply
        (_, [ (Asttypes.Labelled "y", y);
              (Asttypes.Labelled "x", x);
              (Asttypes.Nolabel, n);
              (Asttypes.Nolabel, s) ]);
           _ } ->
      has_type Predef.path_bool y
      && has_type Predef.path_int x
      && has_type Predef.path_int n
      && has_type Predef.path_string s
  | Some _ -> false
;;

[%%expect{|
val duplicate_locations_preserve_application_correspondence : bool = true
|}]

(* A PPX may erase every source location while using positional syntax for a
   labelled function.  RED's legacy completeness heuristic rejects first;
   GREEN identifies the ambiguous source/typed argument pairing.  Both paths
   must be located errors, never fatal correspondence failures. *)
let duplicate_locations_ambiguous_application_is_located =
  let env = Lazy.force Env.initial in
  let total_mode =
    Mode.Alloc.of_const
      { Mode.Alloc.Const.legacy with
        totality = Mode.Totality.Const.Total }
  in
  let arrow label argument result =
    Ctype.newty
      (Tarrow
         ((label, None, total_mode, total_mode),
          Ctype.newmono argument, result, Types.commu_ok))
  in
  let fn_type =
    arrow (Types.Labelled "x") Predef.type_int
      (arrow (Types.Labelled "y") Predef.type_bool Predef.type_bool)
  in
  let sort =
    match Ctype.type_sort ~why:Let_binding ~fixed:false env fn_type with
    | Ok sort -> sort
    | Error _ -> assert false
  in
  let fn_id = Ident.create_local "f" in
  let fn_desc =
    { val_type = fn_type;
      val_kind = Val_reg sort;
      val_lpoly = Lpoly.determined [];
      val_attributes = [];
      val_zero_alloc = Zero_alloc.default;
      val_modalities = Mode.Modality.undefined;
      val_loc = Location.none;
      val_uid = Uid.mk ~current_unit:(Env.get_current_unit ()) }
  in
  let fn_mode =
    Mode.Value.of_const
      { Mode.Value.Const.legacy with
        totality = Mode.Totality.Const.Total }
  in
  let env = Env.add_value ~mode:fn_mode fn_id fn_desc env in
  let predicate = Parse.expression (Lexing.from_string "f 0 true") in
  let remove_locations =
    let open Ast_mapper in
    { default_mapper with
      location = (fun _mapper _location -> Location.none) }
  in
  let predicate = remove_locations.expr remove_locations predicate in
  match
    Typecore.type_refinement_predicate
      env ~loc:predicate.pexp_loc ~payload:Predef.type_unit ~binders:[]
      predicate
  with
  | _ -> false
  | exception exn ->
      begin match Location.error_of_exn exn with
      | Some (`Ok error) ->
          Format.asprintf "%a" Location.print_report error
          |> Misc.Stdlib.String.is_substring
               ~substring:
                 "This application is complete, but surplus arguments were \
                  provided afterwards."
      | Some `Already_displayed | None -> false
      end
;;

[%%expect{|
File "_none_", line 1:
Warning 6 [labels-omitted]: labels x, y were omitted in the application of
  this function.
val duplicate_locations_ambiguous_application_is_located : bool = true
|}]

(* Drive the real queue through its public predicate-typer hook.  Synthetic
   mirrors make progress observable without relying on a fragile source-level
   inference accident. *)
let synthetic_predicate tag =
  { rexp_desc =
      Rexp_constant
        { pconst_desc = Pconst_integer (string_of_int tag, None);
          pconst_loc = Location.none };
    rexp_loc = Location.none;
    rexp_type = Some Predef.type_bool }
;;

let translate_pending_pair source =
  Typetexp.TyVarEnv.reset ();
  let core_type = Parse.core_type (Lexing.from_string source) in
  ignore
    (Typetexp.transl_simple_type
       (Lazy.force Env.initial) ~new_var_jkind:Typetexp.Sort ~closed:false
       Mode.Alloc.Const.legacy core_type)
;;

let with_predicate_typer typer f =
  let saved = !Typetexp.type_refinement_predicate in
  Typetexp.type_refinement_predicate := typer;
  Fun.protect f
    ~finally:(fun () -> Typetexp.type_refinement_predicate := saved)
;;

let queue_fixed_point_calls =
  let calls = ref 0 in
  let typer _env ~loc:_ ~payload:_ ~binders:_ _predicate =
    incr calls;
    (* Two bootstrap results differ from the first strict pass; the next
       strict pass and warning replay retain that new batch. *)
    synthetic_predicate (if !calls <= 2 then 0 else 1)
  in
  with_predicate_typer typer (fun () ->
    translate_pending_pair "x:(int{ true } * int{ x = x }) -> unit";
    !calls)
;;

(* If a later queue job fails, the successful earlier job's unification must
   roll back with the whole batch, leaving the identical variable reusable. *)
let queued_batch_failure_rolls_back =
  let calls = ref 0 in
  let first_payload = ref None in
  let original_predicates = ref [] in
  let remember_original_predicates binders =
    let visited = Hashtbl.create 8 in
    let rec walk ty =
      let id = get_id ty in
      if not (Hashtbl.mem visited id) then begin
        Hashtbl.add visited id ();
        (match get_desc ty with
         | Trefine { ref_pred; _ } ->
             original_predicates :=
               (ref_pred, !ref_pred) :: !original_predicates
         | _ -> ());
        Btype.iter_type_expr walk ty
      end
    in
    List.iter (fun (_, ty) -> walk ty) binders
  in
  let typer env ~loc:_ ~payload ~binders _predicate =
    incr calls;
    if !calls = 1 then begin
      first_payload := Some payload;
      Ctype.unify env payload Predef.type_int;
      synthetic_predicate 0
    end else if !calls = 2 then begin
      remember_original_predicates binders;
      synthetic_predicate 0
    end else if !calls = 4 then
      raise Exit
    else
      synthetic_predicate 0
  in
  let failed =
    with_predicate_typer typer (fun () ->
      match
        translate_pending_pair "x:('a{ true } * int{ x = x }) -> unit"
      with
      | () -> false
      | exception Exit -> true)
  in
  match !first_payload with
  | None -> false
  | Some payload ->
      let targets_restored =
        !original_predicates <> []
        && List.for_all
             (fun (cell, predicate) -> !cell == predicate)
             !original_predicates
      in
      let restored = match get_desc payload with Tvar _ -> true | _ -> false in
      let reusable =
        match Ctype.unify (Lazy.force Env.initial) payload Predef.type_string with
        | () -> true
        | exception _ -> false
      in
      let became_string =
        match get_desc payload with
        | Tconstr (path, [], _) -> Path.same path Predef.path_string
        | _ -> false
      in
      failed && targets_restored && restored && reusable && became_string
;;

let queue_defensive_failures_are_located =
  let located exn =
    match Location.error_of_exn exn with
    | Some (`Ok _) | Some `Already_displayed -> true
    | None -> false
  in
  let run typer =
    let payload = ref None in
    let calls = ref 0 in
    let typer env ~loc ~payload:ty ~binders predicate =
      incr calls;
      if !calls = 1 then begin
        payload := Some ty;
        Ctype.unify env ty Predef.type_int
      end;
      typer !calls env ~loc ~payload:ty ~binders predicate
    in
    let failure =
      with_predicate_typer typer (fun () ->
        match
          translate_pending_pair "x:('a{ true } * int{ x = x }) -> unit"
        with
        | () -> None
        | exception exn -> Some exn)
    in
    match failure, !payload with
    | Some exn, Some ty ->
        let restored = match get_desc ty with Tvar _ -> true | _ -> false in
        let reusable =
          match Ctype.unify (Lazy.force Env.initial) ty Predef.type_string with
          | () -> true
          | exception _ -> false
        in
        located exn && restored && reusable
    | None, _ | _, None -> false
  in
  let fuel_exhaustion =
    run (fun call _env ~loc:_ ~payload:_ ~binders:_ _predicate ->
      synthetic_predicate (((call - 1) / 2) mod 2))
  in
  let warning_replay =
    run (fun call _env ~loc:_ ~payload:_ ~binders:_ _predicate ->
      synthetic_predicate (if call <= 4 then 0 else 1))
  in
  fuel_exhaustion && warning_replay
;;

[%%expect{|
val synthetic_predicate : int -> Types.refinement_expression = <fun>
val translate_pending_pair : string -> unit = <fun>
val with_predicate_typer :
  (Env.t ->
   loc:Location.t ->
   payload:Types.type_expr ->
   binders:(Ident.t * Types.type_expr) list ->
   Parsetree.expression -> Types.refinement_expression) ->
  (unit -> 'a) -> 'a = <fun>
val queue_fixed_point_calls : int = 8
val queued_batch_failure_rolls_back : bool = true
val queue_defensive_failures_are_located : bool = true
|}]

(* Stored node types are derived annotations.  They do not participate in
   mirror equality, while source-written constraint types still do. *)
let equality_ignores_only_derived_annotations =
  let constant rexp_type =
    { rexp_desc =
        Rexp_constant
          { pconst_desc = Pconst_integer ("0", None);
            pconst_loc = Location.none };
      rexp_loc = Location.none;
      rexp_type }
  in
  let same_head ~pairs:_ left right =
    match get_desc left, get_desc right with
    | Tconstr (left, [], _), Tconstr (right, [], _) -> Path.same left right
    | _ -> false
  in
  let left = constant (Some Predef.type_int) in
  let right = constant (Some Predef.type_bool) in
  let derived_ignored =
    Vox_rexp.equal ~type_eq:same_head ~pairs:[] left right
  in
  let constrained expression ty =
    { rexp_desc = Rexp_constraint (expression, ty);
      rexp_loc = Location.none;
      rexp_type = Some ty }
  in
  let written_compared =
    not
      (Vox_rexp.equal ~type_eq:same_head ~pairs:[]
         (constrained left Predef.type_int)
         (constrained right Predef.type_bool))
  in
  derived_ignored && written_compared
;;

[%%expect{|
val equality_ignores_only_derived_annotations : bool = true
|}]

(* Inspect the producer's serialized Types graph directly.  Printing and
   syntactic type equality intentionally ignore [rexp_type], so neither is a
   substitute for these checks. *)
let imported_mirror_contract =
  let signature =
    let cmi = Cmi_format.read_cmi "ocamlc.opt/predicate_typing_defs.cmi" in
    fst cmi.Cmi_format.cmi_sign
  in
  let rec result_signature = function
    | Mty_signature signature -> signature
    | Mty_functor (_, result, _) -> result_signature result
    | Mty_strengthen (mty, _, _) -> result_signature mty
    | Mty_ident _ | Mty_alias _ -> assert false
  in
  let find_type signature name =
    List.find_map
      (function
        | Sig_type (id, declaration, _, _)
          when String.equal (Ident.name id) name -> Some (id, declaration)
        | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_module _
        | Sig_modtype _ | Sig_class _ | Sig_class_type _ | Sig_jkind _ -> None)
      signature
    |> Option.get
  in
  let find_module signature name =
    List.find_map
      (function
        | Sig_module (id, _, declaration, _, _)
          when String.equal (Ident.name id) name -> Some (id, declaration)
        | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_module _
        | Sig_modtype _ | Sig_class _ | Sig_class_type _ | Sig_jkind _ -> None)
      signature
    |> Option.get
  in
  let manifest signature name =
    match snd (find_type signature name) with
    | { type_manifest = Some manifest; _ } -> manifest
    | _ -> assert false
  in
  let predicate manifest =
    match get_desc manifest with
    | Trefine { ref_pred; _ } -> !ref_pred
    | _ -> assert false
  in
  let rec strip_poly ty =
    match get_desc ty with
    | Tpoly (ty, _) -> strip_poly ty
    | _ -> ty
  in
  let has_path expected ty =
    match get_desc (strip_poly ty) with
    | Tconstr (actual, _, _) -> Path.same expected actual
    | _ -> false
  in
  let has_node_type test expression =
    match expression.rexp_type with Some ty -> test ty | None -> false
  in
  let rec exists_expression test expression =
    test expression
    ||
    match expression.rexp_desc with
    | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> false
    | Rexp_apply (fn, args) ->
        exists_expression test fn
        || List.exists (fun (_, arg) -> exists_expression test arg) args
    | Rexp_tuple components ->
        List.exists
          (fun (_, component) -> exists_expression test component)
          components
    | Rexp_construct (_, _, arg) ->
        Option.fold ~none:false ~some:(exists_expression test) arg
    | Rexp_field (record, _, _, _) -> exists_expression test record
    | Rexp_ifthenelse (condition, ifso, ifnot) ->
        exists_expression test condition
        || exists_expression test ifso
        || Option.fold ~none:false ~some:(exists_expression test) ifnot
    | Rexp_let ({ rb_expr; _ }, body) ->
        exists_expression test rb_expr || exists_expression test body
    | Rexp_fun (_, body) -> exists_expression test body
    | Rexp_match (scrutinee, cases) ->
        exists_expression test scrutinee
        || List.exists
             (fun { rc_guard; rc_rhs; _ } ->
               Option.fold
                 ~none:false ~some:(exists_expression test) rc_guard
               || exists_expression test rc_rhs)
             cases
    | Rexp_constraint (expression, _) -> exists_expression test expression
  in
  let node_annotations_are_exact manifests =
    let valid = ref true in
    let contextual = ref 0 in
    let annotated = ref 0 in
    let saw_nested_mirror = ref false in
    let visited = ref Btype.TypeSet.empty in
    let rec type_expr ~nested ty =
      if not (Btype.TypeSet.mem ty !visited) then begin
        visited := Btype.TypeSet.add ty !visited;
        match get_desc ty with
        | Trefine { ref_payload; ref_pred; _ } ->
            if nested then saw_nested_mirror := true;
            type_expr ~nested ref_payload;
            expression !ref_pred
        | _ ->
            Btype.iter_type_expr (type_expr ~nested) ty
      end
    and expression rexp =
      let should_be_contextual =
        match rexp.rexp_desc with
        | Rexp_hole -> true
        | Rexp_var _ -> true
        | Rexp_ident _ | Rexp_constant _ | Rexp_apply _ | Rexp_tuple _
        | Rexp_construct _ | Rexp_field _ | Rexp_ifthenelse _ | Rexp_let _
        | Rexp_fun _ | Rexp_match _ | Rexp_constraint _ -> false
      in
      if should_be_contextual then incr contextual else incr annotated;
      if should_be_contextual <> Option.is_none rexp.rexp_type then
        valid := false;
      Option.iter
        (fun ty ->
          if Ctype.free_variables ty <> [] then valid := false;
          type_expr ~nested:false ty)
        rexp.rexp_type;
      match rexp.rexp_desc with
      | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> ()
      | Rexp_apply (fn, args) ->
          expression fn;
          List.iter (fun (_, arg) -> expression arg) args
      | Rexp_tuple components ->
          List.iter (fun (_, component) -> expression component) components
      | Rexp_construct (_, _, arg) ->
          Option.iter expression arg
      | Rexp_field (record, _, _, _) -> expression record
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
          expression condition;
          expression ifso;
          Option.iter expression ifnot
      | Rexp_let ({ rb_expr; _ }, body) ->
          expression rb_expr;
          expression body
      | Rexp_fun (_, body) -> expression body
      | Rexp_match (scrutinee, cases) ->
          expression scrutinee;
          List.iter
            (fun { rc_guard; rc_rhs; _ } ->
              Option.iter expression rc_guard;
              expression rc_rhs)
            cases
      | Rexp_constraint (expression_, constraint_type) ->
          expression expression_;
          type_expr ~nested:true constraint_type
    in
    List.iter (type_expr ~nested:false) manifests;
    !valid && !contextual > 0 && !annotated > 0 && !saw_nested_mirror
  in
  let root_is_bool predicate =
    has_node_type (has_path Predef.path_bool) predicate
  in
  let int_record_id, _ = find_type signature "int_record" in
  let int_record_path = Path.Pident int_record_id in
  let field_predicate = predicate (manifest signature "selected_field") in
  let exact_field =
    root_is_bool field_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_field (_, parent, "selected", _) ->
               Path.same parent int_record_path
               && has_node_type (has_path Predef.path_int) expression
           | _ -> false)
         field_predicate
  in
  let int_variant_id, _ = find_type signature "int_variant" in
  let int_variant_path = Path.Pident int_variant_id in
  let constructor_predicate =
    predicate (manifest signature "selected_constructor")
  in
  let exact_constructor =
    root_is_bool constructor_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_construct (path, _, _) ->
               Path.same path
                 (Path.Pextra_ty (int_variant_path, Path.Pcstr_ty "Selected"))
               && has_node_type (has_path int_variant_path) expression
           | _ -> false)
         constructor_predicate
  in
  let application_predicate =
    predicate (manifest signature "selected_application")
  in
  let exact_application =
    let int_to_int ty =
      match get_desc (strip_poly ty) with
      | Tarrow (_, argument, result, _) ->
          has_path Predef.path_int argument && has_path Predef.path_int result
      | _ -> false
    in
    root_is_bool application_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_fun (binder, body) ->
               has_node_type int_to_int expression
               && exists_expression
                    (fun use ->
                      match use.rexp_desc with
                      | Rexp_var used when Ident.same binder used ->
                          Option.is_none use.rexp_type
                      | _ -> false)
                    body
           | _ -> false)
         application_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_apply _ ->
               has_node_type (has_path Predef.path_int) expression
           | _ -> false)
         application_predicate
  in
  let dependent_hole_manifest = manifest signature "dependent_hole" in
  let outer_hole_predicate, dependent_hole_binder,
      dependent_hole_predicate, dependent_hole_domain,
      dependent_hole_payload =
    match get_desc (strip_poly dependent_hole_manifest) with
    | Trefine { ref_payload = arrow; ref_pred = outer_predicate; _ } ->
        begin
          match get_desc (strip_poly arrow) with
          | Tarrow ((_, Some binder, _, _), domain, codomain, _) ->
              begin
                match get_desc (strip_poly codomain) with
                | Trefine { ref_payload; ref_pred; _ } ->
                    !outer_predicate, binder, !ref_pred, domain, ref_payload
                | _ -> assert false
              end
          | _ -> assert false
        end
    | _ -> assert false
  in
  let exact_dependent_hole =
    root_is_bool outer_hole_predicate
    && begin
         match outer_hole_predicate.rexp_desc with
         | Rexp_let
             ({ rb_expr =
                  { rexp_desc =
                      Rexp_ifthenelse
                        (_, { rexp_desc = Rexp_hole; rexp_type = None; _ },
                         Some
                           { rexp_desc = Rexp_hole; rexp_type = None; _ });
                    _ };
                _ },
              _) ->
             true
         | _ -> false
       end
    && String.equal (Ident.name dependent_hole_binder) "x"
    && has_path Predef.path_int dependent_hole_domain
    && has_path Predef.path_int dependent_hole_payload
    && root_is_bool dependent_hole_predicate
    && Vox_rexp.mentions_ident
         dependent_hole_binder dependent_hole_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_hole -> Option.is_none expression.rexp_type
           | _ -> false)
         dependent_hole_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_var binder
             when Ident.same binder dependent_hole_binder ->
               Option.is_none expression.rexp_type
           | _ -> false)
         dependent_hole_predicate
  in
  let own_domain_manifest = manifest signature "generic_own_domain" in
  let own_domain_binder, own_domain, own_domain_predicate =
    match get_desc (strip_poly own_domain_manifest) with
    | Tarrow ((_, Some binder, _, _), domain, _, _) ->
        begin
          match get_desc (strip_poly domain) with
          | Ttuple ((_, first) :: _) ->
              begin
                match get_desc (strip_poly first) with
                | Trefine { ref_pred; _ } -> binder, domain, !ref_pred
                | _ -> assert false
              end
          | _ -> assert false
        end
    | _ -> assert false
  in
  let exact_own_domain =
    root_is_bool own_domain_predicate
    && Vox_rexp.mentions_ident own_domain_binder own_domain_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_var binder when Ident.same binder own_domain_binder ->
               Option.is_none expression.rexp_type
           | _ -> false)
         own_domain_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_apply _ ->
               has_node_type (has_path Predef.path_int) expression
           | _ -> false)
         own_domain_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc, expression.rexp_type with
           | Rexp_ident (_, longident), Some ty
             when String.equal (Longident.last longident.txt) "fst" ->
               begin
                 match get_desc (strip_poly ty) with
                 | Tarrow (_, argument, _, _) ->
                     Ctype.is_equal
                       (Lazy.force Env.initial)
                       false [argument] [own_domain]
                 | _ -> false
               end
           | _ -> false)
         own_domain_predicate
  in
  let _, binder_declaration = find_module signature "Binder" in
  let binder_signature = result_signature binder_declaration.md_type in
  let binder_predicate = predicate (manifest binder_signature "t") in
  (* The stored function annotation is Typecore's occurrence type, whose
     result head is stripped to [int].  The source-written constraint keeps
     the nested refinement mirror and its local binder identity. *)
  let nested_binder_parts predicate =
    match predicate.rexp_desc with
    | Rexp_let
        ({ rb_expr =
             { rexp_desc =
                 Rexp_fun
                   (binder,
                    { rexp_desc = Rexp_constraint (_, constraint_type); _ });
               rexp_type = fun_type;
               _ };
           _ },
         _) ->
        begin match get_desc constraint_type, fun_type with
        | Trefine { ref_pred = written_predicate; _ }, Some fun_type ->
            Some (binder, !written_predicate, fun_type)
        | _ -> None
        end
    | _ -> None
  in
  let has_nested_binder_contract predicate =
    match nested_binder_parts predicate with
    | Some (binder, written_predicate, fun_type) ->
        begin match get_desc (strip_poly fun_type) with
        | Tarrow (_, domain, result, _) ->
            has_path Predef.path_int domain
            && has_path Predef.path_int result
            && Vox_rexp.mentions_ident binder written_predicate
        | _ -> false
        end
    | None -> false
  in
  let nested_binder_identity = has_nested_binder_contract binder_predicate in
  (* A local mentioned only from a refinement nested in [Rexp_fun]'s stored
     result type is still a bound [Rexp_var].  It must freshen with the
     function binder when the signature is imported and substituted. *)
  let stored_binder_parts predicate =
    match predicate.rexp_desc with
    | Rexp_let
        ({ rb_expr =
             { rexp_desc = Rexp_fun (binder, _);
               rexp_type = Some fun_type;
               _ };
           _ },
         _) ->
        begin match get_desc (strip_poly fun_type) with
        | Tarrow (_, domain, result, _) ->
            begin match get_desc (strip_poly result) with
            | Tconstr (path, [ element ], _) when Path.same path Predef.path_list ->
                begin match get_desc (strip_poly element) with
                | Trefine { ref_pred; _ } ->
                    if has_path Predef.path_int domain
                    then Some (binder, !ref_pred)
                    else None
                | _ -> None
                end
            | _ -> None
            end
        | _ -> None
        end
    | _ -> None
  in
  let stored_binder_contract predicate =
    match stored_binder_parts predicate with
    | Some (binder, stored_predicate) ->
        Vox_rexp.mentions_ident binder stored_predicate
    | None -> false
  in
  let stored_manifest = manifest binder_signature "stored" in
  let stored_predicate = predicate stored_manifest in
  let stored_binder_identity =
    stored_binder_contract stored_predicate
  in
  let stored_binder_freshens =
    let copied_predicate =
      predicate (Subst.type_expr Subst.identity stored_manifest)
    in
    match
      stored_binder_parts stored_predicate,
      stored_binder_parts copied_predicate
    with
    | Some (old_binder, _), Some (new_binder, copied_stored_predicate) ->
        not (Ident.same old_binder new_binder)
        && Vox_rexp.mentions_ident new_binder copied_stored_predicate
        && not (Vox_rexp.mentions_ident old_binder copied_stored_predicate)
    | None, _ | _, None -> false
  in
  let binder_source_id, _ = find_module signature "Binder_source" in
  let _, binder_result_declaration = find_module signature "Binder_result" in
  let binder_result_signature =
    result_signature binder_result_declaration.md_type
  in
  let binder_result_manifest = manifest binder_result_signature "t" in
  let binder_result_predicate = predicate binder_result_manifest in
  let expected_value_path =
    Path.Pdot (Path.Pident binder_source_id, "zero")
  in
  let substituted_value_path_and_binder =
    has_nested_binder_contract binder_result_predicate
    && begin match nested_binder_parts binder_result_predicate with
       | Some (_, written_predicate, _) ->
           exists_expression
             (fun expression ->
               match expression.rexp_desc with
               | Rexp_ident (path, _) -> Path.same path expected_value_path
               | _ -> false)
             written_predicate
       | None -> false
       end
  in
  let substituted_stored_binder =
    stored_binder_contract
      (predicate (manifest binder_result_signature "stored"))
  in
  let field_source_id, _ = find_module signature "Field_source" in
  let _, field_result_declaration = find_module signature "Field_result" in
  let field_result_signature =
    result_signature field_result_declaration.md_type
  in
  let field_result_manifest = manifest field_result_signature "t" in
  let field_result_predicate = predicate field_result_manifest in
  let expected_field_owner =
    Path.Pdot (Path.Pident field_source_id, "t")
  in
  let substituted_field_owner =
    root_is_bool field_result_predicate
    && exists_expression
         (fun expression ->
           match expression.rexp_desc with
           | Rexp_field (_, parent, "picked", _) ->
               Path.same parent expected_field_owner
               && has_node_type (has_path Predef.path_int) expression
           | _ -> false)
         field_result_predicate
  in
  let imported_manifests =
    [ manifest signature "selected_field";
      manifest signature "selected_constructor";
      manifest signature "selected_application";
      dependent_hole_manifest;
      own_domain_manifest;
      manifest binder_signature "t";
      manifest binder_signature "stored";
      binder_result_manifest;
      manifest binder_result_signature "stored";
      field_result_manifest ]
  in
  let all_imported_manifests_are_closed =
    List.for_all
      (fun manifest -> Ctype.free_variables manifest = [])
      imported_manifests
  in
  exact_field
  && exact_constructor
  && exact_application
  && exact_dependent_hole
  && exact_own_domain
  && nested_binder_identity
  && stored_binder_identity
  && stored_binder_freshens
  && substituted_value_path_and_binder
  && substituted_stored_binder
  && substituted_field_owner
  && node_annotations_are_exact imported_manifests
  && all_imported_manifests_are_closed
;;

[%%expect{|
val imported_mirror_contract : bool = true
|}]
