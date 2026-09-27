open Types

(* Trusted externals (warning 228) *)

let library = ref false

let registered = ref false

let add_arguments () =
  if not !registered
  then begin
    registered := true;
    Clflags.add_arguments __LOC__
      [ ( "-vox-library",
          Arg.Set library,
          " Compile a unit of the verified Vox library or of the standard \
           library, whose trusted declarations are part of the trusted base" ) ]
  end

type external_kind =
  | Refined
  | Total
  | Total_cast

let total_modality modality =
  let open Mode in
  not
    (Modality.Per_axis.is_id (Comonadic Totality)
       (Modality.Const.proj (Comonadic Totality) modality))

(* Whether a mode is certainly total, not merely allowed to be: its upper
   bound, found by zapping it and undoing that. *)
let certainly_total mode =
  let snapshot = Btype.snapshot () in
  let ceil = Mode.Totality.zap_to_ceil mode in
  Btype.backtrack snapshot;
  match ceil with
  | Mode.Totality.Const.Total -> true
  | Mode.Totality.Const.Partial -> false

let value_modality_total modality =
  let snapshot = Btype.snapshot () in
  let floor = Mode.Modality.zap_to_floor modality in
  Btype.backtrack snapshot;
  total_modality floor

(* The values a value of type [ty] provides: [ty] itself, the results of
   functions, components, fields and constructor arguments, and the values
   of first-class modules, through abbreviations and type definitions.
   [visit] sees each such type and each value declared in a module type, and
   stops the search by raising [Found]. *)
exception Found

let search env ~on_type ~on_field ~on_value ty =
  let seen = Hashtbl.create 16 and paths = ref Path.Set.empty in
  let rec visit ty =
    (* Expansion can remove a refinement wrapper, so both forms are seen. *)
    on_type ty;
    let ty = Ctype.expand_head env ty in
    let id = get_id ty in
    if not (Hashtbl.mem seen id)
    then begin
      Hashtbl.add seen id ();
      on_type ty;
      match get_desc ty with
      | Tarrow (_, _, result, _) -> visit result
      | Tpoly (ty, _) -> visit ty
      | Trefine { ref_payload; _ } -> visit ref_payload
      | Ttuple components | Tunboxed_tuple components ->
        List.iter (fun (_, ty) -> visit ty) components
      | Tconstr (path, arguments, _) ->
        List.iter visit arguments;
        if not (Path.Set.mem path !paths)
        then begin
          paths := Path.Set.add path !paths;
          match Env.find_type path env with
          | exception Not_found -> ()
          | declaration -> visit_declaration declaration
        end
      | Tpackage { pack_path; pack_cstrs } ->
        List.iter (fun (_, ty) -> visit ty) pack_cstrs;
        visit_module_type
          (try Some (Env.find_modtype_expansion pack_path env)
           with Not_found -> None)
      | Tvariant _ -> Btype.iter_type_expr visit ty
      | _ -> ()
    end
  and visit_module_type = function
    | Some (Mty_signature signature) ->
      List.iter
        (function
          | Sig_value (_, value, _) ->
            on_value value;
            visit value.val_type
          | Sig_module (_, _, declaration, _, _) ->
            visit_module_type (Some declaration.md_type)
          | _ -> ())
        signature
    | Some (Mty_ident path) ->
      if not (Path.Set.mem path !paths)
      then begin
        paths := Path.Set.add path !paths;
        visit_module_type
          (try Some (Env.find_modtype_expansion path env)
           with Not_found -> None)
      end
    | Some (Mty_functor (_, result, _)) -> visit_module_type (Some result)
    | Some _ | None -> ()
  and visit_declaration declaration =
    Option.iter visit declaration.type_manifest;
    let visit_labels =
      List.iter (fun label ->
          on_field label.ld_modalities;
          visit label.ld_type)
    in
    match declaration.type_kind with
    | Type_record (labels, _, _) | Type_record_unboxed_product (labels, _, _)
      ->
      visit_labels labels
    | Type_variant (constructors, _, _) ->
      List.iter
        (fun constructor ->
          match constructor.cd_args with
          | Cstr_tuple arguments ->
            List.iter
              (fun argument ->
                on_field argument.ca_modalities;
                visit argument.ca_type)
              arguments
          | Cstr_record labels -> visit_labels labels)
        constructors
    | Type_abstract _ | Type_open -> ()
  in
  match visit ty with () -> false | exception Found -> true

let nothing _ = ()

(* Whether [ty] mentions a refinement anywhere, including in arguments,
   through an abbreviation, in the definition of a type it names or in a
   first-class module: a value of a record type with a refined field carries
   that refinement too. *)
let mentions_refinement env ty =
  let exception Found_refinement in
  let types = Hashtbl.create 16 in
  let rec any ty =
    let id = get_id ty in
    if not (Hashtbl.mem types id)
    then begin
      Hashtbl.add types id ();
      (match get_desc ty with
      | Trefine _ -> raise Found_refinement
      | _ -> ());
      if search env ~on_field:nothing ~on_value:nothing
           ~on_type:(fun ty ->
             match get_desc ty with
             | Trefine _ -> raise Found
             | Tarrow (_, argument, _, _) -> any argument
             | _ -> ())
           ty
      then raise Found_refinement
    end
  in
  match any ty with () -> false | exception Found_refinement -> true

(* Whether a value of type [ty] provides a value that is total by assertion:
   a function whose result is declared total, a field declared total or a
   module value declared total. *)
let carries_totality env ty =
  search env
    ~on_type:(fun ty ->
      match get_desc ty with
      | Tarrow ((_, _, result_mode, _), _, _, _)
        when certainly_total Mode.(Alloc.proj_comonadic Totality result_mode)
        ->
        raise Found
      | _ -> ())
    ~on_field:(fun modality -> if total_modality modality then raise Found)
    ~on_value:(fun value ->
      if value_modality_total value.val_modalities then raise Found)
    ty

let declared_total (description : Typedtree.value_description) =
  match description.val_modal_info with
  | Valmi_str_primitive modes -> (
    match modes.mode_modes.totality with
    | Some Mode.Totality.Const.Total -> true
    | Some Mode.Totality.Const.Partial | None -> false)
  | Valmi_sig_value modalities -> total_modality modalities.moda_modalities

(* Whether some partial application of a value of type [ty] is total, as for
   [external trust_total : 'a -> 'a @ total]. *)
let returns_total env ty =
  let seen = Hashtbl.create 8 in
  let rec loop ty =
    let ty = Ctype.expand_head env ty in
    (not (Hashtbl.mem seen (get_id ty)))
    && begin
      Hashtbl.add seen (get_id ty) ();
      match get_desc ty with
      | Tarrow ((_, _, result_mode, _), _, result, _) ->
        certainly_total Mode.(Alloc.proj_comonadic Totality result_mode)
        || loop result
      | Tpoly (ty, _) -> loop ty
      | _ -> false
    end
  in
  loop ty

let is_total_cast env (description : value_description) =
  match description.val_kind with
  | Val_prim { prim_name = "%identity"; _ } ->
    returns_total env description.val_type
  | _ -> false

(* Primitives that the compiler implements and that terminate without raising
   when every argument and the result are of the sorts given: declaring one of
   them total at those types restates a fact about the compiler, which is
   trusted anyway, rather than adding an assumption. *)
let total_primitive env (primitive : Primitive.description) ty =
  let rec sorts ty =
    match get_desc (Ctype.expand_head env ty) with
    | Tarrow (_, argument, result, _) ->
      Vox_type.classify_payload env argument :: sorts result
    | Tpoly (ty, _) -> sorts ty
    | _ -> [Vox_type.classify_payload env ty]
  in
  let all allowed =
    List.for_all
      (function Some sort -> List.mem sort allowed | None -> false)
      (sorts ty)
  in
  match primitive.prim_name with
  | "%addint" | "%subint" | "%mulint" | "%negint" | "%succint" | "%predint"
  | "%andint" | "%orint" | "%xorint" | "%lslint" | "%lsrint" | "%asrint" ->
    all [Vox_type.Int]
  | "%ltint" | "%leint" | "%gtint" | "%geint" ->
    all [Vox_type.Int; Vox_type.Bool]
  | "%boolnot" | "%sequand" | "%sequor" -> all [Vox_type.Bool]
  | "%equal" | "%notequal" | "%lessthan" | "%lessequal" | "%greaterthan"
  | "%greaterequal" | "%compare" ->
    all [Vox_type.Int; Vox_type.Bool; Vox_type.Bigint]
  | _ -> false

let trusted_external env (description : Typedtree.value_description) =
  match description.val_val.val_kind with
  | Val_prim primitive ->
    let ty = description.val_val.val_type in
    if mentions_refinement env ty
    then Some Refined
    else if is_total_cast env description.val_val
    then Some Total_cast
    else if
      (declared_total description || carries_totality env ty)
      && not (total_primitive env primitive ty)
    then Some Total
    else None
  | _ -> None

let check_external env (description : Typedtree.value_description) =
  if not !library
  then
    Builtin_attributes.warning_scope ~ppwarning:false description.val_attributes
    @@ fun () ->
    Option.iter
      (fun kind ->
        Location.prerr_warning description.Typedtree.val_loc
          (Warnings.Trusted_external
             (match kind with
             | Refined -> Warnings.Trusted_refinement
             | Total -> Warnings.Trusted_totality
             | Total_cast -> Warnings.Trusted_total_cast)))
      (trusted_external env description)
