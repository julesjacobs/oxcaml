open Types

(* Trusted externals (warning 228) *)

let library = ref false

let audit = ref false

let registered = ref false

(* Merlin: the compiler registers [-vox-library] and [-vox-audit] here; Merlin
   does not parse compiler command lines through [Clflags], so there is
   nothing to register. *)
let add_arguments () = registered := true

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
   trusted anyway, rather than adding an assumption. Shifts are not among
   them: their result is unspecified for counts outside [0, 63]. *)
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
  | "%andint" | "%orint" | "%xorint" ->
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

(* The record of a unit, and its uses of unsafe features (-vox-audit) *)

let assume_verified = ref false

let unexpected_solver = ref ""

(* Values that break parametricity or memory safety, so that a refinement
   instantiated at their result can be false: everything in [Obj], reading
   marshalled data, primitives named unsafe, and externals whose result may
   be of any type. *)

let contains_substring ~sub s =
  let n = String.length sub in
  let rec loop i =
    i + n <= String.length s && (String.sub s i n = sub || loop (i + 1))
  in
  loop 0

(* The final result of [ty] is a type variable that no argument mentions. *)
let returns_anything env ty =
  let rec split ty =
    match get_desc (Ctype.expand_head env ty) with
    | Tarrow (_, argument, result, _) ->
      let arguments, result = split result in
      argument :: arguments, result
    | Tpoly (ty, _) -> split ty
    | _ -> [], ty
  in
  let arguments, result = split ty in
  let result = Ctype.expand_head env result in
  match get_desc result with
  | (Tvar _ | Tunivar _) when arguments <> [] ->
    let exception Mentioned in
    let seen = Hashtbl.create 16 in
    let rec visit ty =
      let id = get_id ty in
      if id = get_id result
      then raise Mentioned
      else if not (Hashtbl.mem seen id)
      then begin
        Hashtbl.add seen id ();
        Btype.iter_type_expr visit ty
      end
    in
    (match List.iter visit arguments with
    | () -> true
    | exception Mentioned -> false)
  | _ -> false

let unsafe_primitive env (description : value_description) =
  match description.val_kind with
  | Val_prim { prim_name; _ } ->
    contains_substring ~sub:"unsafe" prim_name
    || String.starts_with ~prefix:"%obj_" prim_name
    || String.starts_with ~prefix:"caml_obj_" prim_name
    || (returns_anything env description.val_type
        && not
             (String.starts_with ~prefix:"%raise" prim_name
             || String.starts_with ~prefix:"%reraise" prim_name))
  | _ -> false

(* A refined external, such as a read whose bounds its contract requires, is
   not an unsafe use: its precondition is verified where it is used, and the
   external itself is listed as a refined external. Its declared type decides,
   not the type it is instantiated at. *)
let unsafe_value env path (description : value_description) =
  let path = try Env.normalize_value_path None env path with _ -> path in
  let name = Path.last path in
  let unit_name = Ident.name (Path.head path) in
  let global = Ident.is_global (Path.head path) in
  let declared_type () =
    match Env.find_value path env with
    | declaration -> (Subst.Lazy.force_value_description declaration).val_type
    | exception Not_found -> description.val_type
  in
  (global && String.equal unit_name "Stdlib__Obj")
  || global
     && String.equal unit_name "Stdlib__Marshal"
     && String.starts_with ~prefix:"from_" name
  || (global && String.equal unit_name "Stdlib" && name = "input_value")
  || (unsafe_primitive env description
      || global
         && String.starts_with ~prefix:"Stdlib" unit_name
         && contains_substring ~sub:"unsafe" name)
     && not (mentions_refinement env (declared_type ()))

(* Items, keyed by kind and name, with their first location and a count. *)
module Items = struct
  type t = (Cmi_format.vox_item_kind * string, string * int) Hashtbl.t

  let create () : t = Hashtbl.create 16

  let add (items : t) kind name (loc : Location.t) =
    let key = kind, name in
    match Hashtbl.find_opt items key with
    | Some (location, count) -> Hashtbl.replace items key (location, count + 1)
    | None ->
      let position = loc.loc_start in
      let location =
        if position.pos_fname = ""
        then ""
        else
          Printf.sprintf "%s:%d"
            (Filename.basename position.pos_fname)
            position.pos_lnum
      in
      Hashtbl.replace items key (location, 1)

  let to_list (items : t) =
    Hashtbl.fold
      (fun (kind, name) (location, count) acc ->
        { Cmi_format.vox_kind = kind;
          vox_name = name;
          vox_location = location;
          vox_count = count }
        :: acc)
      items []
    |> List.sort compare
end

let qualified modules name = String.concat "." (List.rev (name :: modules))

let add_external items modules env (description : Typedtree.value_description)
    =
  let name = qualified modules description.val_name.txt in
  let loc = description.val_loc in
  (match description.val_val.val_kind with
  | Val_prim primitive
    when Vox_type.is_builtin_c_primitive primitive
         && Vox_type.carries_builtin_meaning description.val_val.val_uid
              primitive ->
    Items.add items Cmi_format.Vox_builtin name loc
  | _ -> ());
  match trusted_external env description with
  | Some Refined -> Items.add items Cmi_format.Vox_refined_external name loc
  | Some Total -> Items.add items Cmi_format.Vox_total_external name loc
  | Some Total_cast -> Items.add items Cmi_format.Vox_total_cast name loc
  | None ->
    if unsafe_primitive env description.val_val
    then Items.add items Cmi_format.Vox_unsafe name loc
    else if not !library
    then Items.add items Cmi_format.Vox_external name loc

let add_type_declaration items modules
    (declaration : Typedtree.type_declaration) =
  if Builtin_attributes.has_unsafe_allow_any_mode_crossing
       declaration.typ_attributes
  then
    Items.add items Cmi_format.Vox_unsafe
      (Printf.sprintf "type %s [@@unsafe_allow_any_mode_crossing]"
         (qualified modules declaration.typ_name.txt))
      declaration.typ_loc

let binding_name (binding : Typedtree.value_binding) =
  match binding.vb_pat.pat_desc with
  | Tpat_var { name; _ } | Tpat_alias { name; _ } -> Some name.txt
  | _ -> None

let scan ~structure ~signature =
  let items = Items.create () in
  if !Clflags.unsafe
  then
    Items.add items Cmi_format.Vox_unsafe "-unsafe (no bounds checks)"
      (Location.in_file !Location.input_name);
  let modules = ref [] and binding = ref None in
  let within name f =
    let saved = !modules in
    modules := Option.value name ~default:"_" :: saved;
    Fun.protect ~finally:(fun () -> modules := saved) f
  in
  let open Tast_iterator in
  let iterator =
    { default_iterator with
      structure_item =
        (fun self item ->
          (match item.str_desc with
          | Tstr_primitive description ->
            add_external items !modules item.str_env description
          | Tstr_type (_, declarations) ->
            List.iter (add_type_declaration items !modules) declarations
          | _ -> ());
          default_iterator.structure_item self item);
      signature_item =
        (fun self item ->
          (match item.sig_desc with
          | Tsig_value ({ val_val = { val_kind = Val_prim _; _ }; _ } as value)
            ->
            add_external items !modules item.sig_env value
          | Tsig_type (_, declarations) ->
            List.iter (add_type_declaration items !modules) declarations
          | _ -> ());
          default_iterator.signature_item self item);
      module_binding =
        (fun self binding ->
          within binding.mb_name.txt (fun () ->
              default_iterator.module_binding self binding));
      module_declaration =
        (fun self declaration ->
          within declaration.md_name.txt (fun () ->
              default_iterator.module_declaration self declaration));
      value_binding =
        (fun self value_binding ->
          let saved = !binding in
          (match binding_name value_binding with
          | Some name -> binding := Some (qualified !modules name)
          | None -> ());
          Fun.protect
            ~finally:(fun () -> binding := saved)
            (fun () -> default_iterator.value_binding self value_binding));
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_ident { path; desc; _ } ->
            let env = expression.exp_env in
            if is_total_cast env desc
            then
              Items.add items Cmi_format.Vox_cast_use
                (Option.value !binding ~default:"_")
                expression.exp_loc
            else if unsafe_value env path desc
            then
              Items.add items Cmi_format.Vox_unsafe
                (Path.name
                   (try Env.normalize_value_path None env path
                    with _ -> path))
                expression.exp_loc
          | _ -> ());
          default_iterator.expr self expression) }
  in
  Option.iter (iterator.structure iterator) structure;
  Option.iter (iterator.signature iterator) signature;
  Items.to_list items

let file_digest source_file =
  match Digest.file source_file with
  | digest -> Digest.to_hex digest
  | exception Sys_error _ -> ""

(* The program that was verified: the parse tree after preprocessing, and the
   flags that change how it is typed or what the compiled code does where
   verification relied on it. *)
let source_digest (ast : Parsetree.structure) =
  Digest.to_hex (Digest.string (Marshal.to_string ast [Marshal.No_sharing]))

let config () =
  String.concat " "
    (* Merlin: its [Clflags] has no [noassert]; Merlin never compiles code,
       so the flag is recorded as off. *)
    ([ Printf.sprintf "noassert=%b unsafe=%b nopervasives=%b rectypes=%b"
         false !Clflags.unsafe !Clflags.nopervasives
         !Clflags.recursive_types ]
    @ List.rev_map
        (function
          | Clflags.Open name -> "open=" ^ name
          | Clflags.Open_cmi name -> "open_cmi=" ^ name)
        !Clflags.open_args
    @ List.filter_map
        (fun extension ->
          if Language_extension.Exist.is_enabled extension
          then Some ("extension=" ^ Language_extension.Exist.to_string extension)
          else None)
        Language_extension.Exist.all)

let enabled () = Language_extension.is_enabled Refinement_types

let implementation_record = ref None

(* The interfaces a compilation read, with their digests. *)
let imports () =
  List.map
    (fun import ->
      Compilation_unit.Name.to_string (Import_info.name import)
      ^ "="
      ^ Option.fold ~none:"" ~some:Digest.to_hex (Import_info.crc import))
    (Env.imports ())
  |> List.sort_uniq String.compare

let interface_record ~source_file signature =
  if enabled ()
  then
    Some
      { Cmi_format.vox_status = Cmi_format.Vox_interface;
        vox_library = !library;
        vox_source = file_digest source_file;
        vox_config = config ();
        vox_solver = "";
        vox_imports = [];
        vox_items = scan ~structure:None ~signature:(Some signature) }
  else None

(* The record in a compiled interface. *)
let interface_file_record file =
  match Cmi_format.read_cmi_lazy file with
  | cmi -> Cmi_format.vox_unit cmi.cmi_flags
  | exception _ -> None

(* A pack's record gathers its members': their items, named within the pack,
   their imports from outside it, and the weakest status, counting a member
   without a record as not verified. *)
let pack_record members =
  if List.for_all (fun (_, record) -> Option.is_none record) members
  then None
  else
    let records = List.filter_map snd members in
    let internal import =
      List.exists
        (fun (name, _) -> String.starts_with ~prefix:(name ^ "=") import)
        members
    in
    Some
      { Cmi_format.vox_status =
          (if
             List.for_all
               (fun (_, record) ->
                 match record with
                 | Some
                     { Cmi_format.vox_status =
                         Cmi_format.Vox_verified | Cmi_format.Vox_interface;
                       _ } ->
                   true
                 | Some _ | None -> false)
               members
           then Cmi_format.Vox_verified
           else Cmi_format.Vox_not_verified);
        vox_library =
          List.for_all (fun (r : Cmi_format.vox_unit) -> r.vox_library) records;
        vox_source = "";
        vox_config = config ();
        vox_solver =
          String.concat " "
            (List.filter_map
               (fun (r : Cmi_format.vox_unit) ->
                 if r.vox_solver = "" then None else Some r.vox_solver)
               records);
        vox_imports =
          List.sort_uniq String.compare
            (List.filter
               (fun import -> not (internal import))
               (List.concat_map
                  (fun (r : Cmi_format.vox_unit) -> r.vox_imports)
                  records));
        vox_items =
          List.concat_map
            (fun (name, record) ->
              match record with
              | None -> []
              | Some (r : Cmi_format.vox_unit) ->
                List.map
                  (fun (item : Cmi_format.vox_item) ->
                    { item with vox_name = name ^ "." ^ item.vox_name })
                  r.vox_items)
            members }

(* Skipped verification (-smt-assume-verified, warning 229) *)

let counterparts = ref (fun () -> [])

let reset () =
  implementation_record := None;
  unexpected_solver := "";
  counterparts := fun () -> []

(* A compilation with -smt-assume-verified is recorded as verified when a
   verified compilation of the same program, with the same flags and against
   the same interfaces, already produced this unit's .cmo or .cmi, as the
   bytecode half of the library build does for the native half. Verification
   may read more interfaces than compilation, so the other compilation's
   imports need only include these. *)
let status ~source ~config ~imports : Cmi_format.vox_status * string =
  let verifies (record : Cmi_format.vox_unit) =
    record.vox_status = Cmi_format.Vox_verified
    && String.equal record.vox_source source
    && String.equal record.vox_config config
    && Bool.equal record.vox_library !library
    && List.for_all (fun import -> List.mem import record.vox_imports) imports
  in
  if not !assume_verified
  then Cmi_format.Vox_verified, !unexpected_solver
  else
    match List.find_opt verifies (!counterparts ()) with
    (* The solver that verified it, too. *)
    | Some (record : Cmi_format.vox_unit) ->
      Cmi_format.Vox_verified, record.vox_solver
    | None -> Cmi_format.Vox_not_verified, ""

let check_imports ~source_file =
  if not !assume_verified
  then
    List.iter
      (fun import ->
        let name = Import_info.name import in
        match Env.vox_unit name with
        | Some { Cmi_format.vox_status = Cmi_format.Vox_not_verified; _ } ->
          Location.prerr_warning
            (Location.in_file source_file)
            (Warnings.Unverified_import (Compilation_unit.Name.to_string name))
        | Some _ | None -> ())
      (Env.imports ())

let record_implementation ~source_file ~ast structure =
  if enabled ()
  then begin
    check_imports ~source_file;
    let source = source_digest ast
    and config = config ()
    and imports = imports () in
    let status, solver = status ~source ~config ~imports in
    let record : Cmi_format.vox_unit =
      { vox_status = status;
        vox_library = !library;
        vox_source = source;
        vox_config = config;
        vox_solver = solver;
        vox_imports = imports;
        vox_items = scan ~structure:(Some structure) ~signature:None }
    in
    implementation_record := Some record
  end
