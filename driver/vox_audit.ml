open Cmi_format

let names imports =
  List.map
    (fun import -> Compilation_unit.Name.to_string (Import_info.name import))
    imports

let read_cmi file =
  match Cmi_format.read_cmi_lazy file with
  | cmi ->
    Some (Cmi_format.vox_unit cmi.cmi_flags, names (Array.to_list cmi.cmi_crcs))
  | exception _ -> None

let with_magic file magic read =
  match
    In_channel.with_open_bin file (fun channel ->
        let found = really_input_string channel (String.length magic) in
        if String.equal found magic then Some (read channel) else None)
  with
  | result -> result
  | exception _ -> None

(* A .cmo ends with its compilation unit description, whose position follows
   the magic number, and then its Vox record. *)
let read_cmo file =
  with_magic file Config.cmo_magic_number (fun channel ->
      let position = input_binary_int channel in
      seek_in channel position;
      let unit = (input_value channel : Cmo_format.compilation_unit_descr) in
      Cmi_format.input_vox_record channel, names (Array.to_list unit.cu_imports))

(* A .cmx starts with its Vox record (see [Compilenv.write_unit_info]). *)
let read_cmx file =
  with_magic file Config.cmx_magic_number Cmi_format.input_vox_record

let artifact target extension =
  Unit_info.Artifact.filename (Unit_info.artifact target ~extension)

(* The records that earlier compilations left in this unit's .cmo and .cmi. *)
let counterparts target () =
  List.filter_map Fun.id
    [ Option.bind (read_cmo (artifact target ".cmo")) fst;
      Option.bind (read_cmi (artifact target ".cmi")) fst ]

let find name extension =
  match Load_path.find_normalized (name ^ extension) with
  | file -> Some file
  | exception Not_found -> None

type unit_entry =
  { name : string;
    implementation : vox_unit option;
    interface : vox_unit option;
    unrecorded : string list;
        (** extensions of the unit's compiled files that have no record *)
    found : bool  (** some compiled file of the unit was found *)
  }

let import_name import =
  match String.index_opt import '=' with
  | Some i -> String.sub import 0 i
  | None -> import

(* The .cmx and .cmo of a unit may come from different compilations; the
   weaker record counts, and the items of both. *)
let merge (a : vox_unit) (b : vox_unit) =
  { a with
    vox_status =
      (if a.vox_status = Vox_not_verified || b.vox_status = Vox_not_verified
       then Vox_not_verified
       else a.vox_status);
    vox_library = a.vox_library && b.vox_library;
    vox_solver =
      String.concat " "
        (List.sort_uniq String.compare
           (List.filter (fun s -> s <> "") [a.vox_solver; b.vox_solver]));
    vox_imports = List.sort_uniq String.compare (a.vox_imports @ b.vox_imports);
    vox_items = List.sort_uniq compare (a.vox_items @ b.vox_items) }

(* The units [name] depends on through its interface and implementation, and
   what their compiled files record. *)
let lookup name =
  let cmi = Option.bind (find name ".cmi") read_cmi in
  let cmx = Option.bind (find name ".cmx") read_cmx in
  let cmo = Option.bind (find name ".cmo") read_cmo in
  let interface = Option.bind cmi fst in
  let unrecorded =
    List.filter_map Fun.id
      [ (match cmx with Some None -> Some ".cmx" | _ -> None);
        (match cmo with Some (None, _) -> Some ".cmo" | _ -> None) ]
  in
  let implementation =
    match Option.join cmx, Option.bind cmo fst with
    | Some a, Some b -> Some (merge a b)
    | (Some _ as record), None | None, (Some _ as record) -> record
    | None, None -> (
      (* A unit without an .mli: its .cmi has the implementation's record. *)
      match interface with
      | Some { vox_status = Vox_verified | Vox_not_verified; _ } -> interface
      | _ -> None)
  in
  let imports =
    Option.fold ~none:[] ~some:snd cmi
    @ Option.fold ~none:[] ~some:snd cmo
    @
    match implementation with
    | Some record -> List.map import_name record.vox_imports
    | None -> []
  in
  ( { name;
      implementation;
      interface;
      unrecorded;
      found = Option.is_some cmi || Option.is_some cmx || Option.is_some cmo },
    imports )

let closure ~current ~record ~imports =
  let seen = Hashtbl.create 64 in
  Hashtbl.add seen current ();
  let rec visit acc = function
    | [] -> List.rev acc
    | name :: rest when Hashtbl.mem seen name -> visit acc rest
    | name :: rest ->
      Hashtbl.add seen name ();
      let entry, imports = lookup name in
      visit (entry :: acc) (rest @ imports)
  in
  let current_entry =
    { name = current;
      implementation =
        (match record with
        | Some { vox_status = Vox_interface; _ } -> None
        | _ -> record);
      interface = record;
      unrecorded = [];
      found = true }
  in
  current_entry :: visit [] imports

let items entry =
  let all =
    Option.fold ~none:[] ~some:(fun r -> r.vox_items) entry.implementation
    @ Option.fold ~none:[] ~some:(fun r -> r.vox_items) entry.interface
  in
  let seen = Hashtbl.create 16 in
  List.filter
    (fun item ->
      let key = item.vox_kind, item.vox_name in
      if Hashtbl.mem seen key
      then false
      else begin
        Hashtbl.add seen key ();
        true
      end)
    all

let library entry =
  match entry.implementation, entry.interface with
  | Some r, _ | None, Some r -> r.vox_library
  | None, None -> false

let kinds =
  [ Vox_refined_external, "Externals whose refinement is assumed";
    Vox_total_external, "Externals assumed total";
    Vox_builtin, "Externals given a built-in meaning";
    Vox_total_cast, "Casts to total";
    Vox_cast_use, "Definitions that apply a cast to total";
    Vox_unsafe, "Unsafe features (Obj, Marshal, unsafe primitives, -unsafe)";
    Vox_external, "Other externals outside the library" ]

let print ppf ~source_file ~current ~record ~imports =
  let entries = closure ~current ~record ~imports in
  let count = List.length entries - 1 in
  Format.fprintf ppf "Vox audit of %s and the %d unit%s it depends on@."
    source_file count
    (if count = 1 then "" else "s");
  let section title lines =
    if lines <> []
    then begin
      Format.fprintf ppf "@.%s:@." title;
      List.iter (fun line -> Format.fprintf ppf "  %s@." line) lines
    end
  in
  let units predicate =
    List.filter_map
      (fun entry -> if predicate entry then Some entry.name else None)
      entries
  in
  section "Verification skipped (-smt-assume-verified)"
    (units (fun entry ->
         match entry.implementation with
         | Some { vox_status = Vox_not_verified; _ } -> true
         | _ -> false));
  section "Verified with another solver (-smt-solver-any-version)"
    (List.filter_map
       (fun entry ->
         match entry.implementation with
         | Some { vox_solver; _ } when vox_solver <> "" ->
           Some (entry.name ^ ": " ^ vox_solver)
         | _ -> None)
       entries);
  section "Not compiled with refinement types (no Vox record)"
    (List.filter_map
       (fun entry ->
         if entry.found && entry.interface = None && entry.implementation = None
         then Some entry.name
         else if entry.unrecorded <> []
         then
           Some
             (Printf.sprintf "%s (its %s)" entry.name
                (String.concat " and " entry.unrecorded))
         else None)
       entries);
  section "Implementation record not found (no .cmx or .cmo)"
    (units (fun entry ->
         entry.implementation = None && entry.interface <> None));
  section "Compiled files not found"
    (units (fun entry -> not entry.found));
  let total = ref 0 in
  List.iter
    (fun (kind, title) ->
      section title
        (List.concat_map
           (fun entry ->
             let items =
               List.filter (fun item -> item.vox_kind = kind) (items entry)
             in
             total := !total + List.length items;
             let counted item =
               if item.vox_count > 1
               then Printf.sprintf "%s x%d" item.vox_name item.vox_count
               else item.vox_name
             in
             if items = []
             then []
             else if library entry
             then
               (* The library's items are part of the trusted base; one line
                  per unit. *)
               [ Printf.sprintf "%s (library): %s" entry.name
                   (String.concat ", " (List.map counted items)) ]
             else
               List.map
                 (fun item ->
                   Printf.sprintf "%s%s"
                     (if kind = Vox_unsafe
                      then entry.name ^ ": " ^ counted item
                      else entry.name ^ "." ^ counted item)
                     (if item.vox_location = ""
                      then ""
                      else " (" ^ item.vox_location ^ ")"))
                 items)
           entries))
    kinds;
  Format.fprintf ppf "@.%d trusted item%s in total.@." !total
    (if !total = 1 then "" else "s")

let print_unit ~source_file ~current ~record =
  print Format.std_formatter ~source_file ~current ~record
    ~imports:(names (Env.imports ()))
