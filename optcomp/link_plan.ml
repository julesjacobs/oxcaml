open Cmx_format
module CU = Compilation_unit

let stable_magic = "LINKPLAN1-stable\n"
let volatile_magic = "LINKPLAN1-volatile\n"

(* One entry of the ordered link set. No CRCs: identities and dependency NAMES only, so
   the stable plan is unchanged by an implementation-only leaf edit. *)
type unit_stable =
  { us_name : CU.t (* startup name/global tables; join key for the volatile plan *)
  ; us_defines : CU.t list (* startup name_list + globals_map symbols *)
  ; us_cmx_dep_names : CU.t list (* manual-module-init unit_deps_table (names only) *)
  }

type stable =
  { version : string (* Config.cmx_magic_number; reject cross-compiler reuse *)
  ; units : unit_stable list (* ordered tolink set: startup codegen + unit_deps *)
  ; ml_objfiles : string list (* the .a/.o inputs to the final ld command line *)
  ; genfns_entries : generic_fns (* stable startup: GC roots / generic-fn table *)
  ; cached_imported_units :
      CU.t list (* stable startup code/frame tables under -use-cached-generic-functions *)
  ; ccobjs : string list (* final ld command line *)
  ; ccopts : string list (* final ld command line *)
  ; manual_module_init : bool (* stable startup codegen shape *)
  ; use_cached_generic_functions : bool (* stable startup codegen shape *)
  ; output_complete_object : bool (* stable startup codegen shape *)
  }

(* The exact caml_globals_map — the only leaf-edit-varying part. Stored verbatim (as
   make_globals_map produced it) so the plan-driven volatile object is byte-identical to
   the scan-driven one. *)
type volatile =
  { globals_map : (CU.t * Digest.t option * Digest.t option * Symbol.t list) list }

let build ~linkenv ~units_tolink ~ml_objfiles ~genfns_entries ~cached_imported_units =
  let units =
    List.map
      (fun (u : Linkenv.unit_link_info) ->
        { us_name = u.name
        ; us_defines = u.defines
        ; us_cmx_dep_names = List.map Import_info.cu u.imports_cmx
        })
      units_tolink
  in
  (* Absolutize the ML object inputs so the plan's ld line is location-independent (the
     .cmxa are never resolved; the archives may be absent from the consuming cwd). These
     entries are pure .a/.o paths. [ccobjs]/[ccopts] are kept verbatim: they interleave
     paths with linker flags and bare --wrap symbol arguments that must not be rewritten,
     so the plan-consuming link resolves them relative to its cwd exactly as the
     scan-driven link does. *)
  let cwd = Sys.getcwd () in
  let abs p = if Filename.is_relative p then Filename.concat cwd p else p in
  let ml_objfiles = List.map abs ml_objfiles in
  let stable =
    { version = Config.cmx_magic_number
    ; units
    ; ml_objfiles
    ; genfns_entries
    ; cached_imported_units
    ; ccobjs = Linkenv.lib_ccobjs linkenv
    ; ccopts = Linkenv.lib_ccopts linkenv
    ; manual_module_init = !Oxcaml_flags.manual_module_init
    ; use_cached_generic_functions = !Oxcaml_flags.use_cached_generic_functions
    ; output_complete_object = !Clflags.output_complete_object
    }
  in
  (* The globals_map is a pure function of the CRC map + each unit's own CRCs,
     deterministic for a given pass 1; stored as-is so the consumer replays the exact same
     phrase. *)
  let volatile = { globals_map = Linkenv.make_globals_map linkenv units_tolink } in
  stable, volatile
;;

let write_file path magic value =
  let oc = open_out_bin path in
  Misc.try_finally
    (fun () ->
      output_string oc magic;
      Marshal.to_channel oc value [])
    ~always:(fun () -> close_out oc)
;;

let write ~base (s : stable) (v : volatile) =
  write_file (base ^ ".plan-stable") stable_magic s;
  write_file (base ^ ".plan-volatile") volatile_magic v
;;

let read_file (type a) path (magic : string) : a =
  try
    let ic = open_in_bin path in
    Misc.try_finally
      (fun () ->
        let buf = really_input_string ic (String.length magic) in
        if not (String.equal buf magic) then failwith "bad or unsupported plan magic";
        (Marshal.from_channel ic : a))
      ~always:(fun () -> close_in ic)
  with
  | exn ->
    Misc.fatal_errorf
      "-use-link-plan: cannot read plan %s: %s"
      path
      (Printexc.to_string exn)
;;

let check_version version path =
  if not (String.equal version Config.cmx_magic_number)
  then Misc.fatal_errorf "link plan %s was produced by a different compiler version" path
;;

let read_stable path : stable =
  let s : stable = read_file path stable_magic in
  check_version s.version path;
  s
;;

let read_volatile path : volatile = read_file path volatile_magic

(* Impl CRC per unit identity, from the globals_map's defined entries. *)
let impl_crcs (v : volatile) =
  let tbl = Hashtbl.create 1024 in
  List.iter
    (fun (cu, _intf, impl, _syms) ->
      match impl with
      | Some crc -> Hashtbl.replace tbl (CU.full_path_as_string cu) crc
      | None -> ())
    v.globals_map;
  tbl
;;

let units_tolink (s : stable) (v : volatile) : Linkenv.unit_link_info list =
  let crcs = impl_crcs v in
  List.map
    (fun u ->
      { Linkenv.name = u.us_name
      ; defines = u.us_defines
      ; file_name = ""
      ; crc =
          (match Hashtbl.find_opt crcs (CU.full_path_as_string u.us_name) with
           | Some crc -> crc
           | None -> "")
      ; imports_cmx =
          List.map (fun cu -> Import_info.create_normal cu ~crc:None) u.us_cmx_dep_names
      ; dynunit = None
      })
    s.units
;;

let defined_unit_names (v : volatile) =
  List.filter_map
    (fun (cu, _intf, impl, _syms) ->
      match impl with
      | Some _ -> Some (CU.Name.to_string (CU.name cu))
      | None -> None)
    v.globals_map
;;
