open Cmx_format
module CU = Compilation_unit

let magic = "CMXASUM3\000"

(* Unit identities are full [Compilation_unit.t] values (marshal-structural, like the
   linkenv snapshot): [CU.name] alone drops parameterized-library instance arguments,
   collapsing distinct instances of a unit. *)
type unit_summary =
  { u_name : CU.t
  ; u_own_intf_crc : Digest.t option
  ; u_impl_crc : Digest.t
  ; u_defines : CU.t list
  ; u_force_link : bool
  ; u_cmx_deps : CU.t list
  ; u_quoted_cmi : CU.Name.t list
  ; u_quoted_cmx : CU.t list
  }

type t =
  { units : unit_summary list
  ; generic_fns : generic_fns
  ; requires_metaprogramming : bool
  ; ccobjs : string list
  ; ccopts : string list
  }

let nkey n = CU.Name.to_string n
let units_of t = List.map (fun u -> u.u_name) t.units

(* Canonical projection. Fields are ordinary immutable values; unit order is the archive's
   own [lib_units] order (deterministic); dep-name lists are sorted so the bytes do not
   depend on bitmap iteration incidentals. *)
let project ?bump (l : library_infos) : t =
  let own = Hashtbl.create 1024 in
  Array.iter
    (fun imp ->
      match Import_info.crc imp with
      | Some c -> Hashtbl.replace own (nkey (Import_info.name imp)) c
      | None -> ())
    l.lib_imports_cmi;
  let by_path a b = compare (CU.full_path_as_string a) (CU.full_path_as_string b) in
  let by_name a b = compare (CU.Name.to_string a) (CU.Name.to_string b) in
  let units =
    List.map
      (fun (u : lib_unit_info) ->
        let name = nkey (CU.name u.li_name) in
        let deps = ref [] in
        Misc.Bitmap.iter
          (fun i -> deps := Import_info.cu l.lib_imports_cmx.(i) :: !deps)
          u.li_imports_cmx;
        let impl_crc =
          match bump with
          | Some b when String.equal b name -> Digest.string (u.li_crc ^ "perturbed")
          | _ -> u.li_crc
        in
        let quoted_cmi = ref [] in
        Misc.Bitmap.iter
          (fun i -> quoted_cmi := l.lib_quoted_cmi.(i) :: !quoted_cmi)
          u.li_quoted_cmi;
        let quoted_cmx = ref [] in
        Misc.Bitmap.iter
          (fun i -> quoted_cmx := l.lib_quoted_cmx.(i) :: !quoted_cmx)
          u.li_quoted_cmx;
        { u_name = u.li_name
        ; u_own_intf_crc = Hashtbl.find_opt own name
        ; u_impl_crc = impl_crc
        ; u_defines = u.li_defines
        ; u_force_link = u.li_force_link
        ; u_cmx_deps = List.sort by_path !deps
        ; u_quoted_cmi = List.sort by_name !quoted_cmi
        ; u_quoted_cmx = List.sort by_path !quoted_cmx
        })
      l.lib_units
  in
  { units
  ; generic_fns = l.lib_generic_fns
  ; requires_metaprogramming = l.lib_requires_metaprogramming
  ; ccobjs = l.lib_ccobjs
  ; ccopts = l.lib_ccopts
  }
;;

let write_to_channel oc (s : t) =
  output_string oc magic;
  Marshal.to_channel oc s []
;;

let emit ~src ~dst =
  let s = project (Compilenv.read_library_info src) in
  let oc = open_out_bin dst in
  Misc.try_finally (fun () -> write_to_channel oc s) ~always:(fun () -> close_out oc)
;;

let dir () =
  match !Oxcaml_flags.cmxa_summaries_path with
  | "" -> None
  | d -> Some d
;;

let summary_path ~dir name =
  Filename.concat dir (Digest.to_hex (Digest.string name) ^ ".cmxasum")
;;

(* Intern by a canonical string key, keeping the first value seen. *)
let intern tbl rev next key v =
  match Hashtbl.find_opt tbl key with
  | Some i -> i
  | None ->
    let i = !next in
    Hashtbl.add tbl key i;
    rev := v :: !rev;
    incr next;
    i
;;

let inflate (s : t) : library_infos =
  (* Shared import tables in first-use order (deterministic). *)
  let cmx_tbl = Hashtbl.create 256 in
  let cmx_rev = ref [] in
  let cmx_next = ref 0 in
  let qcmi_tbl = Hashtbl.create 16 in
  let qcmi_rev = ref [] in
  let qcmi_next = ref 0 in
  let qcmx_tbl = Hashtbl.create 16 in
  let qcmx_rev = ref [] in
  let qcmx_next = ref 0 in
  let cu_key = CU.full_path_as_string in
  let unit_ids =
    List.map
      (fun u ->
        ( List.map (fun cu -> intern cmx_tbl cmx_rev cmx_next (cu_key cu) cu) u.u_cmx_deps
        , List.map
            (fun n -> intern qcmi_tbl qcmi_rev qcmi_next (CU.Name.to_string n) n)
            u.u_quoted_cmi
        , List.map
            (fun cu -> intern qcmx_tbl qcmx_rev qcmx_next (cu_key cu) cu)
            u.u_quoted_cmx ))
      s.units
  in
  (* Interface imports: one self entry per unit with its own cmi CRC (interfaces are
     per-name; parameterized instances of a unit share their cmi). *)
  let cmi_index = Hashtbl.create 256 in
  let cmi_entries = ref [] in
  let n_cmi = ref 0 in
  List.iter
    (fun u ->
      match u.u_own_intf_crc with
      | None -> ()
      | Some crc ->
        let name = CU.name u.u_name in
        if not (Hashtbl.mem cmi_index (CU.Name.to_string name))
        then (
          Hashtbl.add cmi_index (CU.Name.to_string name) !n_cmi;
          cmi_entries := Import_info.Intf.create_normal name u.u_name ~crc :: !cmi_entries;
          incr n_cmi))
    s.units;
  let bitmap n ids =
    let b = Misc.Bitmap.make n in
    List.iter (fun i -> Misc.Bitmap.set b i) ids;
    b
  in
  let lib_units =
    List.map2
      (fun u (cmx_ids, qcmi_ids, qcmx_ids) ->
        { li_name = u.u_name
        ; li_crc = u.u_impl_crc
        ; li_defines = u.u_defines
        ; li_force_link = u.u_force_link
        ; li_imports_cmi =
            bitmap
              !n_cmi
              (match
                 Hashtbl.find_opt cmi_index (CU.Name.to_string (CU.name u.u_name))
               with
               | Some i -> [ i ]
               | None -> [])
        ; li_imports_cmx = bitmap !cmx_next cmx_ids
        ; li_quoted_cmi = bitmap !qcmi_next qcmi_ids
        ; li_quoted_cmx = bitmap !qcmx_next qcmx_ids
        ; li_external_symbols = [||]
        })
      s.units
      unit_ids
  in
  { lib_imports_cmi = Array.of_list (List.rev !cmi_entries)
  ; lib_imports_cmx =
      List.rev !cmx_rev
      |> List.map (fun cu -> Import_info.create_normal cu ~crc:None)
      |> Array.of_list
  ; lib_quoted_cmi = List.rev !qcmi_rev |> Array.of_list
  ; lib_quoted_cmx = List.rev !qcmx_rev |> Array.of_list
  ; lib_units
  ; lib_generic_fns = s.generic_fns
  ; lib_requires_metaprogramming = s.requires_metaprogramming
  ; lib_ccobjs = s.ccobjs
  ; lib_ccopts = s.ccopts
  }
;;

let read ~dir ~obj_name ~file_name : library_infos =
  let path =
    let p = summary_path ~dir obj_name in
    if Sys.file_exists p then p else summary_path ~dir file_name
  in
  let s =
    try
      let ic = open_in_bin path in
      Misc.try_finally
        (fun () ->
          let buf = really_input_string ic (String.length magic) in
          if buf <> magic then failwith "bad or unsupported summary magic";
          (Marshal.from_channel ic : t))
        ~always:(fun () -> close_in ic)
    with
    | exn ->
      Misc.fatal_errorf
        "-cmxa-summaries: cannot read summary %s for library %s: %s"
        path
        file_name
        (Printexc.to_string exn)
  in
  inflate s
;;
