(* TEST
 readonly_files = "\
   dependent_arrow_cmi_collision_a.mli \
   dependent_arrow_cmi_collision_b.mli \
 ";
 setup-ocamlc.byte-build-env;
 module = "dependent_arrow_cmi_collision_a.mli";
 ocamlc.byte;
 module = "dependent_arrow_cmi_collision_b.mli";
 ocamlc.byte;
 flags = "-principal";
 module = "dependent_arrow_cmi_collision_a.mli";
 ocamlc.byte;
 module = "dependent_arrow_cmi_collision_b.mli";
 ocamlc.byte;
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 include ocamlcommon;
 expect;
*)

open Types

let arrow_type filename : Types.type_expr =
  let cmi = Cmi_format.read_cmi ("ocamlc.byte/" ^ filename) in
  let signature, _ = cmi.Cmi_format.cmi_sign in
  match signature with
  | [Sig_value (_, description, Exported)] -> description.val_type
  | _ -> failwith "expected one exported value"

let identities type_ =
  match get_desc type_ with
  | Tarrow ((_, _, _, Some binder), _, result, _) ->
    begin match get_desc result with
    | Trefine
        { ref_pred =
            { rexp_desc =
                Rexp_apply
                  ( _,
                    [ _;
                      _, { rexp_desc = Rexp_ident (Rbound occurrence); _ };
                    ] );
              _ };
          _ } ->
      binder, occurrence
    | _ -> failwith "expected dependent result refinement"
    end
  | _ -> failwith "expected dependent arrow"

let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file filename contents =
  let channel = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_bytes channel contents)

let cmi_observations =
  let raw_a = arrow_type "dependent_arrow_cmi_collision_a.cmi" in
  let raw_b = arrow_type "dependent_arrow_cmi_collision_b.cmi" in
  let raw_a_binder, raw_a_occurrence = identities raw_a in
  let raw_b_binder, raw_b_occurrence = identities raw_b in
  assert (Ident.same raw_a_binder raw_a_occurrence);
  assert (Ident.same raw_b_binder raw_b_occurrence);
  assert (Ident.same raw_a_binder raw_b_binder);
  let imported_a = Subst.type_expr (Subst.for_loading_cmi ()) raw_a in
  let imported_b = Subst.type_expr (Subst.for_loading_cmi ()) raw_b in
  let imported_a_binder, imported_a_occurrence = identities imported_a in
  let imported_b_binder, imported_b_occurrence = identities imported_b in
  assert (Ident.same imported_a_binder imported_a_occurrence);
  assert (Ident.same imported_b_binder imported_b_occurrence);
  assert (not (Ident.same imported_a_binder imported_b_binder));
  let original =
    read_file "ocamlc.byte/dependent_arrow_cmi_collision_a.cmi"
  in
  let magic = Config.cmi_magic_number in
  assert (String.starts_with ~prefix:magic original);
  assert (String.ends_with ~suffix:"582" magic);
  let old_magic = Bytes.of_string original in
  Bytes.blit_string "581" 0 old_magic (String.length magic - 3) 3;
  write_file "old-dependent-arrow.cmi" old_magic;
  let old_version_rejected =
    match Cmi_format.read_cmi "old-dependent-arrow.cmi" with
    | _ -> false
    | exception
        Cmi_format.Error (Cmi_format.Wrong_version_interface _) -> true
  in
  assert old_version_rejected;
  [ "raw independently-written binder stamps collide";
    "each imported binder remains coherent with its codomain";
    "the two imported binders are distinct";
    "artifact magic 581 is rejected; current magic ends in 582";
  ]

[%%expect {|
val arrow_type : string -> Types.type_expr = <fun>
val identities : Types.type_expr -> Ident.t * Ident.t = <fun>
val read_file : string -> string = <fun>
val write_file : string -> bytes -> unit = <fun>
val cmi_observations : string list =
  ["raw independently-written binder stamps collide";
   "each imported binder remains coherent with its codomain";
   "the two imported binders are distinct";
   "artifact magic 581 is rejected; current magic ends in 582"]
|}]
