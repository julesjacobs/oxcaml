external event : string -> unit = "caml_vox_browser_event"
let json s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (function
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c when Char.code c < 32 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c) s;
  Buffer.add_char b '"'; Buffer.contents b
let range loc = Printf.sprintf "\"from\":%d,\"to\":%d"
  loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum
let metadata typed =
  let entries = ref [] in
  let iterator = { Tast_iterator.default_iterator with
    expr = (fun self exp ->
      let loc = exp.Typedtree.exp_loc in
      if not loc.Location.loc_ghost && loc.loc_start.pos_cnum >= 0 then begin
        let typ = Printtyp.wrap_printing_env ~error:false exp.exp_env (fun () ->
          Format.asprintf "%a" Printtyp.type_expr exp.exp_type) in
        let definition = match exp.exp_desc with
          | Texp_ident { desc; _ } when desc.Types.val_loc.loc_start.pos_fname = loc.loc_start.pos_fname ->
              Printf.sprintf ",\"definition\":%d" desc.val_loc.loc_start.pos_cnum
          | _ -> "" in
        entries := Printf.sprintf "{%s,\"text\":%s%s}" (range loc) (json typ) definition :: !entries
      end;
      Tast_iterator.default_iterator.expr self exp)
  } in
  iterator.structure iterator typed.Typedtree.structure;
  let signature = Format.asprintf "%a" (Printtyp.printed_signature "lesson.ml") typed.signature in
  event (Printf.sprintf "{\"type\":\"metadata\",\"signature\":%s,\"hovers\":[%s]}"
    (json signature) (String.concat "," (List.rev !entries)))
let () =
  try
    Language_extension.enable_of_string_exn "refinement_types";
    Clflags.debug := true;
    Clflags.real_paths := false;
    Clflags.use_prims := "/stdlib/primitives";
    Vox_verify.install ();
    let source_file = "/work/lesson.ml" and output_prefix = "/work/lesson" in
    let target = Compile_common.unit_info_from_cu_or_output_prefix
      ~source_file Unit_info.Impl ~output_prefix
      ~compilation_unit:Compile_common.Inferred_from_output_prefix in
    Compile_common.with_info ~backend:Compile_common.Byte ~tool_name:"vox-browser" ~dump_ext:"dump" target
      (fun info -> Compile_common.implementation ~hook_parse_tree:(fun _ -> ())
        ~hook_typed_tree:metadata info ~backend:(fun info typed ->
          Compile.emit_bytecode info (Compile.to_bytecode info typed ~as_arg_for:None)));
    Bytelink.link ["/work/lesson.cmo"] "/work/lesson.byte";
    event "{\"type\":\"compiled\"}"
  with exn ->
    let message = try Format.asprintf "%a" Location.report_exception exn
      with _ -> Printexc.to_string exn in
    let loc = match Location.error_of_exn exn with
      | Some (`Ok error) -> error.Location.main.loc
      | _ -> Location.none in
    event (Printf.sprintf "{\"type\":\"diagnostic\",\"severity\":\"error\",%s,\"message\":%s}"
      (range loc) (json message));
    exit 2
