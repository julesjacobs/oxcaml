(* TEST
 readonly_files = "ide_emit_span_mapper.ml ide_emit_foreign.ml \
                   ide_emit_metadata_check.sh";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;

 program = "${test_build_directory}/ide_emit_span_mapper.exe";
 all_modules = "ide_emit_span_mapper.ml";
 ocamlc.byte;

 module = "ide_emit_metadata.ml";
 flags = "-vox-dump-vc -vox-dump-vc-json normal.json -c";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "sh ${test_source_directory}/ide_emit_metadata_check.sh \
           positive normal.json";
 script;

 module = "ide_emit_metadata.ml";
 flags = "-ppx '${program} ghost' -vox-dump-vc \
         -vox-dump-vc-json ghost.json -c";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "sh ${test_source_directory}/ide_emit_metadata_check.sh \
           absent ghost.json";
 script;

 module = "ide_emit_metadata.ml";
 flags = "-ppx '${program} malformed' -vox-dump-vc \
         -vox-dump-vc-json malformed.json -c";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "sh ${test_source_directory}/ide_emit_metadata_check.sh \
           absent malformed.json";
 script;

 module = "ide_emit_foreign.ml";
 flags = "-vox-dump-vc -vox-dump-vc-json foreign.json -c";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "sh ${test_source_directory}/ide_emit_metadata_check.sh \
           absent foreign.json";
 script;
*)

let plain_inferred = 1 + 2

let branch_result n =
  match n with
  | 0 -> (0 : int{ _ >= 0 })
  | _ -> (n : int{ _ >= 0 })

let guarded value =
  match value with
  | Some (x : int{ _ >= 0 }) when x = 0 -> (x : int{ _ >= 0 })
  | None -> (0 : int{ _ >= 0 })

let shadowed (shadow : int{ _ >= 5 }) =
  let shadow : int{ _ >= 0 } = 0 in
  (shadow : int{ _ >= 0 })
