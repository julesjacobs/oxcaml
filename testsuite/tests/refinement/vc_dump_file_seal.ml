(* TEST
 readonly_files = "vc_dump_file_seal.mli vc_dump_file_seal.reference";
 setup-ocamlc.byte-build-env;

 module = "vc_dump_file_seal.mli";
 flags = "-c";
 ocamlc.byte;

 module = "vc_dump_file_seal.ml";
 flags = "-vox-dump-vc -c";
 compiler_output = "vc_dump_file_seal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/vc_dump_file_seal.reference";
 check-ocamlc.byte-output;
*)

let positive = (1 : int{ _ = 1 })
