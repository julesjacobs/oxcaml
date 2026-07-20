(* TEST
 readonly_files = "bst.mli wrong_empty.ml wrong_empty.reference";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 script = "cp wrong_empty.ml bst.ml";
 script;
 module = "bst.ml";
 compiler_output = "wrong_empty.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_empty.reference";
 check-ocamlc.byte-output;
*)
