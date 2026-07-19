(* TEST
 readonly_files = "bst.mli wrong_insert.ml wrong_insert.reference";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 script = "cp wrong_insert.ml bst.ml";
 script;
 module = "bst.ml";
 compiler_output = "wrong_insert.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_insert.reference";
 check-ocamlc.byte-output;
*)
