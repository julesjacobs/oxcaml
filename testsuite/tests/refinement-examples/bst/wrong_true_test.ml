(* TEST
 readonly_files = "bst.mli wrong_true.ml wrong_true.reference";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 script = "cp wrong_true.ml bst.ml";
 script;
 module = "bst.ml";
 compiler_output = "wrong_true.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_true.reference";
 check-ocamlc.byte-output;
*)
