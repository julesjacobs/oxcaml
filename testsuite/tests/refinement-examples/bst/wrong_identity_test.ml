(* TEST
 readonly_files = "bst.mli wrong_identity.ml wrong_identity.reference";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 script = "cp wrong_identity.ml bst.ml";
 script;
 module = "bst.ml";
 compiler_output = "wrong_identity.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_identity.reference";
 check-ocamlc.byte-output;
*)
