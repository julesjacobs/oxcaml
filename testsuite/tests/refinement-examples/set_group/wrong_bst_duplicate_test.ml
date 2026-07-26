(* TEST
 readonly_files = "set_intf.ml wrong_bst_duplicate.ml wrong_bst_duplicate.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 script = "cp wrong_bst_duplicate.ml bst.ml";
 script;
 module = "bst.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_bst_duplicate.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_bst_duplicate.reference";
 check-ocamlc.byte-output;
*)
