(* TEST
 readonly_files = "set_intf.ml wrong_bst_insert.ml wrong_bst_insert.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 script = "cp wrong_bst_insert.ml bst.ml";
 script;
 module = "bst.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_bst_insert.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_bst_insert.reference";
 check-ocamlc.byte-output;
*)
