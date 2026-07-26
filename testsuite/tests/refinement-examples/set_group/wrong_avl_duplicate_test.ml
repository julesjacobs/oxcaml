(* TEST
 readonly_files = "set_intf.ml wrong_avl_duplicate.ml wrong_avl_duplicate.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 script = "cp wrong_avl_duplicate.ml avl.ml";
 script;
 module = "avl.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_avl_duplicate.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_avl_duplicate.reference";
 check-ocamlc.byte-output;
*)
