(* TEST
 readonly_files = "set_intf.ml bal_intf.ml wrong_avl_equal.ml wrong_avl_equal.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 module = "bal_intf.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp wrong_avl_equal.ml avl.ml";
 script;
 module = "avl.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_avl_equal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_avl_equal.reference";
 check-ocamlc.byte-output;
*)
