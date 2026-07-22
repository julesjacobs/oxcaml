(* TEST
 readonly_files = "set_intf.ml wrong_ulist_equal.ml wrong_ulist_equal.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 script = "cp wrong_ulist_equal.ml ulist.ml";
 script;
 module = "ulist.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_ulist_equal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_ulist_equal.reference";
 check-ocamlc.byte-output;
*)
