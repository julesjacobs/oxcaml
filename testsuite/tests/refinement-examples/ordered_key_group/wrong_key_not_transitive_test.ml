(* TEST
 readonly_files = "key_intf.ml wrong_key_not_transitive.ml wrong_key_not_transitive.reference";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "wrong_key_not_transitive.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_key_not_transitive.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_key_not_transitive.reference";
 check-ocamlc.byte-output;
*)
