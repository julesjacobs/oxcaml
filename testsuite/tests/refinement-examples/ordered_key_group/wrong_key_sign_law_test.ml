(* TEST
 readonly_files = "key_intf.ml wrong_key_sign_law.ml wrong_key_sign_law.reference";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "wrong_key_sign_law.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_key_sign_law.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_key_sign_law.reference";
 check-ocamlc.byte-output;
*)
