(* TEST
 readonly_files = "key_intf.ml gen_avl.mli wrong_avl_duplicate.ml wrong_avl_duplicate.reference";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "gen_avl.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp wrong_avl_duplicate.ml gen_avl.ml";
 script;
 module = "gen_avl.ml";
 compiler_output = "wrong_avl_duplicate.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_avl_duplicate.reference";
 check-ocamlc.byte-output;
*)
