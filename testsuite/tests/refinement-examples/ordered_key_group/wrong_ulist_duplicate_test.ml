(* TEST
 readonly_files = "key_intf.ml gen_ulist.mli wrong_ulist_duplicate.ml wrong_ulist_duplicate.reference";
 setup-ocamlc.byte-build-env;
 module = "key_intf.ml";
 ocamlc.byte;
 module = "gen_ulist.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp wrong_ulist_duplicate.ml gen_ulist.ml";
 script;
 module = "gen_ulist.ml";
 compiler_output = "wrong_ulist_duplicate.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_ulist_duplicate.reference";
 check-ocamlc.byte-output;
*)
