(* TEST
 readonly_files = "set_intf.ml bal_intf.ml wrong_ulist_cardinal_only.ml wrong_ulist_cardinal_only.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 module = "bal_intf.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp wrong_ulist_cardinal_only.ml ulist.ml";
 script;
 module = "ulist.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_ulist_cardinal_only.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_ulist_cardinal_only.reference";
 check-ocamlc.byte-output;
*)
