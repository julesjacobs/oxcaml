(* TEST
 readonly_files = "set_intf.ml bal_intf.ml wrong_bst_nonstrict.ml wrong_bst_nonstrict.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 module = "bal_intf.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp wrong_bst_nonstrict.ml bst.ml";
 script;
 module = "bst.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_bst_nonstrict.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_bst_nonstrict.reference";
 check-ocamlc.byte-output;
*)
