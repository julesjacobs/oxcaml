(* TEST
 readonly_files = "set_intf.ml bal_intf.ml wrong_rbt_red_root.ml wrong_rbt_red_root.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 module = "bal_intf.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 script = "cp wrong_rbt_red_root.ml rbt.ml";
 script;
 module = "rbt.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_rbt_red_root.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_rbt_red_root.reference";
 check-ocamlc.byte-output;
*)
