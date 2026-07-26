(* TEST
 readonly_files = "set_intf.ml wrong_rbt_trivial_invariant.ml wrong_rbt_trivial_invariant.reference";
 setup-ocamlc.byte-build-env;
 module = "set_intf.ml";
 ocamlc.byte;
 script = "cp wrong_rbt_trivial_invariant.ml rbt.ml";
 script;
 module = "rbt.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_rbt_trivial_invariant.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_rbt_trivial_invariant.reference";
 check-ocamlc.byte-output;
*)
