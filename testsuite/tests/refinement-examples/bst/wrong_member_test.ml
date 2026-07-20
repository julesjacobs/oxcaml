(* TEST
 readonly_files = "bst.mli wrong_member.ml wrong_member.reference";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 script = "cp wrong_member.ml bst.ml";
 script;
 module = "bst.ml";
 compiler_output = "wrong_member.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_member.reference";
 check-ocamlc.byte-output;
*)
