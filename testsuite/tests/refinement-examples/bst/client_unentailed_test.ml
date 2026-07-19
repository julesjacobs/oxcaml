(* TEST
 readonly_files = "\
   bst.mli bst.ml client_unentailed.ml client_unentailed.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 module = "bst.ml";
 ocamlc.byte;
 module = "client_unentailed.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "client_unentailed.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/client_unentailed.reference";
 check-ocamlc.byte-output;
*)
