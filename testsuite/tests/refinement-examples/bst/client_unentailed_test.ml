(* TEST
 readonly_files = "\
   bst.mli bst.ml client_unentailed.ml client_unentailed.reference \
   seal_precondition.mli seal_precondition.ml seal_precondition.reference \
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
 module = "seal_precondition.mli";
 flags = "";
 compiler_output = "seal_precondition_mli.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "seal_precondition.ml";
 compiler_output = "seal_precondition.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/seal_precondition.reference";
 check-ocamlc.byte-output;
*)
