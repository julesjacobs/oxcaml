(* TEST
 readonly_files = "\
   uninhabited_reference_source.ml uninhabited_reference.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "uninhabited_reference_source.ml";
 compiler_output = "uninhabited_reference.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/uninhabited_reference.reference";
 check-ocamlc.byte-output;
*)
