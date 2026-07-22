(* TEST
 readonly_files = "\
   abstract_term_is_not_inhabitant_source.ml \
   abstract_term_is_not_inhabitant.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "abstract_term_is_not_inhabitant_source.ml";
 compiler_output = "abstract_term_is_not_inhabitant_lean.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "\
   ${test_source_directory}/abstract_term_is_not_inhabitant.reference";
 check-ocamlc.byte-output;
*)
