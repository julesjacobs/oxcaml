(* TEST
 readonly_files = "\
   parameterized_abstract_term_is_not_inhabitant_source.ml \
   parameterized_abstract_term_is_not_inhabitant.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "parameterized_abstract_term_is_not_inhabitant_source.ml";
 compiler_output = "parameterized_abstract_term_is_not_inhabitant.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "\
   ${test_source_directory}/parameterized_abstract_term_is_not_inhabitant.reference";
 check-ocamlc.byte-output;
*)
