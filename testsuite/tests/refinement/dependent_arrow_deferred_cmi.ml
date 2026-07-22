(* TEST
 readonly_files = "\
   dependent_arrow_deferred_provider.mli \
   dependent_arrow_deferred_provider.ml \
   dependent_arrow_deferred_client.ml \
   dependent_arrow_deferred_client_bad.ml \
   dependent_arrow_deferred_client_bad.reference \
   dependent_arrow_deferred_returning_bad.ml \
   dependent_arrow_deferred_returning_bad.reference \
   dependent_arrow_deferred_returning_result_bad.ml \
   dependent_arrow_deferred_returning_result_bad.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "dependent_arrow_deferred_provider.mli";
 ocamlc.byte;
 module = "dependent_arrow_deferred_provider.ml";
 ocamlc.byte;
 module = "dependent_arrow_deferred_client.ml";
 ocamlc.byte;
 module = "dependent_arrow_deferred_client_bad.ml";
 compiler_output = "dependent_arrow_deferred_client_bad.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_deferred_client_bad.reference";
 check-ocamlc.byte-output;
 module = "dependent_arrow_deferred_returning_bad.ml";
 compiler_output = "dependent_arrow_deferred_returning_bad.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_deferred_returning_bad.reference";
 check-ocamlc.byte-output;
 module = "dependent_arrow_deferred_returning_result_bad.ml";
 compiler_output = "dependent_arrow_deferred_returning_result_bad.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_deferred_returning_result_bad.reference";
 check-ocamlc.byte-output;

 flags = "-principal";
 module = "dependent_arrow_deferred_provider.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "dependent_arrow_deferred_provider.ml";
 ocamlc.byte;
 module = "dependent_arrow_deferred_client.ml";
 ocamlc.byte;
 module = "dependent_arrow_deferred_client_bad.ml";
 compiler_output = "dependent_arrow_deferred_client_bad.principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_deferred_client_bad.reference";
 check-ocamlc.byte-output;
 module = "dependent_arrow_deferred_returning_bad.ml";
 compiler_output = "dependent_arrow_deferred_returning_bad.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_deferred_returning_bad.reference";
 check-ocamlc.byte-output;
 module = "dependent_arrow_deferred_returning_result_bad.ml";
 compiler_output = "dependent_arrow_deferred_returning_result_bad.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_deferred_returning_result_bad.reference";
 check-ocamlc.byte-output;
*)
