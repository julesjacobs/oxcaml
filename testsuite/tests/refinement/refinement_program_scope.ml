(* TEST
 readonly_files = "\
   refinement_program_scope_provider.mli \
   refinement_program_scope_provider.ml \
   refinement_program_scope_client.ml \
   refinement_program_scope_inferred_provider.ml \
   refinement_program_scope_inferred_client.ml \
   refinement_program_scope_stable_exports.ml \
   refinement_program_scope_qualified_provider.mli \
   refinement_program_scope_qualified_provider.ml \
   refinement_program_scope_qualified_client.ml \
   refinement_program_scope_qualified_valid.ml \
   refinement_program_scope_qualified_result_escape.ml \
   refinement_program_scope_qualified_result_escape.reference \
   refinement_program_scope_qualified_record_escape.ml \
   refinement_program_scope_qualified_record_escape.reference \
   refinement_program_scope_qualified_constructor_escape.ml \
   refinement_program_scope_qualified_constructor_escape.reference \
   refinement_program_scope_qualified_domain_escape.ml \
   refinement_program_scope_qualified_domain_escape.reference \
   refinement_program_scope_hidden_open.ml \
   refinement_program_scope_hidden_open.reference \
   refinement_program_scope_vc_check.py \
   refinement_program_scope_cross_module.ml \
   refinement_program_scope_cross_module.reference \
   refinement_program_scope_cross_functor.ml \
   refinement_program_scope_cross_functor.reference \
   refinement_program_scope_seal_cross_functor.ml \
   refinement_program_scope_seal_cross_functor.reference \
   refinement_program_scope_seal_cross_functor_check.py \
   refinement_program_scope_seal_composed_application.ml \
   refinement_program_scope_seal_composed_application.reference \
   refinement_program_scope_seal_provider.ml \
   refinement_program_scope_seal_client.ml \
   refinement_program_scope_seal_client.reference \
   refinement_program_scope_nested_shadowed.mli \
   refinement_program_scope_nested_shadowed.ml \
   refinement_program_scope_nested_shadowed.reference \
   refinement_program_scope_client_collision.ml \
   refinement_program_scope_client_collision.reference \
   refinement_program_scope_shadowed.ml \
   refinement_program_scope_shadowed.reference \
   refinement_program_scope_annotated_function.ml \
   refinement_program_scope_annotated_function.reference \
   refinement_program_scope_annotated_case.ml \
   refinement_program_scope_annotated_case.reference \
   refinement_program_scope_domain_escape.ml \
   refinement_program_scope_domain_escape.reference \
   refinement_program_scope_case_escape.ml \
   refinement_program_scope_case_escape.reference \
   refinement_program_scope_result_escape.ml \
   refinement_program_scope_result_escape.reference \
   refinement_program_scope_record_escape.ml \
   refinement_program_scope_record_escape.reference \
   refinement_program_scope_constructor_escape.ml \
   refinement_program_scope_constructor_escape.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "refinement_program_scope_provider.mli";
 ocamlc.byte;
 module = "refinement_program_scope_provider.ml";
 ocamlc.byte;
 module = "refinement_program_scope_client.ml";
 ocamlc.byte;
 module = "refinement_program_scope_inferred_provider.ml";
 ocamlc.byte;
 module = "refinement_program_scope_inferred_client.ml";
 ocamlc.byte;
 flags = "-vox-dump-vc-json inferred-default.json";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/refinement_program_scope_vc_check.py inferred-default.json";
 script;
 flags = "";
 module = "refinement_program_scope_seal_provider.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "refinement_program_scope_seal_client.ml";
 compiler_output = "seal-client.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_seal_client.reference";
 check-ocamlc.byte-output;
 ocamlc_byte_exit_status = "0";
 module = "refinement_program_scope_stable_exports.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_provider.mli";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_provider.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_client.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_valid.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_result_escape.ml";
 compiler_output = "qualified-result-escape.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_result_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_qualified_record_escape.ml";
 compiler_output = "qualified-record-escape.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_record_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_qualified_constructor_escape.ml";
 compiler_output = "qualified-constructor-escape.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_constructor_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_qualified_domain_escape.ml";
 compiler_output = "qualified-domain-escape.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_domain_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_hidden_open.ml";
 compiler_output = "hidden-open.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_hidden_open.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_cross_module.ml";
 compiler_output = "cross-module.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_cross_module.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_cross_functor.ml";
 compiler_output = "cross-functor.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_cross_functor.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_seal_cross_functor.ml";
 flags = "-vox-dump-vc-json seal-cross-functor.json";
 compiler_output = "seal-cross-functor.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_seal_cross_functor.reference";
 check-ocamlc.byte-output;
 script = "python3 ${test_source_directory}/refinement_program_scope_seal_cross_functor_check.py seal-cross-functor.json";
 script;
 module = "refinement_program_scope_seal_composed_application.ml";
 flags = "";
 compiler_output = "seal-composed-application.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_seal_composed_application.reference";
 check-ocamlc.byte-output;
 flags = "";
 module = "refinement_program_scope_nested_shadowed.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "refinement_program_scope_nested_shadowed.ml";
 compiler_output = "nested-shadowed.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_nested_shadowed.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_client_collision.ml";
 compiler_output = "client-collision.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_client_collision.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_shadowed.ml";
 compiler_output = "shadowed.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_shadowed.reference";
 check-ocamlc.byte-output;

 module = "refinement_program_scope_annotated_function.ml";
 compiler_output = "annotated-function.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_annotated_function.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_annotated_case.ml";
 compiler_output = "annotated-case.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_annotated_case.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_domain_escape.ml";
 compiler_output = "domain-escape.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_domain_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_case_escape.ml";
 compiler_output = "case-escape.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_case_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_result_escape.ml";
 compiler_output = "result-escape.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_result_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_record_escape.ml";
 compiler_output = "record-escape.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_record_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_constructor_escape.ml";
 compiler_output = "constructor-escape.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_constructor_escape.reference";
 check-ocamlc.byte-output;

 flags = "-principal";
 module = "refinement_program_scope_provider.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "refinement_program_scope_provider.ml";
 ocamlc.byte;
 module = "refinement_program_scope_client.ml";
 ocamlc.byte;
 module = "refinement_program_scope_inferred_provider.ml";
 ocamlc.byte;
 module = "refinement_program_scope_inferred_client.ml";
 ocamlc.byte;
 flags = "-principal -vox-dump-vc-json inferred-principal.json";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/refinement_program_scope_vc_check.py inferred-principal.json";
 script;
 flags = "-principal";
 module = "refinement_program_scope_seal_provider.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "refinement_program_scope_seal_client.ml";
 compiler_output = "seal-client-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_seal_client.reference";
 check-ocamlc.byte-output;
 ocamlc_byte_exit_status = "0";
 module = "refinement_program_scope_stable_exports.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_provider.mli";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_provider.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_client.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_valid.ml";
 ocamlc.byte;
 module = "refinement_program_scope_qualified_result_escape.ml";
 compiler_output = "qualified-result-escape-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_result_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_qualified_record_escape.ml";
 compiler_output = "qualified-record-escape-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_record_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_qualified_constructor_escape.ml";
 compiler_output = "qualified-constructor-escape-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_constructor_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_qualified_domain_escape.ml";
 compiler_output = "qualified-domain-escape-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_qualified_domain_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_hidden_open.ml";
 compiler_output = "hidden-open-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_hidden_open.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_cross_module.ml";
 compiler_output = "cross-module-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_cross_module.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_cross_functor.ml";
 compiler_output = "cross-functor-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_cross_functor.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_seal_cross_functor.ml";
 flags = "-principal -vox-dump-vc-json seal-cross-functor-principal.json";
 compiler_output = "seal-cross-functor-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_seal_cross_functor.reference";
 check-ocamlc.byte-output;
 script = "python3 ${test_source_directory}/refinement_program_scope_seal_cross_functor_check.py seal-cross-functor-principal.json";
 script;
 module = "refinement_program_scope_seal_composed_application.ml";
 flags = "-principal";
 compiler_output = "seal-composed-application-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_seal_composed_application.reference";
 check-ocamlc.byte-output;
 flags = "-principal";
 module = "refinement_program_scope_nested_shadowed.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "refinement_program_scope_nested_shadowed.ml";
 compiler_output = "nested-shadowed-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_nested_shadowed.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_client_collision.ml";
 compiler_output = "client-collision-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_client_collision.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_shadowed.ml";
 compiler_output = "shadowed-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_shadowed.reference";
 check-ocamlc.byte-output;

 module = "refinement_program_scope_annotated_function.ml";
 compiler_output = "annotated-function-principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_annotated_function.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_annotated_case.ml";
 compiler_output = "annotated-case-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_annotated_case.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_domain_escape.ml";
 compiler_output = "domain-escape-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_domain_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_case_escape.ml";
 compiler_output = "case-escape-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_case_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_result_escape.ml";
 compiler_output = "result-escape-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_result_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_record_escape.ml";
 compiler_output = "record-escape-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_record_escape.reference";
 check-ocamlc.byte-output;
 module = "refinement_program_scope_constructor_escape.ml";
 compiler_output = "constructor-escape-principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_program_scope_constructor_escape.reference";
 check-ocamlc.byte-output;
*)
