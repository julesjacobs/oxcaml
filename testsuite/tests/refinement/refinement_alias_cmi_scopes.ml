(* TEST
 readonly_files = "\
   refinement_alias_cmi_provider.mli \
   refinement_alias_cmi_positive.ml \
   refinement_alias_cmi_instance_negative.ml \
   refinement_alias_cmi_instance_negative.reference \
   refinement_alias_cmi_left.mli \
   refinement_alias_cmi_right.mli \
   refinement_alias_cmi_qualified_seal_positive.ml \
   refinement_alias_cmi_qualified_seal_negative.ml \
   refinement_alias_cmi_qualified_seal_negative.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "refinement_alias_cmi_provider.mli";
 ocamlc.byte;
 module = "refinement_alias_cmi_positive.ml";
 ocamlc.byte;

 module = "refinement_alias_cmi_instance_negative.ml";
 compiler_output = "instance-negative.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_alias_cmi_instance_negative.reference";
 check-ocamlc.byte-output;

 module = "refinement_alias_cmi_left.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "refinement_alias_cmi_right.mli";
 ocamlc.byte;
 module = "refinement_alias_cmi_qualified_seal_positive.ml";
 ocamlc.byte;
 module = "refinement_alias_cmi_qualified_seal_negative.ml";
 compiler_output = "qualified-seal-negative.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refinement_alias_cmi_qualified_seal_negative.reference";
 check-ocamlc.byte-output;
*)
