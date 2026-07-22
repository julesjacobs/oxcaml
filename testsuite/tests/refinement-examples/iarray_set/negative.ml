(* TEST
 readonly_files = "\
   iarray_model.mli iarray_model.ml iarray_set.mli iarray_set.ml \
   negative_wrong_member.ml negative_wrong_member.reference \
   negative_wrong_insert.ml negative_wrong_insert.reference \
   negative_nonextensional_equal.ml negative_nonextensional_equal.reference \
   negative_wrong_wrapper_seal.ml negative_wrong_wrapper_seal.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "${test_source_directory}/../set_group/set_intf.ml";
 flags = "-o set_intf.cmo";
 ocamlc.byte;
 module = "iarray_model.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "iarray_model.ml";
 ocamlc.byte;
 module = "iarray_set.mli";
 ocamlc.byte;
 module = "iarray_set.ml";
 ocamlc.byte;
 module = "negative_wrong_member.ml";
 compiler_output = "negative_wrong_member.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/negative_wrong_member.reference";
 check-ocamlc.byte-output;
 module = "negative_wrong_insert.ml";
 compiler_output = "negative_wrong_insert.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/negative_wrong_insert.reference";
 check-ocamlc.byte-output;
 module = "negative_nonextensional_equal.ml";
 compiler_output = "negative_nonextensional_equal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/negative_nonextensional_equal.reference";
 check-ocamlc.byte-output;
 module = "negative_wrong_wrapper_seal.ml";
 compiler_output = "negative_wrong_wrapper_seal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/negative_wrong_wrapper_seal.reference";
 check-ocamlc.byte-output;
*)
