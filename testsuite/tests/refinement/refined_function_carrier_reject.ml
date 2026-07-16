(* TEST
 readonly_files = "\
   refined_carrier_option.ml refined_carrier_option.reference \
   refined_carrier_list.ml refined_carrier_list.reference \
   refined_carrier_tuple.ml refined_carrier_tuple.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "refined_carrier_option.ml";
 compiler_output = "refined_carrier_option.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_carrier_option.reference";
 check-ocamlc.byte-output;

 module = "refined_carrier_list.ml";
 compiler_output = "refined_carrier_list.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_carrier_list.reference";
 check-ocamlc.byte-output;

 module = "refined_carrier_tuple.ml";
 compiler_output = "refined_carrier_tuple.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_carrier_tuple.reference";
 check-ocamlc.byte-output;
*)

(* Batch (default-mode) regressions for the modelability gate on anonymous
   structural carriers of functions.  Reading a self whose type carries a
   function through an [option], a [list], or a tuple is not modelable and must
   be rejected at elaboration.  These compile under plain [ocamlc.byte]
   (non-principal, non-toplevel) and pin the dedicated
   [Refinement_self_not_modelable] error in default mode.

   Companion note: under [-principal] the same declarations still REJECT, but
   with the pre-existing use-site mode error ("logical but expected physical")
   instead of the dedicated one, because the ikind solver that computes the
   totality crossing consults the global [Clflags.principal] rather than the
   always-principal context [crossing_of_jkind_principal] passes it.  The reject
   verdict is identical in both modes; only the message differs, and principality
   parity of the message is not a requirement.  See the direct-arrow case in
   [refined_function_self_reject.ml] and the report's message-parity note. *)
