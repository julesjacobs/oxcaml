(* TEST
 readonly_files = "\
   refined_tot_read_int.ml refined_tot_read_int.reference \
   refined_tot_deref.ml refined_tot_deref.reference \
   refined_tot_stdlib.ml refined_tot_stdlib.reference \
   refined_poly_self_reject.ml refined_poly_self_reject.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "refined_tot_read_int.ml";
 compiler_output = "refined_tot_read_int.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_tot_read_int.reference";
 check-ocamlc.byte-output;

 module = "refined_tot_deref.ml";
 compiler_output = "refined_tot_deref.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_tot_deref.reference";
 check-ocamlc.byte-output;

 module = "refined_tot_stdlib.ml";
 compiler_output = "refined_tot_stdlib.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_tot_stdlib.reference";
 check-ocamlc.byte-output;

 module = "refined_poly_self_reject.ml";
 compiler_output = "refined_poly_self_reject.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_poly_self_reject.reference";
 check-ocamlc.byte-output;
*)

(* Batch (default-mode) companions for toplevel-only totality/logicality
   witnesses (expect-audit convergent finding).  Each subject is compiled under
   plain [ocamlc.byte] (non-principal, non-toplevel) and pinned to REJECT:
   three core totality rejects -- a partial IO primitive ([read_int]), a mutable
   read ([!]), and a partial stdlib function ([List.map]) used inside a [@ total]
   function -- and the polymorphic-self predicate reject (a polymorphic self is
   not known to cross logicality, so comparing it in its own predicate is
   rejected).  These behaviours were previously witnessed only by toplevel
   %%expect tests; the toplevel is principal-like in both its passes, which is
   the masking class that hid the function-self default-batch bug, so these
   default-mode witnesses guard against the same masking. *)
