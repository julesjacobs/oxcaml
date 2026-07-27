(* TEST
 readonly_files = "\
   decreases_tot_grant.ml decreases_tot_grant.reference \
   decreases_tot_no_measure.ml decreases_tot_no_measure.reference \
   decreases_tot_loop.ml decreases_tot_loop.reference \
   decreases_tot_partial_op.ml decreases_tot_partial_op.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "decreases_tot_grant.ml";
 compiler_output = "decreases_tot_grant.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/decreases_tot_grant.reference";
 check-ocamlc.byte-output;

 module = "decreases_tot_no_measure.ml";
 compiler_output = "decreases_tot_no_measure.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/decreases_tot_no_measure.reference";
 check-ocamlc.byte-output;

 module = "decreases_tot_loop.ml";
 compiler_output = "decreases_tot_loop.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/decreases_tot_loop.reference";
 check-ocamlc.byte-output;

 module = "decreases_tot_partial_op.ml";
 compiler_output = "decreases_tot_partial_op.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/decreases_tot_partial_op.reference";
 check-ocamlc.byte-output;
*)

(* What a [@vox.decreases] measure buys, witnessed in batch rather than at the
   toplevel.  The toplevel cannot witness it: in the expect harness even an
   ordinary [fun x -> x + 1] is reported partial where a total value is
   wanted, so a toplevel test would say nothing either way.

   Four subjects.  The first is accepted: an integer recursion is not
   structural, so the measure is the only thing that can make it total.  The
   second is the same body with the measure removed and is rejected, which is
   what makes the first one evidence.  The third and fourth are measured but
   contain a loop and an integer division, and are rejected too: the measure
   pays for termination of the recursion and for nothing else. *)
