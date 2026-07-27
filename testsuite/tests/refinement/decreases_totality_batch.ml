(* TEST
 readonly_files = "\
   decreases_tot_grant.ml decreases_tot_grant.reference \
   decreases_tot_no_measure.ml decreases_tot_no_measure.reference \
   decreases_tot_loop.ml decreases_tot_loop.reference \
   decreases_tot_partial_op.ml decreases_tot_partial_op.reference \
   decreases_tot_record.ml decreases_tot_record.reference \
   decreases_tot_record_mutable.ml decreases_tot_record_mutable.reference \
   decreases_tot_record_shadow.ml decreases_tot_record_shadow.reference \
   decreases_tot_orbit.ml decreases_tot_orbit.reference \
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

 module = "decreases_tot_record.ml";
 compiler_output = "decreases_tot_record.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/decreases_tot_record.reference";
 check-ocamlc.byte-output;

 module = "decreases_tot_record_mutable.ml";
 compiler_output = "decreases_tot_record_mutable.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/decreases_tot_record_mutable.reference";
 check-ocamlc.byte-output;

 module = "decreases_tot_record_shadow.ml";
 compiler_output = "decreases_tot_record_shadow.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/decreases_tot_record_shadow.reference";
 check-ocamlc.byte-output;

 module = "decreases_tot_orbit.ml";
 compiler_output = "decreases_tot_orbit.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/decreases_tot_orbit.reference";
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
   pays for termination of the recursion and for nothing else.

   Three more subjects concern the other route to termination, structural
   recursion, and specifically what a record pattern does with it.  A record
   of children is accepted where before it was not; the same record with a
   mutable field stays refused; and a mutable field the parse tree cannot see
   is refused with a message of its own.

   The last subject is a two-parameter recursion that does not terminate,
   asked for where a total value is wanted.  Its measure descends only if
   each argument is read at the values the call passes; it is here because a
   reading that let one position's argument be rewritten by another's
   accepted it as total. *)
