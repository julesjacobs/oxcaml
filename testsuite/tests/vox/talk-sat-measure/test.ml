(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml";
 readonly_files = "run.sh";
 arguments = "${test_source_directory}/run.sh ${ocamlc_opt} ${ocamlsrcdir}/stdlib ${test_source_directory}/../../../../verification/library";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run;
   check-program-output;
 }
*)

(* Talk, section 4f ("totality and certificates: CDCL SAT"): "the
   termination measure is absent * (n+1) + unassigned ... Drop the weight
   and the proof fails at the line that needs it." run.sh compiles the real
   Vox_cdcl_total_proof (accepted) and a MUTANT COPY, derived from the
   library's current source by one substitution, whose measure is
   absent * 1 + unassigned. The mutant is rejected in the lemma that needs
   the weight, [progress_learning]: without it, learning one clause does
   not pay for backjumping over many variables. From the investigation's
   demos-algorithms experiment 1. *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command "sh" arguments))
