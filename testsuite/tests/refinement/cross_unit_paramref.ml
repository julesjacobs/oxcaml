(* TEST
 readonly_files = "\
   cup_provider.ml \
   cup_neg_result.ml cup_neg_result.reference \
   cup_neg_binder.ml cup_neg_binder.reference \
   cup_neg_sibling.ml cup_neg_sibling.reference \
   cup_neg_captured.ml cup_neg_captured.reference \
   cup_incomplete.ml cup_incomplete.reference \
   cup_reexport.ml cup_alias_clash.ml \
 ";
 setup-ocamlc.byte-build-env;

 module = "cup_provider.ml";
 ocamlc.byte;

 (* Original hole, result-fact path: false parameter claim must be rejected. *)
 module = "cup_neg_result.ml";
 compiler_output = "cup_neg_result.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_neg_result.reference";
 check-ocamlc.byte-output;

 (* Original hole, binder-fact path. *)
 module = "cup_neg_binder.ml";
 compiler_output = "cup_neg_binder.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_neg_binder.reference";
 check-ocamlc.byte-output;

 (* (b)-arity-1 witness: result mentions a SIBLING, not the parameter -- a naive
    argument substitution would prove a false claim.  Must be rejected. *)
 module = "cup_neg_sibling.ml";
 compiler_output = "cup_neg_sibling.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_neg_sibling.reference";
 check-ocamlc.byte-output;

 (* (b)-arity-1 witness: result mentions a CAPTURED local. *)
 module = "cup_neg_captured.ml";
 compiler_output = "cup_neg_captured.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_neg_captured.reference";
 check-ocamlc.byte-output;

 (* Accepted incompleteness: a TRUE cross-unit dependent claim is left opaque
    (dependent results are not argument-substituted) and does not prove. *)
 module = "cup_incomplete.ml";
 compiler_output = "cup_incomplete.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_incomplete.reference";
 check-ocamlc.byte-output;

 (* Alias edge (known accepted limitation): the same dangling parameter freshened
    via two import routes no longer unifies -- a fail-closed rigid clash.  Assert
    by exit status only; the clash message embeds build-varying fresh stamps. *)
 ocamlc_byte_exit_status = "0";
 module = "cup_reexport.ml";
 ocamlc.byte;

 module = "cup_alias_clash.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)

(* Cross-unit dependent-result refinement regression.  A separately compiled
   provider exports functions whose result refinement mentions a parameter, a
   sibling, or a captured local -- all lowered as free local Pidents that could,
   pre-fix, collide with a caller-local binder (a soundness hole) and that a
   naive argument-substitution heuristic could not tell apart (a second hole).
   Foreign parameter references are freshened to opaque symbols on import: false
   claims are fail-closed rejected through every path, and dependent cross-unit
   results are soundly opaque (accepted incompleteness). *)
