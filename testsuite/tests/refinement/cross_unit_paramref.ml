(* TEST
 readonly_files = "\
   cup_provider.ml cup_pos.ml \
   cup_neg_result.ml cup_neg_result.reference \
   cup_neg_binder.ml cup_neg_binder.reference \
   cup_reexport.ml cup_alias_clash.ml \
 ";
 setup-ocamlc.byte-build-env;

 module = "cup_provider.ml";
 ocamlc.byte;

 (* Positives: the imported parameter is substituted by the actual argument,
    so these dependent results verify.  Must compile cleanly. *)
 module = "cup_pos.ml";
 ocamlc.byte;

 (* Negative (result path): a false cross-unit dependent claim must be rejected.
    Pre-fix this PROVED via a foreign/caller parameter stamp collision. *)
 module = "cup_neg_result.ml";
 compiler_output = "cup_neg_result.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_neg_result.reference";
 check-ocamlc.byte-output;

 (* Negative (binder-fact path): the same hole reached through a let-binder
    whose inferred type is the imported result refinement. *)
 module = "cup_neg_binder.ml";
 compiler_output = "cup_neg_binder.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/cup_neg_binder.reference";
 check-ocamlc.byte-output;

 (* Alias edge (known accepted limitation): re-export the provider, then unify
    the direct and re-exported aliases -- a fail-closed rigid clash because the
    two import routes freshen the dangling parameter to different stamps.  Assert
    the rejection by exit status only; the clash message embeds fresh stamps that
    are not stable across builds. *)
 ocamlc_byte_exit_status = "0";
 module = "cup_reexport.ml";
 ocamlc.byte;

 module = "cup_alias_clash.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)

(* Cross-unit dependent-result refinement regression.  A separately compiled
   provider exports functions whose result refinement mentions a parameter
   ([int{ _ = x }]).  Because a parameter reference keeps its unit's local stamp
   and stamps are only unit-unique, an imported reference could collide with a
   caller-local binder and launder two distinct values -- a soundness hole
   (fixed by freshening foreign parameter references on import).  The positives
   pin the arity-1 substitution that makes legitimate cross-unit dependent
   results verify; the negatives pin the fail-closed rejection of false claims
   through both the result-fact and binder-fact paths. *)
