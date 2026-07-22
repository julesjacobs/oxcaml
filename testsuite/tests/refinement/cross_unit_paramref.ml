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

 (* A true cross-unit dependent claim is instantiated at the supplied
    argument. *)
 module = "cup_incomplete.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 (* Re-exporting the same binder through an alias preserves its identity. *)
 module = "cup_reexport.ml";
 ocamlc.byte;

 module = "cup_alias_clash.ml";
 ocamlc.byte;
*)

(* Cross-unit dependent-result regression.  Arrow-bound references are
   freshened coherently on import and remain distinct from sibling and captured
   references: false claims reject, while true dependent results instantiate at
   applications. *)
