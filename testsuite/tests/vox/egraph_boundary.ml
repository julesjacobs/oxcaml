(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 set root = "${test_source_directory}/../../..";
 set lib = "";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli";
 all_modules += " vox_int_sequence.ml vox_iarray.mli vox_iarray.ml";
 all_modules += " borrow_iarray.mli borrow_iarray.ml vox_table_model.ml";
 all_modules += " vox_table_model_proofs.ml vox_table_bits.ml";
 all_modules += " vox_table_probe.ml vox_table_wrap.ml";
 all_modules += " vox_table_mask.ml vox_table_map.ml";
 all_modules += " vox_table_invariant.ml vox_table_initial.ml";
 all_modules += " vox_table_update_proofs.ml vox_table_insert_proofs.ml";
 all_modules += " vox_table_migration_proofs.ml";
 all_modules += " vox_table_read_proofs.ml vox_table_search_spec.ml";
 all_modules += " vox_table_stop_proof.ml pref.mli pref.ml";
 all_modules += " ghost_pref.mli ghost_pref.ml vox_table_storage.mli";
 all_modules += " vox_table_storage.ml vox_table_search.ml";
 all_modules += " vox_table_mutation.ml vox_table_coverage.ml";
 all_modules += " vox_table_occupancy.ml vox_table_vacancy_progress.ml";
 all_modules += " vox_table_vacancy.ml vox_table_insert.ml";
 all_modules += " vox_table_migrate.ml vox_table_resize.ml";
 all_modules += " vox_verified_flat_hashtbl.mli vox_table_bindings.ml";
 all_modules += " vox_table_bindings_bridge.ml";
 all_modules += " vox_table_implementation.ml";
 all_modules += " vox_verified_flat_hashtbl.ml vox_egraph_key.ml";
 all_modules += " vox_egraph_arena_spec.ml vox_egraph_index_spec.ml";
 all_modules += " vox_egraph_owner.ml vox_egraph_union_spec.ml";
 all_modules += " vox_egraph_union.ml vox_egraph_language_spec.ml";
 all_modules += " vox_egraph_language_proof.ml vox_egraph_rule_spec.ml";
 all_modules += " vox_egraph_rules.ml vox_egraph_derivation_spec.ml";
 all_modules += " vox_egraph_derivation.ml vox_egraph_origin_frame.ml";
 all_modules += " vox_egraph_rule_semantics.ml";
 all_modules += " vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml";
 all_modules += " vox_egraph_match_spec.ml vox_egraph_rule_node.ml";
 all_modules += " vox_egraph_rule_store.ml vox_egraph_rule_hashcons.ml";
 all_modules += " vox_egraph_rule_query.ml vox_egraph_rule_apply.ml";
 all_modules += " vox_egraph_match_observation.ml";
 all_modules += " vox_egraph_match_scan.ml vox_egraph_match_evidence.ml";
 all_modules += " vox_egraph_match_subst.ml vox_egraph_match_intro.ml";
 all_modules += " vox_egraph_match_bindings.ml";
 all_modules += " vox_egraph_binding_frame.ml";
 all_modules += " vox_egraph_match_store_intro.ml";
 all_modules += " vox_egraph_pattern_admit.ml";
 all_modules += " vox_egraph_rule_rewrite.ml vox_egraph_closure_spec.ml";
 all_modules += " vox_egraph_quantifier.mli";
 all_modules += " vox_egraph_quantifier_measure.ml";
 all_modules += " vox_egraph_quantifier.ml";
 all_modules += " vox_egraph_saturation_spec.ml";
 all_modules += " vox_egraph_saturation_proof.ml";
 all_modules += " vox_egraph_assignment_spec.ml";
 all_modules += " vox_egraph_assignments.ml";
 all_modules += " vox_egraph_assignment_proof.ml";
 all_modules += " vox_egraph_rule_scan.ml vox_egraph_rule_cursor.ml";
 all_modules += " vox_egraph_rules_scan.ml";
 all_modules += " vox_egraph_congruence_spec.ml";
 all_modules += " vox_egraph_congruence_proof.ml";
 all_modules += " vox_egraph_fixedpoint_spec.ml";
 all_modules += " vox_egraph_rule_saturate.ml";
 all_modules += " vox_egraph_snapshot_spec.ml";
 all_modules += " vox_egraph_snapshot_proof.ml";
 all_modules += " vox_egraph_preservation_spec.ml";
 all_modules += " vox_egraph_preservation_proof.ml";
 all_modules += " vox_egraph_model_evidence.ml";
 all_modules += " vox_egraph_rule_handle.mli vox_egraph_rule_handle.ml";
 all_modules += " vox_egraph_interpret_wrapping.mli";
 all_modules += " vox_egraph_interpret_wrapping.ml";
 setup-ocamlc.opt-build-env;
 lib = "${test_build_directory_prefix}/ocamlc.opt";
 compile_only = "true";
 ocamlc.opt;
 compiler_directory_suffix = ".lambda";
 all_modules = "vox_egraph_match_subst.ml vox_egraph_rule_rewrite.ml";
 all_modules += " vox_egraph_rule_saturate.ml";
 all_modules += " vox_egraph_model_evidence.ml";
 all_modules += " vox_egraph_rule_handle.ml";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -I ${lib} -dlambda";
 (* Dependents first: a recompiled unit's new .cmi does not match the
    library's objects. *)
 compiler_output2 = "${lib}.lambda/rule_handle.lambda";
 module = "vox_egraph_rule_handle.ml";
 ocamlc.opt;
 compiler_output2 = "${lib}.lambda/model_evidence.lambda";
 module = "vox_egraph_model_evidence.ml";
 ocamlc.opt;
 compiler_output2 = "${lib}.lambda/rule_saturate.lambda";
 module = "vox_egraph_rule_saturate.ml";
 ocamlc.opt;
 compiler_output2 = "${lib}.lambda/rule_rewrite.lambda";
 module = "vox_egraph_rule_rewrite.ml";
 ocamlc.opt;
 compiler_output2 = "${lib}.lambda/match_subst.lambda";
 module = "vox_egraph_match_subst.ml";
 ocamlc.opt;
 unset module;
 flags = "-extension refinement_types";
 compiler_output2 = "${lib}/ocamlc.opt.output";
 src = "${lib}/vox_egraph_language_spec.cmi";
 src += " ${lib}/vox_egraph_rule_spec.cmi";
 src += " ${lib}/vox_egraph_derivation_spec.cmi";
 src += " ${lib}/vox_egraph_match_spec.cmi";
 src += " ${lib}/vox_egraph_snapshot_spec.cmi";
 src += " ${lib}/vox_egraph_preservation_spec.cmi";
 src += " ${lib}/vox_egraph_saturation_spec.cmi";
 src += " ${lib}/vox_egraph_congruence_spec.cmi";
 src += " ${lib}/vox_egraph_fixedpoint_spec.cmi";
 src += " ${lib}/vox_egraph_interpret_wrapping.cmi";
 src += " ${lib}/vox_egraph_rule_handle.cmi";
 src += " ${lib}/vox_egraph_closure_spec.cmi";
 src += " ${lib}/vox_egraph_quantifier.cmi";
 dst = "${test_build_directory_prefix}/ocamlc.opt.public/";
 compiler_directory_suffix = ".public";
 all_modules = "egraph_rule_public.ml";
 readonly_files = "egraph_rule_public.ml egraph_boundary.ml";
 readonly_files += " emitted_code.ml egraph_boundary_check.ml";
 readonly_files += " egraph_inventory.ml";
 setup-ocamlc.opt-build-env;
 copy;
 compiler_output2 = "${lib}.public/client.output";
 ocamlc.opt;
 check-ocamlc.opt-output;
 compile_only = "false";
 flags = "";
 compiler_output2 = "${lib}.public/checker.output";
 all_modules = "emitted_code.ml egraph_boundary_check.ml";
 program = "${lib}.public/check.exe";
 ocamlc.opt;
 arguments = "${lib}.lambda";
 output = "${lib}.public/check.output";
 stdout = "${output}";
 stderr = "${output}";
 reference = "${test_source_directory}/egraph_boundary.checks.reference";
 run;
 check-program-output;
 all_modules = "egraph_inventory.ml";
 program = "${lib}.public/inventory.exe";
 ocamlc.opt;
 arguments = "${root}";
 output = "${lib}.public/inventory.output";
 stdout = "${output}";
 stderr = "${output}";
 reference = "${root}/verification/library/vox_egraph_rule_handle.spec.json";
 run;
 check-program-output;
 flags = "-extension refinement_types";
 run-expect;
 check-program-output;
*)

(* The e-graph demo's boundary. The library is compiled; five modules are
   compiled again with -dlambda, and egraph_boundary_check.ml checks that
   they call nothing in the proof modules. egraph_rule_public.ml is
   compiled with only the thirteen public semantic interfaces.
   egraph_inventory.ml prints the declaration inventory of the public
   semantic surface, which must equal
   verification/library/vox_egraph_rule_handle.spec.json. The phrases
   below are rejected against the public interfaces. *)

(* A positive control. *)
let (valid_empty @ total) () :
    {u : unit | Vox_egraph_rule_spec.valid Vox_egraph_rule_spec.No_rules} =
  ghost_ (Vox_egraph_rule_spec.valid_def Vox_egraph_rule_spec.No_rules);
  ();;
[%%expect{|
val valid_empty :
  unit ->
  {u : unit | Vox_egraph_rule_spec.valid Vox_egraph_rule_spec.No_rules} =
  <fun>
|}]

(* The graph handle is abstract. *)
let forge () : Vox_egraph_rule_handle.t = ();;
[%%expect{|
Line 1, characters 42-44:
1 | let forge () : Vox_egraph_rule_handle.t = ();;
                                              ^^
Error: The constructor "()" has type "unit"
       but an expression was expected of type "Vox_egraph_rule_handle.t"
|}]

(* A rule whose sides have different sorts is invalid. *)
let make () =
  let module R = Vox_egraph_rule_spec in
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Bool_lit true} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  ghost_ (R.valid_def rules; R.rule_valid_def rule;
    R.pat_sort_def [] rule.lhs; R.pat_sort_def [] rule.rhs);
  Vox_egraph_rule_handle.create rules;;
[%%expect{|
Line 7, characters 32-37:
7 |   Vox_egraph_rule_handle.create rules;;
                                    ^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_egraph_rule_handle.mli", line 12, characters 40-50:
  The refinement is stated here.
|}]

(* A saturation that stops at a limit does not establish a fixed point. *)
let claim (state : Vox_egraph_rule_handle.t @ unique) =
  let module G = Vox_egraph_rule_handle in
  let module F = Vox_egraph_fixedpoint_spec in
  let #{G.status; fuel = _; state} = G.saturate state 0 1 0 in
  ghost_ (
    let view = borrow_ state in
    let _ : {u : unit | F.fixed (G.model view) (G.rules view)} = () in ());
  state;;
[%%expect{|
Line 7, characters 65-67:
7 |     let _ : {u : unit | F.fixed (G.model view) (G.rules view)} = () in ());
                                                                     ^^
Error: Refinement could not be proved (counterexample)
Line 7, characters 24-61:
7 |     let _ : {u : unit | F.fixed (G.model view) (G.rules view)} = () in ());
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
