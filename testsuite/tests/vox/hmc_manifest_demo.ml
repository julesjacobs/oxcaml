(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_semantics.ml hmc_monomorphic_typing.ml hmc_monomorphic_globals.ml hmc_manifest_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module M = Hmc_manifest
module C = Hmc_monomorphic
module T = Hmc_templates
module B = Hmc_specialized_body
module I = Hmc_instance
module A = Hmc_ground_arguments
module G = Hmc_ground_type
let rec index n = if n <= 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {Hmc_word64.lo = n; hi = 0}
let id = D.Lambda (var 0)
let build : D.term @ immutable -> C.program @ immutable = fun term ->
  match Hmc_frontend.prepare term with
  | Hmc_frontend.Prepared p -> C.build (M.build (Hmc_expansion.build (refine_ p)))
  | _ -> raise (Failure "manifest source rejected")
let rec count = function C.No_definitions -> 0 | C.Definition (_, rest) -> 1 + count rest
let rec check table = function
  | C.No_definitions -> ()
  | C.Definition (d, rest) ->
    if not (Hm_elaboration_check.check D.Z (T.context d.C.body.B.origin.I.earlier)
      (C.erase d.C.code) (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation)
    then failwith "manifest body typing";
    let rec references = function
      | M.Empty -> ()
      | M.Reference (_, id) -> (match M.lookup table id with None -> failwith "dangling id" | Some _ -> ())
      | M.Child a -> references a
      | M.Pair (a, b) -> references a; references b
      | M.Triple (a, b, c) -> references a; references b; references c in
    references (C.links d.C.code); check table rest
let rec checked_references : (p : C.program) @ immutable -> (refs : M.references) @ immutable ->
    {u : unit | M.bounded (M.size (C.manifest p.C.definitions)) refs} -> unit = fun p refs premise ->
  ghost_ (M.bounded_def (M.size (C.manifest p.C.definitions)) refs);
  match refs with
  | M.Empty -> ()
  | M.Reference (_, id) ->
    let d = Hmc_monomorphic_globals.fetch p (refine_ id) in
    ghost_ (C.ready_def p;
      M.lookup_closed (C.manifest p.C.definitions) id {M.body = d.C.body; dependencies = C.links d.C.code} ());
    checked_references p (C.links d.C.code) ()
  | M.Child a -> checked_references p a ()
  | M.Pair (a, b) -> checked_references p a (); checked_references p b ()
  | M.Triple (a, b, c) -> checked_references p a (); checked_references p b (); checked_references p c ()
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics
let differential : (p : C.program) @ immutable -> unit = fun p ->
  ghost_ (C.ready_def p);
  let defs : {d : C.definitions | C.origins d} = refine_ p.C.definitions in
  List.iter (fun input ->
    let argument = {Hmc_word64.lo = input; hi = 0} in
    let source = D.Apply (T.rebuild p.C.source.T.globals p.C.source.T.entry, D.Word argument) in
    let target = C.Apply (p.C.entry, C.Word argument) in
    match S.advance (index 4000) (S.initial source), Q.advance defs (index 4000) (Q.initial target) with
    | S.Done (Hm_interpreter_typing.Word a), Q.Done (Q.V.Word b) ->
      if not (Hmc_word64.equal a b) then failwith "source/manifest result differs"
    | _ -> failwith "source/manifest did not return a word") [0; 1; 9]
let checked : D.term @ immutable -> C.program @ immutable = fun source ->
  let p = build source in ghost_ (C.ready_def p; Hmc_monomorphic_typing.program_typed p);
  checked_references p (C.links p.C.entry) (); check (C.manifest p.C.definitions) p.C.definitions; differential p; p
let () =
  let p = checked (D.Lambda (var 0)) in
  if count p.C.definitions <> 0 then failwith "empty globals";
  let p = checked (D.Let (id, D.Lambda (D.Apply (var 1, var 0)))) in
  (match p.C.entry, p.C.definitions with
  | C.Lambda (C.Apply (C.Global (D.S D.Z, D.Z, D.Z), C.Local D.Z)),
      C.Definition ({C.code = C.Lambda (C.Local D.Z); _}, C.No_definitions) -> ()
  | _ -> failwith "direct specialization");
  let wrapper = D.Lambda (D.Apply (var 1, var 0)) in
  let p = checked (D.Let (id, D.Let (wrapper, D.Lambda (D.Apply (var 1, var 0))))) in
  (match p.C.entry, p.C.definitions with
  | C.Lambda (C.Apply (C.Global (_, _, D.S D.Z), _)),
      C.Definition ({C.code = C.Lambda (C.Apply (C.Global (_, _, D.Z), _)); _},
        C.Definition (_, C.No_definitions)) -> ()
  | _ -> failwith "dependency ids");
  let p = checked (D.Let (id, D.Lambda (D.If (D.Truth,
    D.Apply (var 1, var 0), D.Apply (var 1, var 0))))) in
  (match p.C.entry with
  | C.Lambda (C.If (_, C.Apply (C.Global (_, _, D.Z), _),
      C.Apply (C.Global (_, _, D.S D.Z), _))) -> ()
  | _ -> failwith "branch ids shifted");
  let recursive = D.Recursive (D.If (D.Primitive (D.Unsigned_less, var 0, word 1), var 0,
    D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1)))) in
  let p = checked (D.Let (recursive, D.Lambda (D.Apply (var 1, var 0)))) in
  if count p.C.definitions <> 1 then failwith "self expansion";
  (match p.C.definitions with
  | C.Definition ({C.code = C.Recursive (C.If (_, _, C.Apply (C.Local (D.S D.Z), _))); _}, _) -> ()
  | _ -> failwith "self reference rewritten");
  let map = D.Lambda (D.Recursive (D.CaseList (var 0, D.Nil,
    D.Cons (D.Apply (var 4, var 0), D.Apply (var 3, var 1))))) in
  let p = checked (D.Let (id, D.Let (map, D.Lambda (D.CaseList (
    D.Apply (D.Apply (var 1, var 2), D.Cons (var 0, D.Nil)), word 0, var 0))))) in
  if count p.C.definitions <> 2 then failwith "map and identity";
  let head_id = D.Lambda (D.CaseList (var 0, D.Nil,
    D.Cons (D.Apply (var 3, var 0), var 1))) in
  let p = checked (D.Let (id, D.Let (head_id, D.Lambda (D.CaseList (
    D.Apply (var 1, D.Cons (var 0, D.Nil)), word 0, var 0))))) in
  (match p.C.definitions with
  | C.Definition ({C.code = C.Lambda (C.CaseList (_, _,
      C.Cons (C.Apply (C.Global (D.S (D.S (D.S D.Z)), D.Z, D.Z), C.Local D.Z),
        C.Local (D.S D.Z)))); _}, _) -> ()
  | _ -> failwith "global under list binders");
  let _ = checked (D.Let (id, D.Lambda (D.If (D.Apply (var 1, D.Truth),
    D.Apply (var 1, var 0), word 0)))) in
  let capture = D.Lambda (D.Lambda (var 1)) in
  let _ = checked (D.Let (capture, D.Lambda (D.Apply (D.Apply (var 1, var 0), D.Truth)))) in
  let _ = checked (D.Lambda (D.Let (var 0, D.Primitive (D.Add, var 0, var 1)))) in
  print_endline "manifest, reference rewriting, and source differential execution passed"
