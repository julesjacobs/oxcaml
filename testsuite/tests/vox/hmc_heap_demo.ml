(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_semantics.ml hmc_monomorphic_typing.ml hmc_monomorphic_globals.ml hmc_source_safety.ml hmc_specialization.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module W = Hmc_word64
module C = Hmc_tagged_cell
module R = Hmc_closure_semantics
module K = Hmc_closure_ir
module H = Hmc_frame_shape
module E = Hmc_heap_extent
module M = Hmc_heap_objects
module P = Hmc_heap_preservation
module A = Hmc_heap_allocate
module O = Hmc_heap_operations
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {W.lo = n; hi = 0}
let expect : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (limit : W.limb) ->
    (object_ : M.object_) @ immutable -> (out : {r : A.result | A.correct table heap limit object_ r}) @ immutable ->
    {a : A.allocation | A.correct table heap limit object_ (A.Allocated a)} @ immutable = fun table heap limit object_ out ->
  match out with A.Allocated a -> a | A.Exhausted -> failwith "unexpected heap exhaustion"
let (capture @ total) : (heap : M.heap) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | M.decode_environment (M.view heap) (M.Cell (C.Word word, M.Empty))
      === Some (R.V.Bind (R.V.Word word, R.V.Empty))} @ ghost = fun heap word -> ghost_ (
  M.decode_environment_def (M.view heap) (M.Cell (C.Word word, M.Empty));
  M.decode_environment_def (M.view heap) M.Empty; M.decode_value_def (M.view heap) (C.Word word))
let exercise : K.table @ immutable -> D.index @ immutable -> unit = fun table code ->
  match K.lookup table code with
  | Some entry when entry.K.recursive -> (match entry.K.captured with
    | D.Binding (_, D.Empty_context) ->
      let first = {W.lo = 11; hi = 0} and second = {W.lo = 29; hi = 0} in
      let initial = M.Empty_heap 64 in
      let left_captures = M.Cell (C.Word first, M.Empty) in
      let left_object = M.Closure (code, left_captures) in
      ghost_ (M.valid_def table initial; M.used_def initial; capture initial first;
        H.environment_def entry.K.captured (R.V.Bind (R.V.Word first, R.V.Empty));
        H.environment_def D.Empty_context R.V.Empty);
      let left = expect table initial 1024 left_object (O.closure table initial 1024 code left_captures ()) in
      ghost_ (A.correct_def table initial 1024 left_object (A.Allocated left);
        M.decode_object_def (M.view initial) left_object);
      let right_captures = M.Cell (C.Word second, M.Empty) in
      let right_object = M.Closure (code, right_captures) in
      ghost_ (capture left.A.heap second;
        H.environment_def entry.K.captured (R.V.Bind (R.V.Word second, R.V.Empty)));
      let right = expect table left.A.heap 1024 right_object (O.closure table left.A.heap 1024 code right_captures ()) in
      ghost_ (A.correct_def table left.A.heap 1024 right_object (A.Allocated right);
        M.decode_object_def (M.view left.A.heap) right_object;
        P.decode_preserve table right.A.heap left.A.heap left.A.reference (R.V.Closure (code, R.V.Bind (R.V.Word first, R.V.Empty))) ());
      let object_ = M.Cons (left.A.reference, right.A.reference) in
      ghost_ (O.live_def right.A.heap left.A.reference; O.live_def right.A.heap right.A.reference);
      let pair = expect table right.A.heap 1024 object_ (O.cons table right.A.heap 1024 left.A.reference right.A.reference ()) in
      ghost_ (A.correct_def table right.A.heap 1024 object_ (A.Allocated pair);
        M.decode_object_def (M.view right.A.heap) object_;
        M.decode_def right.A.heap left.A.reference; M.decode_def right.A.heap right.A.reference);
      let left_value = R.V.Closure (code, R.V.Bind (R.V.Word first, R.V.Empty)) in
      let right_value = R.V.Closure (code, R.V.Bind (R.V.Word second, R.V.Empty)) in
      let decoded = O.read_cons table pair.A.heap pair.A.reference left_value right_value () in
      if decoded.O.head <> left.A.reference || decoded.O.tail <> right.A.reference then failwith "heap cons fields";
      let argument = {W.lo = 5; hi = 0} in
      ghost_ (P.decode_preserve table pair.A.heap right.A.heap left.A.reference left_value ();
        P.decode_preserve table pair.A.heap right.A.heap right.A.reference right_value ();
        M.decode_def pair.A.heap (C.Word argument); M.decode_value_def (M.view pair.A.heap) (C.Word argument));
      let entered_left = O.invoke table pair.A.heap left.A.reference (C.Word argument) code
        (R.V.Bind (R.V.Word first, R.V.Empty)) (R.V.Word argument) () in
      let entered_right = O.invoke table pair.A.heap right.A.reference (C.Word argument) code
        (R.V.Bind (R.V.Word second, R.V.Empty)) (R.V.Word argument) () in
      let expected_left = R.V.Bind (R.V.Word argument, R.V.Bind (left_value, R.V.Bind (R.V.Word first, R.V.Empty))) in
      let expected_right = R.V.Bind (R.V.Word argument, R.V.Bind (right_value, R.V.Bind (R.V.Word second, R.V.Empty))) in
      if M.decode_environment (M.view pair.A.heap) entered_left.O.environment <> Some expected_left
        || M.decode_environment (M.view pair.A.heap) entered_right.O.environment <> Some expected_right then failwith "closure captures or self binding";
      if left.A.reference <> C.Closure_pointer 64 || right.A.reference <> C.Closure_pointer 96
        || pair.A.reference <> C.Cons_pointer 128 || M.used pair.A.heap <> 160 then failwith "heap byte addresses";
      if M.decode pair.A.heap (C.Cons_pointer 64) <> None || M.decode pair.A.heap (C.Closure_pointer 128) <> None
        || M.decode pair.A.heap (C.Closure_pointer 80) <> None || M.decode pair.A.heap (C.Cons_pointer 160) <> None then failwith "invalid heap pointer";
      ghost_ (O.live_def pair.A.heap left.A.reference; O.live_def pair.A.heap right.A.reference);
      let too_small = O.cons table pair.A.heap (M.used pair.A.heap + 31) left.A.reference right.A.reference () in
      (match too_small with A.Exhausted -> () | _ -> failwith "missing heap exhaustion");
      let exact = O.cons table pair.A.heap (M.used pair.A.heap + 32) left.A.reference right.A.reference () in
      (match exact with A.Allocated a when M.used a.A.heap = M.used pair.A.heap + 32 -> () | _ -> failwith "exact heap boundary")
    | _ -> failwith "expected one captured value")
  | _ -> failwith "expected recursive closure entry"
let () =
  let source = D.Lambda (D.Apply (D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), var 2,
    D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1)))), word 9)) in
  (match Hmc_specialization.compile source with
  | Hmc_specialization.Compiled p ->
    let p = Hmc_closure_program.build p in exercise p.Hmc_closure_program.table D.Z
  | _ -> failwith "heap frontend fixture rejected");
  if E.reserve (index 2) 64 95 <> None || E.reserve (index 2) 64 96 <> Some 96 then failwith "cons capacity boundary";
  if E.reserve (index 1) 4294967279 4294967295 <> Some 4294967295 then failwith "u32 exact boundary";
  if E.reserve (index 1) 4294967280 4294967295 <> None then failwith "u32 overflow";
  if E.reserve (index 2) 4294967264 4294967295 <> None then failwith "two-cell overflow";
  if E.reserve D.Z 17 16 <> None || E.reserve D.Z 16 16 <> Some 16 then failwith "empty extent boundary";
  print_endline "checked heap allocation, stable references, closure captures, and exact bounds passed"

module Wire = Hmc_heap_wire
module Bytes = Wasm_u32
module Index = Hmc_u32_index
let rec drop_last = function
  | Bytes.End -> Bytes.End
  | Bytes.Byte (_, Bytes.End) -> Bytes.End
  | Bytes.Byte (b, rest) -> Bytes.Byte (b, drop_last rest)
let () =
  let suffix = Bytes.Byte (173, Bytes.Byte (42, Bytes.End)) in
  let captures = M.Cell (C.Boolean true, M.Cell (C.Cons_pointer 128, M.Empty)) in
  let objects = [Wire.Cons (C.Word {W.lo = 4294967295; hi = 4294967295}, C.Nil);
    Wire.Closure (4294967295, captures); Wire.Closure (0, M.Empty)] in
  List.iter (fun object_ ->
    let bytes = Wire.encode object_ suffix in
    if Wire.decode (Wire.schema object_) bytes <> Some (object_, suffix) then failwith "object wire suffix";
    let bytes = Wire.encode object_ Bytes.End in
    if Wire.decode (Wire.schema object_) (drop_last bytes) <> None then failwith "truncated heap object") objects;
  let schema = Wire.Closure_schema D.Z in
  if Wire.decode schema (C.encode (C.Boolean true) Bytes.End) <> None then failwith "closure header tag";
  if Wire.decode schema (C.encode (C.Word {W.lo = 0; hi = 1}) Bytes.End) <> None then failwith "closure code overflow";
  if Index.encode 3 (index 3) <> Some 3 || Index.encode 3 (index 4) <> None
    || Index.encode 0 D.Z <> Some 0 || Index.encode 0 (index 1) <> None then failwith "bounded code index";
  if Wire.lower 3 (M.Closure (index 3, captures)) <> Some (Wire.Closure (3, captures))
    || Wire.lower 3 (M.Closure (index 4, captures)) <> None then failwith "object code lowering";
  let start : W.limb = 64 and stop : W.limb = 96 in
  ghost_ (E.span_def (D.S (D.S D.Z)) start stop;
    E.span_def (D.S D.Z) 80 stop; E.span_def D.Z stop stop;
    D.present_def (D.S (D.S D.Z)) D.Z;
    D.present_def (D.S (D.S D.Z)) (D.S D.Z); D.present_def (D.S D.Z) D.Z);
  if Hmc_heap_bounds.slot (D.S (D.S D.Z)) D.Z start stop () <> 64
    || Hmc_heap_bounds.slot (D.S (D.S D.Z)) (D.S D.Z) start stop () <> 80 then failwith "object slot addresses";
  print_endline "heap object bytes, schemas, bounded code indices, and slot addresses passed"
