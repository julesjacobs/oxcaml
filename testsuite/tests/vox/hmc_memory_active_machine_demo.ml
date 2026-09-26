(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_frontend.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_semantics.ml hmc_monomorphic_typing.ml hmc_monomorphic_globals.ml hmc_source_safety.ml hmc_specialization.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml hmc_heap_state.ml hmc_heap_simple.ml hmc_heap_simple_proofs.ml hmc_heap_machine.ml hmc_heap_machine_proofs.ml hmc_heap_control.ml hmc_heap_allocating.ml hmc_heap_globals.ml hmc_heap_step.ml hmc_heap_invariant.ml hmc_heap_runs.ml hmc_heap_extent_math.ml hmc_heap_static.ml hmc_heap_initialize.ml hmc_heap_execute.ml hmc_heap_source.ml hmc_linear_bytes.ml hmc_linear_bounds.ml hmc_linear_preservation.ml hmc_memory_extent.ml hmc_memory_object.ml hmc_heap_image.ml hmc_heap_code_bounds.ml hmc_linear_create.ml hmc_heap_image_extend.ml hmc_memory_prefix.ml hmc_memory_header.ml hmc_memory_lookup.ml hmc_memory_operations.ml hmc_memory_invoke.ml hmc_memory_read_step.ml hmc_memory_suffix.ml hmc_memory_allocate.ml hmc_memory_machine.ml hmc_memory_runs.ml hmc_memory_cells.ml hmc_pointer_frame_codec.ml hmc_pointer_frame_shape.ml hmc_memory_block_lookup.ml hmc_memory_saved_frame.ml hmc_memory_stack.ml hmc_memory_stack_capacity.ml hmc_memory_call_shape.ml hmc_memory_stack_machine.ml hmc_memory_stack_runs.ml hmc_memory_stack_source.ml hmc_memory_current_shape.ml hmc_memory_active_machine.ml hmc_memory_active_runs.ml hmc_memory_active_source.ml hmc_heap_demand.ml hmc_heap_resource_step.ml hmc_heap_resources.ml hmc_memory_active_resources.ml hmc_memory_active_machine_demo.ml";
 { bytecode; }
 { native; }
*)

module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module M = Hmc_heap_objects
module K = Hmc_closure_ir
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module H = Hmc_heap_invariant
module Step = Hmc_heap_step
module P = Hmc_closure_program
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module A = Hmc_heap_initialize
module Index = Hmc_u32_index
module Image = Hmc_heap_image
module Code = Hmc_heap_code_bounds
module Bounds = Hmc_linear_bounds
module Wire = Hmc_heap_wire
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let var n = D.Bound (index n)
let word n = D.Word {W.lo = n; hi = 0}
let build : D.term @ immutable -> I.program @ immutable = fun source ->
  match Hmc_specialization.compile source with
  | Hmc_specialization.Compiled p -> I.build (C.build (P.build p))
  | _ -> failwith "heap image fixture rejected"
module Machine = Hmc_memory_active_machine
module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity
module Saved = Hmc_memory_saved_frame
let rec inspect_stack blocks width memory base top frames = match frames with
  | Q.Halt -> if Stack.pop blocks width memory base top <> Stack.Empty then failwith "nonempty concrete stack"
  | Q.Frame (a, rest) -> (match Stack.pop blocks width memory base top with
    | Stack.Popped (actual, previous) when actual = a -> inspect_stack blocks width memory base previous rest
    | _ -> failwith "concrete stack frame differs")
type outcome = Normal of int | Out_of_stack | Out_of_heap
let rec execute : (program : I.program) @ immutable -> (globals : X.globals) @ immutable -> (code_capacity : W.limb) ->
    (width : W.limb) -> (heap_limit : W.limb) -> (base : W.limb) -> (stack_limit : W.limb) ->
    (active : W.limb) -> (active_end : W.limb) -> (memory_limit : W.limb) ->
    (frame_limit : D.index) @ immutable -> (concrete : Machine.configuration) @ immutable ->
    (model : X.configuration) @ immutable -> (abstract : S.state) @ immutable -> int ->
    {u : unit | H.valid program globals heap_limit model abstract
      && Machine.related program.I.origin.C.blocks width base active concrete model
      && Index.fits (K.size program.I.origin.C.origin.P.table) code_capacity && Index.fits (Hmc_cfg_ir.size program.I.origin.C.blocks) code_capacity
      && width > 0 && Hmc_heap_extent.span (Saved.slots program.I.origin.C.blocks) (Stack.zero ()) width
      && Capacity.region width frame_limit base stack_limit && heap_limit <= base && base <= concrete.Machine.top && concrete.Machine.top <= stack_limit
      && Bounds.covers concrete.Machine.memory heap_limit && Bounds.covers concrete.Machine.memory stack_limit
      && Bounds.covers concrete.Machine.memory memory_limit && stack_limit <= active
      && Hmc_heap_extent.span (Saved.slots program.I.origin.C.blocks) active active_end && active_end <= memory_limit} -> outcome =
  fun program globals code_capacity width heap_limit base stack_limit active active_end memory_limit frame_limit concrete model abstract fuel premise ->
    ghost_ (Machine.related_def program.I.origin.C.blocks width base active concrete model);
    (match model.X.state with Q.Running (a, frames) -> if Saved.load program.I.origin.C.blocks concrete.Machine.memory active <> Some a then failwith "active frame differs"; inspect_stack program.I.origin.C.blocks width concrete.Machine.memory base concrete.Machine.top frames | _ -> ());
    match concrete.Machine.status with
    | Machine.Done (Hmc_tagged_cell.Word word) -> if word.W.hi = 0 then Normal word.W.lo else failwith "high word"
    | Machine.Done _ | Machine.Stuck -> failwith "stack machine terminal state"
    | Machine.Running -> if fuel = 0 then failwith "stack machine fuel" else (
      ghost_ (H.step program globals heap_limit frame_limit model abstract ());
      let actual = Machine.step program globals code_capacity width heap_limit base stack_limit active active_end memory_limit concrete model abstract frame_limit () in
      let expected = X.step program globals heap_limit frame_limit model in
      ghost_ (Machine.result_related_def program.I.origin.C.blocks width base active heap_limit stack_limit memory_limit concrete.Machine.memory actual expected);
      match actual, expected with
      | Machine.Exhausted reason, X.Exhausted expected ->
        if reason <> expected then failwith "stack machine exhaustion differs";
        (match reason with X.Stack -> Out_of_stack | X.Heap -> Out_of_heap)
      | Machine.Advanced next, X.Advanced next_model ->
        execute program globals code_capacity width heap_limit base stack_limit active active_end memory_limit frame_limit next next_model (U.step program abstract) (fuel - 1) ()
      | _ -> failwith "stack machine transition differs")
let check source input frame_limit (heap_limit : W.limb) expected =
  let program = build source in
  let code_capacity : W.limb = 4294967295 in
  let size = index 4096 in
  match Index.encode code_capacity (K.size program.I.origin.C.origin.P.table),
    Index.encode code_capacity (Hmc_cfg_ir.size program.I.origin.C.blocks), Index.encode code_capacity size with
  | Some _, Some _, Some memory_limit ->
    if 64 <= heap_limit && heap_limit <= memory_limit then (
      let memory = Hmc_linear_create.zeroed size memory_limit () in
      let base = heap_limit in
      match Hmc_heap_extent.reserve (Saved.slots program.I.origin.C.blocks) 0 memory_limit with
      | None -> failwith "frame width"
      | Some width -> if width > 0 then (
        match Capacity.reserve width frame_limit base memory_limit () with
        | None -> failwith "stack region"
        | Some stack_limit ->
          ghost_ (let _ = Bounds.suffix memory memory_limit heap_limit () in Bounds.covers_def memory heap_limit;
            let _ = Bounds.suffix memory memory_limit stack_limit () in Bounds.covers_def memory stack_limit;
            Stack.zero_def ());
          let input = {W.lo = input; hi = 0} in
          let initial = A.initialize program 64 heap_limit input () in
          ghost_ (A.correct_def program 64 heap_limit input initial);
          match initial with
          | A.Heap_exhausted _ -> failwith "stack fixture initialization"
          | A.Initialized start ->
            ghost_ (H.valid_def program start.A.globals heap_limit start.A.configuration (U.initial program input);
              Code.encodable program.I.origin.C.origin.P.table code_capacity start.A.configuration.X.heap ());
            let bytes = Image.materialize program.I.origin.C.origin.P.table code_capacity memory start.A.configuration.X.heap heap_limit () in
            ghost_ (Bounds.same_length memory bytes stack_limit ());
            match start.A.configuration.X.state with
            | Q.Running (a, Q.Halt) ->
              ghost_ (Bounds.same_length memory bytes memory_limit ();
                Hmc_memory_current_shape.current program start.A.globals heap_limit start.A.configuration (U.initial program input) ());
              (match Hmc_heap_extent.reserve (Saved.slots program.I.origin.C.blocks) stack_limit memory_limit with
              | None -> failwith "active frame extent"
              | Some active_end ->
                let active = stack_limit in
                let updated = Saved.store program.I.origin.C.blocks code_capacity bytes memory_limit active active_end a () in
                ghost_ (Capacity.ordered width frame_limit base stack_limit (); Image.preserve program.I.origin.C.origin.P.table bytes updated start.A.configuration.X.heap active ();
                  Bounds.same_length bytes updated heap_limit (); Bounds.same_length bytes updated stack_limit ();
                  Stack.related_def program.I.origin.C.blocks width updated base base Q.Halt);
                let concrete = {Machine.memory = updated; frontier = M.used start.A.configuration.X.heap; top = base; status = Machine.Running} in
                ghost_ (Machine.related_def program.I.origin.C.blocks width base active concrete start.A.configuration;
                  Capacity.ordered width frame_limit base stack_limit ());
              let fuel = index 5000 in
              let bounded = Hmc_memory_active_runs.run program start.A.globals code_capacity width heap_limit base stack_limit active active_end memory_limit fuel
                concrete start.A.configuration (U.initial program input) frame_limit () in
              let abstract = U.initial program input in
              let plan = Hmc_heap_demand.heap_plan program fuel abstract in
              (match Hmc_heap_extent.reserve plan concrete.Machine.frontier heap_limit with
              | None -> (match expected with Normal _ -> failwith "normal fixture heap plan" | _ -> ())
              | Some _ ->
                if Hmc_frame_capacity.le (Hmc_heap_demand.stack_plan program fuel abstract) frame_limit then (
                  ghost_ (Hmc_memory_active_resources.sufficient program start.A.globals width base active heap_limit stack_limit memory_limit
                    frame_limit fuel start.A.configuration abstract updated bounded ());
                  match U.advance program fuel abstract with
                  | S.Done (Hmc_closure_semantics.V.Word word) ->
                    ghost_ (Hmc_memory_active_resources.normal program start.A.globals width base active heap_limit stack_limit memory_limit
                      frame_limit fuel start.A.configuration abstract updated bounded word ())
                  | _ -> ())
                else (match expected with Normal _ -> failwith "normal fixture stack plan" | _ -> ()));
              ghost_ (Hmc_monomorphic.ready_def program.I.origin.C.origin.P.origin;
                Hmc_memory_active_source.safe program program.I.origin.C.origin.P.origin.Hmc_monomorphic.definitions start.A.globals width base heap_limit stack_limit active memory_limit
                  frame_limit input fuel start.A.configuration updated bounded ());
              (match bounded with
              | Hmc_memory_active_runs.Finished out -> (match out.Machine.status with
                | Machine.Done (Hmc_tagged_cell.Word word) ->
                  ghost_ (Hmc_memory_active_source.returned_def bounded word);
                  let source_steps = Hmc_memory_active_source.reflection program program.I.origin.C.origin.P.origin.Hmc_monomorphic.definitions
                    start.A.globals width base heap_limit stack_limit active memory_limit frame_limit input fuel start.A.configuration updated bounded word () in
                  if Hmc_source_semantics.advance source_steps (Hmc_monomorphic_simulation.source_start program.I.origin.C.origin.P.origin input)
                    <> Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) then failwith "original source reflection"
                | _ -> ())
              | _ -> ());
              let bounded_outcome = match bounded with
                | Hmc_memory_active_runs.Finished out -> (match out.Machine.status with
                  | Machine.Done (Hmc_tagged_cell.Word word) when word.W.hi = 0 -> Normal word.W.lo
                  | _ -> failwith "bounded stack execution result")
                | Hmc_memory_active_runs.Blocked (_, X.Stack, _) -> Out_of_stack
                | Hmc_memory_active_runs.Blocked (_, X.Heap, _) -> Out_of_heap in
              if bounded_outcome <> expected then failwith "bounded stack execution differs";
                let out = execute program start.A.globals code_capacity width heap_limit base stack_limit active active_end memory_limit frame_limit concrete start.A.configuration (U.initial program input) 5000 () in
              if out <> expected then failwith "active machine source result")
            | _ -> failwith "initial stack") else failwith "zero frame width") else failwith "heap limit"
  | _ -> failwith "code capacity"
let () =
  let list = D.Lambda (D.CaseList (D.Cons (var 0, D.Cons (word 2, D.Nil)), word 0, var 0)) in
  check list 9 D.Z 1024 (Normal 9);
  check list 9 D.Z 80 Out_of_heap;
  let closure_list = D.Lambda (D.CaseList (D.Cons (D.Lambda (D.Primitive (D.Add, var 1, var 0)), D.Nil), word 0, D.Apply (var 0, word 2))) in
  check closure_list 7 (index 4) 1024 (Normal 9);
  check closure_list 7 D.Z 1024 Out_of_stack;
  let factory = D.Lambda (D.Lambda (D.Primitive (D.Add, var 1, var 0))) in
  check (D.Let (factory, D.Lambda (D.Let (D.Apply (var 1, var 0),
    D.Let (D.Apply (var 2, word 2), D.Primitive (D.Add, D.Apply (var 1, word 5), D.Apply (var 0, word 7))))))) 10 (index 4) 1024 (Normal 24);
  check (D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
    D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1))))) 100 D.Z 1024 (Normal 17);
  print_endline "byte-resident machine: active frame, saved frames, heap, calls, returns, tail loops, and exhaustion passed"
