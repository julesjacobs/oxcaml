(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_reference_tree.ml hmc_expansion.ml hmc_specialization_coherence.ml hmc_manifest.ml hmc_monomorphic.ml hmc_monomorphic_typing.ml hmc_source_semantics.ml hmc_monomorphic_semantics.ml hmc_monomorphic_globals.ml hmc_monomorphic_links.ml hmc_catalog_semantics.ml hmc_monomorphic_values.ml hmc_monomorphic_states.ml hmc_monomorphic_step.ml hmc_monomorphic_simulation.ml hmc_source_safety.ml hmc_monomorphic_safety.ml hmc_closure_ir.ml hmc_closure_extension.ml hmc_closure_lower.ml hmc_closure_program.ml hmc_closure_semantics.ml hmc_closure_values.ml hmc_closure_states.ml hmc_closure_step.ml hmc_closure_simulation.ml hmc_cfg_ir.ml hmc_cfg_extension.ml hmc_cfg_origin.ml hmc_cfg_lower.ml hmc_cfg_program.ml hmc_cfg_semantics.ml hmc_cfg_execution.ml hmc_cfg_start.ml hmc_cfg_descent.ml hmc_cfg_states.ml hmc_cfg_height.ml hmc_cfg_evaluate.ml hmc_cfg_return.ml hmc_cfg_step.ml hmc_cfg_normalize.ml hmc_cfg_simulation.ml hmc_tail_sites.ml hmc_tail_ir.ml hmc_tail_semantics.ml hmc_tail_continuation.ml hmc_tail_execution.ml hmc_tail_step.ml hmc_tail_runs.ml hmc_tail_normalize.ml hmc_tail_simulation.ml hmc_tail_stack.ml hmc_frame_shape.ml hmc_frame_values.ml hmc_frame_edges.ml hmc_frame_step.ml hmc_frame_reachable.ml hmc_frame_codec.ml hmc_frame_capacity.ml hmc_frame_storage.ml hmc_frame_bound.ml hmc_slot_copy.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_heap_extent.ml hmc_heap_objects.ml hmc_heap_preservation.ml hmc_heap_allocate.ml hmc_heap_operations.ml hmc_heap_bounds.ml hmc_u32_index.ml hmc_heap_wire.ml hmc_heap_frame.ml hmc_heap_frame_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module M = Hmc_heap_objects
module C = Hmc_tagged_cell
module F = Hmc_heap_frame
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module Codec = Hmc_frame_codec
let () =
  let heap = M.Empty_heap 64 in
  let context = D.Binding (D.Forall (D.Z, D.Boolean), D.Empty_context) in
  let env = M.Cell (C.Boolean true, M.Empty) in
  let abstract_env = R.V.Bind (R.V.True, R.V.Empty) in
  let temporaries = F.Value (C.Nil, env, F.Environment (M.Empty, F.Empty)) in
  let abstract_temporaries = S.Value (R.V.Nil, abstract_env, S.Environment (R.V.Empty, S.Empty)) in
  let schema = G.Value (context, D.List_type D.Boolean, G.Environment (D.Empty_context, G.Empty_temporaries)) in
  let signature = {G.locals = context; temporaries = schema; accumulator = Some D.Boolean} in
  let a = {F.pc = D.Z; env; accumulator = C.Boolean false; temporaries; current = C.Nil} in
  let abstract = {S.pc = D.Z; env = abstract_env; accumulator = R.V.False;
    temporaries = abstract_temporaries; current = R.V.Nil} in
  let tail = M.Cell (C.Word {Hmc_word64.lo = 17; hi = 0}, M.Empty) in
  let values = Codec.Cell (R.V.Word {Hmc_word64.lo = 17; hi = 0}, Codec.Empty) in
  ghost_ (
    M.view_def heap; M.decode_environment_def M.No_values env; M.decode_environment_def M.No_values M.Empty;
    M.decode_value_def M.No_values (C.Boolean true); M.decode_value_def M.No_values (C.Boolean false);
    M.decode_value_def M.No_values C.Nil; M.decode_value_def M.No_values (C.Word {Hmc_word64.lo = 17; hi = 0});
    M.decode_def heap (C.Boolean true); M.decode_def heap (C.Boolean false); M.decode_def heap C.Nil;
    M.decode_def heap (C.Word {Hmc_word64.lo = 17; hi = 0});
    F.decode_temporaries_def heap F.Empty;
    F.decode_temporaries_def heap (F.Environment (M.Empty, F.Empty));
    F.decode_temporaries_def heap temporaries; F.decode_def heap a;
    Hmc_frame_shape.environment_def context abstract_env;
    Hmc_frame_shape.environment_def D.Empty_context R.V.Empty;
    Codec.temporaries_shape_def G.Empty_temporaries S.Empty;
    Codec.temporaries_shape_def (G.Environment (D.Empty_context, G.Empty_temporaries)) (S.Environment (R.V.Empty, S.Empty));
    Codec.temporaries_shape_def schema abstract_temporaries; Codec.shape_def signature abstract;
    F.decode_cells_def heap tail; F.decode_cells_def heap M.Empty);
  let encoded = F.encode heap signature a abstract tail values () in
  (match F.decode_cells heap encoded with
  | Some cells -> if Codec.decode signature D.Z cells <> Some (abstract, values) then failwith "heap frame roundtrip"
  | None -> failwith "heap frame dangling cell");
  let expected = M.Cell (C.Nil, M.Cell (C.Boolean false, M.Cell (C.Boolean true,
    M.Cell (C.Nil, M.Cell (C.Boolean true, tail))))) in
  if encoded <> expected then failwith "heap frame field order";
  if F.decode_cells heap (M.Cell (C.Cons_pointer 64, M.Empty)) <> None then failwith "dangling frame value";
  print_endline "pointer frame layout, nested temporaries, suffix preservation, and dangling rejection passed"
