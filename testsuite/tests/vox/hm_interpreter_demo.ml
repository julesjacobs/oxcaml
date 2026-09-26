(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_primitive_constraints.ml hm_conditional_constraints.ml hm_list_case_constraints.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_annotation_trace.ml hm_annotation_trace_spec.ml hm_routed_infer.mli hm_routed_infer.ml hm_readback_runtime.ml hm_annotation_owned.ml hm_annotation_snapshot.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_annotation_shape.ml hm_annotation_equations.ml hm_elaboration_projection.ml hm_generalization_proofs.ml hm_generalization_instances.ml hm_template_generalization.ml hm_elaboration_instance_scope.ml hm_elaboration_freshness.ml hm_elaboration_continuation.ml hm_elaboration_binding.ml hm_reconstruction_instances.ml hm_elaboration_preparation.ml hm_reconstruction_environment.ml hm_reconstruction_variable.ml hm_reconstruction_run.ml hm_typed_elaboration.ml verified_hm.mli verified_hm.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hm_interpreter_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module I = Hm_interpreter
module V = Verified_hm

let evaluate (term : D.term @ immutable) =
  if D.scoped_term D.Z term then
    let inferred = V.infer term in
    let checked = V.elaborate term (borrow_ inferred) in
    (match checked with
     | None -> failwith "missing checked elaboration"
     | Some checked ->
       let derivation = Hm_checked_elaboration.derivation checked in
       if not (Hm_elaboration_check.check D.Z D.Empty_context term
         (Hm_checked_elaboration.root checked) derivation)
       then failwith "invalid checked elaboration");
    let runtime_type = V.read_type (borrow_ inferred) in
    (match runtime_type with
     | None -> failwith "missing runtime type"
     | Some _ -> ());
    match inferred.#root with
    | None -> raise (Failure "unexpected type rejection")
    | Some _ ->
      let typing = ghost_ (match inferred.#inferred_type with
        | None -> unreachable_ ()
        | Some ty ->
          #{I.ty = D.embed ty;
            derivation = V.Spec.typing term ty inferred.#evidence ()}) in
      let out = I.run term #{I.ty = ghost_ typing.#ty;
        derivation = ghost_ typing.#derivation} in
      out.#value
  else raise (Failure "unexpected scope error")

let expect_true (term : D.term @ immutable) =
  let out = evaluate term in
  match out with I.Spec.True -> () | _ -> failwith "expected true"

let expect_closure (term : D.term @ immutable) =
  let out = evaluate term in
  match out with
  | I.Spec.Closure _ | I.Spec.Recursive_closure _ -> ()
  | _ -> failwith "expected closure"

let () =
  let var = D.Bound D.Z in
  let outer = D.Bound (D.S D.Z) in
  let id = D.Lambda var in
  expect_true D.Truth;
  expect_true (D.Apply (id, D.Truth));
  expect_true (D.Let (id, D.Apply (D.Apply (var, var), D.Truth)));
  expect_true (D.Let (D.Truth, D.Apply (D.Lambda outer, id)));
  expect_true (D.Apply (D.Recursive var, D.Truth));
  expect_true (D.Let (D.Truth,
    D.Apply (D.Recursive (D.Bound (D.S (D.S D.Z))), id)));
  expect_closure (D.Let (id, D.Let (var, D.Apply (var, var))));
  expect_closure (D.Recursive var);
  expect_closure (D.Recursive (D.Apply (outer, var)))

let expect_word expected value =
  match value with
  | I.Spec.Word actual when Hmc_word64.equal actual expected -> ()
  | _ -> failwith "expected word"

let checked_eval term ty derivation =
  match Hm_checked_elaboration.check term ty derivation with
  | None -> failwith "invalid interpreter fixture"
  | Some checked ->
    let d = Hm_checked_elaboration.derivation checked in
    let out = I.run term #{I.ty = ghost_ ty; derivation = ghost_ d} in
    out.#value

let () =
  let zero = {Hmc_word64.lo = 0; hi = 0} in
  let one = {Hmc_word64.lo = 1; hi = 0} in
  let maximum = {Hmc_word64.lo = 4294967295; hi = 4294967295} in
  let id = D.Lambda (D.Bound D.Z) in
  (match evaluate D.False with I.Spec.False -> () | _ -> failwith "expected false");
  expect_word maximum (evaluate (D.Word maximum));
  expect_word one (evaluate (D.Apply (id, D.Word one)));
  expect_word one (evaluate (D.Let (id,
    D.Let (D.Apply (D.Bound D.Z, D.False),
      D.Apply (D.Bound (D.S D.Z), D.Word one)))));
  let wrong = D.Apply (D.Word one, D.False) in
  if D.scoped_term D.Z wrong then (
    let rejected = V.infer wrong in
    match rejected.#root with None -> () | Some _ -> failwith "word used as function");
  let literal = D.Word_constant in
  let primitive = D.Word_primitive (literal, literal) in
  expect_word zero (checked_eval (D.Primitive (D.Add, D.Word maximum, D.Word one)) D.Word64 primitive);
  expect_word maximum (checked_eval (D.Primitive (D.Subtract, D.Word zero, D.Word one)) D.Word64 primitive);
  (match checked_eval (D.Primitive (D.Unsigned_less, D.Word maximum, D.Word one))
    D.Boolean primitive with I.Spec.False -> () | _ -> failwith "unsigned comparison");
  (match checked_eval (D.Primitive (D.Equal_word, D.Word maximum, D.Word maximum))
    D.Boolean primitive with I.Spec.True -> () | _ -> failwith "word equality");
  let list_ty = D.List_type D.Word64 in
  let nil_d = D.Empty_list D.Word64 in
  let list = D.Cons (D.Word one, D.Cons (D.Word one, D.Nil)) in
  let list_d = D.List_cons (D.Word64, literal, D.List_cons (D.Word64, literal, nil_d)) in
  let variable = D.Variable D.No_arguments in
  expect_word one (checked_eval (D.CaseList (list, D.Word zero, D.Bound D.Z))
    D.Word64 (D.List_case (D.Word64, list_d, literal, variable)));
  (match checked_eval (D.CaseList (list, D.Nil, D.Bound (D.S D.Z)))
    list_ty (D.List_case (D.Word64, list_d, nil_d, variable)) with
  | I.Spec.Cons (I.Spec.Word w, I.Spec.Nil) when Hmc_word64.equal w one -> ()
  | _ -> failwith "list tail binding");
  let sum_body = D.CaseList (D.Bound D.Z, D.Word zero,
    D.Primitive (D.Add, D.Bound D.Z,
      D.Apply (D.Bound (D.S (D.S (D.S D.Z))), D.Bound (D.S D.Z)))) in
  let sum_d = D.Recursion (list_ty, D.Word64,
    D.List_case (D.Word64, variable, literal,
      D.Word_primitive (variable, D.Application (list_ty, variable, variable)))) in
  expect_word {Hmc_word64.lo = 2; hi = 0}
    (checked_eval (D.Apply (D.Recursive sum_body, list)) D.Word64
      (D.Application (list_ty, sum_d, list_d)));
  let diverge = D.Apply (D.Recursive (D.Apply (D.Bound (D.S D.Z), D.Bound D.Z)), D.Word zero) in
  let diverge_d = D.Application (D.Word64,
    D.Recursion (D.Word64, D.Word64, D.Application (D.Word64, variable, variable)), literal) in
  expect_word one (checked_eval (D.If (D.Truth, D.Word one, diverge)) D.Word64
    (D.Conditional (D.Constant, literal, diverge_d)));
  expect_word one (checked_eval (D.If (D.False, diverge, D.Word one)) D.Word64
    (D.Conditional (D.Constant, diverge_d, literal)));
  expect_word one (checked_eval (D.CaseList (D.Nil, D.Word one, diverge)) D.Word64
    (D.List_case (D.Word64, nil_d, literal, diverge_d)));
  let polymorphic_nil = D.Forall (D.S D.Z, D.List_type (D.Parameter D.Z)) in
  let instantiate_word = D.Variable (D.Argument (D.Word64, D.No_arguments)) in
  let polymorphic_list = D.Let (D.Nil, D.Cons (D.Word one, D.Bound D.Z)) in
  (match checked_eval polymorphic_list list_ty
    (D.Let_binding (polymorphic_nil, D.Empty_list (D.Parameter D.Z),
      D.List_cons (D.Word64, literal, instantiate_word))) with
  | I.Spec.Cons (I.Spec.Word w, I.Spec.Nil) when Hmc_word64.equal w one -> ()
  | _ -> failwith "polymorphic nil instantiation")

let nil_principal (element : Copy_spec.ty @ immutable) =
  let term = D.Nil in
  ghost_ (D.scoped_term_def D.Z term);
  let inferred = V.infer term in
  let target = Copy_spec.List_type element in
  let d = D.Empty_list (D.embed element) in
  ghost_ (D.embed_def target; Hm_type_proofs.embed_wf D.Z element;
    Hm_type_proofs.embed_wf D.Z target;
    D.context_wf_def D.Z D.Empty_context;
    D.typed_def D.Z D.Empty_context term (D.embed target) d;
    let use : ((delta : (Copy_spec.node Pref.t @ immutable total ->
        Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match inferred.#inferred_type with
        | None -> false | Some ty -> target === Level_mgu_spec.substitute delta ty} ->
      {u : unit | not (inferred.#inferred_type === None)}) @ total =
        fun _delta factor -> () in
    let claim = not (inferred.#inferred_type === None) in
    V.principal term (borrow_ inferred) target d () claim (refine_ use));
  (match V.read_type (borrow_ inferred) with
  | Some (Copy_spec.List_type (Copy_spec.Variable _)) -> ()
  | _ -> failwith "nil must infer a fresh element variable");
  (match V.elaborate term (borrow_ inferred) with
  | Some checked -> (match Hm_checked_elaboration.derivation checked with
    | D.Empty_list (D.Free _) -> () | _ -> failwith "nil elaboration")
  | None -> failwith "nil rejected")

let () =
  nil_principal Copy_spec.Boolean;
  nil_principal Copy_spec.Word64;
  nil_principal (Copy_spec.List_type (Copy_spec.Function (Copy_spec.Word64, Copy_spec.Boolean)));
  (match evaluate D.Nil with I.Spec.Nil -> () | _ -> failwith "nil evaluation");
  (match evaluate (D.Apply (D.Lambda (D.Bound D.Z), D.Nil)) with
  | I.Spec.Nil -> () | _ -> failwith "identity at list type");
  let term = D.Let (D.Nil, D.Bound D.Z) in
  if D.scoped_term D.Z term then (
    let inferred = V.infer term in
    match V.elaborate term (borrow_ inferred) with
    | Some checked -> (match Hm_checked_elaboration.derivation checked with
      | D.Let_binding (D.Forall (D.S D.Z, D.List_type (D.Parameter D.Z)),
          D.Empty_list (D.Parameter D.Z), D.Variable (D.Argument (_, D.No_arguments))) -> ()
      | _ -> failwith "nil element was not generalized and instantiated")
    | None -> failwith "let-bound nil rejected")

let expect_rejected (term : D.term @ immutable) =
  if D.scoped_term D.Z term then (
  let inferred = V.infer term in
  (match inferred.#root with None -> () | Some _ -> failwith "expected inference rejection");
  (match V.elaborate term (borrow_ inferred) with None -> () | Some _ -> failwith "rejected term elaborated"))
  else failwith "unscoped rejection fixture"

let () =
  let one = {Hmc_word64.lo = 1; hi = 0} in
  let words = D.Cons (D.Word one, D.Cons (D.Word one, D.Nil)) in
  (match evaluate words with
  | I.Spec.Cons (I.Spec.Word a, I.Spec.Cons (I.Spec.Word b, I.Spec.Nil))
      when Hmc_word64.equal a one && Hmc_word64.equal b one -> ()
  | _ -> failwith "inferred word list");
  (match evaluate (D.Cons (D.False, D.Cons (D.Truth, D.Nil))) with
  | I.Spec.Cons (I.Spec.False, I.Spec.Cons (I.Spec.True, I.Spec.Nil)) -> ()
  | _ -> failwith "inferred boolean list");
  let singleton = D.Lambda (D.Cons (D.Bound D.Z, D.Nil)) in
  let polymorphic = D.Let (singleton,
    D.Let (D.Apply (D.Bound D.Z, D.Truth),
      D.Apply (D.Bound (D.S D.Z), D.Word one))) in
  (match evaluate polymorphic with
  | I.Spec.Cons (I.Spec.Word w, I.Spec.Nil) when Hmc_word64.equal w one -> ()
  | _ -> failwith "polymorphic singleton");
  let from_nil = D.Let (D.Nil, D.Let (D.Cons (D.Truth, D.Bound D.Z),
    D.Cons (D.Word one, D.Bound (D.S D.Z)))) in
  (match evaluate from_nil with
  | I.Spec.Cons (I.Spec.Word w, I.Spec.Nil) when Hmc_word64.equal w one -> ()
  | _ -> failwith "polymorphic nil at distinct element types");
  let id = D.Lambda (D.Bound D.Z) in
  (match evaluate (D.Cons (id, D.Cons (id, D.Nil))) with
  | I.Spec.Cons (I.Spec.Closure _, I.Spec.Cons (I.Spec.Closure _, I.Spec.Nil)) -> ()
  | _ -> failwith "list of functions");
  (match evaluate (D.Cons (words, D.Cons (D.Nil, D.Nil))) with
  | I.Spec.Cons (I.Spec.Cons _, I.Spec.Cons (I.Spec.Nil, I.Spec.Nil)) -> ()
  | _ -> failwith "nested list");
  let alias = D.Let (words, D.Let (D.Bound D.Z, D.Bound D.Z)) in
  (match evaluate alias with I.Spec.Cons _ -> () | _ -> failwith "list aliases");
  expect_rejected (D.Cons (D.Truth, D.Cons (D.Word one, D.Nil)));
  expect_rejected (D.Cons (D.Word one, D.Truth));
  expect_rejected (D.Cons (D.Apply (D.Truth, D.Truth), D.Nil));
  expect_rejected (D.Cons (D.Word one, D.Apply (D.Truth, D.Truth)));
  expect_rejected (D.Lambda (D.Cons (D.Bound D.Z, D.Bound D.Z)))

let () =
  let zero = {Hmc_word64.lo = 0; hi = 0} in
  let one = {Hmc_word64.lo = 1; hi = 0} in
  let maximum = {Hmc_word64.lo = 4294967295; hi = 4294967295} in
  let word n = D.Word n in
  expect_word zero (evaluate (D.Primitive (D.Add, word maximum, word one)));
  expect_word maximum (evaluate (D.Primitive (D.Subtract, word zero, word one)));
  expect_true (D.Primitive (D.Equal_word, word maximum, word maximum));
  expect_true (D.Primitive (D.Unsigned_less, word zero, word maximum));
  (match evaluate (D.Primitive (D.Unsigned_less, word maximum, word zero)) with
  | I.Spec.False -> () | _ -> failwith "inferred unsigned comparison");
  expect_word one (evaluate (D.Primitive (D.Add,
    D.Primitive (D.Subtract, word zero, word maximum), word zero)));
  let increment = D.Lambda (D.Primitive (D.Add, D.Bound D.Z, word one)) in
  expect_word one (evaluate (D.Apply (increment, word zero)));
  let under_let = D.Let (word one, D.Primitive (D.Subtract, D.Bound D.Z, word one)) in
  expect_word zero (evaluate under_let);
  let nested = D.Cons (D.Primitive (D.Equal_word, word one, word one), D.Cons (D.False, D.Nil)) in
  (match evaluate nested with
  | I.Spec.Cons (I.Spec.True, I.Spec.Cons (I.Spec.False, I.Spec.Nil)) -> ()
  | _ -> failwith "comparison in list");
  expect_rejected (D.Primitive (D.Add, D.Truth, word one));
  expect_rejected (D.Primitive (D.Subtract, word one, D.False));
  expect_rejected (D.Primitive (D.Equal_word, D.Nil, D.Nil));
  expect_rejected (D.Primitive (D.Unsigned_less, increment, word one));
  expect_rejected (D.Primitive (D.Add,
    D.Primitive (D.Unsigned_less, word zero, word one), word one));
  expect_rejected (D.Apply (increment, D.Truth))

let () =
  let zero = D.Word {Hmc_word64.lo = 0; hi = 0} in
  let one = {Hmc_word64.lo = 1; hi = 0} in
  expect_true (D.If (D.Truth, D.Truth, D.False));
  expect_true (D.If (D.False, D.False, D.Truth));
  expect_word one (evaluate (D.If (D.False, zero, D.Word one)));
  let divergent = D.Apply (D.Recursive (D.Apply (D.Bound (D.S D.Z), D.Bound D.Z)), D.Truth) in
  expect_true (D.If (D.Truth, D.Truth, divergent));
  expect_true (D.If (D.False, divergent, D.Truth));
  let choose = D.Lambda (D.Lambda (D.Lambda
    (D.If (D.Bound (D.S (D.S D.Z)), D.Bound (D.S D.Z), D.Bound D.Z)))) in
  let use value = D.Apply (D.Apply (D.Apply (D.Bound D.Z, D.False), value), value) in
  let polymorphic = D.Let (choose, D.Let (use D.Truth,
    D.Apply (D.Apply (D.Apply (D.Bound (D.S D.Z), D.Truth), D.Word one), zero))) in
  expect_word one (evaluate polymorphic);
  (match evaluate (D.If (D.Truth, D.Nil, D.Cons (D.Word one, D.Nil))) with
  | I.Spec.Nil -> () | _ -> failwith "conditional list branches");
  expect_true (D.Apply (D.If (D.False, D.Lambda D.False, D.Lambda (D.Bound D.Z)), D.Truth));
  expect_true (D.If (D.Primitive (D.Equal_word, zero, zero), D.If (D.False, D.False, D.Truth), D.False));
  expect_rejected (D.If (zero, D.Truth, D.Truth));
  expect_rejected (D.If (D.Nil, D.Truth, D.Truth));
  expect_rejected (D.If (D.Truth, D.Truth, zero));
  expect_rejected (D.If (D.False, D.Apply (D.Truth, D.Truth), D.Truth));
  expect_rejected (D.If (D.Truth, D.Truth, D.Apply (D.Truth, D.Truth)));
  expect_rejected (D.Lambda (D.If (D.Truth, D.Bound D.Z, D.Cons (D.Bound D.Z, D.Nil))))

let () =
  let zero = {Hmc_word64.lo = 0; hi = 0} in
  let one = {Hmc_word64.lo = 1; hi = 0} in
  let two = {Hmc_word64.lo = 2; hi = 0} in
  let words = D.Cons (D.Word one, D.Cons (D.Word two, D.Nil)) in
  let index n = let rec loop n = if n = 0 then D.Z else D.S (loop (n - 1)) in D.Bound (loop n) in
  expect_word zero (evaluate (D.CaseList (D.Nil, D.Word zero, index 0)));
  expect_word one (evaluate (D.CaseList (words, D.Word zero, index 0)));
  (match evaluate (D.CaseList (words, D.Nil, index 1)) with
  | I.Spec.Cons (I.Spec.Word w, I.Spec.Nil) when Hmc_word64.equal w two -> ()
  | _ -> failwith "inferred list-case tail binding");
  let divergent = D.Apply (D.Recursive (D.Apply (index 1, index 0)), D.Truth) in
  expect_true (D.CaseList (D.Nil, D.Truth, divergent));
  expect_true (D.CaseList (D.Cons (D.Truth, D.Nil), divergent, index 0));
  let head_or = D.Lambda (D.Lambda (D.CaseList (index 0, index 1, index 0))) in
  let polymorphic = D.Let (head_or,
    D.Let (D.Apply (D.Apply (index 0, D.False), D.Cons (D.Truth, D.Nil)),
      D.Apply (D.Apply (index 1, D.Word zero), words))) in
  expect_word one (evaluate polymorphic);
  let map = D.Lambda (D.Recursive (D.CaseList (index 0, D.Nil,
    D.Cons (D.Apply (index 4, index 0), D.Apply (index 3, index 1))))) in
  let increment = D.Lambda (D.Primitive (D.Add, index 0, D.Word one)) in
  (match evaluate (D.Apply (D.Apply (map, increment), words)) with
  | I.Spec.Cons (I.Spec.Word a, I.Spec.Cons (I.Spec.Word b, I.Spec.Nil))
      when Hmc_word64.equal a two && Hmc_word64.equal b {Hmc_word64.lo = 3; hi = 0} -> ()
  | _ -> failwith "inferred recursive map with captured function");
  let map_twice = D.Let (map,
    D.Let (D.Apply (D.Apply (index 0, increment), words),
      D.Apply (D.Apply (index 1, D.Lambda (D.If (index 0, D.Word one, D.Word zero))),
        D.Cons (D.Truth, D.Cons (D.False, D.Nil))))) in
  (match evaluate map_twice with
  | I.Spec.Cons (I.Spec.Word a, I.Spec.Cons (I.Spec.Word b, I.Spec.Nil))
      when Hmc_word64.equal a one && Hmc_word64.equal b zero -> ()
  | _ -> failwith "map at independent input and result instances");
  let sum = D.Recursive (D.CaseList (index 0, D.Word zero,
    D.Primitive (D.Add, index 0, D.Apply (index 3, index 1)))) in
  expect_word {Hmc_word64.lo = 3; hi = 0} (evaluate (D.Apply (sum, words)));
  expect_word one (evaluate (D.CaseList (D.Cons (words, D.Nil), D.Word zero,
    D.CaseList (index 0, D.Word zero, index 0))));
  expect_true (D.Apply (D.CaseList (D.Cons (D.Lambda (index 0), D.Nil),
    D.Lambda D.False, index 0), D.Truth));
  expect_rejected (D.CaseList (D.Truth, D.Truth, D.Truth));
  expect_rejected (D.CaseList (D.Word zero, D.Nil, D.Nil));
  expect_rejected (D.CaseList (words, D.Truth, index 0));
  expect_rejected (D.CaseList (D.Nil, D.Truth, D.Apply (D.Truth, D.Truth)));
  expect_rejected (D.CaseList (words, D.Apply (D.Truth, D.Truth), index 0));
  expect_rejected (D.Lambda (D.CaseList (index 0, D.Nil, D.Cons (index 0, index 0))));
  expect_rejected (D.CaseList (words, D.Word zero, index 1))
