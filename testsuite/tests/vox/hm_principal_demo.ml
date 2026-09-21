(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_type_proofs.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_runtime_spec.ml hm_runtime_proofs.ml hm_sound_proofs.ml hm_environment_proofs.ml hm_protected_proofs.ml hm_registration_proofs.ml hm_let_runtime_proofs.ml fast_environment.ml fast_term.ml hm_infer.ml leaf_agreement_proofs.ml hm_origin_proofs.ml hm_complete_proofs.ml hm_one_let_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_polymorphic_proofs.ml hm_polymorphic_fixtures.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_scheme_transport_proofs.ml hm_generalized_scheme_proofs.ml hm_polymorphic_sound_proofs.ml hm_principal_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Hm_execution_spec
module D = Hm_declarative
module T = Hm_type_proofs
module P = Hm_polymorphic_proofs

let run : (e : D.term) @ immutable -> (target : ty) @ immutable ghost ->
    (d : D.typing) @ immutable ghost ->
    {u : unit | D.typed D.Z D.Empty_context e (T.embed target) d} @ ghost -> unit =
  fun e target d premise ->
  ghost_ (let refine_ premise = premise in let z = D.Z in let g = D.Empty_context in
    let t = T.embed target in let u = () in D.depth_def g;
    T.typing_scoped z g e t d (refine_ u); ());
  let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_infer.closed_hm input in let refine_ input = input in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (let refine_ premise = premise in let u = () in
    P.closed_completes out.#execution after out.#pool target d (refine_ u));
  match out.#value with None ->
    ghost_ (let _impossible : {u : unit | false} = refine_ () in ()); assert false
  | Some p ->
    ghost_ (let refine_ premise = premise in let u = () in
      let refine_ tree = Hm_forest_proofs.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      Hm_execution_proofs.run_result h 0 pool env out.#execution after out.#pool p (refine_ u);
      let use : ((inferred : D.typing) @ immutable ->
        (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | D.typed D.Z D.Empty_context (source out.#execution) (T.embed (Level_finite_spec.readback tree)) inferred
          && target === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
        {u : unit | true}) @ total = fun _inferred _delta _factor -> let u = () in refine_ u in
      Hm_polymorphic_sound_proofs.closed_principal out.#execution after out.#pool p tree target d (refine_ u) true use; ()); ()

let nested (b : ty @ immutable ghost) =
  let z = D.Z in let v = D.Bound z in let rhs = D.Lambda v in
  let app = D.Apply (v, v) in let inner = D.Let (v, app) in let e = D.Let (rhs, inner) in
  let target = ghost_ (Function (b, b)) in
  let d : {d : D.typing | D.typed D.Z D.Empty_context e (T.embed target) d} @ immutable ghost = ghost_ (let a = T.embed b in T.embed_wf z b; T.embed_def target;
    let u = () in let refine_ d = Hm_polymorphic_fixtures.nested_alias_typing a (refine_ u) in refine_ d) in
  let refine_ d = d in
  run e target d (ghost_ (refine_ ()))

let mixed (b : ty @ immutable ghost) =
  let z = D.Z in let one = D.S z in let v = D.Bound z in
  let outer = D.Bound one in let rhs = D.Lambda outer in
  let truth = D.Truth in let app = D.Apply (v, truth) in
  let body = D.Let (rhs, app) in let e = D.Lambda body in
  let target = ghost_ (Function (b, b)) in
  let d : {d : D.typing | D.typed D.Z D.Empty_context e (T.embed target) d} @ immutable ghost = ghost_ (let refine_ d = Hm_polymorphic_fixtures.mixed_typing b in refine_ d) in
  let refine_ d = d in
  run e target d (ghost_ (refine_ ()))

let () =
  nested (ghost_ Boolean);
  nested (ghost_ (Function (Boolean, Boolean)));
  mixed (ghost_ Boolean);
  mixed (ghost_ (Function (Boolean, Boolean)))

let saved_garbage () =
  let z = D.Z in let one = D.S z in let two = D.S one in
  let v = D.Bound z in let identity = D.Lambda v in let truth = D.Truth in
  let discard = D.Lambda truth in let old = D.Apply (discard, identity) in
  let self_app = D.Apply (v, v) in let inner = D.Let (identity, self_app) in
  let e = D.Let (old, inner) in
  ghost_ (D.scoped_term_def z e; D.scoped_term_def z old;
    D.scoped_term_def z discard; D.scoped_term_def one truth;
    D.scoped_term_def z identity; D.scoped_term_def one v; D.present_def one z;
    D.scoped_term_def one inner; D.scoped_term_def one identity;
    D.scoped_term_def two v; D.present_def two z; D.scoped_term_def two self_app; ());
  let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_infer.closed_hm input in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  match out.#value with None -> assert false | Some p ->
    ghost_ (let u = () in
      let refine_ tree = Hm_forest_proofs.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      Hm_execution_proofs.run_result h 0 pool env out.#execution after out.#pool p (refine_ u);
      let _typing = Hm_polymorphic_sound_proofs.closed_sound out.#execution after out.#pool p tree (refine_ u) in ()); ()

let () = saved_garbage ()

let deep_infer : (e : {e : D.term | D.scoped_term D.Z e}) @ immutable -> unit =
  fun e -> let refine_ out = Hm_infer.closed_hm e in
    match out.#value with None -> assert false | Some _ -> ()

let rec deep_lambdas : int -> (body : D.term) @ immutable ->
    (scope : (((n : D.index) @ immutable ->
      {u : unit | D.scoped_term n body})) Ghost.t) @ total -> unit =
  fun count body scope ->
    if count <= 0 then (
      ghost_ (let z = D.Z in scope.Ghost.ghost z; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ body in
      deep_infer input;
      let z = D.Z in let one = D.S z in let two = D.S one in
      let variable = D.Bound z in
      let copied = D.Let (body, variable) in
      ghost_ (D.scoped_term_def z copied; D.scoped_term_def one variable;
        D.present_def one z; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ copied in
      deep_infer input;
      let identity = D.Lambda variable in
      let applied = D.Apply (identity, body) in
      ghost_ (D.scoped_term_def z applied; D.scoped_term_def z identity; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ applied in
      deep_infer input;
      let call = D.Apply (variable, body) in
      let outer = D.Bound one in
      let inner_call = D.Apply (outer, body) in
      let drop = D.Lambda inner_call in
      let calls = D.Apply (drop, call) in
      let unified = D.Lambda calls in
      ghost_ (scope.Ghost.ghost one; scope.Ghost.ghost two;
        D.scoped_term_def one call; D.scoped_term_def two inner_call;
        D.scoped_term_def two outer; D.present_def two one;
        D.scoped_term_def two variable; D.present_def two z;
        D.scoped_term_def one drop; D.scoped_term_def one calls;
        D.scoped_term_def z unified; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ unified in
      deep_infer input)
    else (
      let next = D.Lambda body in
      let scope : (((n : D.index) @ immutable ->
        {u : unit | D.scoped_term n next})) Ghost.t =
        {Ghost.ghost = ghost_ (fun n ->
          let more = D.S n in scope.Ghost.ghost more;
          D.scoped_term_def n next; let u = () in refine_ u)} in
      deep_lambdas (count - 1) next scope)

let () =
  let body = D.Truth in
  let scope : (((n : D.index) @ immutable ->
    {u : unit | D.scoped_term n body})) Ghost.t =
    {Ghost.ghost = ghost_ (fun n ->
      D.scoped_term_def n body; let u = () in refine_ u)} in
  deep_lambdas 200000 body scope
