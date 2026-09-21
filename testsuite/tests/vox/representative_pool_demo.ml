(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml representative_pool_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Generalize_spec
module R = Representative_level
module S = Representative_pool_spec

let (close_frame @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool} ->
    {u : unit | H.mem h p === H.mem (S.close_heap h cut pool) p} @ ghost =
  fun h cut pool p premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    S.close_heap_def h cut pool; R.representatives_scoped h pool (refine_ u);
    let filtered = R.representatives h pool in
    let after = S.close_heap h cut pool in
    Generalize_proofs.closed_observe h cut filtered p (refine_ u);
    closed_at_def h after cut filtered p; refine_ u)

let run () =
  let refine_ state = Pref.empty () in
  let leaf = cell Var 1 in let refine_ step = Pref.alloc leaf state in
  let q = step.value in let state = step.state in
  let alias = cell (Link q) 2 in let refine_ step = Pref.alloc alias state in
  let p = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let empty = Empty in let tail = Entry (q, empty) in let pool = Entry (p, tail) in
  ghost_ (let var = Var in cell_def var 1; let link = Link q in cell_def link 2;
    source_ok_def h p; source_ok_def h q;
    pool_scoped_def h empty; pool_scoped_def h tail; pool_scoped_def h pool);
  let witness = {Ghost.ghost = ghost_ h} in
  let state : {t : node Pref.token | Pref.own t === h && pool_scoped h pool
    && pool_scoped h empty} = refine_ state in
  let refine_ state = state in
  let refine_ out = Representative_pool.close_and_transfer witness 1 pool empty (refine_ state) in
  let retained = out.#parent in let state = out.#state in
  let middle = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (let u = () in close_frame h 1 pool p (refine_ u);
    close_frame h 1 pool q (refine_ u); pool_scoped_def middle empty);
  let witness = {Ghost.ghost = ghost_ middle} in
  assert (retained = Entry (q, Empty));
  let state : {t : node Pref.token | Pref.own t === middle && pool_scoped middle retained
    && pool_scoped middle empty} = refine_ state in
  let refine_ state = state in
  let refine_ out = Representative_pool.close_and_transfer witness 0 retained empty (refine_ state) in
  assert (out.#parent = Empty);
  let state = out.#state in
  ghost_ (let u = () in close_frame middle 0 retained p (refine_ u);
    close_frame middle 0 retained q (refine_ u));
  let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
  let refine_ pv = Pref.read p (borrow_ state) in let refine_ state = state in
  let state : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ qv = Pref.read q (borrow_ state) in
  assert (pv.desc = Link q); assert (pv.level = Finite 2);
  assert (qv.level = Generic);
  assert (pv.memo = Empty_memo); assert (qv.memo = Empty_memo)

let () = run ()
