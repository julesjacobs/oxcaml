(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.ml graph_occurs.ml graph_representative.ml effective_compression_spec.ml effective_compression_proofs.ml effective_compressed_representative.ml effective_compression_metadata.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_compression_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec

let run stale =
  let refine_ state = Pref.empty () in
  let leaf_node = cell Var 3 in
  let refine_ step = Pref.alloc leaf_node state in
  let leaf = step.value in let state = step.state in
  let link_node = {desc = Link leaf; level = stale; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc link_node state in
  let link = step.value in let state = step.state in
  let root_node = {desc = Link link; level = stale; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc root_node state in
  let root = step.value in let state = step.state in
  let h : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let values : (((x : node Pref.t) @ immutable -> {u : unit |
      H.at h.Ghost.ghost leaf === Some leaf_node
      && H.at h.Ghost.ghost link === Some link_node
      && H.at h.Ghost.ghost root === Some root_node
      && (H.mem h.Ghost.ghost x === (x === leaf || x === link || x === root))})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in refine_ u)} in
  let scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> values.Ghost.ghost x;
      source_ok_def h.Ghost.ghost x; let var = Var in cell_def var 3;
      let u = () in refine_ u)} in
  let path = {Ghost.ghost = ghost_ (Via (link, Via (leaf, Here)))} in
  ghost_ (
    values.Ghost.ghost root; let var = Var in cell_def var 3;
    let here = Here in let child = Via (leaf, here) in
    observe_def h.Ghost.ghost root; observe_def h.Ghost.ghost link; observe_def h.Ghost.ghost leaf;
    terminal_def h.Ghost.ghost leaf; active_def h.Ghost.ghost leaf; at_level_def h.Ghost.ghost leaf;
    resolves_def h.Ghost.ghost leaf leaf here;
    resolves_def h.Ghost.ghost link leaf child;
    resolves_def h.Ghost.ghost root leaf path.Ghost.ghost; ());
  let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost root
      && active h.Ghost.ghost leaf && resolves h.Ghost.ghost root leaf path.Ghost.ghost} = refine_ state in
  let refine_ state_argument = state in
  let refine_ result = Effective_compressed_representative.walk h scope root leaf path (refine_ state_argument) in
  let refine_ equal = Pref.equal result.#value leaf in assert equal;
  let after = ghost_ (Pref.own (borrow_ result.#state)) in
  let edits = ghost_ result.#edits in
  ghost_ (let u = () in
    Effective_compression_proofs.frame h.Ghost.ghost after edits root (refine_ u);
    Effective_compression_proofs.frame h.Ghost.ghost after edits link (refine_ u));
  let state = result.#state in
  let state : {t : Pref.token | H.mem (Pref.own t) root} = refine_ state in
  let refine_ stored = Pref.read root (borrow_ state) in
  (match stored.desc with Link target -> let refine_ equal = Pref.equal target leaf in assert equal | _ -> assert false);
  assert (stored.level = stale);
  let refine_ state = state in
  let state : {t : Pref.token | H.mem (Pref.own t) link} = refine_ state in
  let refine_ stored = Pref.read link (borrow_ state) in
  (match stored.desc with Link target -> let refine_ equal = Pref.equal target leaf in assert equal | _ -> assert false);
  assert (stored.level = stale);
  print_endline "stale-link compression: ok"

let () = run Generic; run (Finite 0)
