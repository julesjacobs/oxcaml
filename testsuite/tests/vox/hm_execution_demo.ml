(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_type_proofs.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_execution_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_execution_spec

let run () =
  let refine_ state = Pref.empty () in let h0 = ghost_ (Pref.own (borrow_ state)) in
  let empty : pool = Generalize_spec.Empty in let env : env = Hm_environment_spec.Empty in
  let desc : desc = Bool in
  ghost_ (pool_scoped_def h0 empty; children_below_def h0 desc 1);
  let state : {t : node Pref.token | Pref.own t === h0 && pool_scoped h0 empty
      && 1 >= 0 && children_below h0 desc 1} = refine_ state in
  let allocation_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h0)} in
  let refine_ state = state in
  let refine_ allocated = Pooled_allocator.allocate allocation_heap 1 desc empty (refine_ state) in
  let p = allocated.#value in let child = allocated.#pool in let state = allocated.#state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in
  let rhs = ghost_ (RBool p) in
  ghost_ (allocated_def h0 1 p desc; ran_def h0 1 empty env rhs h1 child;
    result_def rhs; pool_scoped_def h1 empty);
  let state : {t : node Pref.token | Pref.own t === h1 && pool_scoped h1 child && pool_scoped h1 empty} = refine_ state in
  let close_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h1)} in
  let refine_ state = state in
  let refine_ closed = Nested_pool.close close_heap 0 child empty (refine_ state) in
  let h2 = ghost_ (Pref.own (borrow_ closed.#state)) in let parent = closed.#parent in
  let next_env = Bind (p, env) in
  let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h2 x then source_ok h2 x else H.at h2 x === None}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Bool in cell_def desc 1;
    let old : ((y : node Pref.t) @ immutable ->
      {u : unit | if H.mem h1 y then source_ok h1 y else H.at h1 y === None}) @ total = fun y ->
      source_ok_def h1 y; let u = () in refine_ u in
    let u = () in let refine_ u = Generalize_scheme_proofs.closed_scope h1 old 0 child x (refine_ u) in refine_ u) in
  ghost_ (let u = () in Generalize_proofs.closed_observe h1 0 child p (refine_ u);
    closed_at_def h1 h2 0 child p);
  let p : {p : node Pref.t | H.mem h2 p} = refine_ p in
  let state = closed.#state in
  let state : {t : node Pref.token | Pref.own t === h2 && pool_scoped h2 parent} = refine_ state in
  let depth : {n : int | n >= 0} = refine_ 0 in
  let clean : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at h2 x with None -> true | Some v -> v.memo === Empty_memo}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Bool in cell_def desc 1;
    let u = () in Generalize_proofs.closed_observe h1 0 child x (refine_ u);
    closed_at_def h1 h2 0 child x; refine_ u) in
  let saved_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h2)} in
  let clean_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness1.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean)} in
  let scope_witness3 : (((p : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved_witness1.Ghost.ghost p then source_ok saved_witness1.Ghost.ghost p
        else H.at saved_witness1.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope)} in
  let refine_ state = state in
  let copy_source1 : {p : node Pref.t | H.mem saved_witness1.Ghost.ghost p} =
    let refine_ p = p in refine_ p in
  let refine_ copied = Clean_copy.instantiate saved_witness1 clean_witness2 scope_witness3 parent depth copy_source1 (refine_ state) in
  let refine_ depth = depth in
  let refine_ p = p in let q = copied.#value in
  let after = ghost_ (Pref.own (borrow_ copied.#state)) in let final_pool = copied.#pool in
  (match final_pool with Entry (_, Empty) -> () | _ -> assert false);
  let epoch = ghost_ copied.#epoch in let history = ghost_ copied.#history in
  let body = ghost_ (RVar (D.Z, q, epoch, history)) in
  let execution = ghost_ (RLet (rhs, body, h1, child)) in
  ghost_ (let zero = D.Z in lookup_def next_env zero;
    copy_heap_def h2 epoch 0 history; ran_def h2 0 parent next_env body after final_pool;
    result_def body; result_def execution;
    ran_def h0 0 empty env execution after final_pool;
    let u = () in Hm_execution_proofs.run_result h0 0 empty env execution after final_pool q (refine_ u);
    let refine_ _tree = Hm_forest_proofs.closed_forest execution after final_pool q (refine_ u) in ());
  let state = copied.#state in
  let state : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ v = Pref.read q (borrow_ state) in
  assert (v.desc = Bool && v.level = Finite 0)

let () = run ()
