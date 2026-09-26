(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_routing_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Generalize_spec
module R = Representative_level
module S = Representative_pool_spec
module A = Borrow_iarray.Owned_array

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

let rec repeat : (h : node Pref.heap Ghost.t) @ immutable ->
    (n : int) -> (p : node Pref.t) @ immutable ->
    (tail : {pool : pool | pool_scoped h.Ghost.ghost pool
      && H.mem h.Ghost.ghost p && source_ok h.Ghost.ghost p}) @ immutable ->
    {pool : pool | pool_scoped h.Ghost.ghost pool} @ immutable =
  fun h n p tail ->
    let refine_ tail = tail in
    if n <= 0 then refine_ tail else
      let next = Entry (p, tail) in
      ghost_ (pool_scoped_def h.Ghost.ghost next);
      let refine_ out = repeat h (n - 1) p (refine_ next) in refine_ out

let rec length acc = function
  | Empty -> acc
  | Entry (_, tail) -> length (acc + 1) tail

let run count =
  let refine_ state = Pref.empty () in
  let low = cell Var 0 in let refine_ step = Pref.alloc low state in
  let p = step.value in let state = step.state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in
  let middle = cell Var 2 in let refine_ step = Pref.alloc middle state in
  let q = step.value in let state = step.state in
  ghost_ (Copy_heap_proofs.put_frame h1 q middle p; ());
  let h2 = ghost_ (Pref.own (borrow_ state)) in
  let high = cell Var 4 in let refine_ step = Pref.alloc high state in
  let r = step.value in let state = step.state in
  ghost_ (Copy_heap_proofs.put_frame h2 r high p;
    Copy_heap_proofs.put_frame h2 r high q; ());
  let h3 = ghost_ (Pref.own (borrow_ state)) in
  let alias = cell (Link p) 9 in let refine_ step = Pref.alloc alias state in
  let link = step.value in let state = step.state in
  ghost_ (Copy_heap_proofs.put_frame h3 link alias p;
    Copy_heap_proofs.put_frame h3 link alias q;
    Copy_heap_proofs.put_frame h3 link alias r; ());
  let h = ghost_ (Pref.own (borrow_ state)) in
  let empty = Empty in let one = Entry (link, empty) in
  let two = Entry (r, one) in let tail = Entry (q, two) in
  ghost_ (let var = Var in cell_def var 0; cell_def var 2; cell_def var 4;
    let desc = Link p in cell_def desc 9;
    source_ok_def h p; source_ok_def h q;
    source_ok_def h r; source_ok_def h link;
    pool_scoped_def h empty; pool_scoped_def h one;
    pool_scoped_def h two; pool_scoped_def h tail;
    ());
  let witness = {Ghost.ghost = ghost_ h} in
  let refine_ child = repeat witness count p (refine_ tail) in
  let existing = Entry (q, Empty) in
  let values = [: Empty; existing; Empty; Empty :] in
  let refine_ pools = A.of_iarray values in
  let state : {t : node Pref.token | Pref.own t === witness.Ghost.ghost && pool_scoped witness.Ghost.ghost child} =
    refine_ state in
  let pools : {a : pool A.t | 0 <= 3 && 3 < Iarray.length (A.contents a)} =
    refine_ pools in
  let refine_ out = Level_pool_routing.close_and_route witness 3 child state pools in
  let state = out.#state in let refine_ values = A.into_iarray out.#pools in
  assert (length 0 (Iarray.get values 0) = count);
  assert ((Iarray.get values 1) = existing);
  assert ((Iarray.get values 2) = Entry (q, Empty));
  assert ((Iarray.get values 3) = Empty);
  ghost_ (let u = () in close_frame h 3 child r (refine_ u);
    close_frame h 3 child link (refine_ u));
  let state : {t : node Pref.token | H.mem (Pref.own t) r} = refine_ state in
  let refine_ rv = Pref.read r (borrow_ state) in let refine_ state = state in
  let state : {t : node Pref.token | H.mem (Pref.own t) link} = refine_ state in
  let refine_ lv = Pref.read link (borrow_ state) in
  assert (rv.level = Generic);
  assert (lv.level = Finite 9);
  assert (lv.desc = Link p)

let () = run 1; run 200_000
