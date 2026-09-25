(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml effective_lower_demo.ml effective_lower_shared_demo.ml";
 { bytecode; }
 { native; }
*)

open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec

module E = Effective_level
module R = Representative_level
let[@def] (here @ total) (x : node Pref.t @ immutable total) : R.representative @ immutable total =
  {R.root = x; path = Here}

let (here_below @ total) : (h : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | E.effective_below h here x bound === below h x bound
      && E.effective_active h here x === active h x} @ ghost = fun h x bound -> ghost_ (
  E.effective_below_def h here x bound; E.effective_active_def h here x;
  E.level_def h here x; here_def x; below_def h x bound; active_def h x;
  let u = () in refine_ u)

let rec run : int -> (h : Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (terminal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || terminal h x})) @ total ghost ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ghost ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ghost ->
    (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h && active h p && below h p 4}) @ unique -> unit =
  fun n h scope terminal order trees p state ->
  let refine_ state = state in
  if n <= 0 then (
    let state : {t : Pref.token | Pref.own t === h && 0 >= 0 && active h p} = refine_ state in
    let lower_heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h)} in
    let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ here} in
    let witness : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> terminal x; here_def x; E.valid_head_def h here x;
        let path = Here in resolves_def h x x path; let u = () in refine_ u)} in
    let lower_order_witness : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> order x; ordered_def h x; E.effective_ordered_def h here x;
        (match H.at h x with None -> () | Some v -> match v.desc, v.level with
          | Arrow (a, b), Finite n -> children_below_def h v.desc n; here_below h a n; here_below h b n; ()
          | _ -> ()); let u = () in refine_ u)} in
    let lower_scope_witness : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem lower_heap_witness.Ghost.ghost x) || E.effective_scope lower_heap_witness.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> scope x; finite_scope_def h x; lower_order_witness.Ghost.ghost x;
        let u = () in if H.mem h x then (E.ordered_scope lower_heap_witness.Ghost.ghost heads.Ghost.ghost witness.Ghost.ghost x (refine_ u); ()) else (); refine_ u)} in
    let lower_trees_witness : (((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if H.mem lower_heap_witness.Ghost.ghost x then Level_finite_spec.finite lower_heap_witness.Ghost.ghost t else Level_unifier_spec.observe lower_heap_witness.Ghost.ghost x === None)} @ immutable)) Ghost.t =
      {Ghost.ghost = ghost_ (refine_ trees)} in
    let refine_ state = state in
    ghost_ (here_below h p 0);
    let refine_ out = Effective_lower_runtime.lower lower_heap_witness heads witness lower_scope_witness lower_order_witness lower_trees_witness 0 p (refine_ state) in
    let state = out.#state in
    let after = ghost_ (Pref.own (borrow_ state)) in
    ghost_ (let u = () in Effective_lower_proofs.lowering_at h here 0 out.#edits p (refine_ u);
      lower_frame_def h after p; active_def h p; ());
    let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ v = Pref.read p (borrow_ state) in
    assert (v.level = Finite 0)
  ) else (
    let desc = Arrow (p, p) in let v = cell desc 4 in
    let refine_ step = Pref.alloc v state in let q = step.value in let state = step.state in
    let after = ghost_ (Pref.own (borrow_ state)) in
    let terminal1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || Level_unifier_spec.terminal after x}) @ total ghost = ghost_ (fun x ->
      terminal x; Copy_heap_proofs.put_frame h q v x; cell_def desc 4;
      terminal_def h x; terminal_def after x; observe_def h x; observe_def after x; let u = () in refine_ u) in
    let order1 : ((x : node Pref.t) @ immutable -> {u : unit | ordered after x}) @ total ghost = ghost_ (fun x ->
      order x; cell_def desc 4; children_below_def h desc 4; let u = () in
      Pooled_allocation_proofs.allocation_ordered h q desc 4 x (refine_ u); refine_ u) in
    let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || finite_scope after x}) @ total ghost = ghost_ (fun x ->
      scope x; finite_scope_def h x; cell_def desc 4; active_def h p;
      payload_scoped_def h v; let u = () in
      Pooled_allocation_proofs.allocation_source h q v x (refine_ u);
      order1 x; ordered_def after x; finite_scope_def after x;
      source_ok_def after x; active_def after x; at_level_def after x;
      (match H.at after x with None -> () | Some old -> match old.level with Generic -> () | Finite depth ->
        children_below_def after old.desc depth;
        match old.desc with Var | Bool -> ()
        | Link a -> below_def after a depth; active_def after a; at_level_def after a; ()
        | Arrow (a, b) -> below_def after a depth; active_def after a; at_level_def after a;
          below_def after b depth; active_def after b; at_level_def after b; ());
      refine_ u) in
    let trees1 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
        cell_def desc 4; active_def h p; allocatable_def h v; let u = () in
        let refine_ t = Level_finite_proofs.allocation_finite_at h trees q v x (refine_ u) in refine_ t) in
    ghost_ (cell_def desc 4; active_def after q; below_def after q 4; at_level_def after q);
    let state : {t : Pref.token | Pref.own t === after && active after q && below after q 4} = refine_ state in
    run (n - 1) after scope1 terminal1 order1 trees1 q state)

let () =
  let refine_ state = Pref.empty () in
  let desc = Bool in let v = cell desc 4 in
  let refine_ step = Pref.alloc v state in let p = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let terminal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || terminal h x}) @ total ghost = ghost_ (fun x ->
    cell_def desc 4; terminal_def h x; observe_def h x; let u = () in refine_ u) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    cell_def desc 4; finite_scope_def h x; source_ok_def h x; let u = () in refine_ u) in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x ->
    cell_def desc 4; ordered_def h x; children_below_def h desc 4; let u = () in refine_ u) in
  let trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      cell_def desc 4; let t = if x === p then Constant_tree p else Free x in
      tree_root_def t; finite_def h t; observe_def h x; refine_ t) in
  ghost_ (cell_def desc 4; active_def h p; at_level_def h p; below_def h p 4);
  let state : {t : Pref.token | Pref.own t === h && active h p && below h p 4} = refine_ state in
  run 200000 h scope terminal order trees p state
