(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml optimized_compression_demo.ml";
 { bytecode; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec

let run () =
  let refine_ state = Pref.empty () in
  let gv : node = {desc = Bool; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc gv state in let generic = step.value in let state = step.state in
  let v = cell Bool 0 in let refine_ step = Pref.alloc v state in
  let root = step.value in let state = step.state in
  let qd = Link root in let v = cell qd 1 in let refine_ step = Pref.alloc v state in
  let q = step.value in let state = step.state in
  let pd = Link q in let v = cell pd 2 in let refine_ step = Pref.alloc v state in
  let p = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let active_all : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || x === generic || active h x}) @ total ghost = ghost_ (fun x ->
    let boolean = Bool in cell_def boolean 0; cell_def qd 1; cell_def pd 2;
    active_def h x; at_level_def h x; let u = () in refine_ u) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    let boolean = Bool in cell_def boolean 0; cell_def qd 1; cell_def pd 2;
    active_all root; active_all q; active_all p;
    active_def h x; at_level_def h x;
    finite_scope_def h x; source_ok_def h x; let u = () in refine_ u) in
  ghost_ (active_all p; active_all root);
  let state : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h root && active h p && active h root} = refine_ state in
  let trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let boolean : desc = Bool in cell_def boolean 0; cell_def qd 1; cell_def pd 2;
      let leaf = Constant_tree root in let qt = Alias_tree (q, leaf) in let pt = Alias_tree (p, qt) in
      let gt = Constant_tree generic in
      tree_root_def leaf; tree_root_def qt; tree_root_def pt; tree_root_def gt;
      finite_def h leaf; finite_def h qt; finite_def h pt; finite_def h gt;
      observe_def h root; observe_def h q; observe_def h p; observe_def h generic; observe_def h x;
      let t = if x === p then pt else if x === q then qt else if x === root then leaf else if x === generic then gt else Free x in
      tree_root_def t; refine_ t) in
  let unmarked : ((x : node Pref.t) @ immutable -> {u : unit | match H.at h x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let boolean : desc = Bool in cell_def boolean 0; cell_def qd 1; cell_def pd 2; let u = () in refine_ u) in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x ->
let boolean = Bool in cell_def boolean 0; cell_def qd 1; cell_def pd 2;
    below_def h root 1; below_def h q 2; at_level_def h root; at_level_def h q;
    children_below_def h qd 1; children_below_def h pd 2;
    let v = Var in children_below_def h v 0; let v = Bool in children_below_def h v 4; children_below_def h v 0;
    ordered_def h x; at_level_def h x; let u = () in refine_ u) in
  let h_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
  let scope_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope)} in
  let unmarked_witness3 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness1.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked)} in
  let order_witness4 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order)} in
  let trees_witness5 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness1.Ghost.ghost x then finite h_witness1.Ghost.ghost t else observe h_witness1.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees)} in
  let refine_ state_argument6 = state in
  let refine_ out = Optimized_unifier.unify h_witness1 scope_witness2 unmarked_witness3 order_witness4 trees_witness5 p root (refine_ state_argument6) in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in let d = ghost_ out.#derivation in let ok = out.#ok in
  ghost_ (let u = () in Optimized_metadata.unified_frame h p root ok after d p (refine_ u);
    Optimized_metadata.unified_frame h p root ok after d q (refine_ u);
    Optimized_metadata.unified_frame h p root ok after d generic (refine_ u);
    let refine_ tree = Optimized_finite_proofs.unified_finite_at h trees p root ok after d p (refine_ u) in ());
  assert ok;
  let state = out.#state in
  let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
  let refine_ pv = Pref.read p (borrow_ state) in
  let refine_ state = state in
  let state : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ qv = Pref.read q (borrow_ state) in
  let refine_ state = state in
  let state : {t : node Pref.token | H.mem (Pref.own t) generic} = refine_ state in
  let refine_ actual_generic = Pref.read generic (borrow_ state) in
  assert (actual_generic = gv);
  assert (pv.desc = Link root); assert (qv.desc = Link root);
  assert (pv.level = Finite 2); assert (qv.level = Finite 1)

let () = run ()
