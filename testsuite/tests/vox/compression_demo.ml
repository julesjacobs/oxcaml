(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml compression_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec

let run () =
  let refine_ state = Pref.empty () in
  let gv : node = {desc = Bool; level = Generic; memo = Empty_memo; visited = true} in
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
  ghost_ (active_all p);
  let state : {t : Pref.token | Pref.own t === h && H.mem h p && active h p} = refine_ state in
  let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
  let scope_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope)} in
  let refine_ state_argument3 = state in
  let refine_ out = Compressed_representative.representative h_witness1 scope_witness2 p (refine_ state_argument3) in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  let edits = ghost_ out.#edits in
  ghost_ (let u = () in
    Compression_proofs.frame h after edits p (refine_ u);
    Compression_proofs.frame h after edits q (refine_ u);
    Compression_proofs.frame h after edits generic (refine_ u);
    Level_unifier_metadata.scratch_frame_def h after generic;
    let boolean = Bool in cell_def boolean 0; cell_def qd 1; cell_def pd 2;
    let leaf = Level_finite_spec.Constant_tree root in
    let qt = Level_finite_spec.Alias_tree (q, leaf) in
    let tree = Level_finite_spec.Alias_tree (p, qt) in
    Level_finite_spec.tree_root_def leaf;
    Level_finite_spec.tree_root_def qt; Level_finite_spec.tree_root_def tree;
    Level_finite_spec.finite_def h leaf;
    Level_finite_spec.finite_def h qt; Level_finite_spec.finite_def h tree;
    observe_def h p; observe_def h q; observe_def h root;
    let refine_ final_tree = Compression_proofs.finite h after edits tree (refine_ u) in
    ());
  assert (out.#value = root);
  let state = out.#state in
  let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
  let refine_ pv = Pref.read p (borrow_ state) in
  let refine_ state = state in
  let state : {t : Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ qv = Pref.read q (borrow_ state) in
  let refine_ state = state in
  let state : {t : Pref.token | H.mem (Pref.own t) generic} = refine_ state in
  let refine_ actual_generic = Pref.read generic (borrow_ state) in
  assert (actual_generic = gv);
  assert (pv.desc = Link root); assert (qv.desc = Link root);
  assert (pv.level = Finite 2); assert (qv.level = Finite 1)

let () = run ()
