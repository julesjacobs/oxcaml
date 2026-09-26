(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml hm_readback_runtime.ml effective_unifier_datatype_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_unifier_metadata

module F = Level_finite_spec

module E = Effective_level
module R = Representative_level
let[@def] (here @ total) (x : node Pref.t @ immutable total) : R.representative @ immutable total =
  {R.root = x; path = Here}

let run mode =
  let state = Pref.empty () in
  let d_a = Var in let v_a = cell d_a 0 in
  let refine_ step = Pref.alloc v_a state in
  let a = step.value in let state = step.state in
  let d_w = Word in let v_w = cell d_w 4 in
  let refine_ step = Pref.alloc v_w state in
  let w = step.value in let state = step.state in
  let d_b = (if mode = 4 then Word else if mode = 6 then List w else Bool) in let v_b = cell d_b 4 in
  let refine_ step = Pref.alloc v_b state in
  let b = step.value in let state = step.state in
  let d_la = List (if mode = 5 then b else a) in let v_la = cell d_la 4 in
  let refine_ step = Pref.alloc v_la state in
  let la = step.value in let state = step.state in
  let d_lw = List (if mode = 6 then b else w) in let v_lw = cell d_lw 4 in
  let refine_ step = Pref.alloc v_lw state in
  let lw = step.value in let state = step.state in
  let d_nested = (if mode = 3 then Arrow (w, w) else List la) in let v_nested = cell d_nested 4 in
  let refine_ step = Pref.alloc v_nested state in
  let nested = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let active_all : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || active h x}) @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
    active_def h x; at_level_def h x; ()) in
  let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
    active_all a; active_all w; active_all b; active_all la; active_all lw; active_all nested;
    finite_scope_def h x; source_ok_def h x; ()) in
  let unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true | Some v -> not v.visited})
      @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4; ()) in
  let trees : ((x : node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === x &&
        (if H.mem h x then F.finite h t else observe h x === None)} @ immutable)
      @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4;
    cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
    let ta = F.Free a in let tw = F.Word_tree w in
    let tb = if mode = 4 then F.Word_tree b
      else if mode = 6 then F.List_tree (b, tw) else F.Constant_tree b in
    let te = if mode = 5 then tb else ta in let tl = F.List_tree (la, te) in
    let tr = if mode = 6 then tb else tw in let tu = F.List_tree (lw, tr) in
    let tn = if mode = 3 then F.Branch (nested, tw, tw)
      else F.List_tree (nested, tl) in
    observe_def h a; observe_def h w; observe_def h b;
    observe_def h la; observe_def h lw; observe_def h nested;
    F.tree_root_def ta; F.finite_def h ta;
    F.tree_root_def tw; F.finite_def h tw;
    F.tree_root_def tb; F.finite_def h tb;
    F.tree_root_def te; F.tree_root_def tl; F.finite_def h tl;
    F.tree_root_def tr; F.tree_root_def tu; F.finite_def h tu;
    F.tree_root_def tn; F.finite_def h tn;
    let t = if x === a then ta else if x === w then tw else if x === b then tb
      else if x === la then tl else if x === lw then tu
      else if x === nested then tn else F.Free x in
    F.tree_root_def t; observe_def h x; refine_ t) in
  let p, q, expected = match mode with
    | 0 -> la, lw, true
    | 1 -> a, la, false
    | 2 -> b, w, false
    | 3 -> lw, nested, false
    | 4 -> w, b, true
    | 5 -> la, lw, false
    | 6 -> nested, lw, true
    | 7 -> a, lw, true
    | _ -> a, nested, false in
  let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ here} in
  let valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x ->
      cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
      here_def x; E.valid_head_def h heads.Ghost.ghost x;
      let path = Here in resolves_def h x x path; terminal_def h x; observe_def h x; ())} in
  let effective_active : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || E.effective_active h heads.Ghost.ghost x}) @ total ghost = ghost_ (fun x ->
    active_all x; here_def x; E.level_def h heads.Ghost.ghost x;
    E.effective_active_def h heads.Ghost.ghost x; active_def h x; ()) in
  let effective_scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || E.effective_scope h heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x ->
      scope x; finite_scope_def h x; source_ok_def h x;
      cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
      E.effective_scope_def h heads.Ghost.ghost x; observe_def h x;
      effective_active a; effective_active w; effective_active b;
      effective_active la; effective_active lw; effective_active nested; ())} in
  let order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x ->
      cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
      here_def a; here_def w; here_def b; here_def la; here_def lw; here_def nested;
      E.level_def h heads.Ghost.ghost a; E.level_def h heads.Ghost.ghost w; E.level_def h heads.Ghost.ghost b;
      E.level_def h heads.Ghost.ghost la; E.level_def h heads.Ghost.ghost lw; E.level_def h heads.Ghost.ghost nested;
      at_level_def h a; at_level_def h w; at_level_def h b; at_level_def h la; at_level_def h lw; at_level_def h nested;
      E.effective_below_def h heads.Ghost.ghost a 4; E.effective_below_def h heads.Ghost.ghost w 4; E.effective_below_def h heads.Ghost.ghost b 4;
      E.effective_below_def h heads.Ghost.ghost la 4; E.effective_below_def h heads.Ghost.ghost lw 4; E.effective_below_def h heads.Ghost.ghost nested 4;
      E.effective_ordered_def h heads.Ghost.ghost x; ())} in
  ghost_ (effective_active p; effective_active q);
  let heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ h} in
  let out = Effective_unifier_runtime.unify
    heap heads {Ghost.ghost = ghost_ (refine_ valid.Ghost.ghost)} {Ghost.ghost = ghost_ (refine_ effective_scope.Ghost.ghost)}
    {Ghost.ghost = ghost_ (refine_ unmarked)} {Ghost.ghost = ghost_ (refine_ order.Ghost.ghost)}
    {Ghost.ghost = ghost_ (refine_ trees)} p q state in
  if out.#ok <> expected then failwith "wrong datatype unification result";
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (
    Effective_unifier_frame.unified_frame h p q out.#ok after out.#derivation a ();
    Effective_unifier_frame.unified_frame h p q out.#ok after out.#derivation w ());
  let tree = ghost_ (Effective_unifier_finite.unified_finite_at h (refine_ trees)
    p q out.#ok after out.#derivation a ()) in
  let ty = Hm_readback_runtime.read tree a (borrow_ out.#state) in
  (match mode, ty with
   | (0 | 6), Word64 | 7, List_type Word64 -> ()
   | (1 | 2 | 3 | 4 | 5 | 8), Variable _ -> ()
   | _ -> failwith "wrong type read back after datatype unification");
  let variable = Pref.read a (borrow_ out.#state) in
  if mode = 0 || mode = 6 then
    (match variable.desc with
     | Link target -> if not (Pref.equal target w) then failwith "wrong element binding"
     | _ -> failwith "element variable was not bound")
  else if mode = 7 then (
    (match variable.desc with
     | Link target -> if not (Pref.equal target lw) then failwith "wrong list binding"
     | _ -> failwith "variable was not bound to list");
    let word = Pref.read w (borrow_ out.#state) in
    match word.level with Finite 0 -> () | _ -> failwith "list child level was not lowered")
  else match variable.desc with Var -> () | _ -> failwith "unrelated variable changed"

let () = List.iter run [0; 1; 2; 3; 4; 5; 6; 7; 8]
