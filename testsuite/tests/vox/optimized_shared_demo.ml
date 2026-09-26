(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_mgu_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml optimized_shared_demo.ml";
 { bytecode; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_unifier_metadata
open Level_finite_spec
open Optimized_metadata
open Level_mgu_spec
open Level_mgu_proofs

let run fail =
  let mode = 0 in
  let refine_ state = Pref.empty () in
  let v = cell Var 0 in let refine_ step = Pref.alloc v state in
  let a = step.value in let state = step.state in
  let v = cell Bool 4 in let refine_ step = Pref.alloc v state in
  let b = step.value in let state = step.state in
  let left_desc = Arrow (a, a) in let v = cell left_desc 4 in let refine_ step = Pref.alloc v state in
  let left = step.value in let state = step.state in
  let child = if mode = 2 then left else b in
  let right_desc = Arrow (b, child) in let v = cell right_desc 4 in let refine_ step = Pref.alloc v state in
  let right = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let active_all : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || active h x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
    cell_def left_desc 4; cell_def right_desc 4;
    active_def h x; at_level_def h x; let u = () in refine_ u) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
    cell_def left_desc 4; cell_def right_desc 4;
    active_all a; active_all b; active_all child;
    finite_scope_def h x; source_ok_def h x; let u = () in refine_ u) in
  let unmarked : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at h x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
    cell_def left_desc 4; cell_def right_desc 4;
    let u = () in refine_ u) in
  let trees : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
      cell_def left_desc 4; cell_def right_desc 4;
      let ta = Free a in let tb = Constant_tree b in let tl = Branch (left, ta, ta) in
      tree_root_def ta; tree_root_def tb; tree_root_def tl;
      finite_def h ta; finite_def h tb; finite_def h tl;
      observe_def h a; observe_def h b; observe_def h left; observe_def h right; observe_def h x;
      let tc = if mode = 2 then tl else tb in let tr = Branch (right, tb, tc) in
      tree_root_def tr; finite_def h tr;
      let t = if x === a then ta else if x === b then tb else if x === left then tl else if x === right then tr else Free x in
      tree_root_def t; refine_ t) in
  let order0 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x ->
let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
    cell_def left_desc 4; cell_def right_desc 4;
    below_def h a 4; below_def h b 4; below_def h child 4;
    at_level_def h a; at_level_def h b; at_level_def h child;
    children_below_def h left_desc 4; children_below_def h right_desc 4;
    let v = Var in children_below_def h v 0; let v = Bool in children_below_def h v 4; children_below_def h v 0;
    ordered_def h x; at_level_def h x; let u = () in refine_ u) in
  let ld = Arrow (left, left) in let lv = cell ld 4 in
  let refine_ step = Pref.alloc lv state in let p = step.value in let state = step.state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in
  let child = if fail then b else right in
  let rd = Arrow (right, child) in let rv = cell rd 4 in
  let refine_ step = Pref.alloc rv state in let q = step.value in let state = step.state in
  let h2 = ghost_ (Pref.own (borrow_ state)) in
  let trees1 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      cell_def ld 4; allocatable_def h lv; let u = () in
      let refine_ t = Level_finite_proofs.allocation_finite_at h trees p lv x (refine_ u) in refine_ t) in
  let trees2 : ((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
      cell_def rd 4; allocatable_def h1 rv; let u = () in
      let refine_ t = Level_finite_proofs.allocation_finite_at h1 trees1 q rv x (refine_ u) in refine_ t) in
  let active2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || active h2 x}) @ total ghost = ghost_ (fun x ->
    active_all x; cell_def ld 4; cell_def rd 4; active_def h x; active_def h2 x; at_level_def h x; at_level_def h2 x;
    let u = () in refine_ u) in
  let scope2 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || finite_scope h2 x}) @ total ghost = ghost_ (fun x ->
    scope x; finite_scope_def h x; cell_def ld 4; cell_def rd 4;
    payload_scoped_def h lv; payload_scoped_def h1 rv;
    let u = () in Pooled_allocation_proofs.allocation_source h p lv x (refine_ u);
    Pooled_allocation_proofs.allocation_source h1 q rv x (refine_ u);
    finite_scope_def h2 x; source_ok_def h2 x;
    match H.at h2 x with None -> refine_ u | Some v -> match v.desc with Var | Bool -> refine_ u
    | Link a -> active2 a; refine_ u | Arrow (a, b) -> active2 a; active2 b; refine_ u) in
  let unmarked2 : ((x : node Pref.t) @ immutable -> {u : unit | match H.at h2 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    unmarked x; cell_def ld 4; cell_def rd 4; let u = () in refine_ u) in
  ghost_ (active2 p; active2 q);
  let state : {t : node Pref.token | Pref.own t === h2 && H.mem h2 p && H.mem h2 q && active h2 p && active h2 q} = refine_ state in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h2 x}) @ total ghost = ghost_ (fun x ->
    order0 x; let u = () in
    cell_def left_desc 4; cell_def right_desc 4; let boolean = Bool in cell_def boolean 4;
    cell_def ld 4; cell_def rd 4;
    below_def h left 4; at_level_def h left; children_below_def h ld 4;
    Pooled_allocation_proofs.allocation_ordered h p ld 4 x (refine_ u);
    below_def h1 right 4; below_def h1 child 4; at_level_def h1 right; at_level_def h1 child;
    children_below_def h1 rd 4;
    Pooled_allocation_proofs.allocation_ordered h1 q rd 4 x (refine_ u); refine_ u) in
  let h_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h2)} in
  let scope_witness2 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope2)} in
  let unmarked_witness3 : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h_witness1.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked2)} in
  let order_witness4 : (((x : node Pref.t) @ immutable -> {u : unit | ordered h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ order)} in
  let trees_witness5 : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h_witness1.Ghost.ghost x then finite h_witness1.Ghost.ghost t else observe h_witness1.Ghost.ghost x === None)} @ immutable)) Ghost.t = {Ghost.ghost = ghost_ (refine_ trees2)} in
  let refine_ state_argument6 = state in
  let refine_ out = Optimized_unifier.unify h_witness1 scope_witness2 unmarked_witness3 order_witness4 trees_witness5 p q (refine_ state_argument6) in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in let d = ghost_ out.#derivation in let ok = out.#ok in
  let _mgu_proof = ghost_ (
    if ok then (
      let claim = true in
      let use : ((sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (solution : ((x : node Pref.t) @ immutable ->
            {u : unit | node_equation h2 sigma x && sigma p === sigma q
              && sigma x === substitute sigma (sigma x)
              && (H.mem h2 x || sigma x === Variable x)})) @ total ->
          (factor : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x})) @ total ->
            (x : node Pref.t) @ immutable -> {u : unit | rho p === rho q} ->
            {u : unit | rho x === substitute rho (sigma x)})) @ total ->
          {u : unit | claim}) @ total = fun sigma solution factor ->
        solution p; solution left; solution b;
        let model : (x : node Pref.t) @ immutable -> {u : unit | node_equation h2 sigma x}
            @ total = fun x -> solution x; let u = () in refine_ u in
        let[@def] delta : node Pref.t @ immutable total -> ty @ immutable total =
          fun x -> Function (Variable x, Boolean) in
        let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total =
          fun x -> substitute delta (sigma x) in
        let instance : (x : node Pref.t) @ immutable ->
            {u : unit | rho x === substitute delta (sigma x)} @ total = fun x ->
          rho_def x; let u = () in refine_ u in
        let old_model : (x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}
            @ total = fun x ->
          solution x; let u = () in
          instance_solution_at h2 sigma model delta rho instance p q x (refine_ u);
          refine_ u in
        let u = () in
        instance_solution_at h2 sigma model delta rho instance p q p (refine_ u);
        factor rho old_model p (refine_ u);
        factor rho old_model left (refine_ u);
        refine_ u in
      let u = () in let refine_ u = Optimized_mgu_proofs.with_mgu h2 trees2 p q after d (refine_ u) claim use in u)
    else ()) in
  ghost_ (let u = () in unified_frame h2 p q ok after d left (refine_ u); unified_frame h2 p q ok after d p (refine_ u);
    let refine_ tree = Optimized_finite_proofs.unified_finite_at h2 trees2 p q ok after d p (refine_ u) in ());
  let state = out.#state in let state : {t : node Pref.token | H.mem (Pref.own t) left} = refine_ state in
  let refine_ linked_child = Pref.read left (borrow_ state) in
  let refine_ state = state in let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
  let refine_ linked_parent = Pref.read p (borrow_ state) in
  assert (linked_child.desc = Link right);
  if fail then (assert (not ok); assert (linked_parent.desc = ld))
  else (assert ok; assert (linked_parent.desc = Link q))

let () = run false; run true
