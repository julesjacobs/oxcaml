(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml level_proofs.ml level_lower.ml level_unifier_spec.ml level_unifier_proofs.ml level_unifier_metadata.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml pooled_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Generalize_proofs
open Level_unifier_spec
open Level_unifier_proofs
open Level_unifier_metadata
open Level_finite_spec
open Level_finite_proofs
open Forest_transport
open Pooled_spec
open Pooled_proofs
open Pooled_allocation_proofs

let rec count = function Empty -> 0 | Entry (_, rest) -> 1 + count rest

let run reject =
  let refine_ state = Pref.empty () in
  let h0 = ghost_ (Pref.own (borrow_ state)) in let pool0 = Empty in
  let scope0 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h0 x then source_ok h0 x else H.at h0 x === None}) @ total ghost =
    ghost_ (fun x -> let u = () in refine_ u) in
  let order0 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h0 x}) @ total ghost =
    ghost_ (fun x -> ordered_def h0 x; let u = () in refine_ u) in
  let trees0 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h0 x then finite h0 t else observe h0 x === None)} @ immutable) @ total ghost =
    ghost_ (fun x -> let t = Free x in tree_root_def t; observe_def h0 x; refine_ t) in
  let coverage0 : ((x : node Pref.t) @ immutable -> {u : unit | covered h0 0 pool0 x}) @ total ghost =
    ghost_ (fun x -> covered_def h0 0 pool0 x; let u = () in refine_ u) in
  ghost_ (pool_scoped_def h0 pool0);
  let desc1 : desc = Var in
  ghost_ (children_below_def h0 desc1 2);
  let state : {t : Pref.token | Pref.own t === h0 && pool_scoped h0 pool0 && 2 >= 0 && children_below h0 desc1 2} = refine_ state in
  let refine_ step = Pooled_allocator.allocate h0 2 desc1 pool0 state in
  let a = step.#value in let pool1 = step.#pool in let state = step.#state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in let v1 = cell desc1 2 in
  ghost_ (cell_def desc1 2; payload_scoped_def h0 v1; allocatable_def h0 v1;
    (match desc1 with Var | Bool -> () | Link q -> below_def h0 q 2; ()
    | Arrow (a, b) -> below_def h0 a 2; below_def h0 b 2; ()));
  let trees1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h0 trees0 a v1 x (refine_ u) in refine_ t) in
  let scope1 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h1 x then source_ok h1 x else H.at h1 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = allocation_scope_at h0 scope0 a v1 x (refine_ u) in refine_ u) in
  let order1 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h1 x}) @ total ghost = ghost_ (fun x ->
    order0 x; let u = () in let refine_ u = allocation_ordered h0 a desc1 2 x (refine_ u) in refine_ u) in
  let coverage1 : ((x : node Pref.t) @ immutable -> {u : unit | covered h1 0 pool1 x}) @ total ghost = ghost_ (fun x ->
    coverage0 x; let u = () in let refine_ u = allocation_coverage h0 a v1 pool0 0 x (refine_ u) in refine_ u) in
  let desc2 : desc = if reject then Bool else Var in
  ghost_ (children_below_def h1 desc2 2);
  let state : {t : Pref.token | Pref.own t === h1 && pool_scoped h1 pool1 && 2 >= 0 && children_below h1 desc2 2} = refine_ state in
  let refine_ step = Pooled_allocator.allocate h1 2 desc2 pool1 state in
  let b = step.#value in let pool2 = step.#pool in let state = step.#state in
  let h2 = ghost_ (Pref.own (borrow_ state)) in let v2 = cell desc2 2 in
  ghost_ (cell_def desc2 2; payload_scoped_def h1 v2; allocatable_def h1 v2;
    (match desc2 with Var | Bool -> () | Link q -> below_def h1 q 2; ()
    | Arrow (a, b) -> below_def h1 a 2; below_def h1 b 2; ()));
  let trees2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h1 trees1 b v2 x (refine_ u) in refine_ t) in
  let scope2 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h2 x then source_ok h2 x else H.at h2 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = allocation_scope_at h1 scope1 b v2 x (refine_ u) in refine_ u) in
  let order2 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h2 x}) @ total ghost = ghost_ (fun x ->
    order1 x; let u = () in let refine_ u = allocation_ordered h1 b desc2 2 x (refine_ u) in refine_ u) in
  let coverage2 : ((x : node Pref.t) @ immutable -> {u : unit | covered h2 0 pool2 x}) @ total ghost = ghost_ (fun x ->
    coverage1 x; let u = () in let refine_ u = allocation_coverage h1 b v2 pool1 0 x (refine_ u) in refine_ u) in
  let desc3 : desc = Arrow (a, b) in
  ghost_ (let u = () in allocation_below h1 b v2 a 2 (refine_ u); children_below_def h2 desc3 2);
  let state : {t : Pref.token | Pref.own t === h2 && pool_scoped h2 pool2 && 2 >= 0 && children_below h2 desc3 2} = refine_ state in
  let refine_ step = Pooled_allocator.allocate h2 2 desc3 pool2 state in
  let root = step.#value in let pool3 = step.#pool in let state = step.#state in
  let h3 = ghost_ (Pref.own (borrow_ state)) in let v3 = cell desc3 2 in
  ghost_ (cell_def desc3 2; payload_scoped_def h2 v3; allocatable_def h2 v3;
    (match desc3 with Var | Bool -> () | Link q -> below_def h2 q 2; ()
    | Arrow (a, b) -> below_def h2 a 2; below_def h2 b 2; ()));
  let trees3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h2 trees2 root v3 x (refine_ u) in refine_ t) in
  let scope3 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h3 x then source_ok h3 x else H.at h3 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = allocation_scope_at h2 scope2 root v3 x (refine_ u) in refine_ u) in
  let order3 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h3 x}) @ total ghost = ghost_ (fun x ->
    order2 x; let u = () in let refine_ u = allocation_ordered h2 root desc3 2 x (refine_ u) in refine_ u) in
  let coverage3 : ((x : node Pref.t) @ immutable -> {u : unit | covered h3 0 pool3 x}) @ total ghost = ghost_ (fun x ->
    coverage2 x; let u = () in let refine_ u = allocation_coverage h2 root v3 pool2 0 x (refine_ u) in refine_ u) in
  let finite_scope3 : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h3 x) || finite_scope h3 x}) @ total ghost = ghost_ (fun x ->
    scope3 x; order3 x; let u = () in if H.mem h3 x then (ordered_scope h3 x (refine_ u); refine_ u) else refine_ u) in
  ghost_ (let u = () in allocation_below h2 root v3 a 2 (refine_ u); allocation_below h2 root v3 b 2 (refine_ u);
    below_def h3 a 2; below_def h3 b 2; active_def h3 a; active_def h3 b);
  let target = if reject then root else b in
  ghost_ (below_def h3 root 2; active_def h3 root);
  let state : {t : Pref.token | Pref.own t === h3 && H.mem h3 a && H.mem h3 target && active h3 a && active h3 target} = refine_ state in
  let refine_ solved = Level_unifier.unify h3 finite_scope3 a target state in
  assert (solved.#ok = not reject);
  let ok = solved.#ok in let ud = ghost_ solved.#derivation in let h4 = ghost_ (Pref.own (borrow_ solved.#state)) in
  let trees4 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h4 x then finite h4 t else observe h4 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = unified_finite_at h3 trees3 a target ok h4 ud x (refine_ u) in refine_ t) in
  let scope4 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h4 x then source_ok h4 x else H.at h4 x === None}) @ total ghost = ghost_ (fun x ->
    let refine_ tree = trees4 x in observe_def h4 x;
    let u = () in unified_scope h3 finite_scope3 a target ok h4 ud x (refine_ u); finite_scope_def h4 x; refine_ u) in
  let _order4 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h4 x}) @ total ghost = ghost_ (fun x ->
    order3 x; let u = () in let refine_ u = unified_ordered h3 a target ok h4 ud x (refine_ u) in refine_ u) in
  let coverage4 : ((x : node Pref.t) @ immutable -> {u : unit | covered h4 0 pool3 x}) @ total ghost = ghost_ (fun x ->
    coverage3 x; let u = () in let refine_ u = coverage_after_unify h3 a target ok h4 ud 0 pool3 x (refine_ u) in refine_ u) in
  ghost_ (let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h4 x) || source_ok h4 x}) @ total = fun x -> scope4 x; let u = () in refine_ u in
    let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool3 x) || H.mem h4 x}) @ total = fun x ->
      let u = () in if listed pool3 x then (pool_member h3 pool3 x (refine_ u); unified_frame h3 a target ok h4 ud x (refine_ u); refine_ u) else refine_ u in
    pool_from_members h4 scope pool3 members);
  let state = solved.#state in let state : {t : Pref.token | Pref.own t === h4 && pool_scoped h4 pool3} = refine_ state in
  let refine_ state = Generalize.close h4 0 pool3 state in let h5 = ghost_ (Pref.own (borrow_ state)) in
  let trees5 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h5 x then finite h5 t else observe h5 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = closed_forest_at h4 trees4 0 pool3 x (refine_ u) in refine_ t) in
  let scope5 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h5 x then source_ok h5 x else H.at h5 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = Generalize_scheme_proofs.closed_scope h4 scope4 0 pool3 x (refine_ u) in refine_ u) in
  let coverage5 : ((x : node Pref.t) @ immutable -> {u : unit | covered h5 0 pool3 x}) @ total ghost = ghost_ (fun x ->
    coverage4 x; let u = () in closed_observe h4 0 pool3 x (refine_ u); closed_at_def h4 h5 0 pool3 x;
    covered_def h4 0 pool3 x; covered_def h5 0 pool3 x; at_level_def h4 x; at_level_def h5 x;
    (match H.at h4 x with None -> () | Some v -> close_level_def 0 v.level; ()); refine_ u) in
  ghost_ (let refine_ t = trees4 root in let u = () in unified_frame h3 a target ok h4 ud root (refine_ u);
    unfolding_valid h4 t (refine_ u); unfolding_root t;
    let t = unfolding t in Generalize_scheme_proofs.scheme_valid h4 0 pool3 coverage4 t (refine_ u);
    closed_observe h4 0 pool3 root (refine_ u); closed_at_def h4 h5 0 pool3 root);
  ghost_ (let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h5 x) || source_ok h5 x}) @ total = fun x -> scope5 x; let u = () in refine_ u in
    let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool3 x) || H.mem h5 x}) @ total = fun x ->
      let u = () in if listed pool3 x then (pool_member h4 pool3 x (refine_ u); closed_observe h4 0 pool3 x (refine_ u); closed_at_def h4 h5 0 pool3 x; refine_ u) else refine_ u in
    pool_from_members h5 scope pool3 members);
  let root : {p : node Pref.t | H.mem h5 p} = refine_ root in
  let depth = 2 in let depth : {n : int | n >= 0} = refine_ depth in
  let state : {t : Pref.token | Pref.own t === h5 && pool_scoped h5 pool3} = refine_ state in
  let refine_ out = Pooled_copy.instantiate h5 scope5 pool3 depth root state in
  let refine_ root = root in let refine_ depth = depth in
  assert (count out.#pool = (if reject then 7 else 6));
  let h6 = ghost_ (Pref.own (borrow_ out.#state)) in let d = ghost_ out.#history in let epoch = ghost_ out.#epoch in
  let trees6 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h6 x then finite h6 t else observe h6 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = copy_forest_at h5 trees5 epoch depth d x (refine_ u) in refine_ t) in
  let coverage6 : ((x : node Pref.t) @ immutable -> {u : unit | covered h6 0 out.#pool x}) @ total ghost = ghost_ (fun x ->
    coverage5 x; let u = () in let refine_ u = registered_covers h5 pool3 epoch depth d 0 x (refine_ u) in refine_ u) in
  let result = out.#value in ghost_ (let u = () in target_allocated h5 epoch depth d root result (refine_ u));
  let state = out.#state in let state : {t : Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ value = Pref.read result (borrow_ state) in let refine_ state = state in
  assert (value.level = Finite 2);
  (match value.desc with Arrow (a, b) -> let refine_ same = Pref.equal a b in assert (same = not reject) | _ -> assert false);
  let scope6 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h6 x then source_ok h6 x else H.at h6 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = Copy_model_proofs.history_scope h5 scope5 epoch depth d x (refine_ u) in refine_ u) in
  let pool6 = out.#pool in
  let state : {t : Pref.token | Pref.own t === h6 && pool_scoped h6 pool6} = refine_ state in
  let refine_ state = Generalize.close h6 0 pool6 state in
  let h7 = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (let u = () in closed_observe h6 0 pool6 result (refine_ u); closed_at_def h6 h7 0 pool6 result);
  let state : {t : Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ generalized = Pref.read result (borrow_ state) in assert (generalized.level = Generic);
  let trees7 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h7 x then finite h7 t else observe h7 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = closed_forest_at h6 trees6 0 pool6 x (refine_ u) in refine_ t) in
  ghost_ (let claim = true in
    let use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h7 rho x})) @ total ->
      {u : unit | claim}) @ total = fun _rho model -> model result; let u = () in refine_ u in
    with_finite_model h7 trees7 claim use; ());
  let scope7 : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h7 x then source_ok h7 x else H.at h7 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = Generalize_scheme_proofs.closed_scope h6 scope6 0 pool6 x (refine_ u) in refine_ u) in
  ghost_ (let u = () in closed_pool h6 scope6 0 pool6 (refine_ u));
  let refine_ state = state in
  let state : {t : Pref.token | Pref.own t === h7 && pool_scoped h7 pool6} = refine_ state in
  let result : {p : node Pref.t | H.mem h7 p} = refine_ result in
  let depth = 1 in let depth : {n : int | n >= 0} = refine_ depth in
  let refine_ second = Pooled_copy.instantiate h7 scope7 pool6 depth result state in
  let refine_ result = result in
  let refine_ same = Pref.equal result second.#value in assert (not same);
  assert (count second.#pool = (if reject then 11 else 9));
  ghost_ (coverage6 root); ()

let () = run false; run true
