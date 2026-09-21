(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml relative_generalization_demo.ml";
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
open Pooled_spec
open Pooled_proofs
open Pooled_allocation_proofs
open Level_finite_spec
open Level_finite_proofs
open Forest_transport
open Relative_generalization
open Provenance_spec
open Provenance_proofs

let () =
  let refine_ state = Pref.empty () in
  let h0 = ghost_ (Pref.own (borrow_ state)) in let pool0 = Empty in
  let scope0 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h0 x then source_ok h0 x else H.at h0 x === None})
      @ total ghost = ghost_ (fun x -> let u = () in refine_ u) in
  let order0 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h0 x})
      @ total ghost = ghost_ (fun x -> ordered_def h0 x; let u = () in refine_ u) in
  let coverage0 : ((x : node Pref.t) @ immutable -> {u : unit | covered h0 1 pool0 x})
      @ total ghost = ghost_ (fun x -> covered_def h0 1 pool0 x; let u = () in refine_ u) in
  let trees0 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h0 x then finite h0 t else observe h0 x === None)} @ immutable) @ total ghost =
    ghost_ (fun x -> let t = Free x in tree_root_def t; observe_def h0 x; refine_ t) in
  ghost_ (pool_scoped_def h0 pool0);
  let desc1 : desc = Var in
  ghost_ (children_below_def h0 desc1 1);
  let state : {t : node Pref.token | Pref.own t === h0 &&
    pool_scoped h0 pool0 && 1 >= 0 && children_below h0 desc1 1} = refine_ state in
  let refine_ r = Pooled_allocator.allocate h0 1 desc1 pool0 state in
  let p1 = r.#value in let pool1 = r.#pool in let state = r.#state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in let v1 = cell desc1 1 in
  ghost_ (cell_def desc1 1; payload_scoped_def h0 v1);
  ghost_ (allocatable_def h0 v1);
  let trees1 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h0 trees0 p1 v1 x (refine_ u) in refine_ t) in
  let scope1 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h1 x then source_ok h1 x else H.at h1 x === None})
      @ total ghost = ghost_ (fun x -> let u = () in
    let refine_ u = allocation_scope_at h0 scope0 p1 v1 x (refine_ u) in refine_ u) in
  let order1 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h1 x})
      @ total ghost = ghost_ (fun x -> order0 x; let u = () in
    let refine_ u = allocation_ordered h0 p1 desc1 1 x (refine_ u) in refine_ u) in
  let coverage1 : ((x : node Pref.t) @ immutable -> {u : unit | covered h1 1 pool1 x})
      @ total ghost = ghost_ (fun x -> coverage0 x; let u = () in
    let refine_ u = allocation_coverage h0 p1 v1 pool0 1 x (refine_ u) in refine_ u) in
  let prior1 : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h1 x 1) || originates h1 h1 1 x o} @ immutable)
      @ total ghost = ghost_ (fun x -> let refine_ o = initial_origin h1 1 x in refine_ o) in
  let desc2 : desc = Var in
  ghost_ (children_below_def h1 desc2 2);
  let state : {t : node Pref.token | Pref.own t === h1 &&
    pool_scoped h1 pool1 && 2 >= 0 && children_below h1 desc2 2} = refine_ state in
  let refine_ r = Pooled_allocator.allocate h1 2 desc2 pool1 state in
  let p2 = r.#value in let pool2 = r.#pool in let state = r.#state in
  let h2 = ghost_ (Pref.own (borrow_ state)) in let v2 = cell desc2 2 in
  ghost_ (cell_def desc2 2; payload_scoped_def h1 v2);
  ghost_ (allocatable_def h1 v2);
  let trees2 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h1 trees1 p2 v2 x (refine_ u) in refine_ t) in
  let scope2 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h2 x then source_ok h2 x else H.at h2 x === None})
      @ total ghost = ghost_ (fun x -> let u = () in
    let refine_ u = allocation_scope_at h1 scope1 p2 v2 x (refine_ u) in refine_ u) in
  let order2 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h2 x})
      @ total ghost = ghost_ (fun x -> order1 x; let u = () in
    let refine_ u = allocation_ordered h1 p2 desc2 2 x (refine_ u) in refine_ u) in
  let coverage2 : ((x : node Pref.t) @ immutable -> {u : unit | covered h2 1 pool2 x})
      @ total ghost = ghost_ (fun x -> coverage1 x; let u = () in
    let refine_ u = allocation_coverage h1 p2 v2 pool1 1 x (refine_ u) in refine_ u) in
  let prior2 : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h2 x 1) || originates h1 h2 1 x o} @ immutable)
      @ total ghost = ghost_ (fun x -> scope1 p2; let u = () in
    let refine_ o = allocation_origin h1 h1 1 prior1 p2 v2 x (refine_ u) in refine_ o) in
  let desc3 : desc = Var in
  ghost_ (children_below_def h2 desc3 2);
  let state : {t : node Pref.token | Pref.own t === h2 &&
    pool_scoped h2 pool2 && 2 >= 0 && children_below h2 desc3 2} = refine_ state in
  let refine_ r = Pooled_allocator.allocate h2 2 desc3 pool2 state in
  let p3 = r.#value in let pool3 = r.#pool in let state = r.#state in
  let h3 = ghost_ (Pref.own (borrow_ state)) in let v3 = cell desc3 2 in
  ghost_ (cell_def desc3 2; payload_scoped_def h2 v3);
  ghost_ (allocatable_def h2 v3);
  let trees3 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h2 trees2 p3 v3 x (refine_ u) in refine_ t) in
  let scope3 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h3 x then source_ok h3 x else H.at h3 x === None})
      @ total ghost = ghost_ (fun x -> let u = () in
    let refine_ u = allocation_scope_at h2 scope2 p3 v3 x (refine_ u) in refine_ u) in
  let order3 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h3 x})
      @ total ghost = ghost_ (fun x -> order2 x; let u = () in
    let refine_ u = allocation_ordered h2 p3 desc3 2 x (refine_ u) in refine_ u) in
  let coverage3 : ((x : node Pref.t) @ immutable -> {u : unit | covered h3 1 pool3 x})
      @ total ghost = ghost_ (fun x -> coverage2 x; let u = () in
    let refine_ u = allocation_coverage h2 p3 v3 pool2 1 x (refine_ u) in refine_ u) in
  let prior3 : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h3 x 1) || originates h1 h3 1 x o} @ immutable)
      @ total ghost = ghost_ (fun x -> scope2 p3; let u = () in
    let refine_ o = allocation_origin h1 h2 1 prior2 p3 v3 x (refine_ u) in refine_ o) in
  let finite_scope3 : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h3 x) || finite_scope h3 x}) @ total ghost = ghost_ (fun x ->
    scope3 x; order3 x; let u = () in
    if H.mem h3 x then (ordered_scope h3 x (refine_ u); refine_ u) else refine_ u) in
  ghost_ (let u = () in allocation_below h1 p2 v2 p1 1 (refine_ u);
    allocation_below h2 p3 v3 p1 1 (refine_ u);
    allocation_below h2 p3 v3 p2 2 (refine_ u);
    below_def h3 p1 1; active_def h3 p1; below_def h3 p2 2; active_def h3 p2);
  let state : {t : node Pref.token | Pref.own t === h3 && H.mem h3 p1 && H.mem h3 p2 &&
    active h3 p1 && active h3 p2} = refine_ state in
  let unmarked : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at h3 x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    cell_def desc1 1; cell_def desc2 2; cell_def desc3 2;
    let u = () in refine_ u) in
  let refine_ solved = Level_unifier.unify h3 finite_scope3 unmarked p1 p2 state in
  assert solved.#ok;
  let ok = solved.#ok in let d = ghost_ solved.#derivation in
  let h4 = ghost_ (Pref.own (borrow_ solved.#state)) in
  let prior4 : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h4 x 1) || originates h1 h4 1 x o} @ immutable)
      @ total ghost = ghost_ (fun x -> let u = () in
    let refine_ o = unified_origin h1 h3 1 prior3 p1 p2 ok h4 d x (refine_ u) in refine_ o) in
  let roots4 : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below h1 x 1) || below h4 x 1}) @ total ghost = ghost_ (fun x ->
    let u = () in if below h1 x 1 then (
      allocation_below h1 p2 v2 x 1 (refine_ u);
      allocation_below h2 p3 v3 x 1 (refine_ u);
      unified_below h3 p1 p2 ok h4 d 1 x (refine_ u); refine_ u) else refine_ u) in
  let order4 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h4 x})
      @ total ghost = ghost_ (fun x -> order3 x; let u = () in
    let refine_ u = unified_ordered h3 p1 p2 ok h4 d x (refine_ u) in refine_ u) in
  let coverage4 : ((x : node Pref.t) @ immutable -> {u : unit | covered h4 1 pool3 x})
      @ total ghost = ghost_ (fun x -> coverage3 x; let u = () in
    let refine_ u = coverage_after_unify h3 p1 p2 ok h4 d 1 pool3 x (refine_ u) in refine_ u) in
  ghost_ (let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h4 x) || source_ok h4 x}) @ total = fun x ->
      let u = () in unified_scope h3 finite_scope3 p1 p2 ok h4 d x (refine_ u);
      finite_scope_def h4 x; refine_ u in
    let members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (listed pool3 x) || H.mem h4 x}) @ total = fun x ->
      let u = () in if listed pool3 x then (pool_member h3 pool3 x (refine_ u);
        unified_frame h3 p1 p2 ok h4 d x (refine_ u); refine_ u) else refine_ u in
    pool_from_members h4 scope pool3 members);
  let trees4 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h4 x then finite h4 t else observe h4 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = unified_finite_at h3 trees3 p1 p2 ok h4 d x (refine_ u) in refine_ t) in
  let scope4 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h4 x then source_ok h4 x else H.at h4 x === None}) @ total ghost = ghost_ (fun x ->
    let refine_ t = trees4 x in observe_def h4 x;
    let u = () in unified_scope h3 finite_scope3 p1 p2 ok h4 d x (refine_ u); finite_scope_def h4 x; refine_ u) in
  let desc5 : desc = Arrow (p3, p2) in
  ghost_ (let u = () in
    below_def h3 p3 2; unified_below h3 p1 p2 ok h4 d 2 p3 (refine_ u);
    below_def h3 p2 2; unified_below h3 p1 p2 ok h4 d 2 p2 (refine_ u);
    children_below_def h4 desc5 2);
  let state = solved.#state in
  let state : {t : node Pref.token | Pref.own t === h4 && pool_scoped h4 pool3 &&
    2 >= 0 && children_below h4 desc5 2} = refine_ state in
  let refine_ allocated = Pooled_allocator.allocate h4 2 desc5 pool3 state in
  let root = allocated.#value in let pool5 = allocated.#pool in let state = allocated.#state in
  let h5 = ghost_ (Pref.own (borrow_ state)) in let v5 = cell desc5 2 in
  ghost_ (cell_def desc5 2; payload_scoped_def h4 v5; allocatable_def h4 v5;
    below_def h4 p2 2; below_def h4 p3 2);
  let trees5 : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h5 x then finite h5 t else observe h5 x === None)} @ immutable) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ t = allocation_finite_at h4 trees4 root v5 x (refine_ u) in refine_ t) in
  let scope5 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h5 x then source_ok h5 x else H.at h5 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = allocation_scope_at h4 scope4 root v5 x (refine_ u) in refine_ u) in
  let order5 : ((x : node Pref.t) @ immutable -> {u : unit | ordered h5 x}) @ total ghost = ghost_ (fun x ->
    order4 x; let u = () in let refine_ u = allocation_ordered h4 root desc5 2 x (refine_ u) in refine_ u) in
  let coverage5 : ((x : node Pref.t) @ immutable -> {u : unit | covered h5 1 pool5 x}) @ total ghost = ghost_ (fun x ->
    coverage4 x; let u = () in let refine_ u = allocation_coverage h4 root v5 pool3 1 x (refine_ u) in refine_ u) in
  let prior5 : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h5 x 1) || originates h1 h5 1 x o} @ immutable) @ total ghost = ghost_ (fun x ->
    scope4 root; let u = () in let refine_ o = allocation_origin h1 h4 1 prior4 root v5 x (refine_ u) in refine_ o) in
  let roots5 : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below h1 x 1) || below h5 x 1}) @ total ghost = ghost_ (fun x ->
    roots4 x; let u = () in if below h1 x 1 then (allocation_below h4 root v5 x 1 (refine_ u); refine_ u) else refine_ u) in
  let forest5 : ((x : node Pref.t) @ immutable ->
      {t : bounded | not (H.mem h5 x) || (bound_root t === x && unfolded h5 t)} @ immutable) @ total ghost = ghost_ (fun x ->
    let refine_ t = trees5 x in let u = () in unfolding_root t;
    if H.mem h5 x then (unfolding_valid h5 t (refine_ u); let s = unfolding t in refine_ s)
    else (let s = unfolding t in refine_ s)) in
  let state : {t : node Pref.token | Pref.own t === h5 && pool_scoped h5 pool5} = refine_ state in
  let refine_ state = Generalize.close h5 1 pool5 state in
  let h6 = ghost_ (Pref.own (borrow_ state)) in
  let scope6 : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h6 x then source_ok h6 x else H.at h6 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = Generalize_scheme_proofs.closed_scope h5 scope5 1 pool5 x (refine_ u) in refine_ u) in
  ghost_ (let u = () in closed_pool h5 scope5 1 pool5 (refine_ u);
    closed_observe h5 1 pool5 root (refine_ u); closed_at_def h5 h6 1 pool5 root);
  let root : {p : node Pref.t | H.mem h6 p} = refine_ root in
  let depth = 2 in let depth : {n : int | n >= 0} = refine_ depth in
  let state : {t : node Pref.token | Pref.own t === h6 && pool_scoped h6 pool5} = refine_ state in
  let refine_ copied = Pooled_copy.instantiate h6 scope6 pool5 depth root state in
  let refine_ root = root in let refine_ depth = depth in
  let cd = ghost_ copied.#history in let epoch = ghost_ copied.#epoch in
  let q = copied.#value in
  ghost_ (
    let use_model : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (raw_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h5 rho x})) @ total ->
        {u : unit | true}) @ total = fun rho raw_model ->
      let model : ((x : node Pref.t) @ immutable -> {u : unit | equation h5 rho x}) @ total = fun x ->
        raw_model x; node_equation_def h5 rho x; observe_def h5 x; equation_def h5 rho x;
        let u = () in refine_ u in
      let[@def] choices : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
      let refine_ tree = forest5 root in
      let use_eta : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h5 eta x})) @ total ->
          (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (below h1 x 1) || eta x === rho x})) @ total ->
          {u : unit | eta (bound_root tree) === interpret rho choices (scheme h5 1 tree)} ->
          {u : unit | true}) @ total = fun eta eta_model equal fit ->
        let refine_ fit = fit in
        let equal : ((x : node Pref.t) @ immutable -> {u : unit | not (below h1 x 1) || rho x === eta x}) @ total = refine_ equal in
        let u = () in
        relative_interpret h1 h5 1 prior5 order5 rho model eta eta_model equal tree (refine_ u);
        let use_copy : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap (closed_heap h5 1 pool5) epoch depth cd) tau x})) @ total ->
            (preserved : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h5 x) || tau x === rho x})) @ total ->
            {u : unit | tau q === eta (bound_root tree)} ->
            {u : unit | tau q === interpret rho choices (scheme h5 1 tree)}) @ total = fun tau next preserved value ->
          let refine_ value = value in next q; preserved root; equal p1; eta_model root;
          let u = () in refine_ u in
        let use_copy : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap (closed_heap h5 1 pool5) epoch depth cd) tau x})) @ total ->
            (preserved : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h5 x) || tau x === rho x})) @ total ->
            {u : unit | tau q === eta (bound_root tree)} -> {u : unit | true}) @ total = refine_ use_copy in
        let refine_ u = with_relative_copy h1 h5 1 pool5 scope5 forest5 coverage5 order5 prior5
          rho model eta eta_model equal epoch depth cd tree q (refine_ u) true use_copy in refine_ u in
      let u = () in
      let refine_ u = with_relative_model h1 h5 1 pool5 scope5 forest5 coverage5 order5 roots5
        rho model choices tree (refine_ u) true use_eta in refine_ u in
    with_finite_model h5 trees5 true use_model; ());
  ghost_ (let u = () in target_allocated h6 epoch depth cd root q (refine_ u));
  let state = copied.#state in
  let state : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ value = Pref.read q (borrow_ state) in
  assert (value.level = Finite 2);
  (match value.desc with
  | Arrow (arg, result) ->
      let refine_ same = Pref.equal arg p3 in assert (not same);
      let refine_ same = Pref.equal result p2 in assert same
  | _ -> failwith "expected copied arrow");
  ()
