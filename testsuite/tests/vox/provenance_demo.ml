(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml provenance_demo.ml";
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
  ghost_ (pool_scoped_def h0 pool0);
  let desc1 : desc = Var in
  ghost_ (children_below_def h0 desc1 1);
  let state : {t : Pref.token | Pref.own t === h0 &&
    pool_scoped h0 pool0 && 1 >= 0 && children_below h0 desc1 1} = refine_ state in
  let refine_ r = Pooled_allocator.allocate h0 1 desc1 pool0 state in
  let p1 = r.#value in let pool1 = r.#pool in let state = r.#state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in let v1 = cell desc1 1 in
  ghost_ (cell_def desc1 1; payload_scoped_def h0 v1);
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
  let state : {t : Pref.token | Pref.own t === h1 &&
    pool_scoped h1 pool1 && 2 >= 0 && children_below h1 desc2 2} = refine_ state in
  let refine_ r = Pooled_allocator.allocate h1 2 desc2 pool1 state in
  let p2 = r.#value in let pool2 = r.#pool in let state = r.#state in
  let h2 = ghost_ (Pref.own (borrow_ state)) in let v2 = cell desc2 2 in
  ghost_ (cell_def desc2 2; payload_scoped_def h1 v2);
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
  let state : {t : Pref.token | Pref.own t === h2 &&
    pool_scoped h2 pool2 && 2 >= 0 && children_below h2 desc3 2} = refine_ state in
  let refine_ r = Pooled_allocator.allocate h2 2 desc3 pool2 state in
  let p3 = r.#value in let pool3 = r.#pool in let state = r.#state in
  let h3 = ghost_ (Pref.own (borrow_ state)) in let v3 = cell desc3 2 in
  ghost_ (cell_def desc3 2; payload_scoped_def h2 v3);
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
  let state : {t : Pref.token | Pref.own t === h3 && H.mem h3 p1 && H.mem h3 p2 &&
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
  let state = solved.#state in
  let state : {t : Pref.token | Pref.own t === h4 && pool_scoped h4 pool3} = refine_ state in
  let refine_ state = Generalize.close h4 1 pool3 state in
  let h5 = ghost_ (Pref.own (borrow_ state)) in
  let _classification : ((x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
      {u : unit | not (active h4 x) || not (at_level h5 x === Generic) ||
        not (originates h1 h4 1 x origin)}) @ total ghost = ghost_ (fun x origin ->
    let u = () in coverage4 x;
    if active h4 x && at_level h5 x === Generic then (
      generic_excludes_origin h1 h4 1 pool3 roots4 order4 x origin (refine_ u); refine_ u)
    else refine_ u) in
  let _nongeneric : ((x : node Pref.t) @ immutable ->
      {o : origin | not (active h4 x) || at_level h5 x === Generic ||
        originates h1 h4 1 x o} @ immutable) @ total ghost = ghost_ (fun x ->
    coverage4 x; let u = () in if active h4 x && not (at_level h5 x === Generic) then (
      let refine_ o = nongeneric_origin h1 h4 1 pool3 prior4 x (refine_ u) in refine_ o)
    else (let o = Origin (x, Stop) in refine_ o)) in
  ghost_ (let u = () in unified_frame h3 p1 p2 ok h4 d p2 (refine_ u);
    Generalize_proofs.closed_observe h4 1 pool3 p2 (refine_ u);
    closed_at_def h4 h5 1 pool3 p2);
  let state : {t : Pref.token | H.mem (Pref.own t) p2} = refine_ state in
  let refine_ v = Pref.read p2 (borrow_ state) in let refine_ state = state in
  assert (v.level = Finite 1);
  ghost_ (let u = () in unified_frame h3 p1 p2 ok h4 d p3 (refine_ u);
    Generalize_proofs.closed_observe h4 1 pool3 p3 (refine_ u);
    closed_at_def h4 h5 1 pool3 p3);
  let state : {t : Pref.token | H.mem (Pref.own t) p3} = refine_ state in
  let refine_ v = Pref.read p3 (borrow_ state) in let refine_ state = state in
  assert (v.level = Generic);
  ()
