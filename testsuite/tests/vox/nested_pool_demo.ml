(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml nested_pool_demo.ml";
 { native; }
*)
open Copy_spec
open Level_spec
open Generalize_spec
open Pooled_spec
open Nested_pool_spec
open Nested_pool_proofs

let rec count = function Empty -> 0 | Entry (_, rest) -> 1 + count rest

let () =
  let refine_ state = Pref.empty () in
  let pool0 = Empty in
  let h0 = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (pool_scoped_def h0 pool0);
  let desc1 : desc = Var in
  ghost_ (children_below_def h0 desc1 0);
  let state : {t : node Pref.token | Pref.own t === h0 &&
    pool_scoped h0 pool0 && 0 >= 0 && children_below h0 desc1 0} = refine_ state in
  let allocation_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h0)} in
  let refine_ state = state in
  let refine_ r = Pooled_allocator.allocate allocation_heap 0 desc1 pool0 (refine_ state) in
  let p1 = r.#value in let pool1 = r.#pool in let state = r.#state in
  let h1 = ghost_ (Pref.own (borrow_ state)) in
  let desc2 : desc = Var in
  ghost_ (children_below_def h1 desc2 1);
  let state : {t : node Pref.token | Pref.own t === h1 &&
    pool_scoped h1 pool1 && 1 >= 0 && children_below h1 desc2 1} = refine_ state in
  let allocation_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h1)} in
  let refine_ state = state in
  let refine_ r = Pooled_allocator.allocate allocation_heap 1 desc2 pool1 (refine_ state) in
  let p2 = r.#value in let pool2 = r.#pool in let state = r.#state in
  let h2 = ghost_ (Pref.own (borrow_ state)) in
  let desc3 : desc = Var in
  ghost_ (children_below_def h2 desc3 2);
  let state : {t : node Pref.token | Pref.own t === h2 &&
    pool_scoped h2 pool2 && 2 >= 0 && children_below h2 desc3 2} = refine_ state in
  let allocation_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h2)} in
  let refine_ state = state in
  let refine_ r = Pooled_allocator.allocate allocation_heap 2 desc3 pool2 (refine_ state) in
  let p3 = r.#value in let pool3 = r.#pool in let state = r.#state in
  let h3 = ghost_ (Pref.own (borrow_ state)) in
  let parent = Entry (p1, Empty) in
  let child = Entry (p3, Entry (p2, Empty)) in
  ghost_ (pool_scoped_def h3 pool3;
    pool_scoped_def h3 pool2; pool_scoped_def h3 pool1;
    let empty = Empty in let tail = Entry (p2, empty) in
    pool_scoped_def h3 empty; pool_scoped_def h3 tail;
    pool_scoped_def h3 child; pool_scoped_def h3 parent);
  let state : {t : node Pref.token | Pref.own t === h3 &&
    pool_scoped h3 child && pool_scoped h3 parent} = refine_ state in
  let close_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h3)} in
  let refine_ state = state in
  let refine_ r = Nested_pool.close close_heap 1 child parent (refine_ state) in
  let after = ghost_ (Pref.own (borrow_ r.#state)) in
  let parent = r.#parent in let state = r.#state in
  assert (count parent = 2);
  ghost_ (let u = () in
    Generalize_proofs.closed_observe h3 1 child p3 (refine_ u);
    closed_at_def h3 after 1 child p3);
  let state : {t : node Pref.token | H.mem (Pref.own t) p3} = refine_ state in
  let refine_ v = Pref.read p3 (borrow_ state) in
  let refine_ state = state in
  assert (v.level = Generic);
  let empty = Empty in ghost_ (pool_scoped_def after empty);
  let state : {t : node Pref.token | Pref.own t === after &&
    pool_scoped after parent && pool_scoped after empty} = refine_ state in
  let close_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (after)} in
  let refine_ state = state in
  let refine_ r = Nested_pool.close close_heap 0 parent empty (refine_ state) in
  assert (count r.#parent = 1)
