(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml level_spec.ml lower_locality_spec.ml level_proofs.ml level_unifier_spec.ml marked_occurs_proofs.ml marked_occurs.ml marked_occurs_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Marked_occurs_proofs

let run hit =
  let refine_ state = Pref.empty () in
  let v = cell Var 0 in let refine_ step = Pref.alloc v state in
  let needle = step.value in let state = step.state in
  let refine_ step = Pref.alloc v state in
  let leaf = step.value in let state = step.state in
  let pair_desc = Arrow (leaf, leaf) in let pair_node = cell pair_desc 0 in
  let refine_ step = Pref.alloc pair_node state in
  let pair = step.value in let state = step.state in
  let right = if hit then needle else pair in
  let root_desc = Arrow (pair, right) in let root_node = cell root_desc 0 in
  let refine_ step = Pref.alloc root_node state in
  let root = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Var in cell_def desc 0; cell_def pair_desc 0; cell_def root_desc 0;
    active_def h needle; active_def h leaf; active_def h pair; active_def h right;
    at_level_def h needle; at_level_def h leaf; at_level_def h pair; at_level_def h right;
    active_def h x; at_level_def h x; source_ok_def h x; finite_scope_def h x;
    let u = () in refine_ u) in
  let unmarked : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at h x with
      None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Var in cell_def desc 0; cell_def pair_desc 0; cell_def root_desc 0;
    active_def h x; at_level_def h x; let u = () in refine_ u) in
  ghost_ (active_def h root; at_level_def h root; cell_def root_desc 0);
  let state : {t : node Pref.token | Pref.own t === h && H.mem h root && active h root} = refine_ state in
  let refine_ out = Marked_occurs.occurs h scope unmarked needle root state in
  assert (out.#found = hit);
  let state = out.#state in let d = ghost_ out.#marks in
  let after = ghost_ (Pref.own (borrow_ state)) in
  let frame : ((x : node Pref.t) @ immutable ->
    {u : unit | H.mem h x === H.mem after x && H.at h x === H.at after x}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = restored_at h needle d x (refine_ u) in refine_ u) in
  ghost_ (frame leaf; frame pair; frame root);
  let state : {t : node Pref.token | H.mem (Pref.own t) leaf} = refine_ state in
  let refine_ a = Pref.read leaf (borrow_ state) in let refine_ state = state in
  assert (not a.visited);
  let state : {t : node Pref.token | H.mem (Pref.own t) pair} = refine_ state in
  let refine_ a = Pref.read pair (borrow_ state) in let refine_ state = state in
  assert (not a.visited);
  let state : {t : node Pref.token | H.mem (Pref.own t) root} = refine_ state in
  let refine_ a = Pref.read root (borrow_ state) in
  assert (not a.visited);
  ()

let () = run false; run true
