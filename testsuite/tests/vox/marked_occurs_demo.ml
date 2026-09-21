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
  let state = Pref.empty () in
  let v = cell Var 0 in let step = Pref.alloc v state in
  let needle = step.value in let state = step.state in
  let step = Pref.alloc v state in
  let leaf = step.value in let state = step.state in
  let pair_desc = Arrow (leaf, leaf) in let pair_node = cell pair_desc 0 in
  let step = Pref.alloc pair_node state in
  let pair = step.value in let state = step.state in
  let right = if hit then needle else pair in
  let root_desc = Arrow (pair, right) in let root_node = cell root_desc 0 in
  let step = Pref.alloc root_node state in
  let root = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Var in cell_def desc 0; cell_def pair_desc 0; cell_def root_desc 0;
    active_def h needle; active_def h leaf; active_def h pair; active_def h right;
    at_level_def h needle; at_level_def h leaf; at_level_def h pair; at_level_def h right;
    active_def h x; at_level_def h x; source_ok_def h x; finite_scope_def h x;
    let u = () in u) in
  let unmarked : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at h x with
      None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Var in cell_def desc 0; cell_def pair_desc 0; cell_def root_desc 0;
    active_def h x; at_level_def h x; let u = () in u) in
  ghost_ (active_def h root; at_level_def h root; cell_def root_desc 0);
  let state : {t : Pref.token | Pref.own t === h && H.mem h root && active h root} = state in
  let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h)} in
  let scope_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost x) || finite_scope h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (scope)} in
  let unmarked_witness3 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness1.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (unmarked)} in
  let state_argument4 = state in
  let out = Marked_occurs.occurs h_witness1 scope_witness2 unmarked_witness3 needle root (state_argument4) in
  assert (out.#found = hit);
  let state = out.#state in let d = ghost_ out.#marks in
  let after = ghost_ (Pref.own (borrow_ state)) in
  let frame : ((x : node Pref.t) @ immutable ->
    {u : unit | H.mem h x === H.mem after x && H.at h x === H.at after x}) @ total ghost = ghost_ (fun x ->
    let u = () in let u = restored_at h needle d x (u) in u) in
  ghost_ (frame leaf; frame pair; frame root);
  let state : {t : Pref.token | H.mem (Pref.own t) leaf} = state in
  let a = Pref.read leaf (borrow_ state) in assert (not a.visited);
  let state : {t : Pref.token | H.mem (Pref.own t) pair} = state in
  let a = Pref.read pair (borrow_ state) in assert (not a.visited);
  let state : {t : Pref.token | H.mem (Pref.own t) root} = state in
  let a = Pref.read root (borrow_ state) in
  assert (not a.visited);
  ()

let () = run false; run true
