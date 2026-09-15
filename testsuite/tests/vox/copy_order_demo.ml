(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml level_unifier_proofs.ml level_unifier_metadata.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml copy_order_proofs.ml ordered_copy.ml copy_order_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Copy_order_proofs

let run (depth : {n : int | n >= 0}) =
  let refine_ depth = depth in
  let refine_ state = Pref.empty () in
  let generic = {desc = Var; level = Generic; memo = Empty_memo} in
  let refine_ step = Pref.alloc generic state in let a = step.value in let state = step.state in
  let finite = cell Var 0 in let refine_ step = Pref.alloc finite state in
  let boundary = step.value in let state = step.state in
  let inner = {desc = Arrow (a, a); level = Generic; memo = Empty_memo} in
  let refine_ step = Pref.alloc inner state in let pair = step.value in let state = step.state in
  let outer = {desc = Arrow (pair, boundary); level = Generic; memo = Empty_memo} in
  let refine_ step = Pref.alloc outer state in let root = step.value in let state = step.state in
  let alias = {desc = Link root; level = Generic; memo = Empty_memo} in
  let refine_ step = Pref.alloc alias state in let link = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; source_ok_def saved x;
    let u = () in refine_ u) in
  let bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || not (finite_node saved x) || below saved x depth}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; finite_node_def saved x;
    below_def saved x depth; at_level_def saved x; let u = () in refine_ u) in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered saved x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; ordered_def saved x; children_below_def saved desc 0;
    let u = () in refine_ u) in
  let base = Empty in ghost_ (pool_scoped_def saved base);
  let link : {p : node Pref.t | H.mem saved p} = refine_ link in
  let state : {t : Pref.token | Pref.own t === saved && pool_scoped saved base && depth >= 0} = refine_ state in
  let refine_ out = Ordered_copy.instantiate saved scope base depth bounds order link state in
  let refine_ link = link in
  let result = out.#value in let state = out.#state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let history = ghost_ out.#history in let epoch = ghost_ out.#epoch in
  let next_order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = copy_ordered saved depth bounds order epoch history x (refine_ u) in refine_ u) in
  let next_bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || not (finite_node h x) || below h x depth}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = copy_bounds saved depth bounds epoch history x (refine_ u) in refine_ u) in
  ghost_ (next_bounds result; next_bounds boundary; next_order result; next_order boundary; below_def h result depth);
  let state : {t : Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ value = Pref.read result (borrow_ state) in let refine_ state = state in
  assert (value.level = Finite depth);
  (match value.desc with
  | Arrow (_, b) -> let refine_ same = Pref.equal b boundary in assert same
  | _ -> failwith "expected arrow");
  ()

let () =
  let zero = 0 in let zero : {n : int | n >= 0} = refine_ zero in run zero;
  let three = 3 in let three : {n : int | n >= 0} = refine_ three in run three
