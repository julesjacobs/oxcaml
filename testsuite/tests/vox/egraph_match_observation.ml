(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml vox_egraph_language_spec.ml vox_egraph_rule_spec.ml vox_egraph_union_spec.ml vox_egraph_match_spec.ml vox_egraph_match_observation.ml vox_egraph_match_scan.ml vox_egraph_match_budget_spec.ml vox_egraph_match_bounded.ml egraph_match_observation.ml";
 { bytecode; }
*)

module O = Vox_egraph_match_observation
module Q = Vox_egraph_match_spec
module M = Vox_egraph_union_spec
module I = Vox_iarray

let () = ghost_ (
  let parents = [: 0; 0; 1 :] in
  let nodes = [: Some (Q.Int_lit 0); Some (Q.Int_lit 1);
    Some (Q.Add (0, 1)) :] in
  O.same nodes parents 3 0 2 ();
  M.root_def parents 2;
  M.parent_def parents 2;
  I.at_get parents (2);
  M.root_def parents 1;
  M.parent_def parents 1;
  I.at_get parents (1);
  M.root_def parents 0;
  M.parent_def parents 0;
  I.at_get parents (0);
  let _ : {u : unit | Q.same (O.observe nodes parents 3) 0 2} =
    () in
  O.node_at nodes parents 3 2 ();
  I.at_get nodes (2);
  let _ : {u : unit | Q.node (O.observe nodes parents 3) 2 ===
    Some (Q.Add (0, 1))} = () in
  ()); ()

module Scan = Vox_egraph_match_scan
module R = Vox_egraph_rule_spec

let () =
  let nodes = [: Some (Q.Int_lit 0); Some (Q.Int_lit 1);
    Some (Q.Add (0, 1)); Some (Q.Add (2, 0)) :] in
  let separate = [: 0; 1; 2; 3 :] in
  let merged = [: 0; 0; 2; 3 :] in
  let repeated = R.Add (R.Var 0, R.Var 0) in
  assert (not (Scan.matches nodes separate 4 repeated [0] 2));
  assert (Scan.matches nodes merged 4 repeated [1] 2);
  assert (Scan.matches nodes merged 4
    (R.Add (repeated, R.Var 0)) [0] 3);
  assert (not (Scan.matches nodes merged 4 repeated [-1] 2));
  assert (not (Scan.matches nodes merged 4 repeated [0] 4));
  assert (Scan.find_layer nodes merged 4 (R.Int_lit 0) [] [] [] 0 4 = Some 0);
  assert (Scan.find_layer nodes merged 4 (R.Int_lit 9) [] [] [] 0 4 = None);
  let cyclic = [: 0; 1; 0; 3 :] in
  assert (Scan.matches nodes cyclic 4
    (R.Add (R.Add (R.Int_lit 0, R.Int_lit 1), R.Int_lit 1)) [] 0)

module Bounded = Vox_egraph_match_bounded

let () =
  let nodes = [: Some (Q.Int_lit 0); Some (Q.Add (0, 0)) :] in
  let parents = [: 0; 1 :] in
  let pat = R.Add (R.Int_lit 0, R.Var 0) in
  assert (Bounded.classes nodes parents 2 pat [0] 0 = Bounded.Exhausted);
  assert (Bounded.classes nodes parents 2 pat [0] 6 = Bounded.Exhausted);
  assert (Bounded.classes nodes parents 2 pat [0] 7 = Bounded.Done ([1], 0));
  assert (Bounded.classes nodes parents 2 pat [0] 8 = Bounded.Done ([1], 1));
  assert (Bounded.classes nodes parents 2 pat [0] (-1) = Bounded.Exhausted)
