(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_egraph_language_spec.ml vox_egraph_rule_spec.ml vox_egraph_match_spec.ml egraph_match_spec.ml";
 { native; }
*)

module M = Vox_egraph_match_spec
module R = Vox_egraph_rule_spec

let () = ghost_ (
  let graph = {M.count = 1; nodes = [: Some (M.Int_lit 0) :];
    classes = [: 0 :]} in
  M.matches_def graph (R.Int_lit 0) [] 0;
  M.classes_def graph (R.Int_lit 0) [];
  M.collect_def graph (R.Int_lit 0) [] [] [] 1;
  M.collect_def graph (R.Int_lit 0) [] [] [] 0;
  M.class_id_def graph 0;
  M.node_def graph 0;
  M.layer_def graph (R.Int_lit 0) [] [] [] (M.Int_lit 0);
  M.in_classes_def graph 0 [0];
  M.member_def 0 [0];
  let _ : {u : unit | M.matches graph (R.Int_lit 0) [] 0} = () in
  ());
  ()

let () =
  let graph = {M.count = 3;
    nodes = [: Some M.Int_input; Some (M.Int_lit 0); Some (M.Add (0, 1)) :];
    classes = [: 0; 1; 2 :]} in
  let repeated = R.Add (R.Var 0, R.Var 0) in
  assert (not (M.matches graph repeated [0] 2));
  let merged = {graph with classes = [: 0; 0; 2 :]} in
  assert (M.matches merged repeated [0] 2);
  let cyclic = {graph with classes = [: 0; 1; 0 :]} in
  let nested = R.Add (R.Add (R.Var 0, R.Int_lit 0), R.Int_lit 0) in
  assert (M.matches cyclic nested [2] 0);
  assert (not (M.matches cyclic nested [3] 0));
  assert (not (M.matches cyclic nested [2] 3));
  assert (not (M.matches cyclic R.Bool_input [] 0))
