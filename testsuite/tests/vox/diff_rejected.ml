(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_diff_spec.ml vox_diff_metric.ml vox_diff.mli vox_diff.ml";
 readonly_files = "diff_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.byte-build-env; ocamlc.byte; run-expect; check-program-output; }
*)

open Vox_diff_spec;;
[%%expect{|
|}]

let nonminimal () =
  let script = [Delete 97; Insert 97] in
  cost_def script;
  cost_def [Insert 97];
  cost_def [];
  Vox_diff_metric.equation [97] [97];
  Vox_diff_metric.equation [] [];
  size_def [];
  let computed : {s : script | cost s = Vox_diff_metric.metric [97] [97]} =
    refine_ script in
  Vox_diff.optimal_at [97] [97] computed [Keep 97];;
[%%expect{|
Line 10, characters 4-18:
10 |     refine_ script in
         ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_target old fresh =
  let refine_ result = Vox_diff.diff old fresh in
  match result with
  | Error _ -> ()
  | Ok script ->
    let u = () in
    (refine_ u : {u : unit | apply old script === Some old});;
[%%expect{|
Line 7, characters 5-14:
7 |     (refine_ u : {u : unit | apply old script === Some old});;
         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let omit_validity old fresh other =
  let refine_ result = Vox_diff.diff old fresh in
  match result with
  | Error _ -> ()
  | Ok script ->
    let computed : {s : script |
      cost s = Vox_diff_metric.metric old fresh} = refine_ script in
    ghost_ (Vox_diff.optimal_at old fresh computed other);
    let u = () in
    (refine_ u : {u : unit | cost script <= cost other});;
[%%expect{|
Line 10, characters 5-14:
10 |     (refine_ u : {u : unit | cost script <= cost other});;
          ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
