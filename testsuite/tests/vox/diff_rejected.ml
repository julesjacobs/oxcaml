(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_diff_spec.mli vox_diff_spec.ml vox_diff.mli vox_diff.ml";
 readonly_files = "diff_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
*)

open Vox_diff_spec;;
[%%expect{|
|}]

let nonminimal () =
  let script = [Delete 97; Insert 97] in
  cost_def script;
  cost_def [Insert 97];
  cost_def [];
  minimum_cost_equation [97] [97];
  minimum_cost_equation [] [];
  size_def [];
  let computed : {s : script | cost s = minimum_cost [97] [97]} =
    script in
  Vox_diff.optimal_at [97] [97] computed [Keep 97];;
[%%expect{|
Line 10, characters 4-10:
10 |     script in
         ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_target old fresh =
  let result = Vox_diff.diff old fresh in
  match result with
  | Error _ -> ()
  | Ok script ->
    (() : {u : unit | apply old script === Some old});;
[%%expect{|
Line 6, characters 5-7:
6 |     (() : {u : unit | apply old script === Some old});;
         ^^
Error: Refinement could not be proved (counterexample)
|}]

let omit_validity old fresh other =
  let result = Vox_diff.diff old fresh in
  match result with
  | Error _ -> ()
  | Ok script ->
    let computed : {s : script |
      cost s = minimum_cost old fresh} = script in
    ghost_ (Vox_diff.optimal_at old fresh computed other);
    (() : {u : unit | cost script <= cost other});;
[%%expect{|
Line 9, characters 5-7:
9 |     (() : {u : unit | cost script <= cost other});;
         ^^
Error: Refinement could not be proved (counterexample)
|}]

let hidden_reconstruction = Vox_diff.Proof.reverse_into;;
[%%expect{|
Line 1, characters 28-42:
1 | let hidden_reconstruction = Vox_diff.Proof.reverse_into;;
                                ^^^^^^^^^^^^^^
Error: Unbound module "Vox_diff.Proof"
|}]

let hidden_metric_fuel = Vox_diff_spec.distance;;
[%%expect{|
Line 1, characters 25-47:
1 | let hidden_metric_fuel = Vox_diff_spec.distance;;
                             ^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Vox_diff_spec.distance"
|}]
