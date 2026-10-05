(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_diff_spec.ml vox_diff.mli vox_diff.ml";
 readonly_files = "diff_rejected.ml";
 { setup-ocamlc.opt-build-env; run-expect; check-program-output; }
*)

open Vox_diff_spec;;

let (equal @ total) (x : int) (y : int) :
    {same : bool | same = (x === y)} = x = y;;
[%%expect{|
val equal : (x : int) -> (y : int) -> {same : bool | same = (x === y)} =
  <fun>
|}]

let nonminimal () =
  let edits = [Delete 97; Insert 97] in
  let other = [Keep 97] in
  cost_def edits;
  cost_def [Insert 97];
  cost_def other;
  cost_def ([] : int diff);
  (() : {u : unit | cost edits <= cost other});;
[%%expect{|
Line 8, characters 3-5:
8 |   (() : {u : unit | cost edits <= cost other});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 8, characters 20-44:
8 |   (() : {u : unit | cost edits <= cost other});;
                        ^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let wrong_target (old : int list) (fresh : int list) =
  let result = Vox_diff.diff equal old fresh in
  let patched = ghost_ (Vox_diff.apply equal old result.edits) in
  (() : {u : unit | patched === Some old});;
[%%expect{|
Line 4, characters 3-5:
4 |   (() : {u : unit | patched === Some old});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 20-40:
4 |   (() : {u : unit | patched === Some old});;
                        ^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let omit_validity (old : int list) (fresh : int list) (other : int diff) =
  let result = Vox_diff.diff equal old fresh in
  ghost_ (result.optimality other);
  (() : {u : unit | cost result.edits <= cost other});;
[%%expect{|
Line 4, characters 3-5:
4 |   (() : {u : unit | cost result.edits <= cost other});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 20-51:
4 |   (() : {u : unit | cost result.edits <= cost other});;
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let hidden_reconstruction = Vox_diff.Proof.reverse_into;;
[%%expect{|
Line 1, characters 28-42:
1 | let hidden_reconstruction = Vox_diff.Proof.reverse_into;;
                                ^^^^^^^^^^^^^^
Error: Unbound module "Vox_diff.Proof"
|}]

let hidden_apply = Vox_diff.apply_edits;;
[%%expect{|
Line 1, characters 19-39:
1 | let hidden_apply = Vox_diff.apply_edits;;
                       ^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Vox_diff.apply_edits"
|}]

let (dishonest_equal @ total) (x : int) (y : int) = true;;
[%%expect{|
val dishonest_equal : int -> int -> bool = <fun>
|}]

let bad_comparison () = Vox_diff.diff dishonest_equal [1] [2];;
[%%expect{|
Line 1, characters 38-53:
1 | let bad_comparison () = Vox_diff.diff dishonest_equal [1] [2];;
                                          ^^^^^^^^^^^^^^^
Error: The value "dishonest_equal" has type "int -> int -> bool"
       but an expression was expected of type
         "(x : int) -> (y : int) -> {same : bool | same = (x === y)}"
|}]

let hidden_distance = Vox_diff_spec.minimum_cost;;
[%%expect{|
Line 1, characters 22-48:
1 | let hidden_distance = Vox_diff_spec.minimum_cost;;
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Vox_diff_spec.minimum_cost"
|}]
