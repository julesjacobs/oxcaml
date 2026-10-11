(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_compact_diff_spec.ml vox_compact_diff.mli";
 readonly_files = "compact_diff_rejected.ml";
 { setup-ocamlc.opt-build-env; run-expect; check-program-output; }
*)

open Vox_compact_diff_spec;;
[%%expect{|
|}]

let unconditional_minimum (r : int optimal_diff) (other : int diff) =
  ghost_ (r.optimality other);
  (() : {u : unit | cost r.edits <= cost other});;
[%%expect{|
Line 3, characters 3-5:
3 |   (() : {u : unit | cost r.edits <= cost other});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 20-46:
3 |   (() : {u : unit | cost r.edits <= cost other});;
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let wrong_target (equal : int Vox_compact_diff.equality @ total)
    (old : int list) (fresh : int list) =
  let result = Vox_compact_diff.diff equal old fresh in
  let patched = ghost_ (Vox_compact_diff.apply equal old result.edits) in
  (() : {u : unit | patched === Some old});;
[%%expect{|
Line 5, characters 3-5:
5 |   (() : {u : unit | patched === Some old});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 20-40:
5 |   (() : {u : unit | patched === Some old});;
                        ^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let negative_keep () =
  let edits = [Keep (-1)] in
  patch_def edits [0];
  relates_def edits [0] [0];
  (() : {u : unit | relates edits [0] [0]});;
[%%expect{|
Line 5, characters 3-5:
5 |   (() : {u : unit | relates edits [0] [0]});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 20-41:
5 |   (() : {u : unit | relates edits [0] [0]});;
                        ^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let ghost_input (r : int optimal_diff) = print_int (List.length r.old);;
[%%expect{|
Line 1, characters 64-69:
1 | let ghost_input (r : int optimal_diff) = print_int (List.length r.old);;
                                                                    ^^^^^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]
