(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli sorted_array.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "sorted_array.mli";
 ocamlc.opt;
 expect;
*)

#directory "ocamlc.opt";;

let invalid_representation : Sorted_array.t = [: 2; 1 :];;
[%%expect{|
Line 1, characters 46-56:
1 | let invalid_representation : Sorted_array.t = [: 2; 1 :];;
                                                  ^^^^^^^^^^
Error: This expression has type "'a iarray"
       but an expression was expected of type "Sorted_array.t"
|}]

let invalid_removal () =
  let source = Sorted_array.empty in
  let index = 0 in
  let result = Sorted_array.remove_at source index () in
  ignore result;;
[%%expect{|
Line 4, characters 51-53:
4 |   let result = Sorted_array.remove_at source index () in
                                                       ^^
Error: Refinement could not be proved (counterexample)
File "sorted_array.mli", line 29, characters 31-55:
  The refinement is stated here.
|}]

let invalid_search_result (source : Sorted_array.t) (value : int) =
  match Sorted_array.find_first source value with
  | None -> (() : {u : unit | Sorted_array.occurs source value})
  | Some _ -> ();;
[%%expect{|
Line 3, characters 13-15:
3 |   | None -> (() : {u : unit | Sorted_array.occurs source value})
                 ^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 30-62:
3 |   | None -> (() : {u : unit | Sorted_array.occurs source value})
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let invalid_insert_position (source : Sorted_array.t) (value : int) =
  let position, _ = Sorted_array.insert source value in
  (() : {u : unit | position = 0});;
[%%expect{|
Line 3, characters 3-5:
3 |   (() : {u : unit | position = 0});;
       ^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 20-32:
3 |   (() : {u : unit | position = 0});;
                        ^^^^^^^^^^^^
  The refinement is stated here.
|}]
