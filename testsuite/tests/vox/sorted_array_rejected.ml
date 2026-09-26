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
  let refine_ source = Sorted_array.empty in
  let index = 0 in
  let u = () in
  let refine_ result = Sorted_array.remove_at source index (refine_ u) in
  ignore result;;
[%%expect{|
Line 5, characters 59-70:
5 |   let refine_ result = Sorted_array.remove_at source index (refine_ u) in
                                                               ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "sorted_array.mli", line 29, characters 14-55:
  The refinement is stated here.
|}]

let invalid_search_result (source : Sorted_array.t) (value : int) =
  let refine_ result = Sorted_array.find_first source value in
  match result with
  | None ->
    let u = () in
    let proof : {u : unit | Sorted_array.occurs source value} = refine_ u in
    let refine_ proof = proof in
    ()
  | Some _ -> ();;
[%%expect{|
Line 6, characters 64-73:
6 |     let proof : {u : unit | Sorted_array.occurs source value} = refine_ u in
                                                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 6, characters 28-60:
6 |     let proof : {u : unit | Sorted_array.occurs source value} = refine_ u in
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let invalid_insert_position (source : Sorted_array.t) (value : int) =
  let refine_ pair = Sorted_array.insert source value in
  let (position : int), (_ : Sorted_array.t) = pair in
  let u = () in
  let proof : {u : unit | position = 0} = refine_ u in
  let refine_ proof = proof in
  ();;
[%%expect{|
Line 5, characters 42-51:
5 |   let proof : {u : unit | position = 0} = refine_ u in
                                              ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 26-38:
5 |   let proof : {u : unit | position = 0} = refine_ u in
                              ^^^^^^^^^^^^
  The refinement is stated here.
|}]
