(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_int_sequence.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.byte;
 module = "vox_int_sequence.mli";
 ocamlc.byte;
 expect;
*)

#directory "ocamlc.byte";;

let invalid_representation : Vox_int_sequence.multiset = [2; 1];;
[%%expect{|
Line 1, characters 57-63:
1 | let invalid_representation : Vox_int_sequence.multiset = [2; 1];;
                                                             ^^^^^^
Error: This constructor has type "'a list"
       but an expression was expected of type "Vox_int_sequence.multiset"
|}]

let invalid_permutation (left : int list) (right : int list) =
  let (same @ total) : (target : int) ->
      {u : unit | Vox_int_sequence.count left target ===
        Vox_int_sequence.count right target} = fun target ->
    let u = () in refine_ u in
  let refine_ result = Vox_int_sequence.count_extensional left right same in
  ();;
[%%expect{|
Line 5, characters 18-27:
5 |     let u = () in refine_ u in
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_count (before : int list) (after : int list) (target : int) =
  let refine_ equality = Vox_int_sequence.permutation_count before after target in
  let u : {u : unit | Vox_int_sequence.count before target ===
    Vox_int_sequence.count after target} = let u = () in refine_ u in
  let refine_ result = u in
  ();;
[%%expect{|
Line 4, characters 57-66:
4 |     Vox_int_sequence.count after target} = let u = () in refine_ u in
                                                             ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
