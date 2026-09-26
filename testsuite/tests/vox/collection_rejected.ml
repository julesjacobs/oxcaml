(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_int_sequence.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "vox_int_sequence.mli";
 ocamlc.opt;
 expect;
*)

#directory "ocamlc.opt";;

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
    let u = () in u in
  let _ = Vox_int_sequence.count_extensional left right same in
  ();;
[%%expect{|
Line 5, characters 18-19:
5 |     let u = () in u in
                      ^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_count (before : int list) (after : int list) (target : int) =
  let _ = Vox_int_sequence.permutation_count before after target in
  let u : {u : unit | Vox_int_sequence.count before target ===
    Vox_int_sequence.count after target} = let u = () in u in
  let _ = u in
  ();;
[%%expect{|
Line 4, characters 57-58:
4 |     Vox_int_sequence.count after target} = let u = () in u in
                                                             ^
Error: Refinement could not be proved (counterexample)
|}]
