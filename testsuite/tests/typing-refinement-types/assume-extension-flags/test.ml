(* TEST
 readonly_files = "producer.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -extension small_numbers -nolabels";
 module = "producer.mli";
 ocamlc.byte;
 flags = "-extension refinement_types -noassert -strict-sequence -no-extension immutable_arrays -no-extension layouts -no-extension small_numbers";
 expect;
*)

#directory "ocamlc.byte";;

let x = 0
let checked : Producer.checked = assume_ x
let sequenced : Producer.sequenced = assume_ x
let labels : Producer.labels = assume_ x
let result = let refine_ result = checked in result;;
[%%expect{|
val x : int = 0
val checked : Producer.checked = 0
val sequenced : Producer.sequenced = 0
val labels : Producer.labels = 0
val result : int = 0
|}]

let x = 1
let failed =
  match let checked : Producer.checked = assume_ x in ignore checked with
  | () -> false
  | exception Assert_failure _ -> true;;
[%%expect{|
val x : int = 1
val failed : bool = true
|}]

let forbidden = [: x :];;
[%%expect{|
Line 1, characters 16-23:
1 | let forbidden = [: x :];;
                    ^^^^^^^
Error: The extension "immutable_arrays" is disabled and cannot be used
|}]

let forbidden_sequence = 1; true;;
[%%expect{|
Line 1, characters 25-26:
1 | let forbidden_sequence = 1; true;;
                             ^
Error: The constant "1" has type "int" but an expression was expected of type
         "unit"
       because it is in the left-hand side of a sequence
|}]

let forbidden_labels = Producer.ignore_labelled (fun x -> x);;
[%%expect{|
Line 1, characters 48-60:
1 | let forbidden_labels = Producer.ignore_labelled (fun x -> x);;
                                                    ^^^^^^^^^^^^
Error: This function should have type "label:int -> int"
       but its first argument is unlabeled instead of being labeled "~label"
|}]
