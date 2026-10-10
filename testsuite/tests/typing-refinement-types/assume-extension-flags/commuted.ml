(* TEST
 readonly_files = "labelled.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "labelled.mli";
 ocamlc.byte;
 flags = "-extension refinement_types -nolabels -noassert";
 expect;
*)

#directory "ocamlc.byte";;

let x = 42
let checked : Labelled.checked = assume_ x;;
[%%expect{|
val x : int = 42
val checked : Labelled.checked = 42
|}]
