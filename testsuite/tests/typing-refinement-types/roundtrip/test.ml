(* TEST
 flags = "-extension refinement_types";
 readonly_files = "roundtrip_defs.mli";
 setup-ocamlc.byte-build-env;
 module = "roundtrip_defs.mli";
 ocamlc.byte;
 expect;
*)

#directory "ocamlc.byte";;

let l : { y : int | Roundtrip_defs.positive y } list =
  ([] : Roundtrip_defs.nat list);;
[%%expect{|
val l : {y : int | Roundtrip_defs.positive y} list = []
|}]
