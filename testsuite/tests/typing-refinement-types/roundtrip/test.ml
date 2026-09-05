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

let unpacking :
    { z : int | let refine_ one = Roundtrip_defs.one in true } list =
  ([] : Roundtrip_defs.unpacking list);;
[%%expect{|
val unpacking : {z : int | let refine_ one = Roundtrip_defs.one in true} list =
  []
|}]

let local_polymorphism :
    { z : int | let _ignored = fun _value -> true in true } list =
  ([] : Roundtrip_defs.local_polymorphism list);;
[%%expect{|
val local_polymorphism : {z : int | let _ignored _value = true in true} list =
  []
|}]
