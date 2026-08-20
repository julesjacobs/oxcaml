(* TEST
 readonly_files = "roundtrip_defs.mli";
 setup-ocamlc.byte-build-env;
 module = "roundtrip_defs.mli";
 ocamlc.byte;
 expect;
*)

(* Write a refined signature to a .cmi, read it back, print it.  This cheaply
   guards printed written structure and identity keys.  Stored node types are
   intentionally absent from printing and are checked by the raw-CMI test. *)

#directory "ocamlc.byte";;

#show Roundtrip_defs;;
[%%expect{|
module Roundtrip_defs :
  sig
    type nat = int{ _ >= 0 }
    type dep = x:int{ x > 0 } -> int{ _ >= x }
    val sub : s:string -> int{ _ < (String.length s) } -> char
    val labelled : ~x:int{ x > 0 } -> unit
    type wf = { size : int{ _ >= 0 }; }
    type pos = Pos of int{ _ > 0 }
    val positive : int -> bool
    type p = int{ positive _ }
    type fr1 = { sel : int; }
    type fr2 = { sel : bool; }
    type selected = fr1{ _.sel > 0 }
    type fv1 = C of int
    type fv2 = C of bool
    type chosen = fv1{ _ = (C 1) }
  end
|}]

(* The imported types are the same types: unification across the .cmi,
   with binders freshened on import *)
let l : (x:int{ x > 0 } -> int{ _ >= x }) list = ([] : Roundtrip_defs.dep list);;
[%%expect{|
val l : (x:int{ x > 0 } -> int{ _ >= x }) list = []
|}]

let l : int{ _ >= 0 } list = ([] : Roundtrip_defs.nat list);;
[%%expect{|
val l : int{ _ >= 0 } list = []
|}]

(* A same-signature value reference is prefixed on import *)
let l : int{ Roundtrip_defs.positive _ } list = ([] : Roundtrip_defs.p list);;
[%%expect{|
val l : int{ Roundtrip_defs.positive _ } list = []
|}]

(* Typed-mirror identities: the consumer's environment shadows the label
   and constructor names with different owners, but the imported
   predicate's identities are the producer's, and the consumer's fresh
   elaboration of the same source disambiguates by the payload type to
   the same identities. *)
type shadow_rec = { sel : string };;
type shadow_var = C of string;;
[%%expect{|
type shadow_rec = { sel : string; }
type shadow_var = C of string
|}]

let l : Roundtrip_defs.selected list =
  ([] : Roundtrip_defs.fr1{ _.sel > 0 } list);;
[%%expect{|
val l : Roundtrip_defs.selected list = []
|}]

let l : Roundtrip_defs.chosen list =
  ([] : Roundtrip_defs.fv1{ _ = C 1 } list);;
[%%expect{|
Line 2, characters 32-33:
2 |   ([] : Roundtrip_defs.fv1{ _ = C 1 } list);;
                                    ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val l : Roundtrip_defs.chosen list = []
|}]
