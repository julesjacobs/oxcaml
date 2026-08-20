(* TEST
 readonly_files = "roundtrip_defs.mli";
 setup-ocamlc.opt-build-env;
 module = "roundtrip_defs.mli";
 ocamlc.opt;
 expect;
*)

(* Write a refined signature to a .cmi, read it back, print it.  This cheaply
   guards printed written structure and identity keys.  Stored node types are
   intentionally absent from printing and are checked by the raw-CMI test. *)

#directory "ocamlc.opt";;

#show Roundtrip_defs;;
[%%expect{|
module Roundtrip_defs :
  sig
    type nat = int{ _ >= 0 }
    type dep = x:int{ x > 0 } -> int{ _ >= x }
    val total_length : string -> int @@ total
    val sub : s:string -> int{ _ < (total_length s) } -> char
    val labelled : ~x:int{ x > 0 } -> unit
    type wf = { size : int{ _ >= 0 }; }
    type pos = Pos of int{ _ > 0 }
    val positive : int -> bool @@ total
    type p = int{ positive _ }
    type fr1 = { sel : int; }
    type fr2 = { sel : bool; }
    type selected = fr1{ _.sel > 0 }
    type fv1 = C of int
    type fv2 = C of bool
    type chosen = fv1{ let _v = if true then _ else C 1 in true }
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
  ([] : Roundtrip_defs.fv1{
    let _v = if true then _ else C 1 in
    true
  } list);;
[%%expect{|
Line 3, characters 33-34:
3 |     let _v = if true then _ else C 1 in
                                     ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val l : Roundtrip_defs.chosen list = []
|}]
