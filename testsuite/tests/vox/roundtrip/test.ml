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
    val mirror_labelled :
      x:int @ total -> (y:bool @ total -> bool @ total) @ total @@ total
    val mirror_optional :
      ?o:int @ total -> (unit @ total -> bool @ total) @ total @@ total
    val mirror_optional_labelled :
      ?o:int @ total -> (y:bool @ total -> bool @ total) @ total @@ total
    val mirror_positional :
      p:[%call_pos] ->
      (y:bool @ total -> (unit @ total -> bool @ total) @ total) @ total @@
      total
    val mirror_id : bool @ total -> bool @ total @@ total
    val mirror_accepts_format :
      (int -> string, unit, string) format @ total -> bool @ total @@ total
    val mirror_accepts_unlabelled :
      (unit @ total -> bool @ total) @ total -> bool @ total @@ total
    type completion_source = bool{ mirror_labelled ~y:true ~x:0 }
    type completion_wrapper = bool{ mirror_optional ~o:0 () }
    type completion_default = bool{ mirror_optional () }
    type completion_eta_default =
        bool{ mirror_accepts_unlabelled mirror_optional }
    type completion_omitted_optional =
        bool{ let _f = mirror_optional_labelled ~y:true in true }
    type completion_call_pos = bool{ mirror_positional ~y:true () }
    type completion_omitted_position =
        bool{ let _f = mirror_positional ~y:true in true }
    type completion_omitted_required =
        bool{ let _f = mirror_labelled ~y:true in true }
    type primitive_apply = bool{ mirror_id @@ true }
    type primitive_revapply = bool{ true |> mirror_id }
    type format_mirror = bool{ mirror_accepts_format "%d" }
    type _ mirror_gadt = Mirror_int : int mirror_gadt
    type gadt_mirror =
        int{ match (Mirror_int : int mirror_gadt) with | Mirror_int -> _ > 0 }
    type mirror_exists = Mirror_pack : 'a -> mirror_exists
    type existential_mirror =
        bool{ match Mirror_pack 0 with | Mirror_pack _ -> true }
    type mirror_exists_pair = Mirror_pair : 'a * int -> mirror_exists_pair
    type existential_bound_mirror =
        bool{ match Mirror_pair ((), 0) with | Mirror_pair (_, n) -> n = n }
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

(* Re-elaboration in the consumer must agree with the imported mirror, while
   printing retains source order and the original rewrite syntax. *)
let completion_source_roundtrip : Roundtrip_defs.completion_source list =
  ([] : bool{
    Roundtrip_defs.mirror_labelled ~y:true ~x:0
  } list);;
[%%expect{|
val completion_source_roundtrip : Roundtrip_defs.completion_source list = []
|}]

let primitive_apply_roundtrip : Roundtrip_defs.primitive_apply list =
  ([] : bool{ Roundtrip_defs.mirror_id @@ true } list);;
[%%expect{|
val primitive_apply_roundtrip : Roundtrip_defs.primitive_apply list = []
|}]

let primitive_revapply_roundtrip : Roundtrip_defs.primitive_revapply list =
  ([] : bool{ true |> Roundtrip_defs.mirror_id } list);;
[%%expect{|
val primitive_revapply_roundtrip : Roundtrip_defs.primitive_revapply list =
  []
|}]

let format_roundtrip : Roundtrip_defs.format_mirror list =
  ([] : bool{ Roundtrip_defs.mirror_accepts_format "%d" } list);;
[%%expect{|
val format_roundtrip : Roundtrip_defs.format_mirror list = []
|}]

let gadt_roundtrip : Roundtrip_defs.gadt_mirror list =
  ([] : int{
    match
      (Roundtrip_defs.Mirror_int : int Roundtrip_defs.mirror_gadt)
    with
    | Roundtrip_defs.Mirror_int -> _ > 0
  } list);;
[%%expect{|
val gadt_roundtrip : Roundtrip_defs.gadt_mirror list = []
|}]

let existential_roundtrip : Roundtrip_defs.existential_mirror list =
  ([] : bool{
    match Roundtrip_defs.Mirror_pack 0 with
    | Roundtrip_defs.Mirror_pack _ -> true
  } list);;
[%%expect{|
val existential_roundtrip : Roundtrip_defs.existential_mirror list = []
|}]

let existential_bound_roundtrip :
    Roundtrip_defs.existential_bound_mirror list =
  ([] : bool{
    match Roundtrip_defs.Mirror_pair ((), 0) with
    | Roundtrip_defs.Mirror_pair (_, n) -> n = n
  } list);;
[%%expect{|
val existential_bound_roundtrip :
  Roundtrip_defs.existential_bound_mirror list = []
|}]
