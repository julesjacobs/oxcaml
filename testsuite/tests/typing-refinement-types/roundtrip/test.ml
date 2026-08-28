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

let pattern_predicates :
    { value : int |
      match { Roundtrip_defs.x = value; y = value } with
      | { x = field; y = 0 } -> field = value
      | { x = 0; y = field } -> field = value
      | _ -> true } list =
  ([] : Roundtrip_defs.pattern_predicates list)

let open_pattern_predicate :
    { value : int |
      match { Roundtrip_defs.x = value; y = value } with
      | { x = field; _ } -> field = value } list =
  ([] : Roundtrip_defs.open_pattern_predicate list)

let or_predicate :
    { value : int |
      match Roundtrip_defs.Pair (value, value) with
      | Roundtrip_defs.Pair (0, field)
      | Roundtrip_defs.Pair (field, 0) -> field = value
      | Roundtrip_defs.Pair (_, _) -> true } list =
  ([] : Roundtrip_defs.or_predicate list);;
[%%expect{|
val pattern_predicates :
  {value : int
    | match { Roundtrip_defs.x = value; Roundtrip_defs.y = value } with
      | { Roundtrip_defs.x = field; Roundtrip_defs.y = 0 } -> field = value
      | { Roundtrip_defs.x = 0; Roundtrip_defs.y = field } -> field = value
      | _ -> true}
  list = []
val open_pattern_predicate :
  {value : int
    | match { Roundtrip_defs.x = value; Roundtrip_defs.y = value } with
      | { Roundtrip_defs.x = field;_} -> field = value}
  list = []
val or_predicate :
  {value : int
    | match Roundtrip_defs.Pair (value, value) with
      | Roundtrip_defs.Pair (0, field) | Roundtrip_defs.Pair (field, 0) ->
          field = value
      | Roundtrip_defs.Pair (_, _) -> true}
  list = []
|}]
